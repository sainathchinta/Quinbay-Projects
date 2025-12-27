package com.gdn.partners.pbp.workflow.product;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.entity.workflow.product.ProductWfHistory;
import com.gdn.partners.pbp.repository.workflow.ProductWfHistoryRepository;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

public class ProductWorkflowProcessorTest {

  private static final String DEFAULT_FILENAME = "workflow/workflow.properties";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final VerificationMode CALLED_TWICE = Mockito.times(2);
  private static final VerificationMode CALLED_FOUR_TIMES = Mockito.times(4);
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String STATE = "state";
  private static final String DRAFT = "DRAFT";
  private static final String PROCESS_CODE = "processCode";
  private static final String NEED_FOR_CORRECTION_PROCESS_CODE = "RETURN_FOR_CORRECTION";
  private static final String IN_VENDOR = "IN_VENDOR";


  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private ProductWfRepository productWorkflowRepository;

  @Mock
  private ProductWfHistoryRepository productWorkflowHistoryRepository;

  @Mock
  private WorkflowWorker workflowWorker;

  @InjectMocks
  private ProductWorkflowProcessorBean productWorkflowProcessorBean;

  private Properties generateProperties() throws Exception {
    ClassLoader classLoader = this.getClass().getClassLoader();
    Properties properties = new Properties();
    properties.load(classLoader.getResourceAsStream(ProductWorkflowProcessorTest.DEFAULT_FILENAME));
    return properties;
  }

  private ProductWf generateProductWorkflow() throws Exception {
    ProductWf productWorkflow = new ProductWf();
    return productWorkflow;
  }

  private List<ProductWf> generateProductWorkflows() throws Exception {
    List<ProductWf> productWorkflows = new ArrayList<ProductWf>();
    productWorkflows.add(this.generateProductWorkflow());
    return productWorkflows;
  }

  private Map<String, Object> generateDatas() throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put("productCode", ProductWorkflowProcessorTest.DEFAULT_PRODUCT_CODE);
    return datas;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    Properties properties = this.generateProperties();
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    this.productWorkflowProcessorBean =
        new ProductWorkflowProcessorBean(objectMapper, properties.getProperty("workflow.product.configuration"),
            properties.getProperty("workflow.product.worker"));
    MockitoAnnotations.initMocks(this);
    ProductWf productWorkflow = this.generateProductWorkflow();
    List<ProductWf> productWorkflows = this.generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.eq("PRE_TEST"))).thenReturn(productWorkflow);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.eq("PRE_TEST_2"))).thenReturn(null);
    Mockito.doNothing().when(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.when(this.productWorkflowRepository.save(Mockito.any(ProductWf.class))).thenReturn(null);
    Mockito.when(this.productWorkflowHistoryRepository.save(Mockito.any(ProductWfHistory.class))).thenReturn(null);
    Mockito.when(this.autowireCapableBeanFactory.getBean(Mockito.anyString())).thenReturn(this.workflowWorker);
    Mockito.doNothing().when(this.workflowWorker).process(Mockito.anyMap());
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(productWorkflows);
    Mockito.doNothing().when(this.productWorkflowRepository).deleteAll(Mockito.anyList());
    ReflectionTestUtils.setField(productWorkflowProcessorBean, "populateWorkFlowHistory", true);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.autowireCapableBeanFactory);
    Mockito.verifyNoMoreInteractions(this.productWorkflowRepository);
    Mockito.verifyNoMoreInteractions(this.productWorkflowHistoryRepository);
    Mockito.verifyNoMoreInteractions(this.workflowWorker);
  }

  @Test
  public void processTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.any(), Mockito.any(), Mockito.eq(IN_VENDOR)))
        .thenReturn(null);
    this.productWorkflowProcessorBean.process("TEST_1", this.generateDatas());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowProcessorTest.CALLED_TWICE)
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.eq("PRE_TEST"));
    Mockito.verify(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory).getBean(Mockito.anyString());
    Mockito.verify(this.workflowWorker).process(Mockito.anyMap());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.eq(IN_VENDOR));
  }

  @Test
  public void processWithAutoRunNotProcessedTest() throws Exception {
    this.productWorkflowProcessorBean.process("TEST_2", this.generateDatas());
    Mockito.verify(this.productWorkflowRepository, Mockito.times(3))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory).getBean(Mockito.anyString());
    Mockito.verify(this.workflowWorker).process(Mockito.anyMap());
  }

  @Test
  public void processWithAutoRunTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.any(), Mockito.any(), Mockito.eq(IN_VENDOR)))
        .thenReturn(null);
    this.productWorkflowProcessorBean.process("TEST_1", this.generateDatas());
    this.productWorkflowProcessorBean.process("TEST_3", this.generateDatas());
    Mockito.verify(this.productWorkflowRepository, Mockito.times(6))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.eq("PRE_TEST"));
    Mockito.verify(this.productWorkflowRepository, Mockito.times(3)).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository, Mockito.times(3)).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository, Mockito.times(3)).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory, Mockito.times(3)).getBean(Mockito.anyString());
    Mockito.verify(this.workflowWorker, Mockito.times(3)).process(Mockito.anyMap());
    Mockito.verify(this.productWorkflowRepository, Mockito.times(3))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_PRODUCT_CODE),
            Mockito.eq(IN_VENDOR));
  }

  @Test
  public void processWithFailedAutoRunTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.any(), Mockito.any(), Mockito.eq(IN_VENDOR)))
        .thenReturn(null);
    this.productWorkflowProcessorBean.process("TEST_4", this.generateDatas());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowProcessorTest.CALLED_TWICE)
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.eq("PRE_TEST"));
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString(), Mockito.eq("PRE_TEST_2"));
    Mockito.verify(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory).getBean(Mockito.anyString());
    Mockito.verify(this.workflowWorker).process(Mockito.anyMap());
    Mockito.verify(this.productWorkflowRepository, Mockito.times(2))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.eq(IN_VENDOR));
  }

  @Test
  public void processWithAnyCurrentStateTest() throws Exception {
    this.productWorkflowProcessorBean.process("TEST_6", this.generateDatas());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productWorkflowRepository).deleteAll(Mockito.anyList());
    Mockito.verify(this.autowireCapableBeanFactory).getBean(Mockito.anyString());
    Mockito.verify(this.workflowWorker).process(Mockito.anyMap());
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processWithInVendorStateTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString()))
        .thenReturn(new ProductWf(DEFAULT_PRODUCT_CODE, IN_VENDOR));
    Mockito.when(this.productWorkflowHistoryRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, DRAFT))
        .thenReturn(new ProductWfHistory(DEFAULT_PRODUCT_CODE, DRAFT));
    Map<String, Object> datas = generateDatas();
    this.productWorkflowProcessorBean.process(NEED_FOR_CORRECTION_PROCESS_CODE, datas);
    Mockito.verify(this.productWorkflowHistoryRepository)
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, DRAFT);
    Mockito.verify(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory).getBean(NEED_FOR_CORRECTION_PROCESS_CODE + WorkflowWorker.SUFFIX_BEAN_NAME);
    Mockito.verify(this.workflowWorker).process(datas);
    Mockito.verify(this.productWorkflowRepository, Mockito.times(2))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.anyString());
  }

  @Test
  public void processWithInVendorState_PopulateWorkFlowHistory_Test() throws Exception {
    ReflectionTestUtils.setField(productWorkflowProcessorBean, "populateWorkFlowHistory", false);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(new ProductWf(DEFAULT_PRODUCT_CODE, IN_VENDOR));
    Mockito.when(
        this.productWorkflowHistoryRepository.findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID,
            DEFAULT_PRODUCT_CODE, DRAFT)).thenReturn(new ProductWfHistory(DEFAULT_PRODUCT_CODE, DRAFT));
    Map<String, Object> datas = generateDatas();
    this.productWorkflowProcessorBean.process(NEED_FOR_CORRECTION_PROCESS_CODE, datas);
    Mockito.verify(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(NEED_FOR_CORRECTION_PROCESS_CODE + WorkflowWorker.SUFFIX_BEAN_NAME);
    Mockito.verify(this.workflowWorker).process(datas);
    Mockito.verify(this.productWorkflowRepository, Mockito.times(2))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.anyString());
  }

  @Test
  public void processWithInVendorStateValidateFalseTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString()))
        .thenReturn(new ProductWf(DEFAULT_PRODUCT_CODE, IN_VENDOR));
    Mockito.when(this.productWorkflowHistoryRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, DRAFT))
        .thenReturn(new ProductWfHistory(DEFAULT_PRODUCT_CODE, DRAFT));
    Map<String, Object> datas = generateDatas();
    datas.put(Constants.VALIDATE_DRAFT_STATE, false);
    this.productWorkflowProcessorBean.process(NEED_FOR_CORRECTION_PROCESS_CODE, datas);
    Mockito.verify(this.productWorkflowRepository).delete(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowRepository).save(Mockito.any(ProductWf.class));
    Mockito.verify(this.productWorkflowHistoryRepository).save(Mockito.any(ProductWfHistory.class));
    Mockito.verify(this.autowireCapableBeanFactory).getBean(NEED_FOR_CORRECTION_PROCESS_CODE + WorkflowWorker.SUFFIX_BEAN_NAME);
    Mockito.verify(this.workflowWorker).process(datas);
    Mockito.verify(this.productWorkflowRepository, Mockito.times(2))
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.anyString());
  }

  @Test
  public void processWithInVendorStateValidateFalseNullTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, IN_VENDOR))
        .thenReturn(null);
    Mockito.when(this.productWorkflowHistoryRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, DRAFT))
        .thenReturn(new ProductWfHistory(DEFAULT_PRODUCT_CODE, DRAFT));
    Map<String, Object> datas = generateDatas();
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productWorkflowProcessorBean.process(NEED_FOR_CORRECTION_PROCESS_CODE, datas);
      });
    } finally {
      Mockito.verify(this.productWorkflowRepository, Mockito.times(2))
          .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
              Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.anyString());
    }
  }

  @Test()
  public void processWithInVendorStateThrowExceptionTest() throws Exception {
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, IN_VENDOR))
        .thenReturn(new ProductWf(DEFAULT_PRODUCT_CODE, IN_VENDOR));
    Mockito.when(this.productWorkflowHistoryRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, DRAFT))
        .thenReturn(null);
    Map<String, Object> datas = generateDatas();
    try {
      this.productWorkflowProcessorBean.process(NEED_FOR_CORRECTION_PROCESS_CODE, datas);
    } catch (Exception e) {
      Mockito.verify(this.productWorkflowRepository)
          .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
              Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.eq(IN_VENDOR));
      Mockito.verify(this.productWorkflowHistoryRepository)
          .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE, DRAFT);
    }
  }

  @Test
  public void validateTest() throws Exception {
    this.productWorkflowProcessorBean.validate(new ArrayList<String>(), null);
  }

  @Test
  public void completeTest() throws Exception {
    this.productWorkflowProcessorBean.complete(new ArrayList<String>(), null);
  }

  @Test
  public void generateTest() throws Exception {
    this.productWorkflowProcessorBean.generate(new ArrayList<String>(), null);
  }

  @Test
  public void deleteProductAutoApprovalCriteriaByStoreIdAndProductCodeTest() {
    productWorkflowProcessorBean.deleteProductWfHistoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productWorkflowHistoryRepository).deleteByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }
}
