package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;

import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.ItemService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

/**
 * Created by Akshay on 27/07/18.
 */
public class CreateProductDirectWorkflowWorkerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String REQUEST_PARAM = "request";
  private static final String PROCESS_CODE_PARAM = "processCode";

  @InjectMocks
  private CreateProductDirectWorkflowWorkerBean createProductDirectWorkflowWorkerBean;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private WorkflowWorker workflowWorker;

  @Mock
  private ItemService itemService;

  @Captor
  private ArgumentCaptor<Map> dataArgumentCapture;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.when(autowireCapableBeanFactory.getBean(Mockito.anyString())).thenReturn(workflowWorker);
    Mockito.doNothing().when(workflowWorker).process(dataArgumentCapture.capture());
    Mockito.doNothing().when(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel1HistoryService, productWfService, workflowWorker, itemService);
  }

  @Test
  public void processWithErrorInBarCodeGeneration() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBusinessPartnerCode(null);
    productCreationRequest.setBusinessPartnerName(null);
    Map<String, Object> datas = new HashMap<>();
    datas.put(REQUEST_PARAM, productCreationRequest);
    datas.put(PROCESS_CODE_PARAM, PROCESS_CODE);
    Map<String, String> map = new HashMap<>();
    createProductDirectWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).updateActivated(Mockito.eq(productCreationRequest.getProductCode()), Mockito.eq(Boolean.TRUE));
    Mockito.verify(workflowWorker).process(Mockito.eq(datas));
    Mockito.verify(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
    Assertions.assertTrue(dataArgumentCapture.getValue().equals(datas));
  }
}
