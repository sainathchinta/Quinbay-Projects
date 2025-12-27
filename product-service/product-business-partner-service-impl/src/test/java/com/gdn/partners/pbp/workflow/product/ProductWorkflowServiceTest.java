package com.gdn.partners.pbp.workflow.product;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

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
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductReviewStatus;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.repository.SequenceRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductWorkflowService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.publisher.Publisher;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.WorkflowProcessor;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.entity.Product;

public class ProductWorkflowServiceTest {

  private static final Long DEFAULT_SEQUENCE = 1L;
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-0000001";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BUSINESS PARTNER";
  private static final String DEFAULT_PRODUCT_CODE_1 = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_CODE_2 = "MTA-0000002";
  private static final String DEFAULT_FILENAME = "workflow/workflow.properties";
  private static final String DEFAULT_WORKFLOW_STATE = "IN_VENDOR";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final String DEFAULT_NOTES = "test";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_CODE = "productCode";
  private static final String NOTES = "notes";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "APPROVED";
  private static final String ACTIVE_STATE = "ACTIVE";
  private static final String NAME = "name";
  private static final String PRODUCT_ID = "productId";
  private static final String bulkUploadType = "UNIFIED_BULK_UPLOAD";

  private ProductDetailResponse productDetailResponse;
  private ProductCollection productCollection;

  @Mock
  private WorkflowProcessor workflowProcessor;

  @Mock
  private SequenceRepository sequenceRepository;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductWfRepository productWorkflowRepository;

  @Mock
  private ProductWorkflowRepository oldProductWorkflowRepository;

  @Mock
  private ProductWorkflowService productWorkflowService;

  @Mock
  private Publisher publisher;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductService productService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @InjectMocks
  private ProductWorkflowServiceBean productWorkflowServiceBean;

  @Captor
  private ArgumentCaptor<Map<String, Object>> mapArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductDetailResponse> productDetailResponseArgumentCaptor;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  private Properties generateProperties() throws Exception {
    ClassLoader classLoader = this.getClass().getClassLoader();
    Properties properties = new Properties();
    properties.load(classLoader.getResourceAsStream(ProductWorkflowServiceTest.DEFAULT_FILENAME));
    return properties;
  }

  private ProductRequest generateProductRequest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    return productRequest;
  }

  private Product generateProduct() throws Exception {
    Product product = new Product();
    product.setProductCode(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    return product;
  }

  private ProductWf generateProductWorkflow() throws Exception {
    ProductWf productWorkflow = new ProductWf();
    productWorkflow.setState(ProductWorkflowServiceTest.DEFAULT_WORKFLOW_STATE);
    return productWorkflow;
  }

  private List<ProductWf> generateProductWorkflows() throws Exception {
    List<ProductWf> productWorkflows = new ArrayList<ProductWf>();
    productWorkflows.add(this.generateProductWorkflow());
    return productWorkflows;
  }

  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCode(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    productDetailResponse.setReviewPending(true);
    return productDetailResponse;
  }

  private ProductWorkflow generateOldProductWorkflow(Integer state) throws Exception {
    ProductWorkflow productWorkflow = new ProductWorkflow();
    productWorkflow.setState(state);
    return productWorkflow;
  }

  private List<ProductWorkflow> generateOldProductWorkflows() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    productWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_DRAFT));
    productWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_REVIEW_CONTENT));
    productWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_REVIEW_IMAGE));
    productWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_PROCESS_IMAGE));
    productWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_ACTIVE));
    productWorkflows.add(this.generateOldProductWorkflow(-1));
    return productWorkflows;
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    Properties properties = this.generateProperties();
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    this.productWorkflowServiceBean =
        new ProductWorkflowServiceBean(objectMapper, properties.getProperty("workflow.product.flow"));
    MockitoAnnotations.initMocks(this);
    List<ProductWf> productWorkflows = this.generateProductWorkflows();
    productDetailResponse = this.generateProductDetailResponse();
    List<ProductWorkflow> oldProductWorkflows = this.generateOldProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1))).thenReturn(productWorkflows);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2))).thenReturn(new ArrayList<ProductWf>());
    Mockito.when(this.sequenceRepository.findByCode(Mockito.anyString())).thenReturn(
        ProductWorkflowServiceTest.DEFAULT_SEQUENCE);
    Mockito.doNothing().when(this.productLevel1WipService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.eq(bulkUploadType),
            Mockito.any(ProductRequest.class));
    Mockito.doNothing().when(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
    Mockito.doReturn(productCollection).when(this.productLevel1WipService).approveDraft(Mockito.anyString(), Mockito.any());
    Mockito.doNothing().when(this.productWorkflowService).submit(Mockito.anyString(), Mockito.anyString());
    Mockito.doNothing().when(this.publisher).publish(Mockito.anyMap());
    Mockito.doNothing().when(this.productService)
        .approveContent(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.doNothing().when(this.productWorkflowService)
        .processImage(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    Mockito.when(productService.approveImage(Mockito.anyString(), Mockito.anyString(), Mockito.eq(false)))
        .thenReturn(true);
    Mockito.doNothing().when(this.productWorkflowService).rejectProcessImage(Mockito.anyString(), Mockito.anyString());
    Mockito.when(this.productRepository.findProductDetailByProductCode(Mockito.anyString())).thenReturn(
        productDetailResponse);
    Mockito.when(
        this.oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(oldProductWorkflows);
    MDC.put("storeId", STORE_ID);

    productCollection = new ProductCollection();
    productCollection.setActivated(false);
    productCollection.setProductName(NAME);
    productCollection.setViewable(false);
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE_2);
    productCollection.setReviewPending(true);
    productCollection.setProductId(PRODUCT_ID);

    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.anyString()))
        .thenReturn(productCollection);

    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.workflowProcessor);
    Mockito.verifyNoMoreInteractions(this.sequenceRepository);
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.productWorkflowRepository);
    Mockito.verifyNoMoreInteractions(this.oldProductWorkflowRepository);
    Mockito.verifyNoMoreInteractions(this.productWorkflowService);
    Mockito.verifyNoMoreInteractions(this.publisher);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.mandatoryParameterHelper);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void createTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, ProductWorkflowServiceTest.DEFAULT_USERNAME);
    ProductRequest request = this.generateProductRequest();
    this.productWorkflowServiceBean.create(ProductWorkflowServiceTest.DEFAULT_BUSINESS_PARTNER_CODE,
        ProductWorkflowServiceTest.DEFAULT_BUSINESS_PARTNER_NAME, ProductCreationType.UNIFIED_BULK_UPLOAD,request);
    Mockito.verify(this.sequenceRepository).findByCode(Mockito.anyString());
    Mockito.verify(this.productLevel1WipService, ProductWorkflowServiceTest.NEVER_CALLED).create(Mockito.anyString(),
        Mockito.anyString(), Mockito.eq(bulkUploadType), Mockito.any(ProductRequest.class));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, "");
  }

  @SuppressWarnings("unchecked")
  @Test
  public void createWithoutBusinessPartnerCodeTest() throws Exception {
    ProductRequest request = this.generateProductRequest();
    this.productWorkflowServiceBean.create(null, null,ProductCreationType.UNIFIED_BULK_UPLOAD, request);
    Mockito.verify(this.sequenceRepository).findByCode(Mockito.any());
    Mockito.verify(this.productLevel1WipService)
        .create(Mockito.any(), Mockito.any(), Mockito.eq(bulkUploadType), Mockito.any(ProductRequest.class));
    Mockito.verify(this.workflowProcessor, ProductWorkflowServiceTest.NEVER_CALLED).process(Mockito.any(),
        Mockito.anyMap());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void createDirectTest() throws Exception {
    this.productWorkflowServiceBean.createDirect(DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveDraftTest() throws Exception {
    this.productWorkflowServiceBean.approveDraft(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveDraftWithOldWorkflowTest() throws Exception {
    this.productWorkflowServiceBean.approveDraft(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(this.productLevel1WipService).approveDraft(Mockito.anyString(), Mockito.any());
    Mockito.verify(this.productWorkflowService).submit(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.publisher).publish(Mockito.anyMap());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveQCTest() throws Exception {
    this.productWorkflowServiceBean.approveQC(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
  }

  @Test
  public void approveQCWithOldWorkflowTest() throws Exception {
    this.productWorkflowServiceBean.approveQC(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveContentTest() throws Exception {
    this.productWorkflowServiceBean.approveContent(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
  }

  @Test
  public void approveContentWithOldWorkflowTest() throws Exception {
    this.productWorkflowServiceBean.approveContent(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(this.productService).approveContent(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void processImageTest() throws Exception {
    this.productWorkflowServiceBean.processImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
  }

  @Test
  public void processImageWithOldWorkflowTest() throws Exception {
    this.productWorkflowServiceBean.processImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(this.productWorkflowService).processImage(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyBoolean());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveImageTest() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1))).thenReturn(new ProductCollection());
    this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
  }

  @Test
  public void approveImageWithOldWorkflowTest() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2))).thenReturn(new ProductCollection());
    this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(this.productService).approveImage(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
  }

  @Test
  public void approveImageExceptionHandlingTest() throws Exception {
    List<ProductWf> productWorkflows = new ArrayList<>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_CODE)).thenReturn(productWorkflows);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.PRODUCT_CODE))).thenReturn(new ProductCollection());
    Mockito.when(productService.approveImage(STORE_ID, PRODUCT_CODE, false)).thenThrow(new ApplicationException());
    Mockito.doNothing().when(productService).setReviewPendingFlagToTrue(STORE_ID, PRODUCT_CODE);
    this.productWorkflowServiceBean.approveImage(PRODUCT_CODE);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,PRODUCT_CODE);
    Mockito.verify(productService).approveImage(STORE_ID, PRODUCT_CODE, false);
    Mockito.verify(productService).setReviewPendingFlagToTrue(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.PRODUCT_CODE));
  }

  @Test
  public void approveImageSkipReviewTrueTest() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setSkipReview(true);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1))).thenReturn(productCollection);
    this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productService).approveImage(STORE_ID, DEFAULT_PRODUCT_CODE_1, false);
  }


  @SuppressWarnings("unchecked")
  @Test
  public void rejectImageTest() throws Exception {
    this.productWorkflowServiceBean.rejectImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.workflowProcessor).process(Mockito.anyString(), Mockito.anyMap());
  }

  @Test
  public void rejectImageWithOldWorkflowTest() throws Exception {
    this.productWorkflowServiceBean.rejectImage(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(this.productWorkflowService).rejectProcessImage(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void statusTest() throws Exception {
    ProductWorkflowStatus response =
        this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Assertions.assertTrue(response.isReviewPending());
  }

  @Test
  public void statusTest_unknownState() throws Exception {
    ProductWf productWf = new ProductWf();
    productWf.setState("test");
    Mockito.when(this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1)))
        .thenReturn(Arrays.asList(productWf));
    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
  }

  @Test
  public void statusWhenOldWfIsNull() throws Exception {
    Mockito.when(this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(),Mockito.anyString())).thenReturn(null);
    Mockito.when(oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString())).thenReturn(null);

    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_1);

    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.eq(productCollection.getProductId()));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
  }

  @Test
  public void statusWithOldWorkflowTest() throws Exception {
    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void statusWithOldWorkflowAndImageApprovedTest() throws Exception {
    List<ProductWorkflow> oldProductWorkflows = new ArrayList<ProductWorkflow>();
    oldProductWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_REVIEW_CONTENT));
    Mockito.when(
        this.oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(oldProductWorkflows);
    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void statusWithOldWorkflowAndImageProcessedTest() throws Exception {
    List<ProductWorkflow> oldProductWorkflows = new ArrayList<ProductWorkflow>();
    oldProductWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_REVIEW_CONTENT));
    oldProductWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_PROCESS_IMAGE));
    Mockito.when(
        this.oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(oldProductWorkflows);
    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void statusWithOldWorkflowAndContentApprovedTest() throws Exception {
    List<ProductWorkflow> oldProductWorkflows = new ArrayList<ProductWorkflow>();
    oldProductWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_REVIEW_IMAGE));
    Mockito.when(
        this.oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(oldProductWorkflows);
    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void statusWithOldWorkflowAndDraftTest() throws Exception {
    List<ProductWorkflow> oldProductWorkflows = new ArrayList<ProductWorkflow>();
    oldProductWorkflows.add(this.generateOldProductWorkflow(ProductWorkflowLookup.STATE_DRAFT));
    Mockito.when(
        this.oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(oldProductWorkflows);
    this.productWorkflowServiceBean.status(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductWorkflowServiceTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_2);
    Mockito.verify(this.oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteTest_withActivatedFalse() throws Exception {
    List<ProductWf> productWfs = new ArrayList<>();
    productCollection.setViewable(true);
    productCollection.setEdited(true);
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    Mockito.doNothing().when(productService)
        .delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
            Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productService).delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
        Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deleteTest_withViewableFalse() throws Exception {
    List<ProductWf> productWfs = new ArrayList<>();
    productCollection.setActivated(true);
    productCollection.setViewable(false);
    productCollection.setEdited(true);
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    Mockito.doNothing().when(productService)
        .delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
            Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productService).delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
        Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deleteTest_withViewable_ActivatedFalse() throws Exception {
    List<ProductWf> productWfs = new ArrayList<>();
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    Mockito.doNothing().when(productService)
        .delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
            Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productService).delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
        Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deletePostLiveActiveProductTest() throws Exception {
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setPostLive(true);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(Collections.singletonList(new ProductWf(DEFAULT_PRODUCT_CODE_1, ACTIVE_STATE)));
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()), mapArgumentCaptor.capture());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE_1, mapArgumentCaptor.getValue().get(PRODUCT_CODE));
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deletePostLiveActiveProductEditedTrueTest() throws Exception {
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(Collections.singletonList(new ProductWf(DEFAULT_PRODUCT_CODE_1, ACTIVE_STATE)));
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()), mapArgumentCaptor.capture());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE_1, mapArgumentCaptor.getValue().get(PRODUCT_CODE));
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deleteTest_withViewable_ActivatedTrue() throws Exception {
    List<ProductWf> productWfs = new ArrayList<>();
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    try {
      productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    } catch (Exception e) {
      Mockito.verify(productWorkflowRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
              Mockito.eq(DEFAULT_PRODUCT_CODE_1));
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
      verify(mandatoryParameterHelper).getStoreId();
    }
  }

  @Test
  public void deleteTest_withNullCollection() throws Exception {
    List<ProductWf> productWfs = new ArrayList<>();
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setEdited(true);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(Collections.singletonList(new ProductWf(DEFAULT_PRODUCT_CODE_1, ACTIVE_STATE)));
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()), mapArgumentCaptor.capture());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE_1, mapArgumentCaptor.getValue().get(PRODUCT_CODE));
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deleteTest_withNonEmptyProductWfList() throws Exception {
    ProductWf productWf = new ProductWf();
    productWf.setProductCode(DEFAULT_PRODUCT_CODE_1);
    List<ProductWf> productWfs = Arrays.asList(productWf);
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    Mockito.doNothing().when(workflowProcessor).process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()),
        Mockito.anyMap());
    productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()), Mockito.anyMap());
    verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void deleteTest_withProductNotExists() throws Exception {
    List<ProductWf> productWfs = new ArrayList<>();
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(null);
    try {
      productWorkflowServiceBean.delete(DEFAULT_PRODUCT_CODE_1, DEFAULT_NOTES, false);
    } catch (Exception e) {
      Mockito.verify(productWorkflowRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
              Mockito.eq(DEFAULT_PRODUCT_CODE_1));
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
      verify(mandatoryParameterHelper).getStoreId();
    }
  }

  @Test
  public void updateAndPublishTest() throws Exception {
    Product request = this.generateProduct();
    this.productWorkflowServiceBean.updateAndPublish(request, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS);

    Mockito.verify(productService).update(request, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS);
    Mockito.verify(publisher).publish(Mockito.anyMap());
  }

  @Test
  public void deleteByProductCodesTest() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(DEFAULT_PRODUCT_CODE_1);
    List<ProductWf> productWfs = new ArrayList<>();
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productWfs);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE_1, false))
        .thenReturn(new ProductDetailResponse());
    Mockito.doNothing().when(productService)
        .delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
            Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    productWorkflowServiceBean.delete(productCodes, DEFAULT_NOTES);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(productCollectionRepository, Mockito.times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE_1);
    Mockito.verify(productService).delete(Mockito.eq(DEFAULT_PRODUCT_CODE_1), Mockito.anyString(),
        Mockito.eq(ProductReviewStatus.SCREENING_REJECTED.name()));
    Mockito.verify(productService).findProductDetailByProductCode(DEFAULT_PRODUCT_CODE_1, false);
    Mockito.verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), eq(DEFAULT_NOTES));
    verify(mandatoryParameterHelper, times(2)).getStoreId();
  }

  @Test
  public void publishDirectProductCreationDataTest() throws Exception {
    productWorkflowServiceBean.publishDirectProductCreationData(PRODUCT_CODE);
    Mockito.verify(publisher).publish(Mockito.anyMap());
  }

}
