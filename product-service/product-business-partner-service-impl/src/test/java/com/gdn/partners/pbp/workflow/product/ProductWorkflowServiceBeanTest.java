package com.gdn.partners.pbp.workflow.product;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.ProductWfStateResponse;
import com.gda.mta.product.dto.generator.StuckProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductWfState;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.service.EmailNotificationService;
import com.gdn.mta.product.service.FileStorageService;
import com.gdn.mta.product.service.ImageProcessorService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseService;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductWorkflowService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.WorkflowProcessor;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

/**
 * Created by Vishal on 20/07/17.
 *
 */
public class ProductWorkflowServiceBeanTest {


  private static final String flow =
      "{\"UPDATE_REJECTED_PRODUCT\":[\"ANY\"],\"DRAFT\":[],\"IN_VENDOR\":[\"DRAFT\"],\"QC_PASS\":[\"DRAFT\",\"IN_VENDOR\"],\"CONTENT_APPROVAL\":[\"DRAFT\",\"IN_VENDOR\",\"QC_PASS\"],\"CONTENT_APPROVED\":[\"DRAFT\",\"IN_VENDOR\",\"QC_PASS\",\"CONTENT_APPROVAL\"],\"IMAGE_APPROVAL\":[\"DRAFT\",\"IN_VENDOR\",\"QC_PASS\"],\"PROCESS_IMAGE\":[\"DRAFT\",\"IN_VENDOR\",\"QC_PASS\",\"IMAGE_APPROVAL\"],\"IMAGE_APPROVED\":[\"DRAFT\",\"IN_VENDOR\",\"QC_PASS\",\"IMAGE_APPROVAL\",\"PROCESS_IMAGE\"],\"ACTIVE\":[\"DRAFT\",\"IN_VENDOR\",\"QC_PASS\",\"CONTENT_APPROVAL\",\"CONTENT_APPROVED\",\"IMAGE_APPROVAL\",\"PROCESS_IMAGE\",\"IMAGE_APPROVED\"]}";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "MTA-100";
  private static final String PRODUCT_CODE_1 = "MTA-101";
  private static final String NOTES = "Test Note";
  private static final String DEFAULT_WORKFLOW_STATE = "UPDATE_REJECTED_PRODUCT";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_ID = "44fa4b52-7920-45a0-8ef9-3cbe63bd3594";
  private static final Date DEFAULT_CURRENT_TIMESTAMP = Calendar.getInstance().getTime();
  private static final String DEFAULT_ID = "1";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String STATE = WorkflowStates.PROCESS_IMAGE.getValue();
  private static final List<String> STUCK_PRODUCT_STATUS = Collections
      .unmodifiableList(Arrays.asList(WorkflowStates.PROCESS_IMAGE.getValue(),
          WorkflowStates.CONTENT_APPROVAL.getValue(),
          WorkflowStates.CONTENT_APPROVED.getValue(),
          WorkflowStates.IMAGE_APPROVAL.getValue(),
          WorkflowStates.IMAGE_APPROVED.getValue()));
  private static final Integer RETRY_BATCH_SIZE_COUNT = 1;
  private static final Integer DEFAULT_RETRY_BATCH_SIZE_COUNT = 5;
  private static final Integer RETRY_COUNT = 5;
  private static final Integer CRON_JOB_UPDATED_TIME_LIMIT = 30;
  private static final String OLD_PRODUCT_CODE = "oldProductCode";
  private static final String NON_EXISTING_STATE = "NON_EXISTING_STATE";
  private static final String IMAGE_LOCATION_PATH_1 = "productCode/path1";
  private static final String IMAGE_LOCATION_PATH_2 = "productCode/path2";
  private static final String IMAGE_LOCATION_PATH_3 = "productCode/path3";
  private static final String IMAGE_LOCATION_PATH_4 = "productCode/path4";
  private static final String IMAGE_HASH_CODE_1 = "hashCode1";
  private static final String IMAGE_HASH_CODE_2 = "hashCode2";
  private static final String IMAGE_HASH_CODE_3 = "hashCode3";
  private static final String IMAGE_HASH_CODE_4 = "hashCode4";
  private static final String PRODUCT_NAME = "productName";
  private static final String USP = "usp";
  private static final String DESCRIPTION = "description";
  private static final String CATEGORY_CODE = "CAT-0000001";

  private List<ProductWfState> productCodeAndState = new ArrayList<>();
  private ProductWfState productWf = new ProductWfState();
  private Page<ProductWfState> productCodeAndStatePage;
  private List<ProductWfStateResponse> productWfStateResponseList = new ArrayList<>();
  private ProductWfStateResponse productWfStateResponse = new ProductWfStateResponse();
  private List<String> productCodeList;
  private ProductCollection productCollection = new ProductCollection();
  private ActivateImageResponse activateImageResponse = new ActivateImageResponse();
  private ScaleImageResponse scaleImageResponse = new ScaleImageResponse();

  private ProfileResponse profileResponse;
  private ProductImageQcProcessingResponse productImageQcProcessingResponse = new ProductImageQcProcessingResponse();

  @InjectMocks
  private static ProductWorkflowServiceBean productWorkflowServiceBean;

  @Mock
  @Qualifier(value = ProductWorkflowProcessorBean.BEAN_NAME + WorkflowProcessor.SUFFIX_BEAN_NAME)
  private WorkflowProcessor workflowProcessor;

  @Mock
  private ImageProcessorService imageProcessorService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductWorkflowService productWorkflowService;

  @Mock
  private ProductWorkflowRepository oldProductWorkflowRepository;

  @Mock
  private ProductWfRepository productWorkflowRepository;

  @Mock
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Captor
  private  ArgumentCaptor<Map> requestCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Object>> deleteProductMapCaptor;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkResizeImageRequest> bulkResizeImageRequestArgumentCaptor;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private EmailNotificationService emailNotificationService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @BeforeAll
  public static void start() throws Exception {
    productWorkflowServiceBean = new ProductWorkflowServiceBean(new ObjectMapper(), flow);
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put("storeId", STORE_ID);
    ReflectionTestUtils.setField(productWorkflowServiceBean, "stuckProductMaxRetryCount", RETRY_COUNT);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);

    productWf.setProductCode(DEFAULT_PRODUCT_CODE);
    productWf.setState(STATE);
    productCodeAndState.addAll(Collections.singleton(productWf));

    productCodeAndStatePage = new PageImpl<>(productCodeAndState);
    productWfStateResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    productWfStateResponse.setState(STATE);
    productWfStateResponseList.add(productWfStateResponse);

    productCodeList = new ArrayList<>();
    productCodeList.add(DEFAULT_PRODUCT_CODE);

    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    profileResponse =
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(false)
        .build();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(profileResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(workflowProcessor);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(productWorkflowService);
    Mockito.verifyNoMoreInteractions(oldProductWorkflowRepository);
    Mockito.verifyNoMoreInteractions(productWorkflowRepository);
    Mockito.verifyNoMoreInteractions(productCollectionRepository);
    Mockito.verifyNoMoreInteractions(imageProcessorService);
    Mockito.verifyNoMoreInteractions(emailNotificationService);
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void updateRejectedProduct() throws Exception {
    ProductWf productWf = new ProductWf();
    productWf.setState("DELETED");
    List<ProductWf> list = new ArrayList<>();
    list.add(productWf);
    ProductRequest request = new ProductRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setStoreId(STORE_ID);
    productWorkflowServiceBean.updateRejectedProduct(request);
    Mockito.verify(workflowProcessor).process(Mockito.any(), Mockito.anyMap());
    Mockito.verify(productService).updateRejectedProduct(Mockito.any(ProductRequest.class));
  }

  @Test
  public void updateRejectedProduct_whenFailed() throws Exception {
    ProductWf productWf = new ProductWf();
    productWf.setState("DRAFT");
    List<ProductWf> list = new ArrayList<>();
    list.add(productWf);
    ProductRequest request = new ProductRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setStoreId(STORE_ID);
    Mockito.doThrow(new Exception()).when(workflowProcessor)
        .process(Mockito.any(), Mockito.anyMap());
    try {
      productWorkflowServiceBean.updateRejectedProduct(request);
    } catch (Exception e) {
      Mockito.verify(workflowProcessor).process(Mockito.any(), Mockito.anyMap());
    }
  }

  @Test
  public void returnForCorrectionTest() throws Exception {
    productWorkflowServiceBean.returnForCorrection(PRODUCT_CODE, NOTES, new NeedRevisionNotes(), false, false, true);
    verify(this.workflowProcessor).process(eq("RETURN_FOR_CORRECTION"),
        Mockito.anyMap());
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.NEED_CORRECTION), eq(NOTES));
  }

  @Test
  public void returnForCorrectionMultipleProductCodesTest() throws Exception {
    productWorkflowServiceBean.returnForCorrection(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1), NOTES, new NeedRevisionNotes(),
        false);
    verify(this.workflowProcessor, times(2)).process(eq("RETURN_FOR_CORRECTION"),
        Mockito.anyMap());
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.NEED_CORRECTION), eq(NOTES));
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(eq(PRODUCT_CODE_1), eq(ProductStatus.NEED_CORRECTION), eq(NOTES));
  }

  @Test
  public void resubmitTest() throws Exception {
    productWorkflowServiceBean.resubmit(new ProductRequest(), new UpdateProductLevel3Wip());
    Mockito.verify(workflowProcessor).process(Mockito.any(), Mockito.anyMap());
  }

  @Test
  public void productCreateTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setProductCreationType(ProductCreationType.UNIFIED_BULK_UPLOAD);
    Map<String, Object> map = new HashMap<>();
    map.put("request", productCreationRequest);
    map.put("productCode", PRODUCT_CODE);
    map.put("productCreationType", ProductCreationType.UNIFIED_BULK_UPLOAD.getProductCreationType());
    map.put("IS_SKIP_NOTIFICATION", false);
    map.put("MPPFlow", false);
    productWorkflowServiceBean.create(productCreationRequest, false, false);
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.CREATE_PRODUCT.getValue()), Mockito.eq(map));
  }

  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    productDetailResponse.setName("Produk 1");
    productDetailResponse.setImages(Arrays.asList(new Image()));
    productDetailResponse.setId(DEFAULT_ID);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(new Image()));
    Set<ProductItemResponse> productItemResponses = new HashSet<>();
    productItemResponses.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productItemResponses);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(new AttributeResponse());
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(new CategoryResponse());
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    return productDetailResponse;

  }

  @Test
  public void getProductWorkFlowByProductCodesTest() throws Exception{
    List<String> productCodes = new ArrayList<>();
    ProductWf productWorkflow = new ProductWf();
    productWorkflow.setProductCode(DEFAULT_PRODUCT_CODE);
    productWorkflow.setState(DEFAULT_WORKFLOW_STATE);
    List<ProductWf> productWorkflows = new ArrayList<ProductWf>();
    productWorkflows.add(productWorkflow);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    Map<String, List<ProductWf>> productCodeAndProductWfList = new HashMap<>();
    productCodeAndProductWfList.put(DEFAULT_PRODUCT_CODE,productWorkflows);
    Mockito.when(productWorkflowService.getProductWfByProductCodes(Mockito.eq(Arrays.asList(DEFAULT_PRODUCT_CODE)))).thenReturn(productCodeAndProductWfList);
    List<ProductWorkflowStatus> productWorkflowStatusList= productWorkflowServiceBean.getProductWorkFlowByProductCodes(productCodes);
    Mockito.verify(productWorkflowService).getProductWfByProductCodes(Mockito.eq(Arrays.asList(DEFAULT_PRODUCT_CODE)));
    Assertions.assertNotNull(productWorkflowStatusList);
  }

  @Test
  public void getProductWorkFlowByProductCodesWithNonExistingOldState() throws Exception {
    List<String> productCodes = new ArrayList<>();
    List<ProductWf> productWfs = new ArrayList<>();
    ProductWf productWf = new ProductWf();
    productWf.setState(NON_EXISTING_STATE);
    productWf.setProductCode(DEFAULT_PRODUCT_CODE);
    productWfs.add(productWf);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    Map<String, List<ProductWf>> productCodeAndProductWfList = new HashMap<>();
    productCodeAndProductWfList.put(DEFAULT_PRODUCT_CODE, productWfs);
    Mockito.when(productWorkflowService.getProductWfByProductCodes(productCodes))
        .thenReturn(productCodeAndProductWfList);
    List<ProductWorkflowStatus> productWorkflowStatuses =
        productWorkflowServiceBean.getProductWorkFlowByProductCodes(productCodes);
    Mockito.verify(productWorkflowService).getProductWfByProductCodes(productCodes);
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, productWorkflowStatuses.get(0).getProductCode());
    Assertions.assertTrue((productWorkflowStatuses.get(0).getStatus()).isEmpty());
    Assertions.assertEquals(NON_EXISTING_STATE,productWorkflowStatuses.get(0).getStates().get(0));
  }

  @Test
  public void getProductWorkFlowByEmptyProductCodesTest() throws Exception{
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      List<ProductWorkflowStatus> productWorkflowStatusList =
          productWorkflowServiceBean.getProductWorkFlowByProductCodes(new ArrayList<>());
    });
  }

  private List<ProductWorkflow> generateProductWorkflows() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.STORE_ID));
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.STORE_ID));
    return productWorkflows;
  }

  @Test
  public void getProductWorkFlowByProductCodesEmptyProductWorkFlowTest() throws Exception{
    List<String> productCodes = new ArrayList<>();
    ProductWf productWorkflow = new ProductWf();
    ProductDetailResponse response = generateProductDetailResponse();
    productWorkflow.setProductCode(DEFAULT_PRODUCT_CODE);
    productWorkflow.setState(DEFAULT_WORKFLOW_STATE);
    List<ProductWf> productWfs = new ArrayList<ProductWf>();
    productCodes.add(DEFAULT_PRODUCT_CODE);
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Map<String, List<ProductWf>> productCodeAndProductWfList = new HashMap<>();
    productCodeAndProductWfList.put(DEFAULT_PRODUCT_CODE,productWfs);
    Mockito.when(productWorkflowService.getProductWfByProductCodes(Mockito.eq(Arrays.asList(DEFAULT_PRODUCT_CODE)))).thenReturn(productCodeAndProductWfList);
    Mockito.when(productRepository.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE)).thenReturn(response);
    Mockito.when(oldProductWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,DEFAULT_ID)).thenReturn(productWorkflows);
    List<ProductWorkflowStatus> productWorkflowStatuseList = productWorkflowServiceBean.getProductWorkFlowByProductCodes(productCodes);
    Mockito.verify(productWorkflowService).getProductWfByProductCodes(Mockito.eq(Arrays.asList(DEFAULT_PRODUCT_CODE)));
    Mockito.verify(productRepository).findProductDetailByProductCode(DEFAULT_PRODUCT_CODE);
    Mockito.verify(oldProductWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,DEFAULT_ID);
    Assertions.assertNotNull(productWorkflowStatuseList);
  }

  @Test
  public void productCreateDirectTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setProductCreationType(ProductCreationType.UNIFIED_BULK_UPLOAD);
    Map<String, Object> map = new HashMap<>();
    map.put(GdnBaseLookup.REQUEST, productCreationRequest);
    map.put(GdnBaseLookup.PRODUCT_CODE, PRODUCT_CODE);
    map.put("productCreationType", ProductCreationType.UNIFIED_BULK_UPLOAD.getProductCreationType());
    map.put("IS_SKIP_NOTIFICATION", false);
    map.put("MPPFlow", false);
    Mockito.doNothing().when(workflowProcessor).process(Mockito.any(), requestCaptor.capture());
    productWorkflowServiceBean.create(productCreationRequest, false, false);
    Map requestParam = requestCaptor.getValue();
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.CREATE_PRODUCT_DIRECT.getValue()), Mockito.eq(map));
    Assertions.assertEquals(map.get(GdnBaseLookup.REQUEST), requestParam.get(GdnBaseLookup.REQUEST));
    Assertions.assertEquals(map.get(GdnBaseLookup.PRODUCT_CODE), requestParam.get(GdnBaseLookup.PRODUCT_CODE));
    Assertions.assertEquals(map, requestParam);
  }

  @Test
  public void getStuckProductsTest() throws Exception {
    Mockito.doReturn(productCodeAndStatePage).when(productWorkflowRepository)
        .getProductCodesByState(Mockito.anyInt(), Mockito.any(Date.class), Mockito.any());
    Mockito.when(productWorkflowRepository.getProductAboveCronJobRetryCount(Mockito.any(), Mockito.anyInt()))
        .thenReturn(productCodeAndState);
    StuckProductResponse stuckProductResponse =
        productWorkflowServiceBean.getStuckProducts(RETRY_BATCH_SIZE_COUNT, CRON_JOB_UPDATED_TIME_LIMIT);
    Mockito.verify(productWorkflowRepository)
        .getProductCodesByState(Mockito.anyInt(), Mockito.any(Date.class), Mockito.any());
    Mockito.verify(productCollectionRepository, Mockito.times(1)).updateCronJobRetryCount(Mockito.any(), Mockito.anyList());
    Mockito.verify(productWorkflowRepository).getProductAboveCronJobRetryCount(Mockito.any(), Mockito.anyInt());
    Mockito.verify(emailNotificationService).sendProductStuckAlertMail(productCodeAndState, RETRY_BATCH_SIZE_COUNT);
    Assertions.assertEquals(productWfStateResponseList.get(0).getProductCode(),
        stuckProductResponse.getProductWfStateResponseList().get(0).getProductCode());
  }

  @Test
  public void getStuckProductsZeroProductsTest() throws Exception {
    List<ProductWfState> productCodeAndState = new ArrayList<>();
    productCodeAndStatePage = new PageImpl<>(productCodeAndState);
    Mockito.when(productWorkflowRepository
        .getProductCodesByState(eq(RETRY_COUNT), Mockito.any(), eq(PageRequest.of(0, RETRY_BATCH_SIZE_COUNT))))
        .thenReturn(productCodeAndStatePage);
    Mockito.when(productWorkflowRepository.getProductAboveCronJobRetryCount(STORE_ID, RETRY_COUNT))
        .thenReturn(productCodeAndState);
    productWorkflowServiceBean.getStuckProducts(RETRY_BATCH_SIZE_COUNT, CRON_JOB_UPDATED_TIME_LIMIT);
    Mockito.verify(productWorkflowRepository)
        .getProductCodesByState(eq(RETRY_COUNT), Mockito.any(), eq(PageRequest.of(0, RETRY_BATCH_SIZE_COUNT)));
    Mockito.verify(productWorkflowRepository).getProductAboveCronJobRetryCount(STORE_ID, RETRY_COUNT);
    Mockito.verify(emailNotificationService).sendProductStuckAlertMail(productCodeAndState, RETRY_BATCH_SIZE_COUNT);
  }

  @Test
  public void productCreateAndDeleteOldProductTest() throws Exception {
    List<ProductWf> productWfs = Arrays.asList(new ProductWf());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setOldProductCode(OLD_PRODUCT_CODE);
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setProductCreationType(ProductCreationType.UNIFIED_BULK_UPLOAD);
    Map<String, Object> map = new HashMap<>();
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, productCreationRequest.getOldProductCode()))
        .thenReturn(productWfs);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, productCreationRequest.getOldProductCode()))
        .thenReturn(new ProductCollection());
    Mockito.doNothing().when(workflowProcessor).process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()),
        deleteProductMapCaptor.capture());
    map.put(GdnBaseLookup.REQUEST, productCreationRequest);
    map.put(GdnBaseLookup.PRODUCT_CODE, PRODUCT_CODE);
    map.put("productCreationType", ProductCreationType.UNIFIED_BULK_UPLOAD.getProductCreationType());
    map.put("IS_SKIP_NOTIFICATION", false);
    map.put("MPPFlow", false);
    Mockito.doNothing().when(workflowProcessor).process(
        Mockito.eq(WorkflowProcessCode.CREATE_PRODUCT_DIRECT.getValue()), requestCaptor.capture());
    productWorkflowServiceBean.create(productCreationRequest, false, false);
    Map requestParam = requestCaptor.getValue();
    Mockito.verify(workflowProcessor)
        .process(Mockito.eq(WorkflowProcessCode.CREATE_PRODUCT_DIRECT.getValue()), Mockito.eq(map));
    Mockito.verify(workflowProcessor).process(Mockito.eq(WorkflowProcessCode.DELETE.getValue()), Mockito.anyMap());
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, productCreationRequest.getOldProductCode());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, productCreationRequest.getOldProductCode());
    Assertions.assertEquals(map.get(GdnBaseLookup.REQUEST), requestParam.get(GdnBaseLookup.REQUEST));
    Assertions.assertEquals(map.get(GdnBaseLookup.PRODUCT_CODE), requestParam.get(GdnBaseLookup.PRODUCT_CODE));
    Assertions.assertEquals(map, requestParam);
    Assertions.assertEquals(productCreationRequest.getOldProductCode(),
        deleteProductMapCaptor.getValue().get(GdnBaseLookup.PRODUCT_CODE));
    Assertions.assertEquals(3, deleteProductMapCaptor.getValue().size());
  }

  @Test
  public void getStuckProductsRetryCountGreaterThanStuckProductCountTest() throws Exception {
    Mockito.doReturn(productCodeAndStatePage).when(productWorkflowRepository)
        .getProductCodesByState(Mockito.anyInt(), Mockito.any(Date.class), Mockito.any());
    Mockito.when(productWorkflowRepository.getProductAboveCronJobRetryCount(Mockito.any(), Mockito.anyInt()))
        .thenReturn(productCodeAndState);
    StuckProductResponse stuckProductResponse =
        productWorkflowServiceBean.getStuckProducts(DEFAULT_RETRY_BATCH_SIZE_COUNT, CRON_JOB_UPDATED_TIME_LIMIT);
    Mockito.verify(productWorkflowRepository)
        .getProductCodesByState(Mockito.anyInt(), Mockito.any(Date.class), Mockito.any());
    Mockito.verify(productCollectionRepository, Mockito.times(1)).updateCronJobRetryCount(Mockito.any(), Mockito.anyList());
    Mockito.verify(productWorkflowRepository).getProductAboveCronJobRetryCount(Mockito.any(), Mockito.anyInt());
    Assertions.assertEquals(productWfStateResponseList.get(0).getProductCode(),
        stuckProductResponse.getProductWfStateResponseList().get(0).getProductCode());
    Mockito.verify(emailNotificationService).sendProductStuckAlertMail(productCodeAndState, DEFAULT_RETRY_BATCH_SIZE_COUNT);
  }

  @Test
  public void updateResubmitCountOnProductResubmissionTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    Mockito.doReturn(new ProductCollection()).when(this.productCollectionRepository)
        .saveAndFlush(Mockito.any(ProductCollection.class));
    this.productWorkflowServiceBean.updateResubmitCountOnProductResubmission(productCollection);
    Mockito.verify(this.productCollectionRepository).saveAndFlush(productCollectionArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productCollectionArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void getStuckProductsRetryCountGreaterTest() throws Exception {
    Mockito.when(productWorkflowRepository
        .getProductCodesByState(eq(RETRY_COUNT), Mockito.any(Date.class),
            eq(PageRequest.of(0, DEFAULT_RETRY_BATCH_SIZE_COUNT)))).thenReturn(productCodeAndStatePage);
    Mockito.when(productWorkflowRepository.getProductAboveCronJobRetryCount(STORE_ID, DEFAULT_RETRY_BATCH_SIZE_COUNT))
        .thenReturn(productCodeAndState);
    StuckProductResponse stuckProductResponse =
        productWorkflowServiceBean.getStuckProducts(DEFAULT_RETRY_BATCH_SIZE_COUNT, CRON_JOB_UPDATED_TIME_LIMIT);
    Mockito.verify(productWorkflowRepository)
        .getProductCodesByState(eq(RETRY_COUNT), Mockito.any(Date.class),
            eq(PageRequest.of(0, DEFAULT_RETRY_BATCH_SIZE_COUNT)));
    Mockito.verify(productCollectionRepository, Mockito.times(1)).updateCronJobRetryCount(STORE_ID, productCodeList);
    Mockito.verify(productWorkflowRepository)
        .getProductAboveCronJobRetryCount(STORE_ID, DEFAULT_RETRY_BATCH_SIZE_COUNT);
    Assertions.assertEquals(productWfStateResponseList.get(0).getProductCode(),
        stuckProductResponse.getProductWfStateResponseList().get(0).getProductCode());
    Mockito.verify(emailNotificationService).sendProductStuckAlertMail(productCodeAndState, DEFAULT_RETRY_BATCH_SIZE_COUNT);
  }

  @Test
  public void getStuckProductsRetryCountEmptyListTest() throws Exception {
    Mockito.when(productWorkflowRepository
        .getProductCodesByState(eq(RETRY_COUNT), Mockito.any(Date.class),
            eq(PageRequest.of(0, DEFAULT_RETRY_BATCH_SIZE_COUNT)))).thenReturn(null);
    StuckProductResponse stuckProductResponse =
        productWorkflowServiceBean.getStuckProducts(DEFAULT_RETRY_BATCH_SIZE_COUNT, CRON_JOB_UPDATED_TIME_LIMIT);
    Mockito.verify(productWorkflowRepository)
        .getProductCodesByState(eq(RETRY_COUNT), Mockito.any(Date.class),
            eq(PageRequest.of(0, DEFAULT_RETRY_BATCH_SIZE_COUNT)));
    Mockito.verify(productWorkflowRepository).getProductAboveCronJobRetryCount(STORE_ID, RETRY_COUNT);
    Mockito.verify(emailNotificationService).sendProductStuckAlertMail(Collections.emptyList(), DEFAULT_RETRY_BATCH_SIZE_COUNT);
  }

  @Test
  public void getProductCollectionByProductCodeTest() {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(new ProductCollection());
    productWorkflowServiceBean.getProductCollectionByProductCode(PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductCollectionByProductCodeExceptionTest() {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(new ProductCollection());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productWorkflowServiceBean.getProductCollectionByProductCode("");
    });
  }

  private ProductDetailResponse getProductDetailResponseWithImages() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setUniqueSellingPoint(USP);
    productDetailResponse.setDescription(DESCRIPTION.getBytes());
    productDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productDetailResponse.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));

    Image image = new Image();
    image.setLocationPath(IMAGE_LOCATION_PATH_1);
    image.setHashCode(IMAGE_HASH_CODE_1);
    image.setOriginalImage(true);
    image.setEdited(true);
    image.setMarkForDelete(false);
    Image image1 = new Image();
    image1.setLocationPath(IMAGE_LOCATION_PATH_2);
    image1.setHashCode(IMAGE_HASH_CODE_2);
    image1.setOriginalImage(true);
    image1.setMarkForDelete(false);
    Image image2 = new Image();
    image2.setLocationPath(IMAGE_LOCATION_PATH_3);
    image2.setHashCode(IMAGE_HASH_CODE_3);
    image2.setOriginalImage(true);
    image2.setEdited(true);
    image2.setMarkForDelete(false);
    Image image3 = new Image();
    image3.setLocationPath(IMAGE_LOCATION_PATH_4);
    image3.setHashCode(IMAGE_HASH_CODE_4);
    image3.setOriginalImage(true);
    image3.setEdited(true);
    image3.setMarkForDelete(true);
    Image image4 = new Image();
    image4.setLocationPath(IMAGE_LOCATION_PATH_4);
    image4.setHashCode(IMAGE_HASH_CODE_4);
    image4.setOriginalImage(false);
    image4.setEdited(true);
    image4.setMarkForDelete(true);
    productDetailResponse.setImages(Arrays.asList(image, image1, image2, image3,image4));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(image, image1));
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setImages(Arrays.asList(image2, image1));
    productDetailResponse
        .setProductItemResponses(new HashSet<>(Arrays.asList(productItemResponse, productItemResponse1)));
    return productDetailResponse;
  }

  @Test
  public void retryResizeEdtiedImagesTest() throws Exception{
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    Mockito.doNothing().when(imageProcessorService).resizeEditedImage(any());
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponse);
    productWorkflowServiceBean.retryResizeEditedImages(PRODUCT_CODE);
    Mockito.verify(this.imageProcessorService).resizeEditedImageInFileStoreOrGcs(bulkResizeImageRequestArgumentCaptor.capture());
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(imageProcessorService).resizeEditedImageInFileStoreOrGcs(any());
    verify(fileStorageService, times(2)).generateFinalImageFullPath(any());
    Assertions.assertEquals(2,bulkResizeImageRequestArgumentCaptor.getValue().getImageRequests().size());
  }

  @Test
  public void retryResizeEdtiedImagesExceptionTest() throws Exception{
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    List<Image> images = productDetailResponse.getImages();
    for(Image image : images){
      image.setEdited(false);
    }
    productDetailResponse.setImages(images);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponse);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productWorkflowServiceBean.retryResizeEditedImages(PRODUCT_CODE);
      });
    }finally {
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    }
  }

  @Test
  public void approveImageForRevisedProductTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
        .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductForceReviewFalseTest() throws Exception {
    ReflectionTestUtils.setField(productWorkflowServiceBean, "overrideForceReview", true);
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(true);
    productImageQcProcessingResponse.setForceReview(true);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productImageQcProcessingResponse);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
        .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductForceReviewFalseSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productWorkflowServiceBean, "overrideForceReview", false);
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(true);
    productImageQcProcessingResponse.setForceReview(true);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
      .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
      .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productImageQcProcessingResponse);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
      SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
      .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
      SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductForceReviewTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(true);
    productImageQcProcessingResponse.setForceReview(false);
    profileResponse.setTrustedSeller(false);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productImageQcProcessingResponse);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
        .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductForceReviewTrueTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productImageQcProcessingResponse.setForceReview(true);
    profileResponse.setTrustedSeller(false);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productImageQcProcessingResponse);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
        .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductForceReviewForTrustedSellersTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(true);
    productImageQcProcessingResponse.setForceReview(false);
    profileResponse.setTrustedSeller(true);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
      .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
      .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productImageQcProcessingResponse);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
      SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
      .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
      SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductForceReviewTrueForTrustedSellersTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productImageQcProcessingResponse.setForceReview(true);
    profileResponse.setTrustedSeller(true);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
      .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
      .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productImageQcProcessingResponse);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
      SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
      .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
      SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }



  @Test
  public void approveImageForRevisedProductNullImagesTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(PRODUCT_CODE_1);
    doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, null, false, new ArrayList<>());
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, PRODUCT_CODE_1,
      profileResponse, new ArrayList<>());
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(PRODUCT_CODE_1, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProduct_NoNeedTest() throws Exception {
    activateImageResponse.setActive(true);
    productCollection.setProductId("Id");
    productCollection.setStoreId(STORE_ID);
    this.profileResponse.setTrustedSeller(false);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    when(productService.isProductActivationNeeded(STORE_ID, "Id")).thenReturn(null);
    doNothing().when(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productService)
        .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(productService).isProductActivationNeeded(STORE_ID, "Id");
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void approveImageForRevisedProductExceptionTest() throws Exception {
    productCollection.setStoreId(STORE_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productOutbound.getImagesForScalingByProductCode(PRODUCT_CODE))
        .thenReturn(getProductDetailResponseWithImages());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productWorkflowServiceBean
            .approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
                new ArrayList<>());
      });
    } finally {
      verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService)
          .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
      verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
          SaveHistoryConstants.IMAGE_NOT_ACTIVATED_SUCCESSFULLY, null);
      verify(productOutbound).getImagesForScalingByProductCode(PRODUCT_CODE);
      verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void approveImageForRevisedProductExceptionTest_isEmpty() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setImages(null);
    productCollection.setStoreId(STORE_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse)))
        .thenReturn(activateImageResponse);
    when(productOutbound.getImagesForScalingByProductCode(PRODUCT_CODE)).thenReturn(productDetailResponse);
    productWorkflowServiceBean.approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, Arrays.asList(scaleImageResponse), false,
        new ArrayList<>());
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .updateActiveImagesAndGetActivateImageResponse(PRODUCT_CODE, Arrays.asList(scaleImageResponse));
    verify(productService).saveProductHistory(STORE_ID, PRODUCT_CODE, Constants.DEFAULT_USERNAME,
        SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
    verify(productOutbound).getImagesForScalingByProductCode(PRODUCT_CODE);
    verify(productService).isProductActivationNeeded(STORE_ID, null);
    verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
    verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void deleteAllExistingWorkFlowAndCreateDraftStateTest() {
    productWorkflowServiceBean.deleteAllExistingWorkFlowAndCreateNewState(STORE_ID, DEFAULT_PRODUCT_CODE,
        WorkflowStates.DRAFT.getValue());
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE);
    ProductWf productWf = new ProductWf(DEFAULT_PRODUCT_CODE, WorkflowStates.DRAFT.getValue());
    productWf.setStoreId(STORE_ID);
    Mockito.verify(productWorkflowRepository).save(productWf);
  }

  @Test
  public void deleteAllExistingWorkFlowAndCreateDraftStateNotEmptyTest() {
    ProductWf productWf = new ProductWf(DEFAULT_PRODUCT_CODE, WorkflowStates.DRAFT.getValue());
    productWf.setStoreId(STORE_ID);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(Collections.singletonList(productWf));
    productWorkflowServiceBean.deleteAllExistingWorkFlowAndCreateNewState(STORE_ID, DEFAULT_PRODUCT_CODE,
        WorkflowStates.DRAFT.getValue());
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(productWorkflowRepository).deleteAll(Collections.singletonList(productWf));
    Mockito.verify(productWorkflowRepository).save(productWf);
  }
}
