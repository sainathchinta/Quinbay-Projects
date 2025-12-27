package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.ApproveProductResponseDto;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTPermanentDeleteResultEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductCombinedUpdateToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.CategoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectReasonDto;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.service.api.ProductsPermanentDeleteService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.ProductMigration;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationType;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.QuickApprovalResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import com.gdn.x.mta.distributiontask.service.api.ProductMigrationService;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWipService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

public class ProductWrapperServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String VENDOR_CODE = "VENDOR_CODE";
  private static final String APPROVE = "approved";
  private static final String NOTES = "notes";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_CODE = "brandCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ID = "ID";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ACTION_ASSIGNMENT = "assign";
  private static final String PENDING = "PENDING";
  private static final String MAX_NUMBER_OF_DAYS_TO_APPROVE = "10";
  private static final Integer MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE = 10;
  private static final String MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS = "maxNumberOfDaysToApproveAssignedProducts";
  private static final String PASSED = "PASSED";
  private static final String EVENT = "EVENT";
  private static final String RESULT_EVENT = "RESULT_EVENT";
  private static final String SELLER_CODE = "sellerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String MERCHANT_001 = "MERCHANT_001";
  private static final String MERCHANT_002 = "MERCHANT_002";
  private static final String PURE_DISTRIBUTION = "PURE_DISTRIBUTION";
  private static final String DISTRIBUTION = "DISTRIBUTION";
  private static final String NON_DISTRIBUTION = "NON_DISTRIBUTION";
  private static final String TEST_TOPIC = "test-topic";

  @Captor
  private ArgumentCaptor<RejectProductDTO> rejectProductDTOArgumentCaptor;


  private BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel;
  private final Product product = new Product();
  private final RejectProductDTO rejectProductDTO = new RejectProductDTO();
  private final NeedRevisionRequest needRevisionRequest = new NeedRevisionRequest();
  private BulkVendorProductActionsDTO bulkVendorProductActionsDTO;
  private BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO;
  private SystemParameterConfig systemParameterConfigMaxNumberOfDaysToApprove;
  private AutoNeedRevisionRequest autoNeedRevisionRequest;
  private VendorQuickApprovalRequest vendorQuickApprovalRequest;
  private ProductReviewer productReviewer;
  private ProductMigration commonImageMigration;
  RemoveProductRequest removeProductRequest;
  private ApproveProductResponseDto approveProductResponseDto;

  @InjectMocks
  private ProductWrapperServiceImpl productWrapperService;

  @Mock
  private ProductService productService;

  @Mock
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @Mock
  private ProductWipService productWipService;

  @Mock
  private ProductAutoApprovalService productAutoApprovalService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private SolrReindexPublisherService solrReindexPublisherService;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private ProductUtils productUtils;

  @Mock
  private ProductActionRetryService productActionRetryService;

  @Mock
  private ProductMigrationService commonImageMigrationService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private TaskHistoryService taskHistoryService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductsPermanentDeleteService productsPermanentDeleteService;

  @Captor
  private ArgumentCaptor<BulkScreeningProductActionsDTO> bulkScreeningProductActionsDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductMigration> commonImageMigrationArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    brandApprovedOrRejectedDomainEventModel =
        BrandApprovedOrRejectedDomainEventModel.builder().brandApprovalStatus(APPROVE).brandName(BRAND_NAME).build();
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    rejectProductDTO.setProductCode(PRODUCT_CODE);
    needRevisionRequest.setProductCodes(List.of(PRODUCT_CODE));
    bulkVendorProductActionsDTO = new BulkVendorProductActionsDTO();
    bulkScreeningProductActionsDTO = new BulkScreeningProductActionsDTO();
    bulkScreeningProductActionsDTO.setProductCodes(Collections.singletonList(PRODUCT_CODE));
    bulkScreeningProductActionsDTO.setAssignTo(USERNAME);
    bulkScreeningProductActionsDTO.setAssignedBy(USERNAME);
    bulkVendorProductActionsDTO
        .setBulkScreeningProductActionsRequests(Collections.singletonList(bulkScreeningProductActionsDTO));
    systemParameterConfigMaxNumberOfDaysToApprove = new SystemParameterConfig();
    systemParameterConfigMaxNumberOfDaysToApprove.setValue(MAX_NUMBER_OF_DAYS_TO_APPROVE);
    autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    autoNeedRevisionRequest.setProductCode(PRODUCT_CODE);
    vendorQuickApprovalRequest =
        VendorQuickApprovalRequest.builder().vendorCode(VENDOR_CODE).productCode(PRODUCT_CODE).Notes(NOTES).build();
    productReviewer = new ProductReviewer();
    commonImageMigration = new ProductMigration(PRODUCT_CODE, ProductMigrationStatus.PUBLISHED.name(),
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.any())).thenReturn(productReviewer);

    removeProductRequest = new RemoveProductRequest();
    removeProductRequest.setProductCode(PRODUCT_CODE);

    approveProductResponseDto = new ApproveProductResponseDto();
    ReflectionTestUtils.setField(productWrapperService, "permanentDeleteUpdateDateDifference", 30);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productService, approvedProductPublisherService);
    Mockito.verifyNoMoreInteractions(solrVendorCollectionService);
    Mockito.verifyNoMoreInteractions(productWipService);
    Mockito.verifyNoMoreInteractions(productAutoApprovalService);
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(productActionRetryService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(taskHistoryService);
  }

  @Test
   void updateAndApproveProductTest() throws Exception {
    InternalHistoryEventModel eventModel = new InternalHistoryEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setUsername("PDT");
    eventModel.setActivity(WorkflowState.PASSED.getDesc());
    eventModel.setNotes("Approved");
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setReviewType(ReviewType.CONTENT);
    product.setProductCode(PRODUCT_CODE);
    product.setCurrentVendor(new Vendor());
    approveProductResponseDto.setPublishHistoryEvent(true);
    approveProductResponseDto.setWorkFlowState(WorkflowState.PASSED);
    approveProductResponseDto.setReason("Approved");
    approveProductResponseDto.setTaskCode("TASK");
    approveProductResponseDto.setProduct(product);
    Mockito.when(productService.updateAndApproveProduct(anyString(), any(Product.class), anyString(), anyBoolean(),
        any(ProductReviewer.class))).thenReturn(approveProductResponseDto);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(EVENT);
    Mockito.when(
      taskHistoryService.generatePDTHistoryEventModel(anyString(), anyString(), any(), any(),
        anyString(), any(WorkflowState.class), anyString())).thenReturn(new PDTHistoryEventModel());
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTProductVendorApprovedEventModel());
    this.productWrapperService.updateAndApproveProduct(product, VENDOR_CODE, NOTES);
    Mockito.verify(productService).updateAndApproveProduct(anyString(), any(Product.class), anyString(), anyBoolean(),
        Mockito.any(ProductReviewer.class));
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(any(Product.class), anyBoolean());
    Mockito.verify(kafkaTopicProperties, times(2)).getInternalHistoryEventName();
    Mockito.verify(kafkaProducer, times(2))
      .send(Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(taskHistoryService)
      .generatePDTHistoryEventModel(anyString(), anyString(), any(Product.class), any(Vendor.class),
        anyString(), any(WorkflowState.class), anyString());
  }

  @Test
   void updateAndApproveEditedProductTest() throws Exception {
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setReviewType(ReviewType.CONTENT);
    product.setEdited(true);
    approveProductResponseDto.setProduct(product);
    Mockito.when(productService.updateAndApproveProduct(anyString(), any(Product.class), anyString(), anyBoolean(),
        any(ProductReviewer.class))).thenReturn(approveProductResponseDto);
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTProductVendorApprovedEventModel());
    this.productWrapperService.updateAndApproveProduct(product, VENDOR_CODE, NOTES);
    Mockito.verify(productService).updateAndApproveProduct(anyString(), any(Product.class), anyString(), anyBoolean(),
        any(ProductReviewer.class));
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(any(Product.class));
  }

  @Test
   void updateAndApproveProductTest_2() throws Exception {
    Product product = new Product();
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.CONTENT);
    approveProductResponseDto.setProduct(product);
    Mockito.when(productService.updateAndApproveProduct(anyString(), any(), anyString(), anyBoolean(),
        any())).thenReturn(approveProductResponseDto);
    this.productWrapperService.updateAndApproveProduct(product, VENDOR_CODE, NOTES);
    Mockito.verify(productService).updateAndApproveProduct(anyString(), any(), anyString(), anyBoolean(),
        any());
  }

  @Test
   void updateProductDetails() throws Exception {
    Product product = new Product();
    Mockito.when(productService.updateProductDetails(Mockito.any(Product.class), Mockito.any(Product.class)))
        .thenReturn(new Product());
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(anyString(), anyString())).thenReturn(productReviewer);
    productWrapperService.updateProductDetails(product, product);
    Mockito.verify(productService).updateProductDetails(Mockito.any(Product.class), Mockito.any(Product.class));
    Mockito.verify(solrVendorCollectionService).updateSolrOnApprovalOrSave(any(), eq(null), anyString());
  }

  @Test
   void updateEditedProductDetails() throws Exception {
    Product product = new Product();
    Mockito.when(productService.updateEditedProductDetails(Mockito.any(Product.class), Mockito.any(Product.class), anyList()))
        .thenReturn(new Product());
    productWrapperService.updateEditedProductDetails(product, product, new ArrayList<>());
    Mockito.verify(productService).updateEditedProductDetails(Mockito.any(Product.class), Mockito.any(Product.class), anyList());
  }

  @Test
   void updateImageQcResponseByProductCode() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoHealAutoApprovalProductData", true);
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    Mockito.when(productService.updateImageQcResponseByProductCode(Mockito.any(), Mockito.any())).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().savedProduct(new Product()).doPublish(true)
            .internalHistoryEventModel(internalHistoryEventModel).build());
    Mockito.when(productReviewerService.findProductReviewerByProductCode(anyString())).thenReturn(productReviewer);
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
        .thenReturn(product);
    Mockito.when(productService.autoHealProductData(product,  Constants.AUTOHEAL_AUTO_APPROVE)).thenReturn(product);
    productWrapperService.updateImageQcResponseByProductCode(new ImageQcProcessedResponseDomainEvent());
    Mockito.verify(productService)
        .updateImageQcResponseByProductCode(Mockito.any(), Mockito.any());
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.any());
    Mockito.verify(productService).autoHealProductData(product,  Constants.AUTOHEAL_AUTO_APPROVE);
    Mockito.verify(productService).publishAutoApprovalEvents(eq(true), Mockito.any(Product.class));
    Mockito.verify(productService).publishInternalHistoryEventForProduct(internalHistoryEventModel);
  }

  @Test
   void updateImageQcResponseByProductCodeAndCategoryChange() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoHealAutoApprovalProductData", true);
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    Mockito.when(productService.updateImageQcResponseByProductCode(Mockito.any(), Mockito.any())).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().savedProduct(new Product()).doPublish(true)
            .internalHistoryEventModel(internalHistoryEventModel).build());
    Mockito.when(productReviewerService.findProductReviewerByProductCode(anyString())).thenReturn(productReviewer);
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
        .thenReturn(product);
    Mockito.when(productService.autoHealProductData(product,  Constants.AUTOHEAL_AUTO_APPROVE)).thenReturn(product);
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setCategoryCode(PRODUCT_CODE);
    productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    Mockito.verify(productService)
        .updateImageQcResponseByProductCode(Mockito.any(), Mockito.any());
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.any());
    Mockito.verify(productService).autoHealProductData(product,  Constants.AUTOHEAL_AUTO_APPROVE);
    Mockito.verify(productService).publishAutoApprovalEvents(eq(true), Mockito.any(Product.class));
    Mockito.verify(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Mockito.anyString(), Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(productService).publishInternalHistoryEventForProduct(internalHistoryEventModel);
  }

  @Test
   void updateImageQcResponseByProductCodeAndCategoryChangeWithException() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoHealAutoApprovalProductData", true);
    Mockito.when(productService.updateImageQcResponseByProductCode(Mockito.any(), Mockito.any())).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().savedProduct(new Product()).doPublish(true)
            .internalHistoryEventModel(new InternalHistoryEventModel()).build());
    Mockito.when(productReviewerService.findProductReviewerByProductCode(anyString())).thenReturn(productReviewer);
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.autoHealProductData(product,  Constants.AUTOHEAL_AUTO_APPROVE)).thenReturn(product);
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setCategoryCode(PRODUCT_CODE);
    doThrow(ApplicationRuntimeException.class).when(productService)
      .publishAutoApprovalEvents(anyBoolean(), any());
    try {
      productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    }catch (Exception e) {
      Mockito.verify(productService).updateImageQcResponseByProductCode(Mockito.any(), Mockito.any());
      Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
      Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.any());
      Mockito.verify(productService).autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE);
      Mockito.verify(productService).publishAutoApprovalEvents(eq(true), Mockito.any(Product.class));
      Mockito.verify(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Mockito.anyString(), Mockito.anyList(),
          Mockito.anyMap());
    }
  }

  @Test
   void updateImageQcResponseByProductCodeNullProduct() throws Exception {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.anyString()))
        .thenReturn(null);
    productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Mockito.anyString(), Mockito.anyList(), Mockito.anyMap());
  }

  @Test
   void updateImageQcResponseByProductCodeCategoryChangeProduct() throws Exception {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(null);
    imageQcProcessedResponseDomainEvent.setCategoryCode(PRODUCT_CODE);
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.anyString()))
        .thenReturn(null);
    productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Mockito.anyString(), Mockito.anyList(), Mockito.anyMap());
  }

  @Test
   void updateImageQcResponseByProductCodeNullWithNAApprovalProduct() throws Exception {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.NA);
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.anyString()))
        .thenReturn(null);
    productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
  }

  @Test
   void updateImageQcResponseByProductCodestateNullTest() throws Exception {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(anyString())).thenReturn(productReviewer);
    Mockito.when(productService.updateImageQcResponseByProductCode(Mockito.any(), Mockito.any())).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().savedProduct(new Product()).doPublish(false)
            .internalHistoryEventModel(null).build());
    Mockito.when(productService.getDetailsForProductByProductCode(PRODUCT_CODE))
        .thenReturn(product);
    productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    Mockito.verify(productService)
        .updateImageQcResponseByProductCode(Mockito.any(ImageQcProcessedResponseDomainEvent.class), Mockito.any(Product.class));
    Mockito.verify(productService).getDetailsForProductByProductCode(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());  }

  @Test
   void updateImageQcResponseByProductCodeStateNAAndProductTest() throws Exception {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.NA);
    Mockito.when(productService.getDetailsForProductByProductCode(PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(productService.updateImageQcResponseByProductCode(Mockito.any(), Mockito.any())).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().savedProduct(new Product()).doPublish(false)
            .internalHistoryEventModel(null).build());
    productWrapperService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent);
    Mockito.verify(productService).getDetailsForProductByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateImageQcResponseByProductCode(Mockito.any(ImageQcProcessedResponseDomainEvent.class),
            Mockito.any(Product.class));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());  }

  @Test
   void updateBrandApprovalStatusAndUpdateSolrTest() throws Exception {
    Mockito.when(this.productService.updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel))
        .thenReturn(List.of(PRODUCT_CODE));
    Mockito.doNothing().when(this.solrVendorCollectionService)
        .updateSolrOnBrandApprovalAndRejection(List.of(PRODUCT_CODE), BRAND_NAME, APPROVE);
    productWrapperService.updateBrandApprovalStatusAndUpdateSolr(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(this.productService).updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(this.solrVendorCollectionService)
        .updateSolrOnBrandApprovalAndRejection(List.of(PRODUCT_CODE), BRAND_NAME, APPROVE);
    Mockito.verify(productAutoApprovalService).addProductsToAutoApprovalTable(Mockito.anyString()
      , eq(List.of(PRODUCT_CODE)), Mockito.anyMap());
  }

  @Test
   void updateBrandApprovalStatusAndUpdateSolrNotApprovedStatusTest() throws Exception {
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(PENDING);
    Mockito.when(this.productService.updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel))
        .thenReturn(List.of(PRODUCT_CODE));
    Mockito.doNothing().when(this.solrVendorCollectionService)
        .updateSolrOnBrandApprovalAndRejection(List.of(PRODUCT_CODE), BRAND_NAME, PENDING);
    productWrapperService.updateBrandApprovalStatusAndUpdateSolr(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(this.productService).updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(this.solrVendorCollectionService)
        .updateSolrOnBrandApprovalAndRejection(List.of(PRODUCT_CODE), BRAND_NAME, PENDING);
  }

  @Test
   void deleteProductWipAndReindexSolrTest() throws Exception {
    productWrapperService.deleteProductWipAndReindexSolr(PRODUCT_CODE, NOTES);
    Mockito.verify(this.productWipService).deleteProductWip(PRODUCT_CODE, NOTES);
    Mockito.verify(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
  }

  @Test
   void removeProductAndDeleteOriginalImagesTest() throws Exception {
    Mockito.when(this.productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productService).removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    Mockito.doNothing().when(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.doNothing().when(productService).deleteOriginalImagesForProductAndItems(product);
    productWrapperService.removeProductAndDeleteOriginalImages(removeProductRequest);
    Mockito.verify(this.productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(product.getProductCode());
    Mockito.verify(productService).deleteOriginalImagesForProductAndItems(product);
  }

  @Test
   void removeProductAndDeleteOriginalImagesWithStatePassedTest() throws Exception {
    removeProductRequest.setState(PASSED);
    Mockito.when(this.productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productService).removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    Mockito.doNothing().when(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.doNothing().when(productService).deleteOriginalImagesForProductAndItems(product);
    productWrapperService.removeProductAndDeleteOriginalImages(removeProductRequest);
    Mockito.verify(this.productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(product.getProductCode());
    Mockito.verify(productService).deleteOriginalImagesForProductAndItems(product);
  }

  @Test
   void sendProductBackToVendorAndReindexSolrTest() throws Exception {
    Mockito.doNothing().when(productReviewerService).clearExistingReviewDatesDetails(PRODUCT_CODE);
    Mockito.when(productService.sendProductBackToVendor(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(solrVendorCollectionService).updateSolrOnProductSentBackToVendor(product);
    productWrapperService.sendProductBackToVendorAndReindexSolr(PRODUCT_CODE);
    Mockito.verify(productReviewerService).clearExistingReviewDatesDetails(PRODUCT_CODE);
    Mockito.verify(productService).sendProductBackToVendor(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).updateSolrOnProductSentBackToVendor(product);
  }

  @Test
   void rejectProductByVendorAndDeleteFromSolrTest() throws Exception {
    Mockito.doNothing().when(this.productService).rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.doNothing().when(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    productWrapperService.rejectProductByVendorAndDeleteFromSolr(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(this.productService).rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
  }

  @Test
   void rejectionForProductsHavingAutoRejectAsDefinitiveActionsTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoDistributeDefaultVendorCode", VENDOR_CODE);
    ProductActionRetry productActionRetry = new ProductActionRetry();
    productActionRetry.setProductCode(PRODUCT_CODE);
    productActionRetry.setData(NOTES);
    Mockito.doNothing().when(this.productService).rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.doNothing().when(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    productWrapperService.pdtAutoRejectForPendingProducts(Collections.singletonList(productActionRetry));
    Mockito.verify(this.productService)
        .rejectProductByVendor(rejectProductDTOArgumentCaptor.capture(), Mockito.anyString());
    Mockito.verify(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(this.productActionRetryService).updateProductActionRetryDetails(productActionRetry);
    RejectReasonDto rejectReasonDto = rejectProductDTOArgumentCaptor.getValue().getRejectReasonDto();
    Assertions.assertEquals(Collections.singletonList(StringUtils.EMPTY),
        rejectReasonDto.getProduct());
  }

  @Test
   void rejectionForProductsHavingAutoRejectAsDefinitiveActionsExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoDistributeDefaultVendorCode", VENDOR_CODE);
    ProductActionRetry productActionRetry = new ProductActionRetry();
    productActionRetry.setProductCode(PRODUCT_CODE);
    productActionRetry.setData(NOTES);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productService)
            .rejectProductByVendor(Mockito.any(), Mockito.anyString());
    Mockito.doNothing().when(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    try {
      productWrapperService.pdtAutoRejectForPendingProducts(Collections.singletonList(productActionRetry));
    } finally {
      Mockito.verify(this.productService).rejectProductByVendor(Mockito.any(), Mockito.anyString());
      productActionRetry.setStatus(ActionRetryStatus.FAILED);
      Mockito.verify(productActionRetryService).updateProductActionRetryDetails(productActionRetry);
    }
  }

  @Test
   void doProductNeedForCorrectionAndReindexSolrTest() throws Exception {
    Mockito.when(this.productService.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest))
        .thenReturn(new NeedRevisionResponse(true, true, true, 0));
    Mockito.doNothing().when(this.solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    NeedRevisionResponse result = productWrapperService
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(this.productService)
        .doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Assertions.assertTrue(result.isSuccess());
  }

  @Test
   void doProductNeedForCorrectionAndReindexSolrTest_successFalse() throws Exception {
    Mockito.when(this.productService.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest))
        .thenReturn(new NeedRevisionResponse(false, false, false, 1));
    NeedRevisionResponse result = productWrapperService
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(this.productService)
        .doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Assertions.assertFalse(result.isSuccess());
  }

  @Test
   void doProductNeedForCorrectionAndReindexSolr_exceptionWhileReindexTest() throws Exception {
    Mockito.when(this.productService.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest))
        .thenReturn(new NeedRevisionResponse(true, true, true, 0));
    Mockito.doThrow(Exception.class).when(this.solrVendorCollectionService)
        .deleteProductFromSolr(PRODUCT_CODE);
    NeedRevisionResponse result = productWrapperService
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(this.productService)
        .doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Assertions.assertTrue(result.isSuccess());
  }

  @Test
   void bulkUpdateContentAndImageAssigneeTest() throws Exception {
    BulkVendorProductActionsResponse response =
        productWrapperService.bulkUpdateProductAssignee(STORE_ID, bulkVendorProductActionsDTO);
    Mockito.verify(productService).bulkVendorProductAction(eq(STORE_ID), Mockito.any(Date.class),
        bulkScreeningProductActionsDTOArgumentCaptor.capture());
    Mockito.verify(solrVendorCollectionService)
        .updateReviewerByProductCodes(eq(Collections.singletonList(PRODUCT_CODE)), eq(USERNAME),
            Mockito.any(Date.class));
    Assertions.assertEquals(PRODUCT_CODE,
        bulkScreeningProductActionsDTOArgumentCaptor.getValue().getProductCodes().get(0));
    Assertions.assertTrue(
        Objects.isNull(response.getVendorProductActionsResponses().get(0).getProductCode()));
  }

  @Test
   void bulkUpdateContentAndImageAssigneeExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(solrVendorCollectionService)
        .updateReviewerByProductCodes(eq(Collections.singletonList(PRODUCT_CODE)), eq(USERNAME),
            Mockito.any(Date.class));
    BulkVendorProductActionsResponse response =
        productWrapperService.bulkUpdateProductAssignee(STORE_ID, bulkVendorProductActionsDTO);
    Mockito.verify(productService).bulkVendorProductAction(eq(STORE_ID), Mockito.any(Date.class),
        bulkScreeningProductActionsDTOArgumentCaptor.capture());
    Mockito.verify(solrVendorCollectionService)
        .updateReviewerByProductCodes(eq(Collections.singletonList(PRODUCT_CODE)), eq(USERNAME),
            Mockito.any(Date.class));
    Assertions.assertEquals(PRODUCT_CODE,
        bulkScreeningProductActionsDTOArgumentCaptor.getValue().getProductCodes().get(0));
    Assertions.assertEquals(PRODUCT_CODE,
        response.getVendorProductActionsResponses().get(0).getProductCode().get(0));
  }

  @Test
   void updateContentAndImageAssigneeTest() throws Exception {
    Mockito.when(productService.doVendorProductAction(eq(STORE_ID), eq(Collections.singletonList(PRODUCT_CODE)),
        eq(USERNAME), eq(ACTION_ASSIGNMENT),  eq(USERNAME), Mockito.any(Date.class))).thenReturn(new ArrayList<>());
    productWrapperService
        .updateAssigneeDetails(STORE_ID, Collections.singletonList(PRODUCT_CODE), USERNAME, ACTION_ASSIGNMENT, USERNAME);
    Mockito.verify(productService)
        .doVendorProductAction(eq(STORE_ID), eq(Collections.singletonList(PRODUCT_CODE)),
            eq(USERNAME), eq(ACTION_ASSIGNMENT),  eq(USERNAME), Mockito.any(Date.class));
    Mockito.verify(solrVendorCollectionService)
        .updateReviewerByProductCodes(anyList(), eq(USERNAME), Mockito.any(Date.class));
  }

  @Test
   void updateAndApproveRevisedProductTest() throws Exception {
    Product product = new Product();
    product.setRevised(true);
    product.setState(WorkflowState.PASSED);
    product.setReviewType(ReviewType.CONTENT);
    approveProductResponseDto.setProduct(product);
    Mockito.when(productService.updateAndApproveProduct(anyString(), any(Product.class), anyString(), anyBoolean(),
        any(ProductReviewer.class))).thenReturn(approveProductResponseDto);
    Mockito.when(approvedProductPublisherService.publishRevisedVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTRevisedProductVendorApprovedEventModel());
    this.productWrapperService.updateAndApproveProduct(product, VENDOR_CODE, NOTES);
    Mockito.verify(productService).updateAndApproveProduct(anyString(), any(Product.class), anyString(), anyBoolean(),
        any(ProductReviewer.class));
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(any(Product.class), anyBoolean());
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckFailedTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoHealAutoApprovalProductData", true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    Mockito.when(productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(product);
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    Mockito.when(productService.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.FALSE,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE)).thenReturn(Pair.of(AutoApprovalStatus.NA, internalHistoryEventModel));
    Mockito.when(productService.autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE)).thenReturn(product);
    this.productWrapperService.autoApproveOfPendingProductsAfterEligibilityCheck(STORE_ID, PRODUCT_CODE, Boolean.FALSE,
        new AutoApprovalTypeResponse());
    Mockito.verify(productService).getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productService).autoApproveOfPendingProductsAfterEligibilityCheck(product,
        Boolean.FALSE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productService).autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE);
    Mockito.verify(productService).publishInternalHistoryEventForProduct(internalHistoryEventModel);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckFailedButCategoryUpdatedTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoHealAutoApprovalProductData", true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    Mockito.when(productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(product);
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    Mockito.when(productService.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.FALSE,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE)).thenReturn(Pair.of(AutoApprovalStatus.NA, internalHistoryEventModel));
    Mockito.when(productService.autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE)).thenReturn(product);
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setCategoryCode(BRAND_NAME);
    autoApprovalTypeResponse.setCategoryName(BRAND_NAME);
    this.productWrapperService.autoApproveOfPendingProductsAfterEligibilityCheck(STORE_ID, PRODUCT_CODE, Boolean.FALSE,
        autoApprovalTypeResponse);
    Mockito.verify(productService).getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productService).autoApproveOfPendingProductsAfterEligibilityCheck(product,
        Boolean.FALSE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productService).autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE);
    Mockito.verify(productService).updateProduct(product);
    Mockito.verify(productReviewerService).findProductReviewerByProductCode(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productService).publishInternalHistoryEventForProduct(internalHistoryEventModel);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckSuccessTest() throws Exception {
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    Mockito.when(productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(productService.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.TRUE,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE)).thenReturn(Pair.of(AutoApprovalStatus.SUCCESS, null));
    this.productWrapperService.autoApproveOfPendingProductsAfterEligibilityCheck(STORE_ID, PRODUCT_CODE, Boolean.TRUE,
        new AutoApprovalTypeResponse());
    Mockito.verify(productService).autoApproveOfPendingProductsAfterEligibilityCheck(product,
        Boolean.TRUE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productService).getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).autoApprovalReviewerDetailsAndUpdateState(PRODUCT_CODE, WorkflowState.PASSED);
    Mockito.verify(productService).publishAutoApprovalEvents(Boolean.TRUE, product);
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(STORE_ID,
        PRODUCT_CODE, AutoApprovalStatus.SUCCESS, true);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckExceptionTest() throws Exception {
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    Mockito.when(productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(product);
    Mockito.doThrow(Exception.class).when(productService).autoApproveOfPendingProductsAfterEligibilityCheck(product
        , Boolean.TRUE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    this.productWrapperService.autoApproveOfPendingProductsAfterEligibilityCheck(STORE_ID, PRODUCT_CODE, Boolean.TRUE,
        new AutoApprovalTypeResponse());
    Mockito.verify(productService).getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productService).autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.TRUE,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(STORE_ID,
        PRODUCT_CODE, AutoApprovalStatus.FAILED, true);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS);
  }

  @Test
   void updateProductToAutoNeedRevisionTest() throws Exception {
    Mockito.when(productService.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.TRUE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    this.productWrapperService.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.TRUE);
    Mockito.verify(productService).updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.TRUE);
    Mockito.verify(productService).updateProductTaskAndHistory(product, autoNeedRevisionRequest);
    Mockito.verify(productReviewerService).findProductReviewerByProductCode(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(
        Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
  }

  @Test
   void quickApproveProductTest() throws Exception {
    InternalHistoryEventModel eventModel = new InternalHistoryEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setUsername("PDT");
    eventModel.setActivity(WorkflowState.PASSED.getDesc());
    eventModel.setNotes("Approved");
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setProductCode(PRODUCT_CODE);
    product.setCurrentVendor(new Vendor());
    ApproveProductResponseDto approveProductResponseDto = new ApproveProductResponseDto();
    approveProductResponseDto.setPublishHistoryEvent(true);
    approveProductResponseDto.setWorkFlowState(WorkflowState.PASSED);
    approveProductResponseDto.setReason("Approved");
    approveProductResponseDto.setTaskCode("TASK");
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
      .vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
      .approveProductResponseDto(approveProductResponseDto).build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(
      taskHistoryService.generatePDTHistoryEventModel(anyString(), anyString(), any(), any(),
        anyString(), any(WorkflowState.class), anyString())).thenReturn(new PDTHistoryEventModel());
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(EVENT);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTProductVendorApprovedEventModel());
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(any(Product.class), anyBoolean());
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaProducer, times(2))
      .send(Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(taskHistoryService)
      .generatePDTHistoryEventModel(anyString(), anyString(), any(Product.class), any(Vendor.class),
        anyString(), any(WorkflowState.class), anyString());
  }

  @Test
   void quickApproveProductEditedProductTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "replaceEmptyReviewType", true);
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
      .vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
      .approveProductResponseDto(approveProductResponseDto).build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTProductVendorApprovedEventModel());
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(any(Product.class));
  }

  @Test
   void quickApproveProductEditedProductForNullReviewTypeTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "replaceEmptyReviewType", true);
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setEdited(true);
    product.setReviewType(null);
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
      .vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
      .approveProductResponseDto(approveProductResponseDto).build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
      anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
      .thenReturn(new PDTProductVendorApprovedEventModel());
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService)
      .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
        anyBoolean(), any(Product.class));
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(any(Product.class));
    Mockito.verify(productService)
      .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
        anyBoolean(), any(Product.class));
  }

  @Test
   void quickApproveProductEditedProductForNullReviewTypeSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "replaceEmptyReviewType", false);
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setEdited(true);
    product.setReviewType(null);
    QuickApprovalResponse quickApprovalResponse =
      QuickApprovalResponse.builder().product(product).vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
        .build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
      anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
      .thenReturn(new PDTProductVendorApprovedEventModel());
    try{
      Assertions.assertThrows(Exception.class,
        () -> this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest));
    }
    finally {
      Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
      Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
      Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(any(Product.class));

    }
  }

  @Test
   void quickApproveProductNotExistTest() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoSolrReindexingEnabled", true);
    Product product = new Product();
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setState(WorkflowState.PASSED);
    product.setMarkForDelete(true);
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
        .vendorQuickApprovalResponse(VendorQuickApprovalResponse.builder().errorCodes(
            List.of("error")).build())
        .build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
    Mockito.verify(solrReindexPublisherService)
        .publishPDTProductSolrBatchDeleteDomainEventModelForReindex(any(PDTProductSolrDeleteDomainEventModel.class));
  }

  @Test
   void quickApproveProductReindexSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productWrapperService, "autoSolrReindexingEnabled", true);
    Product product = new Product();
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setState(WorkflowState.PASSED);
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
        .vendorQuickApprovalResponse(VendorQuickApprovalResponse.builder().errorCodes(
            List.of("error")).build())
        .build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
        .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
            anyBoolean(), any(Product.class));
  }

  @Test
   void quickApproveProductPassedAndErrorCodeTest() throws Exception {
    Product product = new Product();
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    QuickApprovalResponse quickApprovalResponse =
        QuickApprovalResponse.builder().product(product).vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
            .build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
  }

  @Test
   void quickApproveProductOldTest() throws Exception {
    Product product = new Product();
    product.setState(WorkflowState.PASSED);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
      .vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
      .approveProductResponseDto(approveProductResponseDto).build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    Mockito.when(approvedProductPublisherService.publishVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTProductVendorApprovedEventModel());
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(any(Product.class), anyBoolean());
  }

  @Test
   void quickApproveProductRevisedProductTest() throws Exception {
    Product product = new Product();
    product.setRevised(true);
    product.setState(WorkflowState.PASSED);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    QuickApprovalResponse quickApprovalResponse = QuickApprovalResponse.builder().product(product)
      .vendorQuickApprovalResponse(new VendorQuickApprovalResponse())
      .approveProductResponseDto(approveProductResponseDto).build();
    Mockito.when(productService.getDetailsForProductByProductCode(Mockito.any()))
      .thenReturn(product);
    Mockito.when(productService.quickApproveProduct(anyString(), anyString(),
        anyString(), any(ProductReviewer.class), anyBoolean(), any(Product.class))).thenReturn(quickApprovalResponse);
    Mockito.when(approvedProductPublisherService.publishRevisedVendorApprovedEvent(any(Product.class), anyBoolean()))
        .thenReturn(new PDTRevisedProductVendorApprovedEventModel());
    this.productWrapperService.quickApproveProduct(vendorQuickApprovalRequest);
    Mockito.verify(productService)
        .quickApproveProduct(anyString(), anyString(), anyString(), any(ProductReviewer.class),
          anyBoolean(), any(Product.class));
    Mockito.verify(productService).getDetailsForProductByProductCode(Mockito.anyString());
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(any(Product.class), anyBoolean());
  }

  @Test
   void backfillCommonImageFlagInProductAndItemImagesTest() {
    when(commonImageMigrationService.findProductMigrationByProductCodeAndStatus(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name())).thenReturn(commonImageMigration);
    when(commonImageMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
        commonImageMigration);
    when(productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productUtils).setCommonImageFlagForProductAndItemImages(product);
    when(productService.update(product)).thenReturn(product);

    productWrapperService.backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE);

    verify(commonImageMigrationService).findProductMigrationByProductCodeAndStatus(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name());
    verify(commonImageMigrationService, times(2)).saveProductMigration(Mockito.any(ProductMigration.class));
    verify(productService).getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    verify(productUtils).setCommonImageFlagForProductAndItemImages(product);
    verify(productService).update(product);

    Assertions.assertEquals(ProductMigrationStatus.SUCCESS.name(),
        commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
    public void backfillCommonImageFlagInProductAndItemImagesExceptionTest() throws Exception {
    when(commonImageMigrationService.findProductMigrationByProductCodeAndStatus(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name())).thenReturn(commonImageMigration);
    when(commonImageMigrationService.saveProductMigration(commonImageMigrationArgumentCaptor.capture())).thenReturn(
        commonImageMigration);
    when(productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenThrow(
        ApplicationRuntimeException.class);

    productWrapperService.backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE);

    verify(commonImageMigrationService).findProductMigrationByProductCodeAndStatus(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name());
    verify(commonImageMigrationService, times(2)).saveProductMigration(Mockito.any(ProductMigration.class));
    verify(productService).getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);

    Assertions.assertEquals(ProductMigrationStatus.FAILED.name(),
        commonImageMigrationArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
   void getAllProductDetailsByCodeAndMarkForDeleteFalseTest() throws Exception {
    productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    verify(productService).getAllProductDetailsByCode(PRODUCT_CODE);
    verify(productService).autoHealProductData(null, Constants.AUTOHEAL);
    verify(productService).autoHealProductDistributionTask(null);
  }

  @Test
   void processVendorSearchAutoHealProductInReviewTest() {
    product.setMarkForDelete(false);
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(solrVendorCollectionService).deltaReindexPDTProductSolr(STORE_ID, PRODUCT_CODE);

    productWrapperService.processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).deltaReindexPDTProductSolr(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void processVendorSearchAutoHealProductFinalQcTest() {
    product.setMarkForDelete(false);
    product.setState(WorkflowState.PASSED);

    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);

    productWrapperService.processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
  }

  @Test
   void processVendorSearchAutoHeaApprovedProductTest() {
    product.setMarkForDelete(true);
    product.setState(WorkflowState.PASSED);

    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productServiceRepository).processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);

    productWrapperService.processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productServiceRepository).processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void processVendorSearchAutoHeaNeedCorrectionProductTest() {
    product.setMarkForDelete(true);
    product.setState(WorkflowState.NEED_CORRECTION);

    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);

    productWrapperService.processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
  }

  @Test
  void processVendorSearchAutoHeaNeedCorrectionProduct_nullTest() {

    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(null);

    productWrapperService.processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
  }


  @Test
   void updateBrandInProductAndProductItemsTest() throws Exception {
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest();
    changeBrandRequest.setBrandCode(BRAND_CODE);
    changeBrandRequest.setBrandName(BRAND_NAME);
    changeBrandRequest.setProductCode(PRODUCT_CODE);
    product.setReviewType(ReviewType.CONTENT);
    Mockito.when(productService.updateBrandOfProduct(changeBrandRequest)).thenReturn(product);
    productWrapperService.updateBrandInProductAndProductItems(changeBrandRequest);
    Mockito.verify(productService).updateBrandOfProduct(changeBrandRequest);
    Mockito.verify(solrReindexPublisherService)
        .publishPDTProductApprovalToSolr(any(PDTProductUpdateProductToSolrEventModel.class));
  }

  @Test
   void updateBrandInProductAndProductItemsTestWithProductAsNull() throws Exception {
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest();
    changeBrandRequest.setBrandCode(BRAND_CODE);
    changeBrandRequest.setBrandName(BRAND_NAME);
    changeBrandRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(productService.updateBrandOfProduct(changeBrandRequest)).thenReturn(null);
    productWrapperService.updateBrandInProductAndProductItems(changeBrandRequest);
    Mockito.verify(productService).updateBrandOfProduct(changeBrandRequest);
  }

  @Test
   void testProcessProductsPermanentDelete_PickedForDeletion() {
    product.setPickedForDeletion(true);
    product.setUpdatedDate(new Date());
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    productWrapperService.processProductsPermanentDelete(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
  }

  @Test
   void testProcessProductsPermanentDelete_NullObject() {
    PDTPermanentDeleteResultEventModel resultEventModel =
        PDTPermanentDeleteResultEventModel.builder().productCode(PRODUCT_CODE).sellerCode(SELLER_CODE)
            .service(Constants.SERVICE_NAME).result(Constants.SUCCESS).build();
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductResult()).thenReturn(RESULT_EVENT);
    productWrapperService.processProductsPermanentDelete(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(RESULT_EVENT, PRODUCT_CODE, resultEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getPermanentDeleteProductResult();
  }

  @Test
   void testProcessProductsPermanentDelete_PickedForDeletion_UpdatedTime() {
    PDTProductCombinedUpdateToSolrEventModel eventModel =
      PDTProductCombinedUpdateToSolrEventModel.builder().productCode(PRODUCT_CODE).build();
    PDTPermanentDeleteResultEventModel resultEventModel =
      PDTPermanentDeleteResultEventModel.builder().productCode(PRODUCT_CODE).sellerCode(SELLER_CODE)
        .service(Constants.SERVICE_NAME).result(Constants.SUCCESS).build();
    product.setPickedForDeletion(true);
    product.setId(ID);
    Date updatedDate = new Date(System.currentTimeMillis() - (31 * 60 * 1000));
    product.setUpdatedDate(updatedDate);
    Mockito.when(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductResult()).thenReturn(RESULT_EVENT);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    productWrapperService.processProductsPermanentDelete(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productsPermanentDeleteService).deleteProducts(PRODUCT_CODE, ID, STORE_ID);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getPdtProductCombinedUpdateToSolrEvent();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getPermanentDeleteProductResult();
    Mockito.verify(kafkaProducer).send(EVENT, PRODUCT_CODE, eventModel);
    Mockito.verify(kafkaProducer).send(RESULT_EVENT, PRODUCT_CODE, resultEventModel);
  }

  @Test
   void testProcessProductsPermanentDelete() {
    PDTProductCombinedUpdateToSolrEventModel eventModel =
      PDTProductCombinedUpdateToSolrEventModel.builder().productCode(PRODUCT_CODE).build();
    PDTPermanentDeleteResultEventModel resultEventModel =
      PDTPermanentDeleteResultEventModel.builder().productCode(PRODUCT_CODE).sellerCode(SELLER_CODE)
        .service(Constants.SERVICE_NAME).result(Constants.SUCCESS).build();
    product.setId(ID);
    Mockito.when(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductResult()).thenReturn(RESULT_EVENT);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    productWrapperService.processProductsPermanentDelete(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productsPermanentDeleteService).deleteProducts(PRODUCT_CODE, ID, STORE_ID);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getPdtProductCombinedUpdateToSolrEvent();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getPermanentDeleteProductResult();
    Mockito.verify(kafkaProducer).send(EVENT, PRODUCT_CODE, eventModel);
    Mockito.verify(kafkaProducer).send(RESULT_EVENT, PRODUCT_CODE, resultEventModel);
  }

  @Test
   void testProcessProductsPermanentDelete_CatchException() {
    PDTPermanentDeleteResultEventModel resultEventModel =
      PDTPermanentDeleteResultEventModel.builder().productCode(PRODUCT_CODE).sellerCode(SELLER_CODE)
        .service(Constants.SERVICE_NAME).result(Constants.FAILED).build();
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE))
      .thenThrow(new RuntimeException());
    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductResult()).thenReturn(RESULT_EVENT);
    productWrapperService.processProductsPermanentDelete(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productsPermanentDeleteService, times(0))
      .deleteProducts(anyString(), anyString(), anyString());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getPermanentDeleteProductResult();
    Mockito.verify(kafkaProducer).send(RESULT_EVENT, PRODUCT_CODE, resultEventModel);
  }

  @Test
  void fetchParentCategoryFromCnCategoryCodeTest() {
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    categoryDTO.setCategoryName(CATEGORY_NAME);
    Mockito.when(productServiceRepository.fetchParentCategoryFromCnCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDTO);
    CategoryDTO result = productWrapperService.fetchParentCategoryFromCnCategoryCode(CATEGORY_CODE);
    Assertions.assertEquals(CATEGORY_CODE, result.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, result.getCategoryName());
  }

  @Test
  void updateDistributionMappingStatusOnChangeTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_001);
    productChange.setDistributionMappingStatus(PURE_DISTRIBUTION);

    Product product = new Product.Builder().productCode(PRODUCT_CODE).state(WorkflowState.IN_REVIEW)
      .reviewType(ReviewType.CONTENT_AND_IMAGE).distributionMappingStatus(0).markForDelete(false)
      .build();

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(product);
    Mockito.when(productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent())
      .thenReturn(TEST_TOPIC);

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository).save(product);
    Assertions.assertEquals(1, product.getDistributionMappingStatus());
  }

  @Test
  void updateDistributionMappingStatusOnChangeRanchDisabledTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_001);
    productChange.setDistributionMappingStatus(PURE_DISTRIBUTION);

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", false);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository, Mockito.never())
      .findByProductCodeAndMarkForDeleteFalse(anyString());
  }

  @Test
  void updateDistributionMappingStatusOnChangeMerchantNotInListTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_002);
    productChange.setDistributionMappingStatus(PURE_DISTRIBUTION);

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository, Mockito.never())
      .findByProductCodeAndMarkForDeleteFalse(anyString());
  }

  @Test
  void updateDistributionMappingStatusOnChangeInvalidStatusTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_001);
    productChange.setDistributionMappingStatus(NON_DISTRIBUTION);

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository, Mockito.never())
      .findByProductCodeAndMarkForDeleteFalse(anyString());
  }

  @Test
  void updateDistributionMappingStatusOnChangeProductNotFoundTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_001);
    productChange.setDistributionMappingStatus(PURE_DISTRIBUTION);

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(null);

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository, Mockito.never()).save(any(Product.class));
  }

  @Test
  void updateDistributionMappingStatusOnChangeProductNotInReviewTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_001);
    productChange.setDistributionMappingStatus(PURE_DISTRIBUTION);

    Product product = new Product.Builder().productCode(PRODUCT_CODE).state(WorkflowState.PASSED)
      .distributionMappingStatus(0).markForDelete(false).build();

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(product);

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository, Mockito.never()).save(any(Product.class));
  }

  @Test
  void updateDistributionMappingStatusOnChangeNoStatusChangeTest() {
    ProductChange productChange = new ProductChange();
    productChange.setProductCode(PRODUCT_CODE);
    productChange.setMerchantCode(MERCHANT_001);
    productChange.setDistributionMappingStatus(PURE_DISTRIBUTION);

    Product product = new Product.Builder().productCode(PRODUCT_CODE).state(WorkflowState.IN_REVIEW)
      .distributionMappingStatus(1).markForDelete(false).build();

    ReflectionTestUtils.setField(productWrapperService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productWrapperService, "distributionSellerList",
      java.util.Set.of(MERCHANT_001));
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(product);

    productWrapperService.updateDistributionMappingStatusOnChange(productChange);

    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository, Mockito.never()).save(any(Product.class));
  }
}
