package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.initMocks;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLogisticsRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Qualifier;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ProductWorkflowServiceBeanWrapperTest {

  @InjectMocks
  private ProductWorkflowServiceBeanWrapper productWorkflowServiceBeanWrapper;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  @Qualifier(value = "productWfService")
  private ProductWfService productWorkflowService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Mock
  private ProductMailEventService productMailEventService;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<UpdateProductLevel3Wip> updateProductLevel3WipArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductDetailResponse> productDetailResponseArgumentCaptor;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductServiceBean productServiceBean;

  @Mock
  private ProductPublisherService productPublisherService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ProductAppealService productAppealService;

  private ProductCollection productCollection;
  private ProductCollection oldProductCollection;
  private ProductCreationRequest productCreationRequest;
  private ProductResubmitRequest productResubmitRequest;
  private PreOrderRequest preOrderRequest;
  private NeedRevisionNotes returnForCorrection;
  private ProfileResponse profileResponse;

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String OLD_PRODUCT_CODE = "oldProductCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String NOTES = "notes";
  private static final String ID = "id";
  private static final String STATE_IN_PROGRESS = "IN_PROGRESS";
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_ID = "item-id";
  private static final String LOGISTIC_PRODUCT_CODE = "logistic-product-code";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String VALID_USERNAME = "validUserName";

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setResubmitCount(0);
    oldProductCollection = new ProductCollection();
    oldProductCollection.setResubmitCount(0);
    oldProductCollection.setProductCode(OLD_PRODUCT_CODE);
    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    Mockito
        .when(productBusinessPartnerRepository.findByStoreIdAndProductId(anyString(), anyString()))
        .thenReturn(new ArrayList<>());
    Mockito.doNothing().when(itemService).publishItemStatusEvent(
        anyList(), any(ProductStatus.class), anyString());
    preOrderRequest =
        PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    returnForCorrection = new NeedRevisionNotes();
    returnForCorrection.setAllVariants(true);
    profileResponse = new ProfileResponse();
    profileResponse.setMerchantStatus(Constants.ACTIVE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString())).thenReturn(profileResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productCollectionRepository, solrReviewProductCollectionService, productWorkflowService);
    Mockito.verifyNoMoreInteractions(productLevel1HistoryService, productBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(productMailEventService, productPublisherService);
    Mockito.verifyNoMoreInteractions(productServiceBean);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(itemService);
    Mockito.verifyNoMoreInteractions(productLevel3LogisticsService);
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
  }

  @Test
  public void returnForCorrectionTest() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService).returnForCorrection(PRODUCT_CODE, NOTES, returnForCorrection,
        false, false, true);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    this.productWorkflowServiceBeanWrapper.returnForCorrection(STORE_ID, PRODUCT_CODE, NOTES);
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).returnForCorrection(PRODUCT_CODE, NOTES, null, false, false, true);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
  }

  @Test
  public void resubmitTest() throws Exception {
    productResubmitRequest = new ProductResubmitRequest();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductCode(PRODUCT_CODE);
    productResubmitRequest.setProductRequest(productRequest);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(new ProductCollection());
    Mockito.doNothing().when(this.solrReviewProductCollectionService).publishKafkaEventToAddProductToReviewProductCollection(Mockito.any(ProductCollection.class));
    this.productWorkflowServiceBeanWrapper.resubmit(STORE_ID, productResubmitRequest, new UpdateProductLevel3Wip());
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).resubmit(productRequestArgumentCaptor.capture(), updateProductLevel3WipArgumentCaptor.capture());
    Mockito.verify(this.solrReviewProductCollectionService).publishKafkaEventToAddProductToReviewProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService).addHistoryForProductResubmissionDueToContentOrImageRejection(productResubmitRequest);
  }

  @Test
  public void deleteProductCollectionTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setAppealedProduct(true);
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_CODE);
    productCollection.setProductId(ProductCollection.COLUMN_PRODUCT_ID);
    Mockito.doNothing().when(this.productWorkflowService)
        .delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.when(productServiceBean.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito
        .when(productBusinessPartnerRepository.findByStoreIdAndProductId(anyString(), anyString()))
        .thenReturn(List.of(productBusinessPartner));
    this.productWorkflowServiceBeanWrapper.deleteProductCollection(STORE_ID, PRODUCT_CODE, NOTES, Boolean.FALSE, false);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID,
        productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.verify(productServiceBean).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(itemService).publishItemStatusEvent(anyList(),
        any(ProductStatus.class), anyString());
    Mockito.verify(productServiceBean).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), Mockito.anyString());
    Mockito.verify(productAppealService).decrementCounterForProductAppeal(STORE_ID, BUSINESS_PARTNER_CODE);
  }


  @Test
  public void deleteProductCollectionPostLiveTest() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService)
        .delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.when(productServiceBean.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    productCollection.setPostLive(false);
    productCollection.setEdited(true);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    this.productWorkflowServiceBeanWrapper
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, NOTES, Boolean.FALSE, Boolean.FALSE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID,
        productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.verify(this.productServiceBean).deleteProductBusinessPartnerForPostLiveRejection(new ArrayList<>(), productCollection, NOTES);
    Mockito.verify(productServiceBean).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(itemService).publishItemStatusEvent(anyList(),
        any(ProductStatus.class), anyString());
    Mockito.verify(productServiceBean).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), Mockito.anyString());
  }

  @Test
  public void deleteProductCollectionPostLiveDeleteFromPdtTrueTest() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService)
        .delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.when(productServiceBean.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    productCollection.setPostLive(false);
    productCollection.setEdited(true);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
        Mockito.when(productServiceBean.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(false);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    this.productWorkflowServiceBeanWrapper
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, NOTES, Boolean.FALSE, Boolean.TRUE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID,
        productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.verify(this.productServiceBean).deleteProductBusinessPartnerForPostLiveRejection(new ArrayList<>(), productCollection, NOTES);
    Mockito.verify(productServiceBean).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(itemService).publishItemStatusEvent(anyList(),
        any(ProductStatus.class), anyString());
    Mockito.verify(productServiceBean).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), Mockito.anyString());
    Mockito.verify(productServiceBean).checkIfProductExistsInPDT(PRODUCT_CODE, true);
  }

  @Test
  public void deleteProductCollectionPostLiveDeleteFromPdtTrueExistInPdtTrueTest() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService)
        .delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.when(productServiceBean.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    productCollection.setPostLive(false);
    productCollection.setEdited(true);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.when(productServiceBean.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.doNothing().when(this.productServiceBean).removeProductFromPDT(PRODUCT_CODE);
    this.productWorkflowServiceBeanWrapper
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, NOTES, Boolean.FALSE, Boolean.TRUE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID,
        productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.verify(this.productServiceBean).deleteProductBusinessPartnerForPostLiveRejection(new ArrayList<>(), productCollection, NOTES);
    Mockito.verify(productServiceBean).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(itemService).publishItemStatusEvent(anyList(),
        any(ProductStatus.class), anyString());
    Mockito.verify(productServiceBean).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), Mockito.anyString());
    Mockito.verify(productServiceBean).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productServiceBean).removeProductFromPDT(PRODUCT_CODE);
  }


  @Test
  public void deleteProductCollectionTestWithEmail() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService)
        .delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productServiceBean.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.doNothing().when(this.productMailEventService).createAndSaveMailEvent(PRODUCT_CODE, NOTES,
        ProductMailEventsEnum.REJECTED);
    this.productWorkflowServiceBeanWrapper
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, NOTES, Boolean.TRUE, Boolean.FALSE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID,
        productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).delete(PRODUCT_CODE, NOTES, Boolean.TRUE);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.verify(this.productMailEventService).createAndSaveMailEvent(PRODUCT_CODE, NOTES,
        ProductMailEventsEnum.REJECTED);
    Mockito.verify(productServiceBean).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(itemService).publishItemStatusEvent(anyList(),
        any(ProductStatus.class), anyString());
    Mockito.verify(productServiceBean).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), Mockito.anyString());
  }


  @Test
  public void deleteProductCollectionPostLiveTestWithEmail() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService)
        .delete(PRODUCT_CODE, NOTES, Boolean.FALSE);
    productCollection.setPostLive(true);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productServiceBean.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.doNothing().when(this.productMailEventService).createAndSaveMailEvent(PRODUCT_CODE, NOTES,
        ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED);
    this.productWorkflowServiceBeanWrapper
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, NOTES, Boolean.TRUE, Boolean.FALSE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID,
        productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).delete(PRODUCT_CODE, NOTES, Boolean.TRUE);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    Mockito.verify(this.productMailEventService).createAndSaveMailEvent(PRODUCT_CODE, NOTES,
        ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED);
    Mockito.verify(this.productServiceBean).deleteProductBusinessPartnerForPostLiveRejection(new ArrayList<>(), productCollection, NOTES);
    Mockito.verify(productServiceBean).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(itemService).publishItemStatusEvent(anyList(),
        any(ProductStatus.class), anyString());
    Mockito.verify(productServiceBean).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.REJECTED), Mockito.anyString());
  }

  @Test
  public void approveDraftTest() throws Exception {
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(productCollection);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    Mockito.doNothing().when(this.productWorkflowService).approveDraft(PRODUCT_CODE);
    this.productWorkflowServiceBeanWrapper.approveDraft(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).approveDraft(PRODUCT_CODE);
  }

  @Test
  public void approveDraftExceptionTest() throws Exception {
    profileResponse.setMerchantStatus(Constants.INACTIVE);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(productCollection);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    Mockito.doNothing().when(this.productWorkflowService).approveDraft(PRODUCT_CODE);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productWorkflowServiceBeanWrapper.approveDraft(STORE_ID, PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void approveDraftProfileResponseNullExceptionTest() throws Exception {
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(productCollection);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(null);
    Mockito.doNothing().when(this.productWorkflowService).approveDraft(PRODUCT_CODE);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productWorkflowServiceBeanWrapper.approveDraft(STORE_ID, PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void createTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCreationRequest.setPreOrder(preOrderRequest);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).create(productCreationRequestArgumentCaptor.capture(),
      eq(false), eq(false));
    Mockito.verify(this.productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    Assertions.assertTrue(productCreationRequestArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PREORDER_TYPE, productCreationRequestArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productCreationRequestArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
  }

  @Test
  public void createTestWithUrlImages() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setCreatedBy(VALID_USERNAME);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setContainsUrlImage(true);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList<>();
    ProductItemLogisticsRequest request = ProductItemLogisticsRequest.builder()
        .logisticProductCode(LOGISTIC_PRODUCT_CODE).isSelected(true).build();
    productItemLogisticsRequests.add(request);
    productItemCreationRequest.setProductItemLogisticsRequests(productItemLogisticsRequests);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito
        .when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(),
            productCollection.getStoreId());
    Mockito
        .when(productBusinessPartnerService
            .getAllItemSkusViewConfigByProductId(productCollection.getProductId()))
        .thenReturn(Arrays.asList(new ItemFlagDetails(ITEM_SKU, PICKUP_POINT_CODE, true, true, true, true, ITEM_ID)));
    Mockito
        .when(productLevel3LogisticsService.saveLogisticsByItemSku(Arrays.asList(ITEM_SKU),
            BUSINESS_PARTNER_CODE,
            Arrays.asList(ProductLevel3Logistics.builder()
                .logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false))
        .thenReturn(true);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, productCollection.getProductId())).thenReturn(Arrays.asList(new ProductBusinessPartner()));
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productBusinessPartnerService).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, productCollection.getProductId());
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService)
        .create(productCreationRequestArgumentCaptor.capture(), eq(false), eq(false));
    Mockito.verify(this.productPublisherService).publishProductImageResizeEvent(
        productCollection.getProductCode(), productCollection.getStoreId());
    Mockito.verify(productBusinessPartnerService)
        .getAllItemSkusViewConfigByProductId(productCollection.getProductId());
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(
            Arrays.asList(ITEM_SKU), BUSINESS_PARTNER_CODE, Arrays.asList(ProductLevel3Logistics
                .builder().logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false);
    Mockito.verify(productServiceBean).saveHistoryForUrlImage(eq(PRODUCT_CODE),
        eq(Arrays.asList(new ItemFlagDetails(ITEM_SKU, PICKUP_POINT_CODE, true, true, true, true, ITEM_ID))),
        productCreationRequestArgumentCaptor.capture());
    Assertions.assertEquals(VALID_USERNAME, productCreationRequestArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void createTest_logisticsSave_itemEmpty() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito
        .when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(),
            productCollection.getStoreId());
    Mockito
        .when(productBusinessPartnerService
            .getAllItemSkusViewConfigByProductId(productCollection.getProductId()))
        .thenReturn(Arrays.asList(new ItemFlagDetails(ITEM_SKU, PICKUP_POINT_CODE, true, true, true, true, ITEM_ID)));
    Mockito.when(productLevel3LogisticsService.saveLogisticsByItemSku(Arrays.asList(ITEM_SKU),
        BUSINESS_PARTNER_CODE, new ArrayList<>(), false)).thenReturn(true);
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService)
        .create(productCreationRequestArgumentCaptor.capture(), eq(false), eq(false));
    Mockito.verify(this.productPublisherService).publishProductImageResizeEvent(
        productCollection.getProductCode(), productCollection.getStoreId());
    Mockito.verify(productBusinessPartnerService)
        .getAllItemSkusViewConfigByProductId(productCollection.getProductId());
    Mockito.verify(productLevel3LogisticsService).saveLogisticsByItemSku(Arrays.asList(ITEM_SKU),
        BUSINESS_PARTNER_CODE, new ArrayList<>(), false);
  }

  @Test
  public void createTest_logisticsSave() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList<>();
    ProductItemLogisticsRequest request = ProductItemLogisticsRequest.builder()
        .logisticProductCode(LOGISTIC_PRODUCT_CODE).isSelected(true).build();
    productItemLogisticsRequests.add(request);
    productItemCreationRequest.setProductItemLogisticsRequests(productItemLogisticsRequests);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito
        .when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(),
            productCollection.getStoreId());
    Mockito
        .when(productBusinessPartnerService
            .getAllItemSkusViewConfigByProductId(productCollection.getProductId()))
        .thenReturn(Arrays.asList(new ItemFlagDetails(ITEM_SKU, PICKUP_POINT_CODE, true, true, ITEM_ID)));
    Mockito
        .when(productLevel3LogisticsService.saveLogisticsByItemSku(Arrays.asList(ITEM_SKU),
            BUSINESS_PARTNER_CODE,
            Arrays.asList(ProductLevel3Logistics.builder()
                .logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false))
        .thenReturn(true);
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService)
        .create(productCreationRequestArgumentCaptor.capture(), eq(false), eq(false));
    Mockito.verify(this.productPublisherService).publishProductImageResizeEvent(
        productCollection.getProductCode(), productCollection.getStoreId());
    Mockito.verify(productBusinessPartnerService)
        .getAllItemSkusViewConfigByProductId(productCollection.getProductId());
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(
            Arrays.asList(ITEM_SKU), BUSINESS_PARTNER_CODE, Arrays.asList(ProductLevel3Logistics
                .builder().logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false);
  }

  @Test
  public void createTestLogisticsSaveMPPFlow() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList<>();
    ProductItemLogisticsRequest request = ProductItemLogisticsRequest.builder()
        .logisticProductCode(LOGISTIC_PRODUCT_CODE).isSelected(true).build();
    productItemLogisticsRequests.add(request);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito
        .when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(),
            productCollection.getStoreId());
    Mockito
        .when(productBusinessPartnerService
            .getAllItemSkusViewConfigByProductId(productCollection.getProductId()))
        .thenReturn(Arrays.asList(new ItemFlagDetails(ITEM_SKU, PICKUP_POINT_CODE, true, true, ITEM_ID)));
    Mockito
        .when(productLevel3LogisticsService.saveLogisticsByItemSku(Arrays.asList(ITEM_SKU),
            BUSINESS_PARTNER_CODE,
            Arrays.asList(ProductLevel3Logistics.builder()
                .logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false))
        .thenReturn(true);
    productCreationRequest.setProductItemLogisticsRequests(productItemLogisticsRequests);
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, true, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService)
        .create(productCreationRequestArgumentCaptor.capture(), eq(false), eq(true));
    Mockito.verify(this.productPublisherService).publishProductImageResizeEvent(
        productCollection.getProductCode(), productCollection.getStoreId());
    Mockito.verify(productBusinessPartnerService)
        .getAllItemSkusViewConfigByProductId(productCollection.getProductId());
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(
            Arrays.asList(ITEM_SKU), BUSINESS_PARTNER_CODE, Arrays.asList(ProductLevel3Logistics
                .builder().logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false);
  }

  @Test
  public void TestForPrioritySellerIsGreaterThanZero() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setPrioritySeller(1);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList<>();
    ProductItemLogisticsRequest request = ProductItemLogisticsRequest.builder()
        .logisticProductCode(LOGISTIC_PRODUCT_CODE).isSelected(true).build();
    productItemLogisticsRequests.add(request);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito
        .when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productPublisherService.publishProductImageResizeEventForPrioritySeller(productCollection.getProductCode(),
        productCollection.getStoreId(), 1)).thenReturn(null);
    Mockito
        .when(productBusinessPartnerService
            .getAllItemSkusViewConfigByProductId(productCollection.getProductId()))
        .thenReturn(Arrays.asList(new ItemFlagDetails(ITEM_SKU, PICKUP_POINT_CODE, true, true, ITEM_ID)));
    Mockito
        .when(productLevel3LogisticsService.saveLogisticsByItemSku(Arrays.asList(ITEM_SKU),
            BUSINESS_PARTNER_CODE,
            Arrays.asList(ProductLevel3Logistics.builder()
                .logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false))
        .thenReturn(true);
    productCreationRequest.setProductItemLogisticsRequests(productItemLogisticsRequests);
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, true, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService)
        .create(productCreationRequestArgumentCaptor.capture(), eq(false), eq(true));
    Mockito.verify(this.productPublisherService)
        .publishProductImageResizeEventForPrioritySeller(productCollection.getProductCode(),
            productCollection.getStoreId(), 1);
    Mockito.verify(productBusinessPartnerService)
        .getAllItemSkusViewConfigByProductId(productCollection.getProductId());
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(
            Arrays.asList(ITEM_SKU), BUSINESS_PARTNER_CODE, Arrays.asList(ProductLevel3Logistics
                .builder().logisticProductCode(LOGISTIC_PRODUCT_CODE).selected(true).build()),
            false);
  }

  @Test
  public void createViaFlow3Test() throws Exception {
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    productCollection.setState(STATE_IN_PROGRESS);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).create(productCreationRequestArgumentCaptor.capture(),
      eq(false), eq(false));
    Mockito.verify(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
  }

  @Test
  public void createWithOldProductCodeTest() throws Exception {
    productCreationRequest.setOldProductCode(OLD_PRODUCT_CODE);
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setProductId(PRODUCT_CODE);
    oldProductCollection.setProductId(PRODUCT_CODE);
    oldProductCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .updateProductIdForRevisedProducts(oldProductCollection.getProductId(), productCollection.getProductId());
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, OLD_PRODUCT_CODE))
        .thenReturn(oldProductCollection);
    Mockito.doNothing().when(this.productWorkflowService).updateResubmitCountOnProductResubmission(productCollection);
    Mockito.doNothing().when(this.productBusinessPartnerService)
        .markItemsAsDeletedOnProductResubmission(BUSINESS_PARTNER_CODE, PRODUCT_CODE);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, OLD_PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).create(productCreationRequestArgumentCaptor.capture(),
      eq(false), eq(false));
    Mockito.verify(this.productWorkflowService).updateResubmitCountOnProductResubmission(productCollection);
    Mockito.verify(productLevel1HistoryService).addHistoryForProductResubmitted(productCreationRequest);
    Mockito.verify(this.productBusinessPartnerService)
        .markItemsAsDeletedOnProductResubmission(BUSINESS_PARTNER_CODE, PRODUCT_CODE);
    Mockito.verify(this.productLevel1HistoryService)
        .updateProductIdForRevisedProducts(oldProductCollection.getProductId(), productCollection.getProductId());
    Mockito.verify(productServiceBean).clearMasterProductCache(productCollection.getProductCode());
    Mockito.verify(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    Assertions.assertEquals(1, productCollection.getResubmitCount());
  }

  @Test
  public void createWithOldProductCodePostLiveErrorTest() throws Exception {
    productCreationRequest.setOldProductCode(OLD_PRODUCT_CODE);
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setProductId(PRODUCT_CODE);
    oldProductCollection.setPostLive(true);
    oldProductCollection.setProductId(PRODUCT_CODE);
    oldProductCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .updateProductIdForRevisedProducts(oldProductCollection.getProductId(), productCollection.getProductId());
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, OLD_PRODUCT_CODE))
        .thenReturn(oldProductCollection);
    Mockito.doNothing().when(this.productWorkflowService).updateResubmitCountOnProductResubmission(productCollection);
    Mockito.doNothing().when(this.productBusinessPartnerService)
        .markItemsAsDeletedOnProductResubmission(BUSINESS_PARTNER_CODE, PRODUCT_CODE);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
      });
    } finally {
      Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, OLD_PRODUCT_CODE);
    }
  }

  @Test
  public void createWithOldProductCodeEditedProductErrorTest() throws Exception {
    productCreationRequest.setOldProductCode(OLD_PRODUCT_CODE);
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setProductId(PRODUCT_CODE);
    oldProductCollection.setPostLive(false);
    oldProductCollection.setEdited(true);
    oldProductCollection.setProductId(PRODUCT_CODE);
    oldProductCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .updateProductIdForRevisedProducts(oldProductCollection.getProductId(), productCollection.getProductId());
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, OLD_PRODUCT_CODE))
        .thenReturn(oldProductCollection);
    Mockito.doNothing().when(this.productWorkflowService).updateResubmitCountOnProductResubmission(productCollection);
    Mockito.doNothing().when(this.productBusinessPartnerService)
        .markItemsAsDeletedOnProductResubmission(BUSINESS_PARTNER_CODE, PRODUCT_CODE);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
      });
    } finally {
      Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, OLD_PRODUCT_CODE);
    }
  }

  @Test
  public void createTestForSkipReviewTrueProduct() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setSkipReview(true);
    Mockito.doNothing().when(this.productServiceWrapper).skipScreeningForSkipReviewProduct(PRODUCT_CODE, null);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).create(productCreationRequestArgumentCaptor.capture(),
      eq(false), eq(false));
    Mockito.verify(this.productServiceWrapper).skipScreeningForSkipReviewProduct(PRODUCT_CODE, null);
  }

  @Test
  public void createForSkipReviewTrueProductExceptionTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    productCollection.setSkipReview(true);
    Mockito.doThrow(Exception.class).when(this.productServiceWrapper).skipScreeningForSkipReviewProduct(PRODUCT_CODE,
        null);
    Mockito.doNothing().when(this.productWorkflowService).create(productCreationRequest, false, false);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doReturn(new ImageResizeEvent()).when(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    this.productWorkflowServiceBeanWrapper.create(STORE_ID, productCreationRequest, false, false, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productWorkflowService).create(productCreationRequestArgumentCaptor.capture(),
      eq(false), eq(false));
    Mockito.verify(this.productServiceWrapper).skipScreeningForSkipReviewProduct(PRODUCT_CODE, null);
  }
}
