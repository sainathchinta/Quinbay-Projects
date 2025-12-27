package com.gdn.partners.pbp.service.productlevel1;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class ProductLevel1CollectionServiceTest {

  private static final String DEFAULT_STORE_ID = "1234567890";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_ID = "TOQ-0001-0001";
  private static final String DEFAULT_NAME = "TEST-123";
  private static final String CLIENT_ID = "x-bulk";
  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "CODE";
  private static final String BUSINESS_PARTNER_NAME = "NAME";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "APPROVED";
  private static final String DRAFT = "DRAFT";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String NEED_CORRECTION = "NEED_CORRECTION";
  private static final String USERNAME = "USERNAME";
  private static final String NOT_ASSIGNED = "NA";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String bulkUploadType = "UNIFIED_BULK_UPLOAD";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final List<String> ERROR_FIELDS = Arrays.asList("Description");
  private static final String PRODUCT_CREATION_TYPE = "productCreationType";

  private ProductCollection productCollection1;
  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;
  private boolean validateDraftState = true;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductWorkflowRepository productWorkflowRepository;

  @Mock
  private ProductHistoryRepository productHistoryRepository;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @InjectMocks
  private ProductLevel1CollectionServiceBean productLevel1CollectionServiceBean;

  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    ProductCategoryResponse productCategoryResponse1 = new ProductCategoryResponse();
    productCategoryResponse1.setCategory(categoryResponse);
    ProductCategoryResponse productCategoryResponse2 = new ProductCategoryResponse();
    productCategoryResponse2.setCategory(categoryResponse);
    productCategoryResponse2.setMarkForDelete(true);
    List<ProductCategoryResponse> productCategoryResponses = new ArrayList<ProductCategoryResponse>();
    productCategoryResponses.add(productCategoryResponse1);
    productCategoryResponses.add(productCategoryResponse2);
    productDetailResponse.setProductCode(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE);
    productDetailResponse.setName(DEFAULT_NAME);
    productDetailResponse.setProductCategoryResponses(productCategoryResponses);
    return productDetailResponse;
  }

  private ProductCollection generateProductCollection() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    return productCollection;
  }

  private ProductBusinessPartner generateProductBusinessPartner() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();

    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartners.add(productItemBusinessPartner);

    List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes = new ArrayList();
    ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttributes.add(productBusinessPartnerAttribute);

    productBusinessPartner.setBusinessPartnerId("PKO-00001");
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    productBusinessPartner.setProductBusinessPartnerAttributes(productBusinessPartnerAttributes);

    return productBusinessPartner;
  }

  private ProductWorkflow generateProductWorkflow() throws Exception {
    ProductWorkflow productWorkflow = new ProductWorkflow();
    productWorkflow.setProductId("SKU-0001-0001");
    return productWorkflow;
  }

  private ProductRequest generateProductRequest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductCode(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE);
    productRequest.setProductCategories(new ArrayList<ProductCategoryRequest>());
    ProductCategoryRequest productCategoryRequest1 = new ProductCategoryRequest();
    productCategoryRequest1.setMarkForDelete(true);
    ProductCategoryRequest productCategoryRequest2 = new ProductCategoryRequest();
    productCategoryRequest2.setCategory(new CategoryRequest());
    productRequest.getProductCategories().add(productCategoryRequest1);
    productRequest.getProductCategories().add(productCategoryRequest2);
    productRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    return productRequest;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductDetailResponse productDetailResponse = this.generateProductDetailResponse();
    ProductCollection productCollection = this.generateProductCollection();
    Mockito.when(this.productRepository.findProductDetailByProductCode(Mockito.any())).thenReturn(
            productDetailResponse);
    Mockito.when(
            this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(),
                    Mockito.any())).thenReturn(productCollection);
    Mockito.when(this.productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);

    productCollection1 = new ProductCollection();
    productCollection1.setPostLive(true);
    productCollection1.setProductName(PRODUCT_NAME);
    productCollection1.setRestrictedKeywordsPresent(true);
    productCollection1.setActivated(false);
    productCollection1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection1.setBrandCode(BRAND_CODE);
    productCollection1.setStoreId(STORE_ID);
    productCollection1.setProductCode(DEFAULT_PRODUCT_CODE);

    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    screeningProductBulkActionsRequest.setProductCodes(Arrays.asList(DEFAULT_PRODUCT_CODE));
    screeningProductBulkActionsRequest.setAllVariants(true);
    screeningProductBulkActionsRequest.setVendorErrorFields(ERROR_FIELDS);
    screeningProductBulkActionsRequest.setVendorNotes(Arrays.asList(CORRECTION_REASON));
    screeningProductBulkActionsRequest.setImageReason(Arrays.asList(CORRECTION_REASON));
    screeningProductBulkActionsRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsRequest.setImagesAdditionalNotes(ADDITIONAL_NOTES);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.productBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(this.productWorkflowRepository);
    Mockito.verifyNoMoreInteractions(this.productHistoryRepository);
  }

  @Test
  public void createTest() throws Exception {
    this.productLevel1CollectionServiceBean.create(null, null, ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE,
            ProductLevel1CollectionServiceTest.BRAND_CODE, BRAND_APPROVAL_STATUS, false, bulkUploadType);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productCollectionRepository).save(productCollectionArgumentCaptor.capture());
    Assertions.assertEquals(Boolean.FALSE, productCollectionArgumentCaptor.getValue().isImageResized());
    Assertions.assertEquals(Boolean.TRUE, productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void createDirectTest() throws Exception {
    this.productLevel1CollectionServiceBean.create(GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE, null,
            ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE, ProductLevel1CollectionServiceTest.BRAND_CODE,
            BRAND_APPROVAL_STATUS, false, bulkUploadType);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productCollectionRepository).save(productCollectionArgumentCaptor.capture());
    Assertions.assertEquals(Boolean.FALSE, productCollectionArgumentCaptor.getValue().isImageResized());
    Assertions.assertEquals(Boolean.TRUE, productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void updateTest() throws Exception {
    this.productLevel1CollectionServiceBean.update(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any());
    Mockito.verify(this.productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
  }

  @Test
  public void approveDraftTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    this.productLevel1CollectionServiceBean.approveDraft(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE, profileResponse);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any());
    Mockito.verify(this.productCollectionRepository).save(Mockito.any(ProductCollection.class));
  }

  @Test
  public void approveDraftTrueTest() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setRestrictedKeywordsPresent(true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean.approveDraft(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE, profileResponse);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).save(productCollection);
    assertFalse(productCollection.isPostLive());
  }

  @Test
  public void approveDraftFalseTest() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setRestrictedKeywordsPresent(true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean.approveDraft(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE, profileResponse);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).save(productCollection);
    assertFalse(productCollection.isPostLive());
  }

  @Test
  public void approveDraftWithRestrictedKeywordTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    Mockito.when(this.productCollectionRepository
                    .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE))
            .thenReturn(productCollection1);
    this.productLevel1CollectionServiceBean.approveDraft(DEFAULT_PRODUCT_CODE, profileResponse);
    Mockito.verify(this.productCollectionRepository)
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).save(productCollectionArgumentCaptor.capture());
    Assertions.assertFalse(productCollectionArgumentCaptor.getAllValues().get(0).isPostLive());
    Assertions.assertTrue(productCollectionArgumentCaptor.getAllValues().get(0).isRestrictedKeywordsPresent());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, productCollectionArgumentCaptor.getAllValues().get(0).getProductCode());
  }

  @Test
  public void activateTest() throws Exception {
    this.productLevel1CollectionServiceBean.activate(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any());
    Mockito.verify(this.productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
  }

  @Test
  public void deleteTest() throws Exception {
    this.productLevel1CollectionServiceBean.delete(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any());
    Mockito.verify(this.productCollectionRepository).save(Mockito.any(ProductCollection.class));
  }

  @Test
  public void deleteWhenError() throws Exception {
    MDC.put("storeId", STORE_ID);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        this.productLevel1CollectionServiceBean.delete(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_PRODUCT_CODE));
  }

  @Test
  public void deleteWithCollectionTest() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    this.productLevel1CollectionServiceBean.delete(productCollection);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setMarkForDelete(true);
    productCollection.setState("DELETED");
    Mockito.verify(this.productCollectionRepository).saveAndFlush(Mockito.eq(productCollection));
  }

  @Test
  public void deleteWithCollectionError() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setMarkForDelete(true);
    productCollection.setState("DELETED");
    Mockito.when(productCollectionRepository.saveAndFlush(
            Mockito.eq(productCollection))).thenThrow(new RuntimeException());
    try {
      Assertions.assertThrows(RuntimeException.class, () -> {
        this.productLevel1CollectionServiceBean.delete(productCollection);
      });
    } catch (RuntimeException e) {
      throw e;
    }
    Mockito.verify(this.productCollectionRepository).saveAndFlush(Mockito.eq(productCollection));

  }

  @Test
  public void returnDraftForCorrectionTest_forDraftState() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(DRAFT);
    when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean
            .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, false,
                    validateDraftState);
    verify(this.productCollectionRepository)
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    verify(this.productCollectionRepository).save(any(ProductCollection.class));
    assertEquals(NEED_CORRECTION, productCollection.getState());
    assertEquals(USERNAME, productCollection.getAssignedTo());
    assertNotNull(productCollection.getNeedCorrectionNotes());
  }

  @Test
  public void returnDraftForCorrectionTestSkipValidation() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(DRAFT);
    when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean
            .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, false,
                    false);
    verify(this.productCollectionRepository)
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    verify(this.productCollectionRepository).save(any(ProductCollection.class));
    assertEquals(NEED_CORRECTION, productCollection.getState());
    assertEquals(USERNAME, productCollection.getAssignedTo());
    assertNotNull(productCollection.getNeedCorrectionNotes());
  }

  @Test
  public void returnDraftForCorrectionTestScreeningActionTest() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(DRAFT);
    when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean
            .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, true,
                    validateDraftState);
    verify(this.productCollectionRepository)
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    verify(this.productCollectionRepository).save(any(ProductCollection.class));
    assertEquals(NEED_CORRECTION, productCollection.getState());
    assertEquals(USERNAME, productCollection.getAssignedTo());
    assertNotNull(productCollection.getNeedCorrectionNotes());
  }

  @Test
  public void returnDraftForCorrectionTestScreeningActionNotDraft() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(IN_PROGRESS);
    when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel1CollectionServiceBean
            .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, true,
                validateDraftState);
      });
    } finally {
      verify(this.productCollectionRepository)
              .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    }
  }

  @Test
  public void returnDraftForCorrectionTest_forInProgressState() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(IN_PROGRESS);
    productCollection.setActivated(true);
    when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean
            .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, false,
                    validateDraftState);
    verify(this.productCollectionRepository)
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    verify(this.productCollectionRepository).save(any(ProductCollection.class));
    assertEquals(NEED_CORRECTION, productCollection.getState());
    assertEquals(NOT_ASSIGNED, productCollection.getAssignedTo());
    assertFalse(productCollection.isActivated());
    assertFalse(productCollection.isAutoNeedRevision());
    assertNotNull(productCollection.getNeedCorrectionNotes());
  }

  @Test
  public void returnDraftForCorrectionAutoNeedRevisionTest() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(IN_PROGRESS);
    productCollection.setActivated(true);
    when(this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    this.productLevel1CollectionServiceBean
            .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, true, false,
                    validateDraftState);
    verify(this.productCollectionRepository)
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    verify(this.productCollectionRepository).save(any(ProductCollection.class));
    assertEquals(NEED_CORRECTION, productCollection.getState());
    assertEquals(NOT_ASSIGNED, productCollection.getAssignedTo());
    assertFalse(productCollection.isActivated());
    assertTrue(productCollection.isAutoNeedRevision());
    assertEquals(1, productCollection.getAutoNeedRevisionCount());
    assertNotNull(productCollection.getNeedCorrectionNotes());
  }

  @Test
  public void resubmitTest() throws Exception {
    this.productLevel1CollectionServiceBean.resubmit(this.generateProductRequest(), new Date());
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.eq(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE));
    Mockito.verify(this.productCollectionRepository).save(Mockito.any(ProductCollection.class));
  }

  @Test
  public void resubmitWithoutCategoryTest() throws Exception {
    ProductRequest productRequest = this.generateProductRequest();
    productRequest.getProductCategories().remove(1);
    this.productLevel1CollectionServiceBean.resubmit(productRequest, new Date());
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.eq(ProductLevel1CollectionServiceTest.DEFAULT_PRODUCT_CODE));
    Mockito.verify(this.productCollectionRepository).save(Mockito.any(ProductCollection.class));
  }

  @Test
  public void updateRejectedProductTest() throws Exception {
    ProductDetailResponse productDetailResponse = this.generateProductDetailResponse();
    ProductCollection productCollection = new ProductCollection();

    List<ProductBusinessPartner> productBusinessPartners = new ArrayList();
    ProductBusinessPartner productBusinessPartner = this.generateProductBusinessPartner();
    productBusinessPartners.add(productBusinessPartner);

    List<ProductWorkflow> productWorkflows = new ArrayList();
    ProductWorkflow productWorkflow = this.generateProductWorkflow();
    productWorkflows.add(productWorkflow);

    Mockito.when(this.productRepository.findProductDetailByProductCode(Mockito.any(), any(Boolean.class)))
            .thenReturn(productDetailResponse);
    Mockito.when(this.productBusinessPartnerRepository.findByStoreIdAndProductId(Mockito.any(), any()))
            .thenReturn(productBusinessPartners);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.any(), any()))
            .thenReturn(productCollection);
    Mockito.when(this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), any()))
            .thenReturn(productWorkflows);

    this.productLevel1CollectionServiceBean.updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any(), any(Boolean.class));
    Mockito.verify(this.productBusinessPartnerRepository).findByStoreIdAndProductId(any(), any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAll(anyList());
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(any(), any());
    Mockito.verify(this.productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(any(), any());
    Mockito.verify(this.productWorkflowRepository).saveAndFlush(any(ProductWorkflow.class));
    Mockito.verify(this.productWorkflowRepository).deleteAll(anyList());
    Mockito.verify(this.productHistoryRepository).saveAndFlush(any(ProductHistory.class));
  }

  @Test
  public void findByProductIdTest() throws Exception {
    this.productLevel1CollectionServiceBean.findByProductId(DEFAULT_PRODUCT_ID);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any());
  }

  @Test
  public void createProductCollectionTest() throws Exception {
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(new CategoryResponse());
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    this.productLevel1CollectionServiceBean
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                    false, bulkUploadType);
    Mockito.verify(this.productCollectionRepository).save(Mockito.any(ProductCollection.class));
  }

  @Test
  public void createProductCollectionTest2() throws Exception {
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(new CategoryResponse());
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    productCollection1.setPrioritySeller(0);
    this.productLevel1CollectionServiceBean
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                    false, false, PRODUCT_CREATION_TYPE, false, productCollection1.getPrioritySeller());
    Mockito.verify(this.productCollectionRepository).save(Mockito.any(ProductCollection.class));
  }
}
