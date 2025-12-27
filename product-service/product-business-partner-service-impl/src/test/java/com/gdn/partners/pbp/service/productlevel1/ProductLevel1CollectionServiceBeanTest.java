package com.gdn.partners.pbp.service.productlevel1;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductLevel3V2Service;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.mta.domain.event.modal.ProductWipDeleteResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by vishal on 18/07/17.
 */
public class ProductLevel1CollectionServiceBeanTest {

  private static final String DEFAULT_PRODUCT_ID = "product-id";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-10101";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_PRODUCT_NAME = "test-product";
  private static final Integer RETRY_COUNT = 1;
  private static final String DEFAULT_NOTE = "default_note";
  private static final Long TIME_STAMP = System.currentTimeMillis();

  @InjectMocks
  ProductLevel1CollectionServiceBean productLevel1CollectionServiceBean;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductWorkflowRepository productWorkflowRepository;

  @Mock
  private ProductHistoryRepository productHistoryRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductLevel3V2Service productLevel3V2Service;
  
  @Mock
  private ProductService productService;

  @Mock
  private ProductWfRepository productWfRepository;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;
  
  private static String REQUEST_ID = "A6S5-DF4A-S3D5-4F6AS-D4FA-S5D4F";
  private static String USERNAME = "username@mail.com";
  private static String STORE_ID = "10001";
  private static String CATEGORY_CODE = "HA-54645";
  private static String BUSINESS_PARTNER_CODE = "TOQ-15130";
  private static String KEYWORD = "aaa";
  private ProductApprovalDetailStatus productApprovalDetailStatus;
  private ProductApprovalDetailStatusEvent productApprovalWithProductCode;
  private ProductCollection productCollection;
  private ProductHistory productHistory;
  private ProductWf productWf;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setApproveRetryCount(RETRY_COUNT);
    productCollection.setPrioritySeller(0);

    productHistory = new ProductHistory();
    productHistory.setProductId(DEFAULT_PRODUCT_ID);
    productApprovalDetailStatus =
        ProductApprovalDetailStatus.PBP_UpdateProductContent_Request_Sent_PCB;
    productApprovalWithProductCode =
        new ProductApprovalDetailStatusEvent(DEFAULT_PRODUCT_CODE, productApprovalDetailStatus, TIME_STAMP);
    Mockito.doNothing().when(productCollectionRepository)
        .updateProductStateByProductCode(eq(STORE_ID), Mockito.anyString(), Mockito.anyInt(), Mockito.any(Date.class));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, DEFAULT_PRODUCT_CODE)).thenReturn(productCollection);

    productWf = new ProductWf(DEFAULT_PRODUCT_CODE, WorkflowStates.NEED_CORRECTION.getValue());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(productWorkflowRepository);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productCollectionRepository);
    Mockito.verifyNoMoreInteractions(productHistoryRepository);
    Mockito.verifyNoMoreInteractions(productWfRepository, productLevel3V2Service);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerService);
  }

  @Test
  public void testUpdateRejectedProduct_whenSuccess() throws Exception {
    MDC.put("storeId", DEFAULT_STORE_ID);
    ProductWorkflow productWorkflow = new ProductWorkflow();
    productWorkflow.setState(4);
    ProductBusinessPartnerAttribute attribute = new ProductBusinessPartnerAttribute();
    List<ProductBusinessPartnerAttribute> attributes = new ArrayList<>();
    attributes.add(attribute);
    ProductItemBusinessPartner itemBusinessPartner = new ProductItemBusinessPartner();
    List<ProductItemBusinessPartner> itemBusinessPartners = new ArrayList<>();
    itemBusinessPartners.add(itemBusinessPartner);
    ProductBusinessPartner partner = new ProductBusinessPartner();
    partner.setProductBusinessPartnerAttributes(attributes);
    partner.setProductItemBusinessPartners(itemBusinessPartners);
    List<ProductBusinessPartner> partners = new ArrayList<>();
    partners.add(partner);
    Mockito.when(productBusinessPartnerRepository
        .findByStoreIdAndProductId(DEFAULT_STORE_ID, DEFAULT_PRODUCT_ID)).thenReturn(partners);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(new ProductCollection());
    List<ProductWorkflow> productWorkflows = new ArrayList<>();
    productWorkflows.add(productWorkflow);
    ProductDetailResponse response = new ProductDetailResponse();
    response.setId(DEFAULT_PRODUCT_ID);
    response.setName(DEFAULT_PRODUCT_NAME);
    Mockito
        .when(productRepository.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE, Boolean.TRUE))
        .thenReturn(response);
    Mockito.when(productWorkflowRepository
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_PRODUCT_ID))
        .thenReturn(productWorkflows);
    productLevel1CollectionServiceBean.updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    Mockito.verify(productRepository)
        .findProductDetailByProductCode(eq(DEFAULT_PRODUCT_CODE), eq(Boolean.TRUE));
    Mockito.verify(productBusinessPartnerRepository)
        .findByStoreIdAndProductId(DEFAULT_STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(eq(DEFAULT_STORE_ID),
            eq(DEFAULT_PRODUCT_ID));
    Mockito.verify(productWorkflowRepository).saveAndFlush(Mockito.any(ProductWorkflow.class));
    Mockito.verify(productBusinessPartnerRepository)
        .saveAll(Mockito.anyList());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(productCollectionRepository).save(Mockito.any(ProductCollection.class));
    Mockito.verify(productHistoryRepository).saveAndFlush(Mockito.any(ProductHistory.class));
    Mockito.verify(productWorkflowRepository).deleteAll(Mockito.anyList());
  }

  @Test
  public void testUpdateRejectedProduct_whenfail() throws Exception {
    MDC.put("storeId", DEFAULT_STORE_ID);
    ProductDetailResponse response = new ProductDetailResponse();
    response.setId(DEFAULT_PRODUCT_ID);
    response.setName(DEFAULT_PRODUCT_NAME);
    Mockito
        .when(productRepository.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE, Boolean.TRUE))
        .thenThrow(new Exception());
    try {
      productLevel1CollectionServiceBean.updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    }catch (Exception e) {
      Mockito.verify(productRepository)
          .findProductDetailByProductCode(eq(DEFAULT_PRODUCT_CODE), eq(Boolean.TRUE));
    }
  }
  
  @Test
  public void findByProductIdTest() throws Exception {
    MDC.put("storeId", "10001");
    this.productLevel1CollectionServiceBean.findByProductId(DEFAULT_PRODUCT_ID);
    verify(this.productCollectionRepository, times(1))
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(anyString(), anyString());
    MDC.clear();
  }

  @Test
  public void updateProductStatusWithProductCode(){
    this.productLevel1CollectionServiceBean.updateProductStatus(STORE_ID, productApprovalWithProductCode);
    Mockito.verify(productCollectionRepository).updateProductStateByProductCode(STORE_ID, DEFAULT_PRODUCT_CODE,
        productApprovalWithProductCode.getStatus().getEvent(), new Date(productApprovalWithProductCode.getStatusTimeStamp()));
  }

  @Test
  public void findProductCollectionByProductCode(){
    ProductCollection productCollection =
        this.productLevel1CollectionServiceBean.findByProductCode(STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, productCollection.getProductCode());
    Assertions.assertEquals(RETRY_COUNT, productCollection.getApproveRetryCount());
  }

  @Test
  public void findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse_fail(){
    Set<String> productIds = new HashSet<>();
    productIds.add(DEFAULT_PRODUCT_ID);
    Mockito.when(productCollectionRepository
        .findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(STORE_ID, productIds))
        .thenThrow(new ApplicationRuntimeException());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel1CollectionServiceBean
            .findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(STORE_ID, productIds, USERNAME,
                DEFAULT_NOTE, null);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(STORE_ID, productIds);
    }
  }

  @Test
  public void findProductCodesByStoreIdAndProductIdsInAndBPCodeAndMarkForDeleteFalse1_success() {
    ReflectionTestUtils.setField(productLevel1CollectionServiceBean,"validateBusinessPartnerCodeForSecurityEnabled",true);
    Set<String> productIds = new HashSet<>();
    ProductWipDeleteResponse response = new ProductWipDeleteResponse(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE);
    productIds.add(DEFAULT_PRODUCT_ID);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(productCollectionRepository
            .findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(STORE_ID, productIds))
        .thenReturn(Collections.singletonList(productCollection));
    Mockito.when(productService.publishProductWipDeleteEvent(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE))
        .thenReturn(response);
    PostLiveProductCountResponse postLiveProductCountResponse = this.productLevel1CollectionServiceBean
        .findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(STORE_ID, productIds, USERNAME, DEFAULT_NOTE,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(productCollectionRepository)
        .findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(STORE_ID, productIds);
    Mockito.verify(productService).publishProductWipDeleteEvent(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE);
    Assertions.assertEquals(0, postLiveProductCountResponse.getPostLiveProductCount());
  }


  @Test
  public void findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalseWithNewNRDeletionEnabled() {
    ReflectionTestUtils.setField(productLevel1CollectionServiceBean, "validateBusinessPartnerCodeForSecurityEnabled",
        true);
    ReflectionTestUtils.setField(productLevel1CollectionServiceBean, "newNRFlowEnabled", true);
    Set<String> productIds = new HashSet<>();
    ProductWipDeleteResponse response = new ProductWipDeleteResponse(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE);
    productIds.add(DEFAULT_PRODUCT_ID);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse = new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    needRevisionEligibilityResponse.setEligibleForDeletion(Boolean.TRUE);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse1 = new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse1.setProductCode("MTA-2000");
    needRevisionEligibilityResponse1.setEligibleForDeletion(Boolean.FALSE);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdInAndMarkForDeleteFalse(STORE_ID, productIds))
        .thenReturn(Collections.singletonList(productCollection));
    Mockito.when(productLevel3V2Service.eligibilityForNeedRevisionDeletion(Mockito.anyString(), Mockito.anyList(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(needRevisionEligibilityResponse, needRevisionEligibilityResponse1));
    Mockito.when(productService.publishProductWipDeleteEvent(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE))
        .thenReturn(response);
    PostLiveProductCountResponse postLiveProductCountResponse =
        this.productLevel1CollectionServiceBean.findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(STORE_ID,
            productIds, USERNAME, DEFAULT_NOTE, BUSINESS_PARTNER_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdInAndMarkForDeleteFalse(STORE_ID, productIds);
    Mockito.verify(productService).publishProductWipDeleteEvent(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE);
    Mockito.verify(productLevel3V2Service).eligibilityForNeedRevisionDeletion(Mockito.anyString(), Mockito.anyList(), Mockito.eq(false));
    Assertions.assertEquals(0, postLiveProductCountResponse.getPostLiveProductCount());
  }


  @Test
  public void findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse_success() {
    Set<String> productIds = new HashSet<>();
    ProductWipDeleteResponse response = new ProductWipDeleteResponse(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE);
    productIds.add(DEFAULT_PRODUCT_ID);
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    Mockito.when(productCollectionRepository
        .findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(STORE_ID, productIds))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productService.publishProductWipDeleteEvent(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE))
        .thenReturn(response);
    PostLiveProductCountResponse postLiveProductCountResponse = this.productLevel1CollectionServiceBean
        .findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(STORE_ID, productIds, USERNAME, DEFAULT_NOTE,
            null);
    Mockito.verify(productCollectionRepository)
        .findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(STORE_ID, productIds);
    Mockito.verify(productService).publishProductWipDeleteEvent(DEFAULT_PRODUCT_CODE, USERNAME, DEFAULT_NOTE);
    Assertions.assertEquals(0, postLiveProductCountResponse.getPostLiveProductCount());
  }

  @Test
  public void findTop1ProductHistoryNotesTest() {
    Mockito.when(productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, DEFAULT_PRODUCT_ID))
        .thenReturn(productHistory);
    String top1ProductHistoryNotes =
        productLevel1CollectionServiceBean.findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, DEFAULT_PRODUCT_ID);
    Assertions.assertTrue(StringUtils.isEmpty(top1ProductHistoryNotes));
  }

  @Test
  public void approveDraftTest() throws Exception {
    Mockito.when(
      productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(),
        any())).thenReturn(productCollection);
    this.productLevel1CollectionServiceBean.approveDraft(DEFAULT_PRODUCT_CODE,
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(false)
        .build());
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), any());
    verify(productCollectionRepository).save(any(ProductCollection.class));
  }

  @Test
  public void approveDraftTestForTrustedSeller() throws Exception {
    Mockito.when(
      productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(),
          any())).thenReturn(productCollection);
    this.productLevel1CollectionServiceBean.approveDraft(DEFAULT_PRODUCT_CODE,
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(true)
        .build());
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), any());
    verify(productCollectionRepository).save(any(ProductCollection.class));
  }

  @Test
  public void approveDraftTestWithRestricted() throws Exception {
    productCollection.setRestrictedKeywordsPresent(true);
    Mockito.when(
      productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(anyString(),
        anyString())).thenReturn(productCollection);
    this.productLevel1CollectionServiceBean.approveDraft(DEFAULT_PRODUCT_CODE,
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(false)
        .build());
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      anyString(), anyString());
    verify(productCollectionRepository).save(any(ProductCollection.class));
  }

  @Test
  public void approveDraftTestForTrustedSelleWithRestrictedKeyword() throws Exception {
    productCollection.setRestrictedKeywordsPresent(true);
    Mockito.when(
      productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(anyString(),
        anyString())).thenReturn(productCollection);
    this.productLevel1CollectionServiceBean.approveDraft(DEFAULT_PRODUCT_CODE,
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(true)
        .build());
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
      anyString(), anyString());
    verify(productCollectionRepository).save(any(ProductCollection.class));
  }



  @Test
  public void findTop1ProductHistoryNotesNullTest() {
    String top1ProductHistoryNotes =
        productLevel1CollectionServiceBean.findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, DEFAULT_PRODUCT_ID);
    Assertions.assertTrue(StringUtils.isEmpty(top1ProductHistoryNotes));
  }

  @Test
  public void findTop1ProductHistoryNoteTest() {
    productHistory.setNotes(DEFAULT_NOTE);
    Mockito.when(productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, DEFAULT_PRODUCT_ID))
        .thenReturn(productHistory);
    String top1ProductHistoryNotes =
        productLevel1CollectionServiceBean.findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, DEFAULT_PRODUCT_ID);
    Assertions.assertEquals(DEFAULT_NOTE, top1ProductHistoryNotes);
  }

  @Test
  public void updateAndSendForCorrectionTest() {
    productWf.setStoreId(Constants.DEFAULT_STORE_ID);
    Mockito.when(this.productCollectionRepository.save(productCollection)).thenReturn(productCollection);
    productLevel1CollectionServiceBean.updateAndSendForCorrection(productCollection);
    Mockito.verify(this.productCollectionRepository).save(productCollection);
    Mockito.verify(this.productWfRepository).save(eq(productWf));
  }

  @Test
  public void updateProductWorkFlowStateForNeedRevisionTest() {
    productWf.setStoreId(Constants.DEFAULT_STORE_ID);
    productLevel1CollectionServiceBean.updateProductWorkFlowStateForNeedRevision(productCollection.getProductCode());
    Mockito.verify(this.productWfRepository).save(eq(productWf));
  }

  @Test
  public void getProductTypeBasedOnProductCodeOrIdTest() {
    productLevel1CollectionServiceBean.getProductTypeBasedOnProductCodeOrId(productCollection.getProductCode(),
        Constants.DEFAULT_STORE_ID);
    Mockito.verify(productBusinessPartnerService).getProductTypeBasedOnProductCodeOrId(productCollection.getProductCode(),
        Constants.DEFAULT_STORE_ID);
  }

}
