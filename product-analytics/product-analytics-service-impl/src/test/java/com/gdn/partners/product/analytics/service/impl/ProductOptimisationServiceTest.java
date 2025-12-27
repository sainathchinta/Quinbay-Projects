package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import com.gdn.partners.product.analytics.entity.ProductOptimiseFeedback;
import com.gdn.partners.product.analytics.model.enums.ProductOptimisationDetailsStatus;
import com.gdn.partners.product.analytics.repository.ProductOptimisationRepository;
import com.gdn.partners.product.analytics.repository.ProductOptimiseFeedbackRepository;
import com.gdn.partners.product.analytics.service.cache.ProductOptimisationCacheableService;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationUpdateStatusRequest;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ProductOptimisationServiceTest {

  private static final long PRODUCT_COUNTS = 2;
  private static final String SELLER_CODE = "sellerCode";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";

  @InjectMocks
  private ProductOptimisationServiceImpl productOptimisationService;

  @Mock
  private ProductOptimisationCacheableService productOptimisationCacheableService;

  @Mock
  private ProductOptimisationRepository productOptimisationRepository;

  @Mock
  private ProductOptimiseFeedbackRepository productOptimiseFeedbackRepository;

  private ProductOptimisationDetails productOptimisationDetails;
  private ProductOptimiseFeedback productOptimiseFeedback;
  private ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest;

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productOptimisationCacheableService);
  }

  @BeforeEach
  void setUp() {
    this.productOptimisationDetails = new ProductOptimisationDetails();
    this.productOptimiseFeedback = new ProductOptimiseFeedback();
    this.productOptimisationUpdateStatusRequest = new ProductOptimisationUpdateStatusRequest();
    productOptimisationUpdateStatusRequest.setViewed(Boolean.TRUE);
    productOptimisationUpdateStatusRequest.setProductSku(PRODUCT_SKU);
    productOptimisationUpdateStatusRequest.setFieldEdited(new ArrayList<>());
  }

  @Test
  void getProductCountsTest() {
    when(productOptimisationCacheableService.findProductCountCacheablesBySellerCode(SELLER_CODE))
        .thenReturn(PRODUCT_COUNTS);
    productOptimisationService.getProductCounts(STORE_ID, SELLER_CODE);
    verify(productOptimisationCacheableService).findProductCountCacheablesBySellerCode(SELLER_CODE);
  }

  @Test
  void updateProductOptimiseStatusTest() {
    when(productOptimisationRepository.findByProductSku(PRODUCT_SKU))
        .thenReturn(productOptimisationDetails);
    when(productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimiseFeedback);
    productOptimisationService.updateStatusForProductOptimisation(productOptimisationUpdateStatusRequest);
    verify(productOptimisationRepository).findByProductSku(PRODUCT_SKU);
    verify(productOptimiseFeedbackRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    verify(productOptimiseFeedbackRepository).save(any());
    verify(productOptimisationRepository).save(any());
    Assertions.assertEquals(ProductOptimisationDetailsStatus.VIEWED.getValue(),
        productOptimisationDetails.getStatusInDb());
  }

  @Test
  void updateProductOptimiseStatusFieldUpdateTest() {
    productOptimisationUpdateStatusRequest.setViewed(Boolean.FALSE);
    productOptimisationUpdateStatusRequest.setFieldEdited(Collections.singletonList(PRODUCT_SKU));
    when(productOptimisationRepository.findByProductSku(PRODUCT_SKU))
        .thenReturn(productOptimisationDetails);
    when(productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimiseFeedback);
    productOptimisationService.updateStatusForProductOptimisation(productOptimisationUpdateStatusRequest);
    verify(productOptimisationRepository).findByProductSku(PRODUCT_SKU);
    verify(productOptimiseFeedbackRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    verify(productOptimiseFeedbackRepository).save(any());
    verify(productOptimisationRepository).save(any());
    Assertions.assertEquals(ProductOptimisationDetailsStatus.EDITED.getValue(),
        productOptimisationDetails.getStatusInDb());
  }

  @Test
  void updateProductOptimiseStatusEmptyFeedbackTest() {
    productOptimisationUpdateStatusRequest.setViewed(null);
    productOptimisationUpdateStatusRequest.setFieldEdited(null);
    when(productOptimisationRepository.findByProductSku(PRODUCT_SKU))
        .thenReturn(productOptimisationDetails);
    when(productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(null);
    productOptimisationService.updateStatusForProductOptimisation(productOptimisationUpdateStatusRequest);
    verify(productOptimisationRepository).findByProductSku(PRODUCT_SKU);
    verify(productOptimiseFeedbackRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    verify(productOptimiseFeedbackRepository).save(any());
    verify(productOptimisationRepository).save(any());
    Assertions.assertEquals(ProductOptimisationDetailsStatus.NEW.getValue(),
        productOptimisationDetails.getStatusInDb());
  }

  @Test
  void clearSellerLevelCacheTest() {
    productOptimisationService.clearSellerLevelCache(SELLER_CODE);
    verify(productOptimisationCacheableService).evictCacheBySellerCode(SELLER_CODE);
  }

  @Test
  void clearSellerLevelCacheEmptyTest() {
    productOptimisationService.clearSellerLevelCache(StringUtils.EMPTY);
    verify(productOptimisationCacheableService, times(0)).evictCacheBySellerCode(SELLER_CODE);
  }
}
