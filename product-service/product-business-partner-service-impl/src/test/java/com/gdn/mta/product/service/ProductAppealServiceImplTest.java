package com.gdn.mta.product.service;

import com.gda.mta.product.dto.AppealProductConfigResponse;
import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.ProductBusinessPartnerCounterRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProductAppealServiceImplTest {

  private final String STORE_ID = "store-id";
  private final String BP_CODE = "bp-code";
  private final String PRODUCT_CODE = "product-code";
  private final String PRODUCT_SKU = "product-sku";

  ProductSystemParameter productSystemParameter;
  ProductBusinessPartnerCounter businessPartnerCounter;
  AppealProductRequest appealProductRequest;
  ProductBusinessPartner productBusinessPartner;

  @InjectMocks
  private ProductAppealServiceImpl productAppealServiceImpl;

  @Mock
  private ProductSystemParameterServiceImpl productSystemParameterService;
  
  @Mock
  private ProductBusinessPartnerCounterRepository businessPartnerCounterRepository;
  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @BeforeEach
  public void setup() {
    productSystemParameter = new ProductSystemParameter();
    businessPartnerCounter = new ProductBusinessPartnerCounter();
    appealProductRequest = new AppealProductRequest();
    productBusinessPartner = new ProductBusinessPartner();
    appealProductRequest.setBusinessPartnerCode(BP_CODE);
    appealProductRequest.setProductCode(PRODUCT_CODE);
    appealProductRequest.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
    Mockito.verifyNoMoreInteractions(businessPartnerCounterRepository);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
  }

  @Test
  void updateAppealProductInEligibleTest() throws ApplicationException {
    productSystemParameter.setValue("10");
    businessPartnerCounter.setAppealedProductCount(10);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    AppealProductResponse response =
      productAppealServiceImpl.updateAppealProductForInProgressProducts(STORE_ID, appealProductRequest);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Assertions.assertTrue(StringUtils.isNotEmpty(response.getErrorCode()));
  }

  @Test
  void updateAppealProductNullExceptionTest() throws ApplicationException {
    productSystemParameter.setValue("10");
    businessPartnerCounter.setAppealedProductCount(9);
    Mockito.when(
      productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(PRODUCT_SKU))
      .thenReturn(null);
    AppealProductResponse response =
      productAppealServiceImpl.updateAppealProductForInProgressProducts(STORE_ID,
        appealProductRequest);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Assertions.assertTrue(StringUtils.isNotEmpty(response.getErrorCode()));
    Assertions.assertEquals(response.getErrorCode(), ApiErrorCode.PRODUCT_IN_INVALID_STATE.getCode());
  }

  @Test
  void updateAppealProductEligibleProductTest() throws ApplicationException {
    productBusinessPartner.setState(Constants.IN_PROGRESS_STATE);
    productSystemParameter.setValue("10");
    businessPartnerCounter.setAppealedProductCount(9);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(productDistributionTaskRepository.updateAppealProduct(appealProductRequest))
      .thenReturn(AppealProductResponse.builder().build());
    AppealProductResponse response =
      productAppealServiceImpl.updateAppealProductForInProgressProducts(STORE_ID,
        appealProductRequest);
    businessPartnerCounter.setAppealedProductCount(10);
    productBusinessPartner.setAppealedProduct(true);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository).save(businessPartnerCounter);
    Mockito.verify(productBusinessPartnerRepository).save(productBusinessPartner);
    Mockito.verify(productDistributionTaskRepository).updateAppealProduct(appealProductRequest);
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorCode()));
  }

  @Test
  void updateAppealProductEligibleNullCounterTest() throws ApplicationException {
    productBusinessPartner.setState("IN_PROGRESS");
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(null);
    Mockito.when(
      productBusinessPartnerRepository.findFirstByGdnProductSku(PRODUCT_SKU)).thenReturn(productBusinessPartner);
    Mockito.when(productDistributionTaskRepository.updateAppealProduct(appealProductRequest))
      .thenReturn(AppealProductResponse.builder().build());
    AppealProductResponse response =
      productAppealServiceImpl.updateAppealProductForInProgressProducts(STORE_ID,
        appealProductRequest);
    productBusinessPartner.setAppealedProduct(true);
    businessPartnerCounter.setAppealedProductCount(1);
    businessPartnerCounter.setBusinessPartnerCode(BP_CODE);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository).save(businessPartnerCounter);
    Mockito.verify(productBusinessPartnerRepository).save(productBusinessPartner);
    Mockito.verify(productDistributionTaskRepository).updateAppealProduct(appealProductRequest);
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorCode()));
  }

  @Test
  void updateAppealProductL3EligibleProductTest() throws ApplicationException {
    productBusinessPartner.setState(Constants.ACTIVE);
    productSystemParameter.setValue("10");
    businessPartnerCounter.setAppealedProductCount(9);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    AppealProductResponse response =
      productAppealServiceImpl.updateAppealProductForInProgressProducts(STORE_ID,
        appealProductRequest);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Assertions.assertTrue(StringUtils.isNotEmpty(response.getErrorCode()));
  }

  @Test
  void fetchThresholdAndCounterForAppealProductTest() {
    productSystemParameter.setValue("10");
    businessPartnerCounter.setAppealedProductCount(10);
    Mockito.when(
        productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
            SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    Mockito.when(
            businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
        .thenReturn(businessPartnerCounter);

    Pair<Integer, ProductBusinessPartnerCounter> pair =
        productAppealServiceImpl.fetchThresholdAndCounterForAppealProduct(STORE_ID, BP_CODE);

    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID,
            SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Mockito.verify(businessPartnerCounterRepository)
        .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Assertions.assertEquals(10, pair.getLeft());
    Assertions.assertEquals(businessPartnerCounter,  pair.getRight());

  }

  @Test
  void incrementCounterForProductAppealTest() {
    productAppealServiceImpl.incrementCounterForProductAppeal(businessPartnerCounter);
    Mockito.verify(businessPartnerCounterRepository).save(businessPartnerCounter);
    Assertions.assertEquals(1, businessPartnerCounter.getAppealedProductCount());
  }

  @Test
  void decrementCounterForProductAppealTest() {
    businessPartnerCounter.setAppealedProductCount(1);
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    businessPartnerCounter.setAppealedProductCount(
      businessPartnerCounter.getAppealedProductCount() - 1);
    productAppealServiceImpl.decrementCounterForProductAppeal(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository).save(businessPartnerCounter);
  }

  @Test
  void fetchAppealProductConfigNonNullBpCounterTest() {
    businessPartnerCounter.setAppealedProductCount(3);
    productSystemParameter.setValue("3");
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    AppealProductConfigResponse appealProductConfigResponse =
      productAppealServiceImpl.fetchAppealProductConfig(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Assertions.assertFalse(appealProductConfigResponse.isEligible());
  }

  @Test
  void fetchAppealProductConfigNonNullBpCounterEligibleTest() {
    businessPartnerCounter.setAppealedProductCount(2);
    productSystemParameter.setValue("3");
    Mockito.when(
        businessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE))
      .thenReturn(businessPartnerCounter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    AppealProductConfigResponse appealProductConfigResponse =
      productAppealServiceImpl.fetchAppealProductConfig(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Assertions.assertTrue(appealProductConfigResponse.isEligible());
  }

  @Test
  void fetchAppealProductConfigNullBpCounterTest() {
    productSystemParameter.setValue("100");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.APPEAL_PRODUCT_LIMIT)).thenReturn(productSystemParameter);
    AppealProductConfigResponse appealProductConfigResponse =
      productAppealServiceImpl.fetchAppealProductConfig(STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerCounterRepository)
      .findByStoreIdAndBusinessPartnerCode(STORE_ID, BP_CODE);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.APPEAL_PRODUCT_LIMIT);
    Assertions.assertTrue(appealProductConfigResponse.isEligible());
  }
}
