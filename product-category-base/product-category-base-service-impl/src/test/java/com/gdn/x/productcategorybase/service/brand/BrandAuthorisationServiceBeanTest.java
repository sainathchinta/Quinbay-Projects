package com.gdn.x.productcategorybase.service.brand;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationWipRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;

import org.apache.commons.lang3.StringUtils;
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
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepository;

public class BrandAuthorisationServiceBeanTest {

  private static final String DEFAULT_BRAND_CODE = "BRD-00000";
  private static final String PREDICTED_BRAND_NAME = "Apple";
  private static final String BRAND_NAME = "brandName";
  private static final String DEFAULT_SELLER_CODE = "seller code";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String NEW_SELLER_CODE = "new seller code";
  private static final String DEFAULT_BRAND_NAME = "TEST_BRAND";
  private static final String DEFAULT_SELLER_CODE_1 = "seller code 1";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DOCUMENT_LINK = "document link";
  private static final String DEFAULT_PRODUCT_CODE = "PRD-00000";
  private BrandAuthorisation brandAuthorisation;
  private BrandAuthorisationWip brandAuthorisationWip;
  private ProductBrandValidationRequest productBrandValidationRequest;
  private ProtectedBrandResponse protectedBrandResponse;
  private ProtectedBrandResponse protectedBrandResponse1;
  private BrandAuthCreateRequest brandAuthCreateRequest;
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @InjectMocks
  private BrandAuthorisationServiceBean brandAuthorisationServiceBean;

  @Mock
  private BrandAuthorisationRepository brandAuthorisationRepository;

  @Mock
  private BrandAuthorisationWipRepository brandAuthorisationWipRepository;

  @Mock
  private BrandService brandService;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private BrandAuthHistoryService brandAuthHistoryService;

  private BrandResponse brand;

  private BrandAuthUpdateRequest brandAuthUpdateRequest = new BrandAuthUpdateRequest();
  private BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    brandAuthorisation.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisation.setSellerCode(DEFAULT_SELLER_CODE);
    productBrandValidationRequest = new ProductBrandValidationRequest();
    productBrandValidationRequest.setPredictedBrandName(PREDICTED_BRAND_NAME);
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_BRAND_CODE);
    productBrandValidationRequest.setSellerCode(DEFAULT_SELLER_CODE);
    protectedBrandResponse = new ProtectedBrandResponse();
    protectedBrandResponse.setBrandCode(DEFAULT_BRAND_CODE);
    protectedBrandResponse.setBrandName(PREDICTED_BRAND_NAME);
    protectedBrandResponse1 = new ProtectedBrandResponse();
    protectedBrandResponse1.setBrandCode(DEFAULT_STORE_ID);
    protectedBrandResponse1.setBrandName(PREDICTED_BRAND_NAME);
    brandAuthUpdateRequest =
      BrandAuthUpdateRequest.builder().brandCode(DEFAULT_BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.name()).authStartDate(new Date())
        .authExpireDate(new Date()).build();
    when(applicationContext.getBean(BrandAuthorisationService.class)).thenReturn(brandAuthorisationServiceBean);
    brand = new BrandResponse();
    brand.setProtectedBrand(true);
    when(brandService.findByBrandCodeCached(anyString(), anyString())).thenReturn(brand);
    brandAuthCreateRequest = new BrandAuthCreateRequest();
    brandAuthorisationHistory.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationHistory.setStoreId(DEFAULT_STORE_ID);
    brandAuthorisationHistory.setUpdatedDate(new Date());
    ReflectionTestUtils.setField(brandAuthorisationServiceBean, "numberOfYears", 5);
    ReflectionTestUtils.setField(brandAuthorisationServiceBean, "brandAuthNearExpiryDaysThreshold",
        1);

    brandAuthorisationWip = new BrandAuthorisationWip();
    brandAuthorisationWip.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisationWip.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(brandAuthorisationRepository);
    Mockito.verifyNoMoreInteractions(applicationContext);
    Mockito.verifyNoMoreInteractions(brandService);
    Mockito.verifyNoMoreInteractions(applicationCacheServiceBean);
  }

  @Test
  public void takeDownProductBasedOnBrandTest() throws Exception {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Collections.singletonList(protectedBrandResponse));
    Mockito.when(brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertFalse(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandProtectedFalseTest() throws Exception {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Collections.emptyList());
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertFalse(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandFalseTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_STORE_ID);
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    Mockito.when(brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Arrays.asList(protectedBrandResponse, protectedBrandResponse1));
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(applicationContext, Mockito.times(2)).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
  }

  @Test
  public void takeDownProductBasedOnBrandPredictedBrandFalseTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_STORE_ID);
    protectedBrandResponse.setBrandCode(DEFAULT_STORE_ID);
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Collections.singletonList(protectedBrandResponse));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    Mockito.when(brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Collections.singletonList(protectedBrandResponse));
    productBrandValidationRequest.setPredictedBrandName(null);
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertFalse(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandTrueTestRelaxTakeDownTest() throws Exception {
    ReflectionTestUtils.setField(brandAuthorisationServiceBean,
      "relaxTakeDownForProtectedBrandSwitch", true);
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_STORE_ID);
    productBrandValidationRequest.setSellerCode(DEFAULT_STORE_ID);
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
      .thenReturn(Arrays.asList(protectedBrandResponse, protectedBrandResponse1));
    boolean takeDown = brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID,
      productBrandValidationRequest);
    verify(applicationContext, Mockito.times(2)).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertTrue(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandTrueTestRelaxTakeDownTest1() throws Exception {
    brand.setSkuCreationAllowedForAllSellers(true);
    ReflectionTestUtils.setField(brandAuthorisationServiceBean,
      "relaxTakeDownForProtectedBrandSwitch", true);
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_STORE_ID);
    productBrandValidationRequest.setSellerCode(DEFAULT_STORE_ID);
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
      .thenReturn(Arrays.asList(protectedBrandResponse, protectedBrandResponse1));
    boolean takeDown = brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID,
      productBrandValidationRequest);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertFalse(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandTrueTest() throws Exception {
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_STORE_ID);
    productBrandValidationRequest.setSellerCode(DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Arrays.asList(protectedBrandResponse, protectedBrandResponse1));
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(applicationContext, Mockito.times(2)).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertTrue(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandFalseTest2() throws Exception {
    productBrandValidationRequest.setRequestedBrandCode(DEFAULT_STORE_ID);
    productBrandValidationRequest.setSellerCode(DEFAULT_STORE_ID);
    productBrandValidationRequest.setPredictedBrandName(DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Arrays.asList(protectedBrandResponse, protectedBrandResponse1));
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_STORE_ID);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    Assertions.assertFalse(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandTest_emptyBrandCode() throws Exception {
    ReflectionTestUtils.setField(brandAuthorisationServiceBean, "fetchBrandCodeFromProduct", true);
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    productBrandValidationRequest.setRequestedBrandCode(StringUtils.EMPTY);
    productBrandValidationRequest.setProductCode(DEFAULT_PRODUCT_CODE);
    Mockito.when(brandService.getProtectedBrandList(anyString()))
        .thenReturn(Collections.singletonList(protectedBrandResponse));
    Mockito.when(brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Mockito.when(brandService.getBrandCodeByProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(DEFAULT_BRAND_CODE);
    boolean takeDown =
        brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(brandService).getProtectedBrandList(DEFAULT_STORE_ID);
    verify(brandService).getBrandCodeByProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertFalse(takeDown);
  }

  @Test
  public void takeDownProductBasedOnBrandTest_emptyBrandCodeFlagFalse() throws Exception {
    ReflectionTestUtils.setField(brandAuthorisationServiceBean, "fetchBrandCodeFromProduct", false);
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    productBrandValidationRequest.setRequestedBrandCode(StringUtils.EMPTY);
    productBrandValidationRequest.setProductCode(DEFAULT_PRODUCT_CODE);
    try {
      boolean takeDown = brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID,
          productBrandValidationRequest);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertEquals(
          "Can not process invalid input data :REQUESTED_BRAND_CODE_MUST_NOT_BE_BLANK",
          ex.getErrorMessage());
    }
  }

  @Test
  public void takeDownProductBasedOnBrandTest_emptyBrandCodeAndEmptyProductCode() throws Exception {
    productBrandValidationRequest.setRequestedBrandCode(StringUtils.EMPTY);
    productBrandValidationRequest.setProductCode(StringUtils.EMPTY);
    try {
      boolean takeDown = brandAuthorisationServiceBean.takeDownProductBasedOnBrand(DEFAULT_STORE_ID,
          productBrandValidationRequest);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertEquals(
          "Can not process invalid input data :REQUESTED_BRAND_CODE_MUST_NOT_BE_BLANK",
          ex.getErrorMessage());
    }
  }

  @Test
  public void checkBrandAuthBySellerCodeNullTest() throws Exception {
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(null);
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertFalse(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(null, DEFAULT_BRAND_CODE,
            DEFAULT_SELLER_CODE, false));
  }

  @Test
  public void checkBrandAuthBySellerCodeBrandCodeNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE,
            null, false));
  }

  @Test
  public void checkBrandAuthBySellerCodeSellerCodeNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, null,
            DEFAULT_BRAND_CODE, false));
  }

  @Test
  public void checkBrandAuthBySellerCodeINActiveTest() throws Exception {
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertFalse(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeDifferentSellerTest() throws Exception {
    brandAuthorisation.setSellerCode(DEFAULT_STORE_ID);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertFalse(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeDateBeforeTest() throws Exception {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertFalse(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeDateBefore1Test() throws Exception {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertFalse(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeDateAfterTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertFalse(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeDateTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeNotAuthorisedBrandTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brand.setProtectedBrand(false);
    when(brandService.findByBrandCodeCached(anyString(), anyString())).thenReturn(brand);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, false);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeNotAuthorisedBrandNullTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brand.setProtectedBrand(false);
    when(brandService.findByBrandCodeCached(anyString(), anyString())).thenReturn(null);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
            DEFAULT_BRAND_CODE, true);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(authorised);
  }

  @Test
  public void checkBrandAuthBySellerCodeNotAuthorisedBrandInternalTest() throws Exception {
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brand.setProtectedBrand(false);
    when(brandService.findByBrandCodeCached(anyString(), anyString())).thenReturn(brand);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Arrays.asList(brandAuthorisation));
    boolean authorised =
        brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, Constants.INTERNAL,
            DEFAULT_BRAND_CODE, false);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(authorised);
  }

  @Test
  public void findBrandAuthorisationBySellerCodeAndBrandCode() throws Exception {
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(new ArrayList<>());
    brandAuthorisationServiceBean.findBrandAuthorisationBySellerCodeAndBrandCode(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
  }

  @Test
  public void findBrandAuthorisationBySellerCodeAndBrandCodeNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> brandAuthorisationServiceBean.findBrandAuthorisationBySellerCodeAndBrandCode(null, DEFAULT_BRAND_CODE));
  }

  @Test
  public void findBrandAuthorisationBySellerCodeAndBrandCodeNullBrandCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> brandAuthorisationServiceBean.findBrandAuthorisationBySellerCodeAndBrandCode(DEFAULT_STORE_ID, null));
  }

  @Test
  public void findBrandAuthorisationByFilterEmptyContentTest() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    brandAuthFilterRequest.setStatus(Constants.ALL);
    brandAuthFilterRequest.setBrandAuthorise(true);
    Mockito.when(brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(Constants.ALL), any(Pageable.class), anyInt())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationRepository).findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(Constants.ALL), any(Pageable.class), anyInt());
    Assertions.assertEquals(0, response.getTotalElements());
  }

  @Test
  public void findBrandAuthorisationByFilterNullResponseTest() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    brandAuthFilterRequest.setStatus(Constants.ALL);
    brandAuthFilterRequest.setBrandAuthorise(true);
    Mockito.when(brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(Constants.ALL), any(Pageable.class), anyInt())).thenReturn(null);
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationRepository).findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(Constants.ALL), any(Pageable.class), anyInt());
    Assertions.assertEquals(0, response.getTotalElements());
    Assertions.assertEquals(0, response.getContent().size());
  }

  @Test
  public void findBrandAuthorisationByFilterTest() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisation brandAuthFilterResponse = new BrandAuthorisation();
    brandAuthFilterResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthFilterResponse.setAuthExpireDate(new Date());
    brandAuthFilterRequest.setStatus(Constants.ALL);
    brandAuthFilterRequest.setBrandAuthorise(true);
    Mockito.when(brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(Constants.ALL), any(Pageable.class), anyInt()))
        .thenReturn(new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)), PageRequest.of(0, 10), 1));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationRepository).findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(Constants.ALL), any(Pageable.class), anyInt());
    Assertions.assertEquals(1, response.getTotalElements());
    Assertions.assertEquals(1, response.getContent().size());
  }

  @Test
  public void findBrandAuthorisationByFilterNullStatusTest() {
    Pageable pageable = PageRequest.of(0, 10);
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisation brandAuthFilterResponse = new BrandAuthorisation();
    brandAuthFilterRequest.setStatus(Constants.ALL);
    brandAuthFilterResponse.setAuthStartDate(new Date());
    brandAuthFilterResponse.setAuthExpireDate(new Date());
    brandAuthFilterRequest.setBrandAuthorise(true);
    Mockito.when(
      brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
        DEFAULT_STORE_ID, null, null, Constants.ALL, pageable, 1)).thenReturn(
      new PageImpl<>(new ArrayList<>(List.of(brandAuthFilterResponse)), PageRequest.of(0, 10),
        1));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationRepository).findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(Constants.ALL), any(Pageable.class), anyInt());
    Assertions.assertEquals(1, response.getTotalElements());
    Assertions.assertEquals(1, response.getContent().size());
  }

  @Test
  public void findBrandAuthorisationWipByFilterTest() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisationWip brandAuthFilterResponse = new BrandAuthorisationWip();
    brandAuthFilterResponse.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthFilterRequest.setStatus(StringUtils.EMPTY);
    brandAuthFilterRequest.setBrandAuthorise(false);
    Mockito.when(brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(StringUtils.EMPTY), any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)), PageRequest.of(0, 10), 1));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationWipRepository).findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(StringUtils.EMPTY), any(Pageable.class));
    Assertions.assertEquals(1, response.getTotalElements());
    Assertions.assertEquals(1, response.getContent().size());
    Assertions.assertEquals(BrandAuthorisationStatus.ACTIVE.name(), response.getContent().getFirst().getStatus());
  }

  @Test
  public void findBrandAuthorisationWipByFilterTest_nearExpiry() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisationWip brandAuthFilterResponse = new BrandAuthorisationWip();
    brandAuthFilterResponse.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    brandAuthFilterRequest.setStatus(BrandAuthorizationWipStatus.NEAR_EXPIRY.name());
    brandAuthFilterRequest.setBrandAuthorise(false);
    Mockito.when(
            brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(
                eq(DEFAULT_STORE_ID), eq(null), eq(null),
                eq(BrandAuthorizationWipStatus.NEAR_EXPIRY.name()), any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)),
            PageRequest.of(0, 10), 1));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID,
            brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationWipRepository)
        .findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(
            eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(BrandAuthorizationWipStatus.NEAR_EXPIRY.name()), any(Pageable.class));
    Assertions.assertEquals(1, response.getTotalElements());
    Assertions.assertEquals(1, response.getContent().size());
    Assertions.assertEquals(BrandAuthorizationWipStatus.NEAR_EXPIRY.name(),
        response.getContent().getFirst().getStatus());
  }

  @Test
  public void findBrandAuthorisationWipByFilterTest_emptyResult() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisationWip brandAuthFilterResponse = new BrandAuthorisationWip();
    brandAuthFilterResponse.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthFilterRequest.setStatus(StringUtils.EMPTY);
    brandAuthFilterRequest.setBrandAuthorise(false);
    Mockito.when(brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(StringUtils.EMPTY), any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 10), 0));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationWipRepository).findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(StringUtils.EMPTY), any(Pageable.class));
    Assertions.assertEquals(0, response.getTotalElements());
    Assertions.assertEquals(0, response.getContent().size());
  }

  @Test
  public void findBrandAuthorisationWipByFilterTest_nullResponse() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisationWip brandAuthFilterResponse = new BrandAuthorisationWip();
    brandAuthFilterResponse.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthFilterRequest.setStatus(StringUtils.EMPTY);
    brandAuthFilterRequest.setBrandAuthorise(false);
    Mockito.when(brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(StringUtils.EMPTY), any(Pageable.class)))
        .thenReturn(null);
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationWipRepository).findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(StringUtils.EMPTY), any(Pageable.class));
    Assertions.assertEquals(0, response.getTotalElements());
    Assertions.assertEquals(0, response.getContent().size());
  }

  @Test
  public void findBrandAuthorisationWipByFilterTest_nullStatus() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisationWip brandAuthFilterResponse = new BrandAuthorisationWip();
    brandAuthFilterResponse.setAuthorisationStatus(null);
    brandAuthFilterRequest.setStatus(StringUtils.EMPTY);
    brandAuthFilterRequest.setBrandAuthorise(false);
    Mockito.when(brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(StringUtils.EMPTY), any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)), PageRequest.of(0, 10), 1));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationWipRepository).findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(StringUtils.EMPTY), any(Pageable.class));
    Assertions.assertEquals(1, response.getTotalElements());
    Assertions.assertEquals(1, response.getContent().size());
    Assertions.assertNull(response.getContent().getFirst().getStatus());
  }

  @Test
  public void findBrandAuthorisationWipByFilterTest_nonNullDocumentLink() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    BrandAuthorisationWip brandAuthFilterResponse = new BrandAuthorisationWip();
    brandAuthFilterResponse.setAuthorisationStatus(null);
    brandAuthFilterRequest.setStatus(StringUtils.EMPTY);
    brandAuthFilterRequest.setBrandAuthorise(false);
    brandAuthFilterResponse.setDocumentLink(DOCUMENT_LINK);
    Mockito.when(brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
            eq(StringUtils.EMPTY), any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)), PageRequest.of(0, 10), 1));
    Page<BrandAuthFilterResponse> response =
        brandAuthorisationServiceBean.findBrandAuthorisationByFilter(DEFAULT_STORE_ID, brandAuthFilterRequest, 0, 10);
    Mockito.verify(brandAuthorisationWipRepository).findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(eq(DEFAULT_STORE_ID), eq(null), eq(null),
        eq(StringUtils.EMPTY), any(Pageable.class));
    Assertions.assertEquals(1, response.getTotalElements());
    Assertions.assertEquals(1, response.getContent().size());
    Assertions.assertEquals(DOCUMENT_LINK, response.getContent().getFirst().getDocumentLinks().getFirst());
  }

  @Test
  public void getBrandAuthDetailByBrandCodeAndSellerCodeTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    this.brandAuthorisationServiceBean.getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID,DEFAULT_BRAND_CODE,DEFAULT_SELLER_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
  }

  @Test
  public void getBrandAuthDetailByBrandCodeAndSellerCode_brandCodeNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      null, DEFAULT_SELLER_CODE));
  }

  @Test
  public void getBrandAuthDetailByBrandCodeAndSellerCode_sellerCodeNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      DEFAULT_BRAND_CODE, null));
  }

  @Test
  public void getBrandAuthDetailByBrandCodeAndSellerCode_storeIdNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.getBrandAuthDetailByBrandCodeAndSellerCode(null,
      DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE));
  }

  @Test
  public void getBrandAuthDetailByBrandCodeAndSellerCode_EmptyBrandAuthTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.emptyList());
    BrandAuthorisationDetailResponse brandAuthDetailByBrandCodeAndSellerCode =
      this.brandAuthorisationServiceBean.getBrandAuthDetailByBrandCodeAndSellerCode(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(Objects.isNull(brandAuthDetailByBrandCodeAndSellerCode));
  }

  @Test
  public void getBrandAuthDetailByBrandCodeAndSellerCode_WrongSellerCodeTest() throws Exception {
    brandAuthorisation.setSellerCode(NEW_SELLER_CODE);
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    BrandAuthorisationDetailResponse brandAuthDetailByBrandCodeAndSellerCode =
      this.brandAuthorisationServiceBean.getBrandAuthDetailByBrandCodeAndSellerCode(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(Objects.isNull(brandAuthDetailByBrandCodeAndSellerCode));
  }

  @Test
  public void updateBrandNameByBrandCodeTest() {
    brandAuthorisationServiceBean.updateBrandNameByBrandCode(BRAND_NAME, PREDICTED_BRAND_NAME, DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).updateBrandNameByBrandCode(PREDICTED_BRAND_NAME, DEFAULT_BRAND_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
  }

  @Test
  public void updateBrandNameByBrandCodeSameBrandNameTest() {
    brandAuthorisationServiceBean.updateBrandNameByBrandCode(BRAND_NAME, BRAND_NAME, DEFAULT_BRAND_CODE);
  }

  @Test
  public void brandAuthCreateTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
  }

  @Test
  public void brandAuthCreateStartDateNullTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    brandAuthCreateRequest.setAuthStartDate(null);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    Assertions.assertNotNull(brandAuthCreateRequest.getAuthStartDate());
  }

  @Test
  public void brandAuthCreateEndDateNullTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setBulkAction(true);
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    brandAuthCreateRequest.setAuthExpireDate(null);
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    Assertions.assertNotNull(brandAuthCreateRequest.getAuthExpireDate());
  }

  @Test
  public void brandAuthCreateDocumentLinkNullTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK, null));
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME));
  }

  @Test
  public void brandAuthCreateDocumentLinkEmptyTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK, StringUtils.EMPTY));
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME));
  }

  @Test
  public void brandAuthCreateDateExceptionTest() throws Exception {
    Date date = new Date();
    Date expireDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, 1);
    date = calendar.getTime();
    calendar.add(Calendar.DATE, -1);
    expireDate = calendar.getTime();
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setAuthStartDate(date);
    brandAuthCreateRequest.setAuthExpireDate(expireDate);
    try {
      this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    }
    catch(Exception ex) {
      verify(this.brandAuthorisationRepository,
        BrandAuthorisationServiceBeanTest.NEVER_CALLED).save(any(BrandAuthorisation.class));
      Assertions.assertTrue(ex.getMessage().contains(ErrorMessage.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE.getMessage()));
    }
  }

  @Test
  public void brandAuthExistingExceptionTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setSellerCode(DEFAULT_SELLER_CODE);
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    try {
      this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    }
    catch(Exception ex) {
      verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
      verify(this.brandAuthorisationRepository,
        BrandAuthorisationServiceBeanTest.NEVER_CALLED).save(any(BrandAuthorisation.class));
      verify(applicationContext).getBean(BrandAuthorisationService.class);
      verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
      Assertions.assertTrue(ex.getMessage().contains(ErrorMessage.AUTH_ALREADY_PRESENT.getMessage()));
    }
  }

  @Test
  public void brandAuthCreateEmptyBrandListTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(new ArrayList<>());
    this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
  }

  private BrandAuthCreateRequest generateCreateBrandAuthRequest() {
    Date date = new Date();
    Date expireDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, 1);
    date = calendar.getTime();
    calendar.add(Calendar.DATE, 10);
    expireDate = calendar.getTime();
    BrandAuthCreateRequest request = new BrandAuthCreateRequest();
    request.setBrandName(DEFAULT_BRAND_NAME);
    request.setBrandCode(DEFAULT_BRAND_CODE);
    request.setSellerCode(DEFAULT_SELLER_CODE_1);
    request.setAuthExpireDate(expireDate);
    request.setAuthStartDate(date);
    request.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue());
    return request;
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCodeTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    doNothing().when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        GdnMandatoryParameterUtil.getUsername(), DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
      StringUtils.EMPTY);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(this.brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(this.brandAuthorisationRepository).deleteByStoreIdAndBrandCodeAndSellerCode(
      DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_BRAND_CODE,
      DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(anyString());
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCode_ApplicationRuntimeTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.emptyList());
    doNothing().when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        GdnMandatoryParameterUtil.getUsername(), DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    try {
      this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
        StringUtils.EMPTY);
    } finally {
      verify(applicationContext).getBean(BrandAuthorisationService.class);
      verify(this.brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    }
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCode_InvalidSellerCodeTest() throws Exception {
    this.brandAuthorisation.setSellerCode(NEW_SELLER_CODE);
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    doNothing().when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        GdnMandatoryParameterUtil.getUsername(), DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    try {
      this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
        StringUtils.EMPTY);
    } finally {
      verify(applicationContext).getBean(BrandAuthorisationService.class);
      verify(this.brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    }
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCode__storeIdNullTest() throws Exception {
    doNothing().when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(anyString(), anyString(), anyString(), anyString());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(null, DEFAULT_USERNAME,
      DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY, StringUtils.EMPTY));
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCode_usernameNullTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    doNothing().when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(anyString(), anyString(), anyString(), anyString());
    this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID, null,
      DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY, StringUtils.EMPTY);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(this.brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(this.brandAuthorisationRepository).deleteByStoreIdAndBrandCodeAndSellerCode(any(),
        any(), any(), any());
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCode_usernameEmptyTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    doNothing().when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(anyString(), anyString(), anyString(), anyString());
    this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID, "",
      DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY, StringUtils.EMPTY);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
    verify(this.brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(this.brandAuthorisationRepository).deleteByStoreIdAndBrandCodeAndSellerCode(anyString(),
        any(), anyString(), anyString());
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
  }

  @Test
  public void deleteMappingByBrandCodeAndSellerCode_SqlExceptionTest() throws Exception {
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    doThrow(ApplicationRuntimeException.class).when(brandAuthorisationRepository)
      .deleteByStoreIdAndBrandCodeAndSellerCode(any(), any(), any(), any());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID, "",
        DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY, StringUtils.EMPTY));
    } finally {
      verify(applicationContext).getBean(BrandAuthorisationService.class);
      verify(this.brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
      verify(this.brandAuthorisationRepository).deleteByStoreIdAndBrandCodeAndSellerCode(
          any(), any(), any(), any());
    }
  }

  @Test()
  public void getBrandAuthBulkResponseTest() {
    List<String> ids = Arrays.asList("id1", "id2");
    Mockito.when(brandAuthorisationRepository
      .findByStoreIdAndIdInAndMarkForDeleteFalse(DEFAULT_STORE_ID, new HashSet<>(ids)))
      .thenReturn(Arrays.asList(brandAuthorisation));
    List<BrandAuthBulkDownloadResponse> responseList =
    this.brandAuthorisationServiceBean.getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids);
    Mockito.verify(brandAuthorisationRepository)
      .findByStoreIdAndIdInAndMarkForDeleteFalse(DEFAULT_STORE_ID, new HashSet<>(ids));
    Assertions.assertEquals(1,responseList.size());
  }

  @Test()
  public void getBrandAuthBulkEmptyResponseTest() {
    List<String> ids = Arrays.asList("id1", "id2");
    Mockito.when(brandAuthorisationRepository
      .findByStoreIdAndIdInAndMarkForDeleteFalse(DEFAULT_STORE_ID, new HashSet<>(ids)))
      .thenReturn(new ArrayList<>());
    List<BrandAuthBulkDownloadResponse> responseList =
      this.brandAuthorisationServiceBean.getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids);
    Mockito.verify(brandAuthorisationRepository)
      .findByStoreIdAndIdInAndMarkForDeleteFalse(DEFAULT_STORE_ID, new HashSet<>(ids));
    Assertions.assertEquals(0,responseList.size());
  }

  @Test
  public void editBrandAuthDetailsTest() throws Exception {
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE.name());
    brandAuthUpdateRequest.setAuthStartDate(null);
    brandAuthUpdateRequest.setAuthExpireDate(null);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID,brandAuthorisation,
      brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetailsTest_editFromUpcoming() throws Exception {
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorisationStatus.UPCOMING.name());
    brandAuthUpdateRequest.setAuthStartDate(null);
    brandAuthUpdateRequest.setAuthExpireDate(null);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID,brandAuthorisation,
        brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetailsDocumentLinksTest() throws Exception {
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE.name());
    brandAuthUpdateRequest.setAuthStartDate(null);
    brandAuthUpdateRequest.setAuthExpireDate(null);
    brandAuthorisation.setDocumentLink(DOCUMENT_LINK);
    brandAuthUpdateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID,brandAuthorisation,
        brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetailsSavedDocumentLinksTest() throws Exception {
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE.name());
    brandAuthUpdateRequest.setAuthStartDate(null);
    brandAuthUpdateRequest.setAuthExpireDate(null);
    brandAuthUpdateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID,brandAuthorisation,
        brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetails_DateTest() throws Exception {
    brandAuthUpdateRequest.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthUpdateRequest.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(10))));
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID,brandAuthorisation,
      brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetails_missingParamTest() throws Exception {
    brandAuthUpdateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    brandAuthUpdateRequest.setAuthStartDate(null);
    brandAuthUpdateRequest.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(10))));
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation))
      .thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
      brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetails_BlankStatusTest() throws Exception {
    brandAuthUpdateRequest.setAuthExpireDate(null);
    brandAuthUpdateRequest.setAuthorisationStatus("");
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation))
      .thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
      brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetails_WrongDateTest() throws Exception {
    brandAuthUpdateRequest.setAuthStartDate(new Date(1689897600000L));
    brandAuthUpdateRequest.setAuthExpireDate(new Date(1689897600000L));
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation))
      .thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
      brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void editBrandAuthDetails_ApplicationExceptionTest() throws Exception {
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID, null, brandAuthUpdateRequest));
  }

  @Test
  public void brandNotExistingAuthCreateTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    when(brandService.findByBrandCodeCached(anyString(), anyString())).thenReturn(null);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    try {
      this.brandAuthorisationServiceBean
        .create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    } catch (Exception ex) {
      Assertions.assertTrue(
        ex.getMessage().contains(ErrorMessage.BRAND_IS_NOT_VALID_OR_NOT_PROTECTED.getMessage()));
    } finally {
      verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    }
  }

  @Test
  public void brandNotExistingAuthCreateTest_editFromUpcomingTab() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    brandAuthCreateRequest.setAuthorisationStatus(BrandAuthorisationStatus.UPCOMING.name());
    Mockito.when(
        brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
  }

  @Test
  public void checkBrandAuthBySellerCodeValidAuthTest() throws Exception {
    brand.setSkuCreationAllowedForAllSellers(true);
    when(brandService.findByBrandCodeCached(anyString(), anyString())).thenReturn(brand);
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    boolean authorised =
      brandAuthorisationServiceBean.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE, false);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertTrue(authorised);
  }

  @Test
  public void editBrandAuthDetailsDiffDateTest() throws Exception {
    brandAuthUpdateRequest.setAuthStartDate(new Date(1689897600000L));
    brandAuthUpdateRequest.setAuthExpireDate(new Date(1619897600000L));
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation))
      .thenReturn(brandAuthorisation);
    this.brandAuthorisationServiceBean.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
      brandAuthUpdateRequest);
    verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
  }

  @Test
  public void brandAuthCreateBulkActionTrueTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Mockito.when(brandAuthorisationRepository.save(brandAuthorisation)).thenReturn(brandAuthorisation);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setBulkAction(true);
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK));
    brandAuthCreateRequest.setSellerCode(DEFAULT_SELLER_CODE);
    Mockito.when(
      brandAuthorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(Collections.singletonList(brandAuthorisation));
    this.brandAuthorisationServiceBean.create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      DEFAULT_BRAND_CODE);
    verify(brandService).findByBrandCodeCached(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    verify(applicationContext).getBean(BrandAuthorisationService.class);
  }

  @Test
  void deleteUpcomingBrandAuthByIdTest() throws Exception {
    String id = "id";
    Mockito.when(brandAuthorisationWipRepository.findById(id))
      .thenReturn(Optional.ofNullable(brandAuthorisationWip));
    this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, Constants.UPCOMING, id);
    Mockito.verify(brandAuthorisationWipRepository).findById(id);
  }

  @Test
  void deleteUpcomingBrandAuthByIdNotPresentTest() throws Exception {
    String id = "id";
    this.brandAuthorisationServiceBean.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, Constants.UPCOMING, id);
    Mockito.verify(brandAuthorisationWipRepository).findById(id);
  }
}
