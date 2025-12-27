package com.gdn.x.productcategorybase.service.brand;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.BrandAuthorisationWipAction;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthActivateEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthWipDetailResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthorisationWipListResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipListRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationWipRepository;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.util.Strings;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class BrandAuthorisationWipServiceImplTest {

  private static final String BRAND_CODE = "brand_code";
  private static final String SELLER_CODE = "seller_code";
  private static final String USERNAME = "username";
  private static final String STORE_ID = "10001";
  private static final String REASON = "REASON";
  private static final String BRAND_NAME = "brand-name";
  private static final String DOCUMENT_LINK = "doc1.pdf,doc2.pdf";
  private static final String IPR_REGISTRATION_NUMBER = "IPR-3334-XVU";
  private static final String IN_REVIEW = "IN_REVIEW";
  private static final String ID = "ID";
  private BrandAuthCreateWipRequest brandAuthCreateWipRequest;
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final int DAYS_THRESHOLD = 1;
  private static final String EVENT = "event";

  @InjectMocks
  private BrandAuthorisationWipServiceImpl brandAuthorisationWipService;

  @Mock
  private BrandAuthorisationWipRepository brandAuthorisationWipRepository;

  @Mock
  private BrandAuthorisationRepository brandAuthorisationRepository;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest;
  @Mock
  private BrandService brandService;
  private BrandAuthorisationWip brandAuthorisationWip;
  private BrandAuthUpdateRequest brandAuthUpdateRequest;
  private BrandAuthorisation brandAuthorisation;
  private BrandAuthorisationWipListRequest brandAuthorisationWipListRequest;
  private BrandAuthActivateEventModel brandAuthActivateEventModel;
  private Pageable pageable;
  private Page<BrandAuthorisationWip> brandAuthorisationWipPage;
  private Page<BrandAuthorisation> brandAuthorisationPage;

  @BeforeEach
  void setup() {
    brandAuthorisationWipActionRequest = new BrandAuthorisationWipActionRequest();
    brandAuthorisationWipActionRequest.setBrandCode(BRAND_CODE);
    brandAuthorisationWipActionRequest.setAuthStartDate(new Date());
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.APPROVE.name());
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date nextDate = calendar.getTime();
    pageable = PageRequest.of(PAGE, SIZE);
    brandAuthorisationWipActionRequest.setAuthExpireDate(nextDate);
    brandAuthorisationWipActionRequest.setSellerCode(SELLER_CODE);
    brandAuthorisationWip = new BrandAuthorisationWip();
    brandAuthorisationWip.setBrandCode(BRAND_CODE);
    brandAuthorisationWip.setSellerCode(SELLER_CODE);
    brandAuthorisationWip.setStoreId(STORE_ID);
    brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setBrandCode(BRAND_CODE);
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setDocumentLink(DOCUMENT_LINK);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(nextDate);
    brandAuthorisationWip.setBrandName(BRAND_NAME);
    brandAuthorisationWip.setDocumentLink("doc3.pdf,doc4.pdf");
    brandAuthUpdateRequest = new BrandAuthUpdateRequest();
    brandAuthUpdateRequest.setBrandCode(BRAND_CODE);
    brandAuthUpdateRequest.setBrandName(BRAND_NAME);
    brandAuthUpdateRequest.setSellerCode(SELLER_CODE);
    brandAuthUpdateRequest.setAuthStartDate(new Date());
    brandAuthUpdateRequest.setAuthorisationStatus(IN_REVIEW);
    brandAuthUpdateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(3).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthUpdateRequest.setDocumentLinks(List.of("doc1.pdf", "doc2.pdf"));
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.valueOf(IN_REVIEW));
    brandAuthorisationWip.setAuthStartDate(new Date());
    brandAuthorisationWip.setAuthExpireDate(nextDate);
    brandAuthorisationWipListRequest = new BrandAuthorisationWipListRequest();
    brandAuthorisationWipListRequest.setKeyword(BRAND_NAME);
    brandAuthorisationWipListRequest.setSellerCode(SELLER_CODE);
    brandAuthorisationWipListRequest.setTabName(Constants.UNDER_REVIEW);
    brandAuthorisationWipListRequest.setStatus(BrandAuthorizationWipStatus.ACTIVE.name());
    brandAuthorisationWipPage = new PageImpl<>(List.of(brandAuthorisationWip), pageable, 1L);
    brandAuthorisationPage = new PageImpl<>(List.of(brandAuthorisation), pageable, 1L);

    brandAuthActivateEventModel = new BrandAuthActivateEventModel();
    brandAuthActivateEventModel.setBrandCode(BRAND_CODE);
    brandAuthActivateEventModel.setSellerCode(SELLER_CODE);
    brandAuthActivateEventModel.setStoreId(STORE_ID);
    ReflectionTestUtils.setField(this.brandAuthorisationWipService,
        "brandAuthNearExpiryDaysThreshold", 1);
    ReflectionTestUtils.setField(this.brandAuthorisationWipService,
        "configDay", "14,7,3,2");
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(brandAuthorisationWipRepository);
    Mockito.verifyNoMoreInteractions(brandAuthorisationRepository);
  }

  @Test
  void approveBrandAuthorisationWipTest() {
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.APPROVE.name());
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
          STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
      .thenReturn(brandAuthorisationWip);
    Mockito.when(
        this.brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCode(STORE_ID,
            BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    Mockito.when(brandAuthorisationRepository.save(Mockito.any(BrandAuthorisation.class)))
      .thenReturn(brandAuthorisation);
    brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest);

    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
        SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCode(STORE_ID, BRAND_CODE, SELLER_CODE);
    Mockito.verify(brandAuthorisationWipRepository).save(any());
    Mockito.verify(brandAuthorisationRepository).save(any());
  }

  @Test
  void approveBrandAuthorisationWithFutureDateWipTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    brandAuthorisationWipActionRequest.setAuthStartDate(calendar.getTime());
    calendar.add(Calendar.DAY_OF_MONTH, 12);
    brandAuthorisationWipActionRequest.setAuthExpireDate(calendar.getTime());
    ReflectionTestUtils.setField(brandAuthorisationWipService, "brandAuthWipThresholdInDays", 2);
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.APPROVE.name());
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
          STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
      .thenReturn(brandAuthorisationWip);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
          BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
      .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest);

    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
        SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.verify(brandAuthorisationWipRepository, times(2)).save(any());
  }

  @Test
  void approveBrandAuthorisationWithFutureDateWipNullUpcomingTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    brandAuthorisationWipActionRequest.setAuthStartDate(calendar.getTime());
    calendar.add(Calendar.DAY_OF_MONTH, 12);
    brandAuthorisationWipActionRequest.setAuthExpireDate(calendar.getTime());
    ReflectionTestUtils.setField(brandAuthorisationWipService, "brandAuthWipThresholdInDays", 2);
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.APPROVE.name());
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
          STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
      .thenReturn(brandAuthorisationWip);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
          BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
      .thenReturn(null);
    brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest);

    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
        SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.verify(brandAuthorisationWipRepository, times(1)).save(any());
  }

  @Test
  void approveActiveBrandAuthorisationWipNotFoundTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    brandAuthorisationWipActionRequest.setAuthStartDate(calendar.getTime());
    calendar.add(Calendar.DAY_OF_MONTH, 12);
    brandAuthorisationWipActionRequest.setAuthExpireDate(calendar.getTime());
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.APPROVE.name());
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.when(
      this.brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
        STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW)).thenReturn(null);
    try {
      brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
          brandAuthorisationWipActionRequest);
    } catch (Exception e) {
      Mockito.verify(brandAuthorisationWipRepository)
        .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
          SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    }
  }

  @Test
  void actionDifferentBrandAuthorisationWipTest() {
    brandAuthorisationWipActionRequest.setAction(Strings.EMPTY);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    try {
      brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
          brandAuthorisationWipActionRequest);
    } catch (Exception ignored) {
      Assertions.assertEquals(Strings.EMPTY, brandAuthorisationWipActionRequest.getAction());
    }
  }

  @Test
  void sendNeedRevisionBrandAuthorisationWipTest() {
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.NEED_REVISION.name());
    brandAuthorisationWipActionRequest.setReason(REASON);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
            STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW)).thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest);
    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
        SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);

    Mockito.verify(brandAuthorisationWipRepository).save(any());
  }

  @Test
  void rejectBrandAuthorisationTestWip() {
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.REJECT.name());
    brandAuthorisationWipActionRequest.setReason(REASON);
    Mockito.when(
        this.brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
          STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
      .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest);

    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
        SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.verify(brandAuthorisationWipRepository).save(any());
  }

  @Test
  void fetchBrandAuthWipDetailsTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    brandAuthorisationWip.setDocumentLink(DOCUMENT_LINK);
    brandAuthorisationWip.setId(ID);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 1);
    Date endDate = calendar1.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthorisation.setAuthExpireDate(endDate);
    Mockito.when(brandAuthorisationWipRepository.findById(ID))
        .thenReturn(Optional.ofNullable(brandAuthorisationWip));
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(brandAuthorisationWipRepository).findById(ID);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Assertions.assertEquals(2, brandAuthWipDetailResponse.getDocumentList().size());
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
    Assertions.assertEquals(BrandAuthorizationWipStatus.IN_REVIEW.name(),
        brandAuthWipDetailResponse.getStatus());
  }

  @Test
  void fetchBrandAuthWipDetailsNullTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    brandAuthorisationWip.setDocumentLink(DOCUMENT_LINK);
    brandAuthorisationWip.setId(ID);
    Mockito.when(brandAuthorisationWipRepository.findById(ID))
        .thenReturn(Optional.ofNullable(brandAuthorisationWip));
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(null);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(brandAuthorisationWipRepository).findById(ID);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Assertions.assertEquals(2, brandAuthWipDetailResponse.getDocumentList().size());
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
    Assertions.assertEquals(BrandAuthorizationWipStatus.IN_REVIEW.name(),
        brandAuthWipDetailResponse.getStatus());
  }


  @Test
  void fetchBrandAuthWipDetailsWipNullTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    brandAuthorisationWip.setDocumentLink(DOCUMENT_LINK);
    brandAuthorisationWip.setId(ID);
    Mockito.when(brandAuthorisationWipRepository.findById(ID)).thenReturn(Optional.empty());
    assertThrows(ValidationException.class,
        () -> brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID));
    Mockito.verify(brandAuthorisationWipRepository).findById(ID);

  }

  @Test
  void fetchBrandAuthWipDetailsActiveStatusDateExpiredTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthorisationWip.setAuthExpireDate(new Date(2024 - 1 - 1));
    brandAuthorisationWip.setDocumentLink(DOCUMENT_LINK);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, -1);
    Date endDate = calendar1.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthorisation.setAuthExpireDate(endDate);
    Mockito.when(brandAuthorisationWipRepository.findById(ID))
        .thenReturn(Optional.ofNullable(brandAuthorisationWip));
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(brandAuthorisationWipRepository).findById(ID);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Assertions.assertEquals(2, brandAuthWipDetailResponse.getDocumentList().size());
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
    Assertions.assertEquals(BrandAuthorizationWipStatus.EXPIRED.name(),
        brandAuthWipDetailResponse.getStatus());
  }

  @Test
  void fetchBrandAuthWipDetailsNoDocumentsTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthorisationWip.setAuthExpireDate(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 2);
    Date endDate = calendar1.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthorisation.setAuthExpireDate(endDate);
    Mockito.when(brandAuthorisationWipRepository.findById(ID))
        .thenReturn(Optional.ofNullable(brandAuthorisationWip));
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(brandAuthorisationWipRepository).findById(ID);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Assertions.assertEquals(2, brandAuthWipDetailResponse.getDocumentList().size());
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
  }

  @Test
  void fetchBrandAuthWipDetailsActiveTest() {
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    brandAuthorisation.setId(ID);
    Mockito.when(brandAuthorisationRepository.findById(ID))
        .thenReturn(Optional.ofNullable(brandAuthorisation));
    brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID,
        BrandAuthorisationStatus.ACTIVE.name(), ID);
    Mockito.verify(brandAuthorisationRepository).findById(ID);
  }

  @Test
  void submitBrandAuthorisationRequestTest() throws Exception {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    brandAuthorisationWip.setId(ID);
    brandAuthUpdateRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
        .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    BeanUtils.copyProperties(brandAuthUpdateRequest, brandAuthorisationWip);
    Mockito.verify(brandAuthorisationWipRepository).save(brandAuthorisationWip);
  }

  @Test
  void submitBrandAuthorisationRequestActiveTest() throws Exception {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthorisation.setSellerCode(SELLER_CODE);
    brandAuthorisationWip.setId(ID);
    brandAuthUpdateRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE.name());
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    Mockito.when(
            brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
                STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
        .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    BeanUtils.copyProperties(brandAuthUpdateRequest, brandAuthorisationWip);
    Mockito.verify(brandAuthorisationWipRepository).save(brandAuthorisationWip);
  }

  @Test
  void submitBrandAuthorisationRequestInvalidActiveTest() throws Exception {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthorisation.setSellerCode(SELLER_CODE);
    brandAuthorisationWip.setId(ID);
    brandAuthUpdateRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE.name());
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    Mockito.when(
            brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
                STORE_ID, BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW))
        .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(STORE_ID, BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    BeanUtils.copyProperties(brandAuthUpdateRequest, brandAuthorisationWip);
    Mockito.verify(brandAuthorisationWipRepository).save(brandAuthorisationWip);
  }

  @Test
  void submitBrandAuthorisationNeedRevisionTest() throws Exception {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.NEED_REVISION);
    brandAuthorisationWip.setDocumentLink(StringUtils.EMPTY);
    brandAuthorisationWip.setId(ID);
    brandAuthUpdateRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.NEED_REVISION.name());
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.NEED_REVISION))
        .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.NEED_REVISION);
    BeanUtils.copyProperties(brandAuthUpdateRequest, brandAuthorisationWip);
    Mockito.verify(brandAuthorisationWipRepository).save(brandAuthorisationWip);
  }

  @Test
  void submitBrandAuthorisationDocumentEmptyTest() throws Exception {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    brandAuthorisationWip.setDocumentLink(StringUtils.EMPTY);
    brandAuthorisationWip.setId(ID);
    brandAuthUpdateRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING.name());
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    BeanUtils.copyProperties(brandAuthUpdateRequest, brandAuthorisationWip);
    Mockito.verify(brandAuthorisationWipRepository).save(brandAuthorisationWip);
  }

  @Test
  void submitBrandAuthorisationRequestRejectedRequestTest() {
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.REJECTED.name());
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.REJECTED);
    brandAuthorisationWip.setId(ID);
    Assertions.assertThrows(ValidationException.class,
        () -> brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID,
            brandAuthUpdateRequest));
  }

  @Test
  void submitBrandAuthorisationRequestNewRequestTest() throws Exception {
    brandAuthUpdateRequest.setIprRegistrationNumber(BRAND_CODE);
    brandAuthorisationWip.setId(ID);
    Mockito.when(
        brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
            BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW)).thenReturn(null);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.IN_REVIEW);
    BeanUtils.copyProperties(brandAuthUpdateRequest, brandAuthorisationWip);
    brandAuthorisationWip.setIprRegistrationNumber(BRAND_CODE);
    Mockito.verify(brandAuthorisationWipRepository).save(Mockito.any());
  }

  @Test
  public void brandAuthCreateWipRequestTest() throws Exception {
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setAuthExpireDate(null);
    brandAuthCreateWipRequest.setAuthStartDate(null);
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(true);
    when(brandService.findByBrandCodeCached(STORE_ID,
        brandAuthCreateWipRequest.getBrandCode())).thenReturn(brandResponse);
    this.brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest, STORE_ID,
        USERNAME);
    verify(brandService).findByBrandCodeCached(STORE_ID, brandAuthUpdateRequest.getBrandCode());
    verify(brandAuthorisationWipRepository).save(any(BrandAuthorisationWip.class));
  }


  @Test
  public void brandAuthCreateWipRequestDocumentLinkNotEmptyTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setDocumentLinks(List.of(DOCUMENT_LINK));
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(true);
    when(this.brandService.findByBrandCodeCached(STORE_ID,
        brandAuthCreateWipRequest.getBrandCode())).thenReturn(brandResponse);
    this.brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest, STORE_ID,
        USERNAME);
    verify(brandService).findByBrandCodeCached(STORE_ID, brandAuthUpdateRequest.getBrandCode());
    verify(brandAuthorisationWipRepository).save(any(BrandAuthorisationWip.class));
  }

  @Test
  public void brandAuthCreateWipRequestBrandResponseNullTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest,
            STORE_ID, USERNAME));
  }

  @Test
  public void brandAuthCreateWipRequestProtectedFalseTest() throws Exception {
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setDocumentLinks(List.of(DOCUMENT_LINK));
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(false);
    when(brandService.findByBrandCodeCached(STORE_ID,
        brandAuthCreateWipRequest.getBrandCode())).thenReturn(brandResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest,
            STORE_ID, USERNAME));
    verify(brandService).findByBrandCodeCached(STORE_ID, brandAuthUpdateRequest.getBrandCode());
  }

  @Test
  public void brandAuthCreateDateWipRequestExceptionTest() throws Exception {
    Date date = new Date();
    Date expireDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, 1);
    date = calendar.getTime();
    calendar.add(Calendar.DATE, -1);
    expireDate = calendar.getTime();
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setAuthStartDate(date);
    brandAuthCreateWipRequest.setAuthExpireDate(expireDate);
    try {
      this.brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest,
          STORE_ID, USERNAME);
    } catch (Exception ex) {
      verify(this.brandAuthorisationWipRepository,
          BrandAuthorisationWipServiceImplTest.NEVER_CALLED).save(any(BrandAuthorisationWip.class));
      Assertions.assertTrue(ex.getMessage()
          .contains(ErrorMessage.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE.getMessage()));
    }
  }

  private BrandAuthCreateWipRequest generateCreateBrandAuthWipRequest() {
    Date date = new Date();
    Date expireDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, 1);
    date = calendar.getTime();
    calendar.add(Calendar.DATE, 10);
    expireDate = calendar.getTime();
    BrandAuthCreateWipRequest request = new BrandAuthCreateWipRequest();
    request.setBrandName(BRAND_NAME);
    request.setBrandCode(BRAND_CODE);
    request.setSellerCode(SELLER_CODE);
    request.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    request.setAuthExpireDate(expireDate);
    request.setAuthStartDate(date);
    return request;
  }

  @Test
  void validateBrandAuthRequestEditedFalseWithUpcomingTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    Mockito.when(
      brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(
        STORE_ID, BRAND_CODE, SELLER_CODE, false)).thenReturn(List.of(brandAuthorisationWip));
    boolean result =
      brandAuthorisationWipService.validateBrandAuthRequest(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
    Assertions.assertFalse(result);
    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
  }

  @Test
  void validateBrandAuthRequestEditedFalseNoRecordInWipTest() {
    Mockito.when(
      brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(STORE_ID, BRAND_CODE, SELLER_CODE, false)).thenReturn(new ArrayList<>());
    Mockito.when(
      brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    boolean result =
      brandAuthorisationWipService.validateBrandAuthRequest(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
    Assertions.assertFalse(result);
    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
    Mockito.verify(brandAuthorisationRepository)
      .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
        SELLER_CODE);
  }

  @Test
  void validateBrandAuthRequestEditedFalseValidateTrueTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    Mockito.when(
      brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(
        STORE_ID, BRAND_CODE, SELLER_CODE, false)).thenReturn(List.of(brandAuthorisationWip));
    Mockito.when(
      brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(null);
    boolean result =
      brandAuthorisationWipService.validateBrandAuthRequest(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
    Assertions.assertTrue(result);
    Mockito.verify(brandAuthorisationWipRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
    Mockito.verify(brandAuthorisationRepository)
      .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
        SELLER_CODE);
  }

  @Test
  void validateBrandAuthRequestEditedTrueTest() {
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.IN_REVIEW);
    Mockito.when(
      brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(
        STORE_ID, BRAND_CODE, SELLER_CODE, false)).thenReturn(List.of(brandAuthorisationWip));
    boolean result =
        brandAuthorisationWipService.validateBrandAuthRequest(STORE_ID, BRAND_CODE, SELLER_CODE,
          true);
    Assertions.assertFalse(result);
    Mockito.verify(brandAuthorisationWipRepository, Mockito.times(1))
      .findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(STORE_ID, BRAND_CODE, SELLER_CODE,
        false);
  }

  @Test
  void getBrandAuthorisationWipListResponseUnderReviewTabTest() throws Exception {
    Mockito.when(
            brandAuthorisationWipRepository.findBrandAuthorisationWipForExternalListing(STORE_ID,
                SELLER_CODE, BRAND_NAME, Constants.UNDER_REVIEW,
                Collections.singletonList(BrandAuthorizationWipStatus.ACTIVE), pageable))
        .thenReturn(brandAuthorisationWipPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationWipRepository, Mockito.times(1))
        .findBrandAuthorisationWipForExternalListing(STORE_ID, SELLER_CODE, BRAND_NAME,
            Constants.UNDER_REVIEW, Collections.singletonList(BrandAuthorizationWipStatus.ACTIVE),
            pageable);
  }

  @Test
  void getBrandAuthorisationWipListResponseUpcomingTabTest() {
    brandAuthorisationWipListRequest.setTabName(Constants.UPCOMING);
    Mockito.when(
        brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(
            STORE_ID, SELLER_CODE, BRAND_NAME, BrandAuthorizationWipStatus.UPCOMING.name(),
            pageable)).thenReturn(brandAuthorisationWipPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationWipRepository, Mockito.times(1))
        .findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(STORE_ID,
            SELLER_CODE, BRAND_NAME, BrandAuthorizationWipStatus.UPCOMING.name(), pageable);
  }

  @Test
  void getBrandAuthorisationWipListResponseStatusNullUnderReviewTabTest() throws Exception {
    brandAuthorisationWipListRequest.setStatus(null);
    Mockito.when(
        brandAuthorisationWipRepository.findBrandAuthorisationWipForExternalListing(STORE_ID,
            SELLER_CODE, BRAND_NAME, Constants.UNDER_REVIEW,
            List.of(BrandAuthorizationWipStatus.IN_REVIEW,
                BrandAuthorizationWipStatus.NEED_REVISION, BrandAuthorizationWipStatus.REJECTED),
            pageable)).thenReturn(brandAuthorisationWipPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationWipRepository, Mockito.times(1))
        .findBrandAuthorisationWipForExternalListing(STORE_ID, SELLER_CODE, BRAND_NAME,
            Constants.UNDER_REVIEW, List.of(BrandAuthorizationWipStatus.IN_REVIEW,
                BrandAuthorizationWipStatus.NEED_REVISION, BrandAuthorizationWipStatus.REJECTED),
            pageable);
  }

  @Test
  void getBrandAuthorisationWipListResponseAuthorisedBrandTabTest() throws Exception {
    brandAuthorisationWipListRequest.setStatus(BrandAuthorizationWipStatus.ACTIVE.name());
    brandAuthorisationWipListRequest.setTabName(Constants.AUTHORIZED_BRAND);
    Mockito.when(
            brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
                STORE_ID, SELLER_CODE, BRAND_NAME, BrandAuthorizationWipStatus.ACTIVE.name(),
                pageable, 1))
        .thenReturn(brandAuthorisationPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationRepository, Mockito.times(1))
        .findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(STORE_ID, SELLER_CODE,
            BRAND_NAME, BrandAuthorizationWipStatus.ACTIVE.name(), pageable, 1);
  }

  @Test
  void getBrandAuthorisationWipListResponseAuthorisedBrandTabNullTest() throws Exception {
    brandAuthorisationWipListRequest.setStatus(BrandAuthorizationWipStatus.ACTIVE.name());
    brandAuthorisationWipListRequest.setTabName(Constants.AUTHORIZED_BRAND);
    Mockito.when(
            brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
                STORE_ID, SELLER_CODE, BRAND_NAME, BrandAuthorizationWipStatus.ACTIVE.name(),
                pageable, 1))
        .thenReturn(null);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationRepository, Mockito.times(1))
        .findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(STORE_ID, SELLER_CODE,
            BRAND_NAME, BrandAuthorizationWipStatus.ACTIVE.name(), pageable, 1);
  }

  @Test
  void getBrandAuthorisationWipListResponseAuthorisedBrandTabEmptyTest() throws Exception {
    brandAuthorisationWipListRequest.setStatus(BrandAuthorizationWipStatus.ACTIVE.name());
    brandAuthorisationPage = new PageImpl<>(new ArrayList<>(), pageable, 1L);
    brandAuthorisationWipListRequest.setTabName(Constants.AUTHORIZED_BRAND);
    Mockito.when(
            brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
                STORE_ID, SELLER_CODE, BRAND_NAME, BrandAuthorizationWipStatus.ACTIVE.name(),
                pageable, 1))
        .thenReturn(brandAuthorisationPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationRepository, Mockito.times(1))
        .findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(STORE_ID, SELLER_CODE,
            BRAND_NAME, BrandAuthorizationWipStatus.ACTIVE.name(), pageable, 1);
  }

  @Test
  void getBrandAuthorisationWipListResponseAuthorisedBrandTabNearExpiryTest() throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -20);
    brandAuthorisation.setAuthStartDate(calendar.getTime());
    calendar.add(Calendar.DAY_OF_MONTH, -10);
    brandAuthorisation.setAuthExpireDate(calendar.getTime());
    brandAuthorisationPage = new PageImpl<>(List.of(brandAuthorisation), pageable, 1L);
    brandAuthorisationWipListRequest.setStatus(BrandAuthorizationWipStatus.NEAR_EXPIRY.name());
    brandAuthorisationWipListRequest.setTabName(Constants.AUTHORIZED_BRAND);
    Mockito.when(
            brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
                STORE_ID, SELLER_CODE, BRAND_NAME, BrandAuthorizationWipStatus.NEAR_EXPIRY.name(), pageable, 1))
        .thenReturn(brandAuthorisationPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationRepository, Mockito.times(1))
        .findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(STORE_ID, SELLER_CODE,
            BRAND_NAME, BrandAuthorizationWipStatus.NEAR_EXPIRY.name(), pageable, 1);
  }

  @Test
  void getBrandAuthorisationWipListResponseStatusNullAuthorisedBrandTabTest() throws Exception {
    brandAuthorisationWipListRequest.setStatus(null);
    brandAuthorisationWipListRequest.setTabName(Constants.AUTHORIZED_BRAND);
    Mockito.when(
        brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
            STORE_ID, SELLER_CODE, BRAND_NAME, null, pageable, 1)).thenReturn(brandAuthorisationPage);
    brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
        brandAuthorisationWipListRequest, pageable);
    Mockito.verify(brandAuthorisationRepository, Mockito.times(1))
        .findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(STORE_ID, SELLER_CODE,
            BRAND_NAME, null, pageable, 1);
  }

  @Test
  void getBrandAuthorisationWipListResponseUnknownTabTest() throws Exception {
    brandAuthorisationWipListRequest.setTabName(Constants.APPROVED_STATUS);
    Page<BrandAuthorisationWipListResponse> responses =
        brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID,
            brandAuthorisationWipListRequest, pageable);
    Assertions.assertEquals(responses.getContent(), new ArrayList<>());
  }

  @Test
  void fetchCountOfPendingRequestsForSellerTest() {
    List<BrandAuthorizationWipStatus> brandAuthorizationWipStatuses =
        List.of(BrandAuthorizationWipStatus.IN_REVIEW, BrandAuthorizationWipStatus.NEED_REVISION);
    Mockito.when(
        brandAuthorisationWipRepository.countBySellerCodeAndAuthorisationStatusIn(SELLER_CODE,
            brandAuthorizationWipStatuses)).thenReturn(1L);
    long response =
        brandAuthorisationWipService.fetchCountOfPendingRequestsForSeller(STORE_ID, SELLER_CODE);
    Assertions.assertEquals(1, response);
    Mockito.verify(brandAuthorisationWipRepository)
        .countBySellerCodeAndAuthorisationStatusIn(SELLER_CODE, brandAuthorizationWipStatuses);
  }

  @Test
  void activateUpcomingBrandAuthorisationTest() {
    Mockito.when(
            brandAuthorisationWipRepository.findByStoreIdAndAuthorisationStatusAndAuthStartDateBetween(
                eq(STORE_ID), eq(BrandAuthorizationWipStatus.UPCOMING),
                Mockito.any(Date.class), Mockito.any(Date.class)))
        .thenReturn(List.of(brandAuthorisationWip));
    Mockito.when(kafkaTopicProperties.getBrandAuthActivateEvent()).thenReturn(EVENT);
    brandAuthorisationWipService.publishUpcomingBrandAuthorisation(STORE_ID, DAYS_THRESHOLD);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByStoreIdAndAuthorisationStatusAndAuthStartDateBetween(eq(STORE_ID),
            eq(BrandAuthorizationWipStatus.UPCOMING), Mockito.any(Date.class),
            Mockito.any(Date.class));
    Mockito.verify(kafkaTopicProperties).getBrandAuthActivateEvent();
    Mockito.verify(kafkaPublisher).send(eq(EVENT),
        eq(BRAND_CODE.concat(Constants.HYPHEN).concat(SELLER_CODE)),
        Mockito.any(BrandAuthActivateEventModel.class));
  }

  @Test
  void activateExistingBrandAuthorisationTest() {
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    BrandAuthorisation savedAuth = new BrandAuthorisation();
    BeanUtils.copyProperties(brandAuthorisation, savedAuth, "authorisationStatus");
    savedAuth.setAuthorisationStatus(BrandAuthorisationStatus.EXPIRED);
    Mockito.when(brandAuthorisationRepository.save(Mockito.any(BrandAuthorisation.class)))
        .thenReturn(savedAuth);
    brandAuthorisationWipService.activateBrandAuthorisation(brandAuthActivateEventModel);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Mockito.verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    Mockito.verify(brandAuthorisationWipRepository).save(any(BrandAuthorisationWip.class));
  }

  @Test
  void activateExistingBrandAuthorisationNoStatusTest() {
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(brandAuthorisation);
    Mockito.when(brandAuthorisationRepository.save(Mockito.any(BrandAuthorisation.class)))
        .thenReturn(brandAuthorisation);
    brandAuthorisationWipService.activateBrandAuthorisation(brandAuthActivateEventModel);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Mockito.verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    Mockito.verify(brandAuthorisationWipRepository).save(any(BrandAuthorisationWip.class));
  }

  @Test
  void activateNullBrandAuthorisationTest() {
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    Mockito.when(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            STORE_ID, BRAND_CODE, SELLER_CODE)).thenReturn(null);
    BrandAuthorisation savedAuth = new BrandAuthorisation();
    BeanUtils.copyProperties(brandAuthorisation, savedAuth, "authorisationStatus");
    savedAuth.setAuthorisationStatus(BrandAuthorisationStatus.EXPIRED);
    Mockito.when(brandAuthorisationRepository.save(Mockito.any(BrandAuthorisation.class)))
        .thenReturn(savedAuth);
    brandAuthorisationWipService.activateBrandAuthorisation(brandAuthActivateEventModel);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    Mockito.verify(brandAuthorisationRepository)
        .findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(STORE_ID, BRAND_CODE,
            SELLER_CODE);
    Mockito.verify(brandAuthorisationRepository).save(any(BrandAuthorisation.class));
    Mockito.verify(brandAuthorisationWipRepository).save(any(BrandAuthorisationWip.class));
  }

  @Test
  void activateExistingBrandAuthorisationWipNotPresentTest() {
    Mockito.when(
        brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
            BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING)).thenReturn(null);
    brandAuthorisationWipService.activateBrandAuthorisation(brandAuthActivateEventModel);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
  }

  @Test
  void createWipFromInternalTest_wipCreation() throws Exception {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    brandAuthCreateRequest.setBrandCode(BRAND_CODE);
    brandAuthCreateRequest.setBrandName(BRAND_NAME);
    brandAuthCreateRequest.setSellerCode(SELLER_CODE);
    brandAuthCreateRequest.setAuthStartDate(new Date());
    brandAuthCreateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(3).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    BrandAuthorisationWip brandAuthorisationWip = new BrandAuthorisationWip();
    BeanUtils.copyProperties(brandAuthCreateRequest, brandAuthorisationWip);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(null);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, BRAND_CODE))
        .thenReturn(brandResponse);
    Pair<BrandAuthCreateResponse, BrandAuthorisation> response =
        brandAuthorisationWipService.createWipFromInternal(brandAuthCreateRequest, STORE_ID,
            USERNAME);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    ArgumentCaptor<BrandAuthorisationWip> captor = ArgumentCaptor.forClass(BrandAuthorisationWip.class);
    Mockito.verify(brandAuthorisationWipRepository).save(captor.capture());
    BrandAuthorisationWip capturedWip = captor.getValue();
    Assertions.assertEquals(BRAND_NAME, capturedWip.getBrandName());
    Assertions.assertEquals(BRAND_CODE, capturedWip.getBrandCode());
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, BRAND_CODE);
    Assertions.assertEquals(BRAND_CODE, response.getLeft().getBrandCode());
  }

  @Test
  void createWipFromInternalTest_wipCreationWithDocumentList() throws Exception {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    brandAuthCreateRequest.setBrandCode(BRAND_CODE);
    brandAuthCreateRequest.setBrandName(BRAND_NAME);
    brandAuthCreateRequest.setSellerCode(SELLER_CODE);
    brandAuthCreateRequest.setAuthStartDate(new Date());
    brandAuthCreateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(3).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK.split(",")));
    BrandAuthorisationWip brandAuthorisationWip = new BrandAuthorisationWip();
    BeanUtils.copyProperties(brandAuthCreateRequest, brandAuthorisationWip);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(null);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, BRAND_CODE))
        .thenReturn(brandResponse);
    Pair<BrandAuthCreateResponse, BrandAuthorisation> response =
        brandAuthorisationWipService.createWipFromInternal(brandAuthCreateRequest, STORE_ID,
            USERNAME);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, BRAND_CODE);
    ArgumentCaptor<BrandAuthorisationWip> captor = ArgumentCaptor.forClass(BrandAuthorisationWip.class);
    Mockito.verify(brandAuthorisationWipRepository).save(captor.capture());
    BrandAuthorisationWip capturedWip = captor.getValue();
    Assertions.assertEquals(BRAND_NAME, capturedWip.getBrandName());
    Assertions.assertEquals(BRAND_CODE, capturedWip.getBrandCode());
    Assertions.assertEquals(BRAND_CODE, response.getLeft().getBrandCode());
  }

  @Test
  void createWipFromInternalTest_wipUpdate() throws Exception {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    brandAuthCreateRequest.setBrandCode(BRAND_CODE);
    brandAuthCreateRequest.setBrandName(BRAND_NAME);
    brandAuthCreateRequest.setSellerCode(SELLER_CODE);
    brandAuthCreateRequest.setAuthStartDate(new Date());
    brandAuthCreateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(3).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    BrandAuthorisationWip brandAuthorisationWip = new BrandAuthorisationWip();
    BeanUtils.copyProperties(brandAuthCreateRequest, brandAuthorisationWip);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    Pair<BrandAuthCreateResponse, BrandAuthorisation> response =
        brandAuthorisationWipService.createWipFromInternal(brandAuthCreateRequest, STORE_ID,
            USERNAME);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(BRAND_CODE,
            SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    ArgumentCaptor<BrandAuthorisationWip> captor = ArgumentCaptor.forClass(BrandAuthorisationWip.class);
    Mockito.verify(brandAuthorisationWipRepository).save(captor.capture());
    BrandAuthorisationWip capturedWip = captor.getValue();
    Assertions.assertEquals(BRAND_NAME, capturedWip.getBrandName());
    Assertions.assertEquals(BRAND_CODE, capturedWip.getBrandCode());
    Assertions.assertNotEquals(USERNAME, capturedWip.getCreatedBy());
    Assertions.assertEquals(BRAND_CODE, response.getLeft().getBrandCode());
  }

  @Test
  void createWipFromInternalTest_wipUpdateWithDocumentList() throws Exception {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    brandAuthCreateRequest.setBrandCode(BRAND_CODE);
    brandAuthCreateRequest.setBrandName(BRAND_NAME);
    brandAuthCreateRequest.setSellerCode(SELLER_CODE);
    brandAuthCreateRequest.setAuthStartDate(new Date());
    brandAuthCreateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(3).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK.split(Constants.COMMA)));
    BrandAuthorisationWip brandAuthorisationWip = new BrandAuthorisationWip();
    BeanUtils.copyProperties(brandAuthCreateRequest, brandAuthorisationWip);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    Pair<BrandAuthCreateResponse, BrandAuthorisation> response =
        brandAuthorisationWipService.createWipFromInternal(brandAuthCreateRequest, STORE_ID,
            USERNAME);
    Mockito.verify(brandAuthorisationWipRepository)
        .findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
            BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    ArgumentCaptor<BrandAuthorisationWip> captor = ArgumentCaptor.forClass(BrandAuthorisationWip.class);
    Mockito.verify(brandAuthorisationWipRepository).save(captor.capture());
    BrandAuthorisationWip capturedWip = captor.getValue();
    Assertions.assertEquals(BRAND_NAME, capturedWip.getBrandName());
    Assertions.assertEquals(BRAND_CODE, capturedWip.getBrandCode());
    Assertions.assertNotEquals(USERNAME, capturedWip.getCreatedBy());
    Assertions.assertEquals(BRAND_CODE, response.getLeft().getBrandCode());
  }

  @Test
  void updateWipEntryForActivationTest() {
    Mockito.when(
            brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
                BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING))
        .thenReturn(brandAuthorisationWip);
    brandAuthorisationWipService.updateWipEntryForActivation(BRAND_CODE, SELLER_CODE);
    Mockito.verify(
        brandAuthorisationWipRepository).findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
            BRAND_CODE, SELLER_CODE, BrandAuthorizationWipStatus.UPCOMING);
    Mockito.verify(brandAuthorisationWipRepository).save(Mockito.any(BrandAuthorisationWip.class));
  }

  @Test
  void fetchBrandAuthorisationForNearExpiryTest() {
    Mockito.when(brandAuthorisationRepository.findSellerCodesByAuthorisationStatusAndConfiguration(
            eq(STORE_ID), eq(BrandAuthorisationStatus.ACTIVE.name()), anyInt()))
        .thenReturn(Collections.singletonList(brandAuthorisation));
    brandAuthorisationWipService.fetchBrandAuthorisationForNearExpiry(STORE_ID);
    Mockito.verify(brandAuthorisationRepository, times(4))
        .findSellerCodesByAuthorisationStatusAndConfiguration(eq(STORE_ID),
            eq(BrandAuthorisationStatus.ACTIVE.name()), anyInt());
  }
}
