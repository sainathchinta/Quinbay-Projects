package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthHistoryService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationService;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import org.springframework.test.util.ReflectionTestUtils;

public class BrandAuthServiceWrapperTest {
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
  private static final String DOCUMENT_LINK_2 = "document link 2";
  private static final String DEFAULT_IPR_REGISTRATION_NUMBER = "pvvs-ddd-sdd";
  private BrandAuthorisation brandAuthorisation;
  private BrandAuthorisationWip brandAuthorisationWip;
  private BrandAuthorisation brandAuthorisationUpdated;
  private BrandAuthCreateRequest brandAuthCreateRequest;
  private BrandAuthCreateResponse brandAuthCreateResponse;
  private BrandAuthCreateWipRequest brandAuthCreateWipRequest;
  private BrandAuthCreateWipResponse brandAuthCreateWipResponse;
  private BrandAuthUpdateRequest brandAuthUpdateRequest = new BrandAuthUpdateRequest();
  private BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
  private BrandAuthDeleteRequest brandAuthDeleteRequest = new BrandAuthDeleteRequest();
  private List<BrandAuthDeleteRequest> brandAuthDeleteRequestList = new ArrayList<>();

  @InjectMocks
  private BrandAuthServiceWrapperImpl brandAuthServiceWrapper;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private BrandAuthorisationService brandAuthorisationService;

  @Mock
  private BrandAuthorisationRepository authorisationRepository;

  @Mock
  private BrandAuthHistoryService brandAuthHistoryService;

  @Mock
  private BrandAuthorisationWipService brandAuthorisationWipService;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    brandAuthorisation = new BrandAuthorisation();
    brandAuthorisationUpdated = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    brandAuthorisation.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisation.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationUpdated.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisationUpdated.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationUpdated.setAuthStartDate(new Date());
    brandAuthorisationUpdated.setAuthExpireDate(new Date());
    brandAuthorisationUpdated.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthUpdateRequest = BrandAuthUpdateRequest.builder().brandCode(DEFAULT_BRAND_CODE)
        .sellerCode(DEFAULT_SELLER_CODE).authorisationStatus(BrandAuthorisationStatus.ACTIVE.name())
        .authStartDate(new Date()).authExpireDate(new Date()).build();
    brandAuthorisationHistory.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationHistory.setStoreId(DEFAULT_STORE_ID);
    brandAuthUpdateRequest = BrandAuthUpdateRequest.builder().brandCode(DEFAULT_BRAND_CODE)
        .sellerCode(DEFAULT_SELLER_CODE).authorisationStatus(BrandAuthorisationStatus.ACTIVE.name())
        .authStartDate(new Date()).authExpireDate(new Date()).build();

    when(authorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE)).thenReturn(brandAuthorisation);
    Mockito.doNothing().when(applicationCacheServiceBean)
        .evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    when(domainEventPublisherService.publishBrandAuthHistoryEvent(DEFAULT_BRAND_CODE,
        DEFAULT_SELLER_CODE, brandAuthorisationHistory)).thenReturn(
        new BrandAuthDomainEventModel());
    brandAuthDeleteRequest.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthDeleteRequest.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthDeleteRequest.setId(StringUtils.EMPTY);
    brandAuthDeleteRequest.setStatus(StringUtils.EMPTY);
    brandAuthDeleteRequestList.add(brandAuthDeleteRequest);
    brandAuthCreateRequest = new BrandAuthCreateRequest();
    brandAuthCreateResponse = new BrandAuthCreateResponse();
    brandAuthorisationWip = new BrandAuthorisationWip();
    brandAuthorisationWip.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisationWip.setSellerCode(DEFAULT_SELLER_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(brandAuthorisationService);
    Mockito.verifyNoMoreInteractions(domainEventPublisherService);
    Mockito.verifyNoMoreInteractions(authorisationRepository);
    Mockito.verifyNoMoreInteractions(applicationCacheServiceBean);
    Mockito.verifyNoMoreInteractions(brandAuthHistoryService);
    Mockito.verifyNoMoreInteractions(brandAuthorisationWipService);
  }

  @Test
  public void editBrandAuthDetailsAndEvictCacheTest() throws Exception {
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisation);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
  }

  @Test
  public void editBrandAuthDetails_withDocUpdateTest() throws Exception {
    brandAuthorisationUpdated.setDocumentLink(DOCUMENT_LINK);
    brandAuthUpdateRequest.setDocumentLinks(Arrays.asList(DOCUMENT_LINK_2));
    brandAuthorisation.setDocumentLink(DOCUMENT_LINK_2);
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService).publishBrandAuthHistoryEvent(anyString(), anyString(),
        any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetails_withDocUpdateDocumentEmptyTest() throws Exception {
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
  }

  @Test
  public void editBrandAuthDetails_withDateUpdateTest() throws Exception {
    brandAuthUpdateRequest.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthUpdateRequest.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(10))));
    brandAuthorisationUpdated.setDocumentLink(DOCUMENT_LINK);
    brandAuthorisationUpdated.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisationUpdated.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(10))));
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService, times(2)).publishBrandAuthHistoryEvent(anyString(),
        anyString(), any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetails_withStatusUpdateTest() throws Exception {
    brandAuthorisationUpdated.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    brandAuthorisationUpdated.setDocumentLink(DOCUMENT_LINK);
    brandAuthorisationUpdated.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService, times(1)).publishBrandAuthHistoryEvent(anyString(),
        anyString(), any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetails_withNullDateUpdateTest() throws Exception {
    brandAuthUpdateRequest.setAuthStartDate(null);
    brandAuthUpdateRequest.setAuthExpireDate(Date.from(Instant.now().minus(Duration.ofDays(10))));
    brandAuthorisationUpdated.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(10))));
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService).publishBrandAuthHistoryEvent(anyString(), anyString(),
        any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_withNullResponseTest() throws Exception {
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(null);
    try {
      brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
          brandAuthUpdateRequest, DEFAULT_USERNAME);
    } finally {
      verify(
          authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
          DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
      verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
          brandAuthUpdateRequest);
    }
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueCreateWipTest() throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    brandAuthUpdateRequest.setAuthStartDate(
        Date.from(LocalDate.now().plusDays(3).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthUpdateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(10).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthorisation.setAuthStartDate(
        Date.from(LocalDate.now().plusDays(2).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthorisation.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(10).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    when(authorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE)).thenReturn(brandAuthorisation);
    when(brandAuthorisationWipService.createWipFromInternal(Mockito.any(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(
        Pair.of(new BrandAuthCreateResponse(), new BrandAuthorisation()));
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(brandAuthorisationWipService).createWipFromInternal(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueCreateBrandAuthorisationTest()
      throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    brandAuthUpdateRequest.setAuthStartDate(
        Date.from(LocalDate.now().plusDays(1).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthUpdateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(10).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    when(authorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE)).thenReturn(null);
    when(brandAuthorisationService.create(Mockito.any(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(
        Pair.of(new BrandAuthCreateResponse(), new BrandAuthorisation()));
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(brandAuthorisationService).create(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(this.brandAuthHistoryService).generateBrandAuthHistoryDomainEventModel(
        any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueCreateBrandAuthorisationTestUpdateFromUpcomingTab()
      throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    brandAuthUpdateRequest.setAuthStartDate(
        Date.from(LocalDate.now().plusDays(1).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthUpdateRequest.setAuthExpireDate(
        Date.from(LocalDate.now().plusDays(10).atStartOfDay(ZoneId.systemDefault()).toInstant()));
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING.name());
    when(authorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE)).thenReturn(null);
    when(brandAuthorisationService.create(Mockito.any(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(
        Pair.of(new BrandAuthCreateResponse(), new BrandAuthorisation()));
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(brandAuthorisationService).create(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(this.brandAuthHistoryService).generateBrandAuthHistoryDomainEventModel(
        any(BrandAuthorisationHistory.class));
    verify(this.brandAuthorisationWipService).updateWipEntryForActivation(DEFAULT_BRAND_CODE,
        DEFAULT_SELLER_CODE);
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueOnlyAuthEndDateUpdated()
      throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date expireDate = calendar.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthorisation.setAuthExpireDate(startDate);
    brandAuthUpdateRequest.setAuthStartDate(startDate);
    brandAuthUpdateRequest.setAuthExpireDate(expireDate);
    brandAuthorisationUpdated.setAuthStartDate(startDate);
    brandAuthorisationUpdated.setAuthExpireDate(expireDate);
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService, times(1)).publishBrandAuthHistoryEvent(anyString(),
        anyString(), any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueOnlyAuthEndDateUpdatedEditFromUpcomingTab()
      throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date expireDate = calendar.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthorisation.setAuthExpireDate(startDate);
    brandAuthUpdateRequest.setAuthStartDate(startDate);
    brandAuthUpdateRequest.setAuthExpireDate(expireDate);
    brandAuthUpdateRequest.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthUpdateRequest.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING.name());
    brandAuthorisationUpdated.setAuthStartDate(startDate);
    brandAuthorisationUpdated.setAuthExpireDate(expireDate);
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(brandAuthorisationWipService).updateWipEntryForActivation(DEFAULT_BRAND_CODE,
        DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService, times(1)).publishBrandAuthHistoryEvent(anyString(),
        anyString(), any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueAuthStartDateAndAuthEndDateAndStatusUpdated()
      throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date expireDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date newStartDate = calendar.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthorisation.setAuthExpireDate(startDate);
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthUpdateRequest.setAuthStartDate(newStartDate);
    brandAuthUpdateRequest.setAuthExpireDate(expireDate);
    brandAuthUpdateRequest.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE.name());
    brandAuthorisationUpdated.setAuthStartDate(newStartDate);
    brandAuthorisationUpdated.setAuthExpireDate(expireDate);
    brandAuthorisationUpdated.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService, times(3)).publishBrandAuthHistoryEvent(anyString(),
        anyString(), any(BrandAuthorisationHistory.class));
  }

  @Test
  public void editBrandAuthDetailsAndEvictCache_wipSwitchTrueThresholdNotBreached()
      throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date newStartDate = calendar.getTime();
    brandAuthorisation.setAuthStartDate(startDate);
    brandAuthUpdateRequest.setAuthStartDate(newStartDate);
    brandAuthorisationUpdated.setAuthStartDate(newStartDate);
    when(brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest)).thenReturn(brandAuthorisationUpdated);
    brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    verify(brandAuthorisationService).editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
        brandAuthUpdateRequest);
    verify(
        authorisationRepository).findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE);
    verify(applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(domainEventPublisherService, times(1)).publishBrandAuthHistoryEvent(anyString(),
        anyString(), any(BrandAuthorisationHistory.class));
  }

  @Test
  public void deleteBrandAuthAndEvictCacheTest() throws Exception {
    Mockito.when(brandAuthorisationService.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
        StringUtils.EMPTY)).thenReturn(StringUtils.EMPTY);
    this.brandAuthServiceWrapper.deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME,
        brandAuthDeleteRequestList);
    verify(brandAuthorisationService).deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
        StringUtils.EMPTY);
  }

  @Test
  public void deleteBrandAuthAndEvictCacheTestException() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(brandAuthorisationService)
        .deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY, StringUtils.EMPTY);
    this.brandAuthServiceWrapper.deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME,
        brandAuthDeleteRequestList);
    verify(brandAuthorisationService).deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
        StringUtils.EMPTY);
  }

  @Test
  public void deleteBrandAuthAndEvictCacheWithFailuresTest() throws Exception {
    Mockito.when(brandAuthorisationService.deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
        DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
        StringUtils.EMPTY)).thenReturn(BRAND_NAME);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.brandAuthServiceWrapper.deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID,
              DEFAULT_USERNAME, brandAuthDeleteRequestList));
    } finally {
      verify(brandAuthorisationService).deleteMappingByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
          DEFAULT_USERNAME, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, StringUtils.EMPTY,
          StringUtils.EMPTY);
    }
  }

  @Test
  public void createBrandAuthAndEvictCacheTest() throws Exception {
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateResponse.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.when(brandAuthorisationService.create(brandAuthCreateRequest, DEFAULT_STORE_ID,
        DEFAULT_USERNAME)).thenReturn(Pair.of(brandAuthCreateResponse, brandAuthorisation));
    BrandAuthCreateResponse authCreateResponse =
        this.brandAuthServiceWrapper.createBrandAuthAndEvictCache(brandAuthCreateRequest,
            DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationService)
        .create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(this.brandAuthHistoryService).generateBrandAuthHistoryDomainEventModel(
        any(BrandAuthorisationHistory.class));
    Assertions.assertEquals(DEFAULT_BRAND_CODE, authCreateResponse.getBrandCode());
  }

  @Test
  public void createBrandAuthAndEvictCacheBulkActionTrueTest() throws Exception {
    brandAuthUpdateRequest.setSellerCode(DEFAULT_SELLER_CODE_1);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthUpdateRequest.setAuthStartDate(brandAuthCreateRequest.getAuthStartDate());
    brandAuthUpdateRequest.setAuthExpireDate(brandAuthCreateRequest.getAuthExpireDate());
    brandAuthUpdateRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandAuthCreateRequest.setBulkAction(true);
    brandAuthCreateResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthCreateResponse.setBulkAction(true);
    Mockito.when(brandAuthorisationService.create(brandAuthCreateRequest, DEFAULT_STORE_ID,
        DEFAULT_USERNAME)).thenReturn(Pair.of(brandAuthCreateResponse, brandAuthorisation));
    Mockito.when(
        brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
            brandAuthUpdateRequest)).thenReturn(brandAuthorisation);
    BrandAuthCreateResponse authCreateResponse =
        this.brandAuthServiceWrapper.createBrandAuthAndEvictCache(brandAuthCreateRequest,
            DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationService)
        .create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    Mockito.verify(brandAuthorisationService)
        .editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation, brandAuthUpdateRequest);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, authCreateResponse.getBrandCode());
  }

  @Test
  public void createBrandAuthAndEvictCacheBulkActionTrueNullResponseTest() throws Exception {
    brandAuthUpdateRequest.setSellerCode(DEFAULT_SELLER_CODE_1);
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthUpdateRequest.setAuthStartDate(brandAuthCreateRequest.getAuthStartDate());
    brandAuthUpdateRequest.setAuthExpireDate(brandAuthCreateRequest.getAuthExpireDate());
    brandAuthUpdateRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandAuthCreateRequest.setBulkAction(true);
    brandAuthCreateResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthCreateResponse.setBulkAction(true);
    Mockito.when(brandAuthorisationService.create(brandAuthCreateRequest, DEFAULT_STORE_ID,
        DEFAULT_USERNAME)).thenReturn(Pair.of(brandAuthCreateResponse, brandAuthorisation));
    Mockito.when(
        brandAuthorisationService.editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation,
            brandAuthUpdateRequest)).thenReturn(null);
    BrandAuthCreateResponse authCreateResponse =
        this.brandAuthServiceWrapper.createBrandAuthAndEvictCache(brandAuthCreateRequest,
            DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationService)
        .create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationService)
        .editBrandAuthDetails(DEFAULT_STORE_ID, brandAuthorisation, brandAuthUpdateRequest);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, authCreateResponse.getBrandCode());
  }

  @Test
  public void createBrandAuthAndEvictCache_newFlowTrueAndWipCreation() throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    brandAuthCreateRequest.setAuthStartDate(brandAuthCreateRequest.getAuthStartDate());
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setSellerCode(DEFAULT_SELLER_CODE_1);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 3);
    Date startDate = calendar.getTime();
    brandAuthCreateRequest.setAuthStartDate(startDate);
    calendar.add(Calendar.DATE, 12);
    Date endDate = calendar.getTime();
    brandAuthCreateRequest.setAuthExpireDate(endDate);
    brandAuthCreateResponse.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.when(
        brandAuthorisationWipService.createWipFromInternal(brandAuthCreateRequest, DEFAULT_STORE_ID,
            DEFAULT_USERNAME)).thenReturn(Pair.of(brandAuthCreateResponse, brandAuthorisation));
    BrandAuthCreateResponse authCreateResponse =
        this.brandAuthServiceWrapper.createBrandAuthAndEvictCache(brandAuthCreateRequest,
            DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationWipService)
        .createWipFromInternal(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, authCreateResponse.getBrandCode());
  }

  @Test
  public void createBrandAuthAndEvictCache_newFlowTrueButNoWipCreation() throws Exception {
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipSwitch", true);
    ReflectionTestUtils.setField(this.brandAuthServiceWrapper, "brandAuthWipThresholdInDays", 2);
    brandAuthCreateRequest.setAuthStartDate(brandAuthCreateRequest.getAuthStartDate());
    brandAuthCreateRequest = generateCreateBrandAuthRequest();
    brandAuthCreateRequest.setSellerCode(DEFAULT_SELLER_CODE_1);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date startDate = calendar.getTime();
    brandAuthCreateRequest.setAuthStartDate(startDate);
    calendar.add(Calendar.DATE, 12);
    Date endDate = calendar.getTime();
    brandAuthCreateRequest.setAuthExpireDate(endDate);
    brandAuthCreateResponse.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.when(brandAuthorisationService.create(brandAuthCreateRequest, DEFAULT_STORE_ID,
        DEFAULT_USERNAME)).thenReturn(Pair.of(brandAuthCreateResponse, brandAuthorisation));
    BrandAuthCreateResponse authCreateResponse =
        this.brandAuthServiceWrapper.createBrandAuthAndEvictCache(brandAuthCreateRequest,
            DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationService)
        .create(brandAuthCreateRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_CODE);
    verify(this.brandAuthHistoryService).generateBrandAuthHistoryDomainEventModel(
        any(BrandAuthorisationHistory.class));
    Assertions.assertEquals(DEFAULT_BRAND_CODE, authCreateResponse.getBrandCode());
  }

  @Test
  void createBrandAuthWipTest() throws Exception {
    brandAuthorisationWip.setAuthStartDate(new Date());
    brandAuthorisationWip.setAuthExpireDate(new Date());
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthCreateWipResponse = new BrandAuthCreateWipResponse();
    brandAuthCreateWipResponse.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.when(brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest,
            DEFAULT_STORE_ID, DEFAULT_USERNAME))
        .thenReturn(Pair.of(brandAuthCreateWipResponse, brandAuthorisationWip));
    BrandAuthCreateWipResponse authCreateResponse =
        this.brandAuthServiceWrapper.createBrandAuthWip(brandAuthCreateWipRequest, DEFAULT_STORE_ID,
            DEFAULT_USERNAME);
    Mockito.verify(brandAuthorisationWipService)
        .brandAuthCreateWipRequest(brandAuthCreateWipRequest, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    verify(this.brandAuthHistoryService).generateBrandAuthHistoryDomainEventModel(
        any(BrandAuthorisationHistory.class));
    Assertions.assertEquals(DEFAULT_BRAND_CODE, authCreateResponse.getBrandCode());
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
    request.setBrandName(DEFAULT_BRAND_NAME);
    request.setBrandCode(DEFAULT_BRAND_CODE);
    request.setIprRegistrationNumber(DEFAULT_IPR_REGISTRATION_NUMBER);
    request.setSellerCode(DEFAULT_SELLER_CODE_1);
    request.setAuthExpireDate(expireDate);
    request.setAuthStartDate(date);
    return request;
  }

}
