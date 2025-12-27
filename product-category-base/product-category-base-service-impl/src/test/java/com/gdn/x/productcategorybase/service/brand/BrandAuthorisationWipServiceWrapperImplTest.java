package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.BrandAuthorisationWipAction;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.BrandAuthorisationStatusIdDTO;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.service.impl.BrandAuthServiceWrapperImpl;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryServiceImpl;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@ExtendWith(MockitoExtension.class)
class BrandAuthorisationWipServiceWrapperImplTest {

  private static final String BRAND_CODE = "brand_code";
  private static final String USERNAME = "username";
  private static final String STORE_ID = "10001";
  private static final String SELLER_CODE = "seller_code";
  private static final String ID = "id";

  @InjectMocks
  private BrandAuthorisationWipServiceWrapperImpl brandAuthorisationWipServiceWrapper;

  @Mock
  private BrandAuthorisationWipServiceImpl brandAuthorisationWipService;

  @Mock
  private BrandAuthServiceWrapperImpl brandAuthServiceWrapper;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private MailDeliveryServiceImpl mailDeliveryService;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  private BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest;
  private BrandAuthorisation brandAuthorisation;

  @BeforeEach
  void setup() {
    brandAuthorisationWipActionRequest = new BrandAuthorisationWipActionRequest();
    brandAuthorisationWipActionRequest.setBrandCode(BRAND_CODE);
    brandAuthorisationWipActionRequest.setAuthStartDate(new Date());
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.APPROVE.name());
    brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setBrandCode(BRAND_CODE);
    brandAuthorisation.setSellerCode(SELLER_CODE);
    ReflectionTestUtils.setField(this.brandAuthorisationWipServiceWrapper, "brandAuthSendEmail", true);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(brandAuthServiceWrapper);
    Mockito.verifyNoMoreInteractions(brandAuthorisationWipService);
    Mockito.verifyNoMoreInteractions(applicationCacheServiceBean);
    Mockito.verifyNoMoreInteractions(domainEventPublisherService);
  }

  @Test
  void brandAuthorisationWipActionTest() {
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(SELLER_CODE);
    Mockito.when(brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest)).thenReturn(
        Pair.of(Collections.singletonList(brandAuthorisationHistory),
            BrandAuthorisationStatusIdDTO.builder()
                .status(BrandAuthorizationWipStatus.ACTIVE.name()).id(ID).build()));
    brandAuthorisationWipServiceWrapper.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest);
    Mockito.verify(brandAuthorisationWipService)
      .brandAuthorisationWipAction(STORE_ID, USERNAME, brandAuthorisationWipActionRequest);
    Mockito.verify(applicationCacheServiceBean).evictBrandAuthorizationCache(BRAND_CODE);
    Mockito.verify(domainEventPublisherService)
      .publishBrandAuthHistoryEvent(Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(mailDeliveryService)
        .sendBrandAuthorisationActionMail(eq(brandAuthorisationWipActionRequest.getSellerCode()),
            any(), eq(brandAuthorisationWipActionRequest.getAction()),
                eq(BrandAuthorisationStatus.ACTIVE.name()), eq(ID), any());
  }

  @Test
  void brandAuthorisationWipApproveActionWithoutHistoryTest() {
    ReflectionTestUtils.setField(this.brandAuthorisationWipServiceWrapper, "brandAuthSendEmail", false);
    Mockito.when(brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest)).thenReturn(
        Pair.of(new ArrayList<>(),
            BrandAuthorisationStatusIdDTO.builder()
                .status(BrandAuthorizationWipStatus.ACTIVE.name()).id(ID).build()));
    brandAuthorisationWipServiceWrapper.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest);
    Mockito.verify(brandAuthorisationWipService)
      .brandAuthorisationWipAction(STORE_ID, USERNAME, brandAuthorisationWipActionRequest);
  }

  @Test
  void brandAuthorisationWipNeedRevisionActionTest() {
    brandAuthorisationWipActionRequest.setAction(BrandAuthorisationWipAction.NEED_REVISION.name());
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(SELLER_CODE);
    Mockito.when(brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest)).thenReturn(Pair.of(new ArrayList<>(),
        BrandAuthorisationStatusIdDTO.builder()
            .status(BrandAuthorizationWipStatus.NEED_REVISION.name()).id(ID).build()));
    brandAuthorisationWipServiceWrapper.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest);
    Mockito.verify(brandAuthorisationWipService)
      .brandAuthorisationWipAction(STORE_ID, USERNAME, brandAuthorisationWipActionRequest);
    Mockito.verify(mailDeliveryService)
        .sendBrandAuthorisationActionMail(eq(brandAuthorisationWipActionRequest.getSellerCode()),
            any(), eq(brandAuthorisationWipActionRequest.getAction()),
            eq(BrandAuthorizationWipStatus.NEED_REVISION.name()), eq(ID), any());
  }

  @Test
  void brandAuthorisationWipActionExceptionTest() {
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(SELLER_CODE);
    Mockito.when(brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
      brandAuthorisationWipActionRequest)).thenReturn(Pair.of(List.of(brandAuthorisationHistory),
        BrandAuthorisationStatusIdDTO.builder()
            .status(BrandAuthorizationWipStatus.NEED_REVISION.name()).id(ID).build()));
    Mockito.doThrow(new RuntimeException("Cache eviction failed")).when(applicationCacheServiceBean)
      .evictBrandAuthorizationCache(Mockito.anyString());

    assertThrows(Exception.class,
      () -> brandAuthorisationWipServiceWrapper.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest));
  }

  @Test
  void checkEligibilityTest() {
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
            Constants.MAX_PENDING_BRAND_AUTH_WIP_REQUESTS))
        .thenReturn(SystemParameter.builder().value(Integer.toString(Constants.TWO)).build());
    Mockito.when(brandAuthorisationWipService.fetchCountOfPendingRequestsForSeller(STORE_ID, SELLER_CODE))
        .thenReturn(1L);
    boolean response = brandAuthorisationWipServiceWrapper.checkEligibility(STORE_ID, SELLER_CODE);
    Assertions.assertTrue(response);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.MAX_PENDING_BRAND_AUTH_WIP_REQUESTS);
    Mockito.verify(brandAuthorisationWipService)
        .fetchCountOfPendingRequestsForSeller(STORE_ID, SELLER_CODE);
  }

  @Test
  void checkEligibilityTest_notEligible() {
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
            Constants.MAX_PENDING_BRAND_AUTH_WIP_REQUESTS))
        .thenReturn(SystemParameter.builder().value(Integer.toString(Constants.TWO)).build());
    Mockito.when(brandAuthorisationWipService.fetchCountOfPendingRequestsForSeller(STORE_ID, SELLER_CODE))
        .thenReturn(2L);
    boolean response = brandAuthorisationWipServiceWrapper.checkEligibility(STORE_ID, SELLER_CODE);
    Assertions.assertFalse(response);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.MAX_PENDING_BRAND_AUTH_WIP_REQUESTS);
    Mockito.verify(brandAuthorisationWipService)
        .fetchCountOfPendingRequestsForSeller(STORE_ID, SELLER_CODE);
  }

  @Test
  void submitBrandAuthorisationRequestTest() throws Exception {
    BrandAuthUpdateRequest brandAuthUpdateRequest = new BrandAuthUpdateRequest();
    brandAuthUpdateRequest.setSellerCode(SELLER_CODE);
    brandAuthUpdateRequest.setBrandCode(BRAND_CODE);
    brandAuthUpdateRequest.setAuthStartDate(new Date());
    brandAuthUpdateRequest.setAuthExpireDate(new Date());
    brandAuthorisationWipServiceWrapper.submitBrandAuthorisationRequest(STORE_ID, USERNAME,
      brandAuthUpdateRequest);
    Mockito.verify(domainEventPublisherService)
      .publishBrandAuthHistoryEvent(eq(BRAND_CODE), eq(SELLER_CODE), Mockito.any());
    Mockito.verify(brandAuthorisationWipService)
      .submitBrandAuthorisationRequest(STORE_ID, brandAuthUpdateRequest);
  }

  @Test
  void sendNearExpiryMailNotificationTest() {
    Mockito.when(brandAuthorisationWipService.fetchBrandAuthorisationForNearExpiry(STORE_ID))
        .thenReturn(Collections.singletonList(brandAuthorisation));
    brandAuthorisationWipServiceWrapper.sendNearExpiryMailNotification(STORE_ID);
    Mockito.verify(brandAuthorisationWipService).fetchBrandAuthorisationForNearExpiry(STORE_ID);
    Mockito.verify(mailDeliveryService).sendBrandAuthorisationActionMail(eq(SELLER_CODE), any(),
        eq(BrandAuthorisationStatus.NEAR_EXPIRY.name()),
        eq(BrandAuthorisationStatus.NEAR_EXPIRY.name()), any(), any());
  }
}
