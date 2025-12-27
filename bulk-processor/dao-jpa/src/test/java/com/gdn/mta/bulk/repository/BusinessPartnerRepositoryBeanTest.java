package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.XBPFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.slf4j.MDC;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;
import org.junit.jupiter.api.Assertions;
import com.gdn.common.exception.ApplicationException;

public class BusinessPartnerRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final String REQUEST_ID = "request-id";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();

  private PickupPointFilterRequest pickupPointFilterRequest;
  private PickupPointResponse pickupPointResponse;

  @Mock
  private XBPFeign xbpFeign;

  @Mock
  private ApplicationContext applicationContext;

  @InjectMocks
  private BusinessPartnerRepositoryBean businessPartnerRepositoryBean;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(xbpFeign);
    Mockito.verifyNoMoreInteractions(applicationContext);
  }

  private ProfileResponse getBusinessPartner() throws Exception {
    Date date = Calendar.getInstance().getTime();
    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    businessPartner.setStoreId(DEFAULT_STORE_ID);
    businessPartner.setCreatedDate(date);
    businessPartner.setCreatedBy(DEFAULT_USERNAME);
    businessPartner.setUpdatedDate(date);
    businessPartner.setUpdatedBy(DEFAULT_USERNAME);
    return businessPartner;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointResponse = new PickupPointResponse();
  }

  @Test
  public void filterByBusinessPartnerCodeTest() throws Exception {
    ProfileResponse businessPartner = getBusinessPartner();
    GdnRestSingleResponse<ProfileResponse> response =
        new GdnRestSingleResponse<ProfileResponse>(null, null, true, businessPartner, DEFAULT_REQUEST_ID);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(response);
    businessPartnerRepositoryBean
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void filterBusinessPartnerPickupPointTest() throws Exception {
    List<PickupPointResponse> pointResponseList = new ArrayList<>();
    pointResponseList.add(pickupPointResponse);
    pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    GdnRestListResponse<PickupPointResponse> response =
      new GdnRestListResponse<>(null, null, true, pointResponseList,
        new PageMetaData(0, 0, pointResponseList.size()), REQUEST_ID);
    Mockito.when(
        xbpFeign.getPickupPointList(DEFAULT_STORE_ID, Constant.USER_NAME, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, 0, 1,
            PickupPointFilterRequest.builder().waitingDeletion(false).businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
                .build())).thenReturn(response);
    businessPartnerRepositoryBean.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).waitingDeletion(false)
            .build());
    Mockito.verify(xbpFeign)
        .getPickupPointList(DEFAULT_STORE_ID, Constant.USER_NAME, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, 0, 1,
            PickupPointFilterRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).waitingDeletion(false)
                .build());
  }

  @Test
  void filterByBusinessPartnerCodeListTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constant.STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constant.USER_NAME);
    BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
    businessPartnerFilterRequest.setBusinessPartnerCodes(Set.of(DEFAULT_BUSINESS_PARTNER_CODE));
    ProfileResponse profileResponse = getBusinessPartner();
    GdnRestListResponse<ProfileResponse> response =
      new GdnRestListResponse<>(null, null, true, List.of(profileResponse),
        new PageMetaData(0, 0, 1), REQUEST_ID);
    Mockito.when(xbpFeign.getBusinessPartnerDetailsByList(Constant.STORE_ID, Constant.CHANNEL_ID,
      Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, 0, 10,
      businessPartnerFilterRequest)).thenReturn(response);
    businessPartnerRepositoryBean.filterByBusinessPartnerCodeList(businessPartnerFilterRequest, 0,
      10);
    Mockito.verify(xbpFeign)
      .getBusinessPartnerDetailsByList(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, 0, 10, businessPartnerFilterRequest);
  }

  @Test
  public void filterByBusinessPartnerCodeV2WithCacheTest() throws Exception {
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "useCacheForBusinessPartnerResponse", true);
    ProfileResponse businessPartner = getBusinessPartner();
    GdnRestSingleResponse<ProfileResponse> response =
        new GdnRestSingleResponse<>(null, null, true, businessPartner, DEFAULT_REQUEST_ID);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(response);
    Mockito.when(applicationContext.getBean(BusinessPartnerRepositoryBean.class))
        .thenReturn(businessPartnerRepositoryBean);
    ProfileResponse result =
        businessPartnerRepositoryBean.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign)
        .filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationContext)
        .getBean(BusinessPartnerRepositoryBean.class);
    Assertions.assertEquals(businessPartner, result);
  }

  @Test
  public void filterByBusinessPartnerCodeV2WithoutCacheTest() throws Exception {
    ProfileResponse businessPartner = getBusinessPartner();
    GdnRestSingleResponse<ProfileResponse> response =
        new GdnRestSingleResponse<>(null, null, true, businessPartner, DEFAULT_REQUEST_ID);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(response);
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "useCacheForBusinessPartnerResponse", false);
    ProfileResponse result =
        businessPartnerRepositoryBean.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign)
        .filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(businessPartner, result);
  }

  @Test
  public void filterByBusinessPartnerCodeV2WithNullResponseTest() {
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "useCacheForBusinessPartnerResponse", false);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(null);
    Assertions.assertThrows(ApplicationException.class,
        () -> businessPartnerRepositoryBean.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(xbpFeign)
        .filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void filterByBusinessPartnerCodeV2WithErrorResponseTest() {
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "useCacheForBusinessPartnerResponse", false);
    GdnRestSingleResponse<ProfileResponse> response =
        new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(response);
    Assertions.assertThrows(ApplicationException.class,
        () -> businessPartnerRepositoryBean.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(xbpFeign)
        .filterByBusinessPartnerCode(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
  }
}
