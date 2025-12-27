package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.businesspartner.entity.Company;
import com.gdn.x.businesspartner.entity.LegalityOfCompany;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.businesspartner.entity.ResponsiblePerson;
import com.gdn.x.businesspartner.entity.Taxation;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;

public class BusinessPartnerRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE_2 = "BLI-00002";
  private static final String BP_CODE = "VAN-1234";
  private static final String DEFAULT_CLIENT_ID = "clientId";

  @Mock
  private XbpFeign xbpFeign;

  private BusinessPartnerRepositoryBean businessPartnerRepositoryBean;
  private String requestId;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(xbpFeign);
  }

  private Profile getBusinessPartner() {
    Profile businessPartner = new Profile();
    businessPartner.setBusinessPartnerCode("BP1-00001");
    businessPartner.setBusinessPartnerType("1");
    businessPartner.setCompany(new Company());
    businessPartner.setLegalityOfCompany(new LegalityOfCompany());
    businessPartner.setResponsiblePerson(new ResponsiblePerson());
    businessPartner.setPickupPoints(new ArrayList<>());
    businessPartner.setBankAccounts(new ArrayList<>());
    businessPartner.setPaymentAccounts(new ArrayList<>());
    businessPartner.setCategories(new ArrayList<>());
    businessPartner.setTaxation(new Taxation());
    businessPartner.setReasonStatus("reason-status");
    businessPartner.setReactivateCount(0);
    businessPartner.setAcceptedDatePKS(new Date());
    businessPartner.setRelationChanged(Boolean.FALSE);
    businessPartner.setCreatedBy(DEFAULT_USERNAME);
    businessPartner.setCreatedDate(Calendar.getInstance().getTime());
    businessPartner.setStoreId(DEFAULT_STORE_ID);
    businessPartner.setId(UUID.randomUUID().toString());
    businessPartner.setShowCncCatalogBanner(false);
    businessPartner.setSlaForPrepareOrderInHour(20);
    return businessPartner;
  }

  public BusinessPartnerRepositoryBean getBusinessPartnerRepositoryBean() {
    return businessPartnerRepositoryBean;
  }

  public String getRequestId() {
    return requestId;
  }

  public XbpFeign getXbpFeign() {
    return xbpFeign;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    businessPartnerRepositoryBean = new BusinessPartnerRepositoryBean();
    getBusinessPartnerRepositoryBean().setXbpFeign(xbpFeign);
    setRequestId(UUID.randomUUID().toString());
    ProfileResponse businessPartnerData = new ProfileResponse();
    GdnRestSingleResponse<ProfileResponse> responseDetail = new GdnRestSingleResponse<ProfileResponse>(businessPartnerData, DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<ProfileResponse> responseDetailError =
        new GdnRestSingleResponse<ProfileResponse>("Read Timeout",
            ErrorCategory.UNSPECIFIED.getCode(), false, null, DEFAULT_REQUEST_ID);
    List<ProfileResponse> profileResponses = new ArrayList<ProfileResponse>();
    GdnRestListResponse<ProfileResponse> baseProfileResponses =
        new GdnRestListResponse<ProfileResponse>(profileResponses, new PageMetaData(1, 0, 1),
            BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
  }

  public void setBusinessPartnerRepositoryBean(BusinessPartnerRepositoryBean businessPartnerRepositoryBean) {
    this.businessPartnerRepositoryBean = businessPartnerRepositoryBean;
  }

  public void setXbpFeign(XbpFeign xbpFeign) {
    this.xbpFeign = xbpFeign;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Test
  public void filterDetailByBusinessPartnerCodeSuccessTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    Profile businessPartner = getBusinessPartner();
    ProfileResponse businessPartnerResponse = new ProfileResponse();
    BeanUtils.copyProperties(businessPartner, businessPartnerResponse);
    Mockito.when(this.getXbpFeign()
        .getBusinessPartnerDetails(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.eq(false),
            Mockito.eq(false), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestSingleResponse<ProfileResponse>(null, null, true, businessPartnerResponse, DEFAULT_REQUEST_ID));
    this.businessPartnerRepositoryBean.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.getXbpFeign())
        .getBusinessPartnerDetails(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.eq(false),
            Mockito.eq(false), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void filterDetailByBusinessPartnerCodeStoreIdEmptySuccessTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, StringUtils.EMPTY);
    Profile businessPartner = getBusinessPartner();
    ProfileResponse businessPartnerResponse = new ProfileResponse();
    BeanUtils.copyProperties(businessPartner, businessPartnerResponse);
    Mockito.when(this.getXbpFeign()
        .getBusinessPartnerDetails(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.eq(false),
            Mockito.eq(false), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestSingleResponse<ProfileResponse>(null, null, true, businessPartnerResponse, DEFAULT_REQUEST_ID));
    this.businessPartnerRepositoryBean.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.getXbpFeign())
        .getBusinessPartnerDetails(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.eq(false),
            Mockito.eq(false), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void filterDetailByBusinessPartnerCodeFailureTest() throws Exception {
    Profile businessPartner = getBusinessPartner();
    ProfileResponse businessPartnerResponse = new ProfileResponse();
    BeanUtils.copyProperties(businessPartner, businessPartnerResponse);
    Mockito.when(this.getXbpFeign()
        .getBusinessPartnerDetails(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.eq(false),
            Mockito.eq(false), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestSingleResponse<ProfileResponse>(null, null, false, businessPartnerResponse, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.businessPartnerRepositoryBean.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
      });
    } finally {
      Mockito.verify(this.getXbpFeign())
          .getBusinessPartnerDetails(Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.eq(false),
              Mockito.eq(false), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void filterPickupPointCodesByPickupPointRequestTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "pickupPointFilterBatchSize", 2);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    PageMetaData pageMetaData = new PageMetaData(2, 0, 1);
    GdnRestListResponse<PickupPointResponse> response =
        new GdnRestListResponse<>(Collections.singletonList(pickupPointResponse), pageMetaData, DEFAULT_REQUEST_ID);
    Mockito.when(this.getXbpFeign()
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(2), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest))).thenReturn(response);
    this.businessPartnerRepositoryBean.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(this.getXbpFeign())
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(2), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest));
  }

  @Test
  public void filterPickupPointCodesByPickupPointRequestForOneTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    PageMetaData pageMetaData = new PageMetaData(2, 0, 1);
    GdnRestListResponse<PickupPointResponse> response =
        new GdnRestListResponse<>(Collections.singletonList(pickupPointResponse), pageMetaData, DEFAULT_REQUEST_ID);
    Mockito.when(this.getXbpFeign()
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest))).thenReturn(response);
    this.businessPartnerRepositoryBean.filterPickupPointsByPickupPointRequestForOne(pickupPointFilterRequest);
    Mockito.verify(this.getXbpFeign())
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest));
  }

  @Test
  public void filterPickupPointCodesByPickupPointRequestForOneFailureTest() throws Exception {
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "pickupPointFilterBatchSize", 2);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, StringUtils.EMPTY);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    GdnRestListResponse<PickupPointResponse> response =
        new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID);
    Mockito.when(this.getXbpFeign()
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.businessPartnerRepositoryBean.filterPickupPointsByPickupPointRequestForOne(pickupPointFilterRequest);
      });
    } finally {
      Mockito.verify(this.getXbpFeign())
          .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.eq(pickupPointFilterRequest));
    }
  }

  @Test
  public void filterPickupPointCodesByPickupPointRequest1Test() throws Exception {
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "pickupPointFilterBatchSize", 1);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    PageMetaData pageMetaData = new PageMetaData(1, 0, 2);
    GdnRestListResponse<PickupPointResponse> response =
        new GdnRestListResponse<>(Collections.singletonList(pickupPointResponse), pageMetaData, DEFAULT_REQUEST_ID);
    Mockito.when(this.getXbpFeign()
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest))).thenReturn(response);
    Mockito.when(this.getXbpFeign()
        .filter(Mockito.any(), Mockito.eq(1), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest))).thenReturn(response);
    this.businessPartnerRepositoryBean.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(this.getXbpFeign())
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest));
    Mockito.verify(this.getXbpFeign())
        .filter(Mockito.any(), Mockito.eq(1), Mockito.eq(1), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest));
  }

  @Test
  public void filterPickupPointCodesByPickupPointRequestFailureTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        BusinessPartnerRepositoryBeanTest.DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(businessPartnerRepositoryBean, "pickupPointFilterBatchSize", 2);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    GdnRestListResponse<PickupPointResponse> response =
        new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID);
    Mockito.when(this.getXbpFeign()
        .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(2), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(pickupPointFilterRequest))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.businessPartnerRepositoryBean.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
      });
    } finally {
      Mockito.verify(this.getXbpFeign())
          .filter(Mockito.any(), Mockito.eq(0), Mockito.eq(2), Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.eq(pickupPointFilterRequest));
    }
  }

  @Test
  public void filterDetailsByBusinessPartnerCodeListSuccessTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_STORE_ID);
    List<String> businessPartnerCodes = new ArrayList<>();
    businessPartnerCodes.add(DEFAULT_BUSINESS_PARTNER_CODE);
    businessPartnerCodes.add(DEFAULT_BUSINESS_PARTNER_CODE_2);
    BusinessPartnerCodesRequest businessPartnerCodesRequest = new BusinessPartnerCodesRequest();
    BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
    businessPartnerCodesRequest.setBusinessPartnerCodes(businessPartnerCodes);
    businessPartnerFilterRequest
        .setBusinessPartnerCodes(new HashSet<>(businessPartnerCodesRequest.getBusinessPartnerCodes()));
    Mockito.when(this.getXbpFeign()
        .getBusinessPartnersDetails(Mockito.any(), Mockito.eq(businessPartnerFilterRequest), Mockito.eq(0),
            Mockito.eq(businessPartnerFilterRequest.getBusinessPartnerCodes().size()), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any())).thenReturn(new GdnRestListResponse<>(null, null, true, DEFAULT_REQUEST_ID));
    this.businessPartnerRepositoryBean.filterDetailsByBusinessPartnerCodeList(businessPartnerCodesRequest);
    Mockito.verify(this.getXbpFeign())
        .getBusinessPartnersDetails(Mockito.any(), Mockito.eq(businessPartnerFilterRequest), Mockito.eq(0),
            Mockito.eq(businessPartnerFilterRequest.getBusinessPartnerCodes().size()), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any());
  }

  @Test
  public void filterDetailsByBusinessPartnerCodeListFailureTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, StringUtils.EMPTY);
    List<String> businessPartnerCodes = new ArrayList<>();
    businessPartnerCodes.add(DEFAULT_BUSINESS_PARTNER_CODE);
    businessPartnerCodes.add(DEFAULT_BUSINESS_PARTNER_CODE_2);
    BusinessPartnerCodesRequest businessPartnerCodesRequest = new BusinessPartnerCodesRequest();
    BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
    businessPartnerCodesRequest.setBusinessPartnerCodes(businessPartnerCodes);
    businessPartnerFilterRequest
        .setBusinessPartnerCodes(new HashSet<>(businessPartnerCodesRequest.getBusinessPartnerCodes()));
    Mockito.when(this.getXbpFeign()
        .getBusinessPartnersDetails(Mockito.any(), Mockito.eq(businessPartnerFilterRequest), Mockito.eq(0),
            Mockito.eq(businessPartnerFilterRequest.getBusinessPartnerCodes().size()), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any())).thenReturn(new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.businessPartnerRepositoryBean.filterDetailsByBusinessPartnerCodeList(businessPartnerCodesRequest);
      });
    } finally {
      Mockito.verify(this.getXbpFeign())
          .getBusinessPartnersDetails(Mockito.any(), Mockito.eq(businessPartnerFilterRequest), Mockito.eq(0),
              Mockito.eq(businessPartnerFilterRequest.getBusinessPartnerCodes().size()), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void filterByCodeTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, StringUtils.EMPTY);
    Profile businessPartner = getBusinessPartner();
    ProfileResponse businessPartnerResponse = new ProfileResponse();
    ResponsiblePersonDTO responsiblePersonResponse = new ResponsiblePersonDTO();
    CompanyDTO companyResponse = new CompanyDTO();
    BeanUtils.copyProperties(businessPartner, businessPartnerResponse);
    BeanUtils.copyProperties(businessPartner.getResponsiblePerson(), responsiblePersonResponse);
    BeanUtils.copyProperties(businessPartner.getCompany(), companyResponse);
    businessPartnerResponse.setResponsiblePerson(responsiblePersonResponse);
    businessPartnerResponse.setCompany(companyResponse);
    businessPartnerResponse.setMerchantStatus("ACTIVE");
    GdnRestSingleResponse<ProfileResponse> response =
        new GdnRestSingleResponse<ProfileResponse>(null, null, true, businessPartnerResponse, getRequestId());
    Mockito.when(getXbpFeign().getBusinessPartnerDetails("BP1-00001", Constants.DEFAULT_USERNAME, false, false,
        Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID)).thenReturn(response);
    Profile result = getBusinessPartnerRepositoryBean().filterByCode(businessPartner.getBusinessPartnerCode());
    Mockito.verify(getXbpFeign(), AT_LEAST_ONE)
        .getBusinessPartnerDetails("BP1-00001", Constants.DEFAULT_USERNAME, false, false, Constants.DEFAULT_STORE_ID,
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID);
    Assertions.assertEquals("BP1-00001", result.getBusinessPartnerCode());
  }

  @Test
  public void filterByCodeTest2() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constants.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    Profile businessPartner = getBusinessPartner();
    ProfileResponse businessPartnerResponse = new ProfileResponse();
    ResponsiblePersonDTO responsiblePersonResponse = new ResponsiblePersonDTO();
    CompanyDTO companyResponse = new CompanyDTO();
    BeanUtils.copyProperties(businessPartner, businessPartnerResponse);
    BeanUtils.copyProperties(businessPartner.getResponsiblePerson(), responsiblePersonResponse);
    BeanUtils.copyProperties(businessPartner.getCompany(), companyResponse);
    businessPartnerResponse.setResponsiblePerson(responsiblePersonResponse);
    businessPartnerResponse.setCompany(companyResponse);
    businessPartnerResponse.setMerchantStatus("ACTIVE");
    GdnRestSingleResponse<ProfileResponse> response =
        new GdnRestSingleResponse<ProfileResponse>(null, null, true, businessPartnerResponse, getRequestId());
    Mockito.when(getXbpFeign().getBusinessPartnerDetails("BP1-00001", Constants.DEFAULT_USERNAME, false, false,
        Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID)).thenReturn(response);
    Profile result = getBusinessPartnerRepositoryBean().filterByCode(businessPartner.getBusinessPartnerCode());
    Mockito.verify(getXbpFeign(), AT_LEAST_ONE)
        .getBusinessPartnerDetails("BP1-00001", Constants.DEFAULT_USERNAME, false, false, Constants.DEFAULT_STORE_ID,
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID);
    Assertions.assertEquals("BP1-00001", result.getBusinessPartnerCode());
  }

}
