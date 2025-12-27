package com.gdn.partners.pcu.internal.service;

import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthCreateWipResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthWipDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.BrandAuthorisationWipServiceImpl;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthorisationWipActionWebRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class BrandAuthorisationWipServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String BRAND_CODE = "brand-code";
  private static final String SELLER_CODE = "seller-code";
  private static final String SELLER_CODE_2 = SELLER_CODE + Constants.TWO;
  private static final String BRAND_NAME = "brand-name";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "brand-id-11";
  private static final String IPR_REGISTRATION_NUMBER = "IPR-3334-XVU";
  private static final String IN_REVIEW = "IN_REVIEW";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String TABNAME = "In Review";
  private static final String ID = "ID";
  private static final String INVALID_STRING =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  @Mock
  PCBFeign pcbFeign;
  @Mock
  BPService bpService;
  @Mock
  FileStorageService fileStorageService;
  @InjectMocks
  private BrandAuthorisationWipServiceImpl brandAuthorisationWipService;
  private BrandAuthWipDetailResponse brandAuthWipDetailResponse;
  private BrandAuthorisationWipActionWebRequest brandAuthorisationWipActionWebRequest;
  private BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest;
  private BrandAuthCreateWipResponse brandAuthCreateWipResponse;
  private BrandAuthCreateWipRequest brandAuthCreateWipRequest;
  private BrandAuthUpdateRequest brandAuthUpdateRequest;
  private ProfileResponse profileResponse;
  private BrandAuthorisationWipListRequest brandAuthorisationWipListRequest;
  private BrandAuthorisationWipListResponse brandAuthorisationWipListResponse;
  private GdnRestSingleResponse<BrandAuthCreateWipResponse> authCreateResponseGdnRestSingleResponse
    = new GdnRestSingleResponse<>();
 

  @BeforeEach
  public void setUp() {
    ReflectionTestUtils.setField(brandAuthorisationWipService, "brandAuthWipNearExpiryDaysThreshold", 2);
    brandAuthWipDetailResponse = new BrandAuthWipDetailResponse();
    brandAuthWipDetailResponse.setBrandName(BRAND_NAME);
    brandAuthWipDetailResponse.setId(ID);
    brandAuthWipDetailResponse.setSellerCode(SELLER_CODE);
    brandAuthorisationWipActionWebRequest = new BrandAuthorisationWipActionWebRequest();
    brandAuthorisationWipActionWebRequest.setBrandCode(BRAND_CODE);
    brandAuthorisationWipActionRequest =
        new BrandAuthorisationWipActionRequest();
    brandAuthorisationWipActionRequest.setBrandCode(BRAND_CODE);
    brandAuthCreateWipRequest = new BrandAuthCreateWipRequest();
    brandAuthCreateWipRequest.setBrandCode(BRAND_CODE);
    brandAuthCreateWipResponse =
    BrandAuthCreateWipResponse.builder().brandCode(BRAND_CODE).build();
    authCreateResponseGdnRestSingleResponse.setValue(brandAuthCreateWipResponse);
    authCreateResponseGdnRestSingleResponse.setRequestId(REQUEST_ID);
    authCreateResponseGdnRestSingleResponse.setSuccess(true);
    brandAuthUpdateRequest = new BrandAuthUpdateRequest();
    brandAuthUpdateRequest.setBrandCode(BRAND_CODE);
    brandAuthUpdateRequest.setSellerCode(SELLER_CODE);
    brandAuthUpdateRequest.setBrandName(BRAND_NAME);
    brandAuthorisationWipListRequest = new BrandAuthorisationWipListRequest();
    brandAuthorisationWipListRequest.setSellerCode(SELLER_CODE);
    brandAuthorisationWipListRequest.setTabName(TABNAME);
    brandAuthorisationWipListResponse = new BrandAuthorisationWipListResponse();
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  void fetchBrandAuthWipDetailsTest() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar startDateCalendar = Calendar.getInstance();
    startDateCalendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = startDateCalendar.getTime();
    Calendar endDateCalendar = Calendar.getInstance();
    endDateCalendar.add(Calendar.DAY_OF_MONTH, 3);
    Date endDate = endDateCalendar.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
      .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
      brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, SELLER_CODE);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
  }

  @Test
  void fetchBrandAuthWipDetailsInReviewTest() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar startDateCalendar = Calendar.getInstance();
    startDateCalendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = startDateCalendar.getTime();
    Calendar endDateCalendar = Calendar.getInstance();
    endDateCalendar.add(Calendar.DAY_OF_MONTH, 3);
    Date endDate = endDateCalendar.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(IN_REVIEW);
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
        .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, SELLER_CODE);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
  }

  @Test
  void fetchBrandAuthWipDetailsTest_differentSeller() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar startDateCalendar = Calendar.getInstance();
    startDateCalendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = startDateCalendar.getTime();
    Calendar endDateCalendar = Calendar.getInstance();
    endDateCalendar.add(Calendar.DAY_OF_MONTH, 3);
    Date endDate = endDateCalendar.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(IN_REVIEW);
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    response.getValue().setSellerCode(SELLER_CODE);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
        .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, SELLER_CODE_2);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertNull(brandAuthWipDetailResponse);
  }

  @Test
  void fetchBrandAuthWipDetailsTest_internalUser() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar startDateCalendar = Calendar.getInstance();
    startDateCalendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = startDateCalendar.getTime();
    Calendar endDateCalendar = Calendar.getInstance();
    endDateCalendar.add(Calendar.DAY_OF_MONTH, 3);
    Date endDate = endDateCalendar.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(IN_REVIEW);
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    response.getValue().setSellerCode(SELLER_CODE);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
        .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, null);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertEquals(SELLER_CODE, brandAuthWipDetailResponse.getSellerCode());
  }

  @Test
  void fetchBrandAuthWipDetailsInactiveTest() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 1);
    Date startDate = calendar1.getTime();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 2);
    Date endDate = calendar1.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
        .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, SELLER_CODE);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
  }

  @Test
  void fetchBrandAuthWipDetailsNotInactiveTest() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = calendar1.getTime();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date endDate = calendar.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
        .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, SELLER_CODE);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
  }

  @Test
  void fetchBrandAuthWipDetailsExpiredTest() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> response = new GdnRestSingleResponse<>();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, -2);
    Date startDate = calendar1.getTime();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    Date endDate = calendar.getTime();
    brandAuthWipDetailResponse.setAuthStartDate(startDate);
    brandAuthWipDetailResponse.setAuthExpireDate(endDate);
    brandAuthWipDetailResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setValue(brandAuthWipDetailResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID))
        .thenReturn(response);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(SELLER_CODE))
        .thenReturn(profileResponse);
    BrandAuthWipDetailResponse brandAuthWipDetailResponse =
        brandAuthorisationWipService.fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID, SELLER_CODE);
    Mockito.verify(pcbFeign).fetchBrandAuthWipDetails(STORE_ID, IN_REVIEW, ID);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(SELLER_CODE);
    Assertions.assertEquals(BRAND_NAME, brandAuthWipDetailResponse.getBrandName());
  }

  @Test
  void brandAuthorisationWipActionTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(pcbFeign.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionRequest)).thenReturn(response);
    brandAuthorisationWipService.brandAuthorisationWipAction(STORE_ID, USERNAME,
        brandAuthorisationWipActionWebRequest);
    Mockito.verify(pcbFeign)
        .brandAuthorisationWipAction(STORE_ID, USERNAME, brandAuthorisationWipActionRequest);
  }

  @Test
  void validateBrandAuthRequestTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> response = new GdnRestSingleResponse<>();
    SimpleBooleanResponse booleanResponse = new SimpleBooleanResponse(true);
    response.setValue(booleanResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.validateBrandAuthRequest(STORE_ID, true, BRAND_CODE, SELLER_CODE))
        .thenReturn(response);
    boolean result =
        brandAuthorisationWipService.validateBrandAuthRequest(STORE_ID, BRAND_CODE, SELLER_CODE,
          true);
    Mockito.verify(pcbFeign).validateBrandAuthRequest(STORE_ID, true, BRAND_CODE, SELLER_CODE);
    Assertions.assertTrue(result);
  }

  @Test
  void createAuthFileStorageTest() {
    brandAuthCreateWipRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    brandAuthCreateWipRequest.setSellerCode(SELLER_CODE);
    brandAuthCreateWipRequest.setBrandName(BRAND_NAME);
    brandAuthCreateWipRequest.setDocumentLinks(Arrays.asList("documents"));
    brandAuthCreateWipRequest.setAuthStartDate(new Date());
    brandAuthCreateWipRequest.setAuthExpireDate(new Date());
    Mockito.when(
            pcbFeign.createBrandAuthWip(STORE_ID, REQUEST_ID, USERNAME, brandAuthCreateWipRequest))
        .thenReturn(authCreateResponseGdnRestSingleResponse);
    Mockito.when(fileStorageService.checkIfFileExisting("documents", BRAND_CODE, SELLER_CODE))
        .thenReturn(new byte[0]);
    BrandAuthCreateWipResponse response =
        brandAuthorisationWipService.createBrandAuthRequest(STORE_ID, USERNAME, REQUEST_ID,
            brandAuthCreateWipRequest);
    Mockito.verify(pcbFeign)
        .createBrandAuthWip(STORE_ID, REQUEST_ID, USERNAME, brandAuthCreateWipRequest);
    Mockito.verify(fileStorageService).checkIfFileExisting("documents", BRAND_CODE, SELLER_CODE);
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
  }

  @Test
  void createAuthIprExceededTest() {
    brandAuthCreateWipRequest.setIprRegistrationNumber(INVALID_STRING);
    brandAuthCreateWipRequest.setSellerCode(SELLER_CODE);
    brandAuthCreateWipRequest.setBrandName(BRAND_NAME);
    brandAuthCreateWipRequest.setDocumentLinks(Arrays.asList("documents"));
    brandAuthCreateWipRequest.setAuthStartDate(new Date());
    brandAuthCreateWipRequest.setAuthExpireDate(new Date());
    try {
      BrandAuthCreateWipResponse response =
          brandAuthorisationWipService.createBrandAuthRequest(STORE_ID, USERNAME, REQUEST_ID,
              brandAuthCreateWipRequest);
    } catch (Exception ignored) {}
  }

  @Test
  void createAuthWitDocDateExceptionTest() throws Exception {
    Date date = new Date();
    Date expireDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, 1);
    date = calendar.getTime();
    calendar.add(Calendar.DATE, -1);
    expireDate = calendar.getTime();
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setAuthStartDate(date);
    brandAuthCreateWipRequest.setAuthExpireDate(expireDate);
    brandAuthCreateWipRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    brandAuthCreateWipRequest.setDocumentLinks(List.of("documents"));
    try {
          brandAuthorisationWipService.createBrandAuthRequest(STORE_ID, USERNAME, REQUEST_ID,
              brandAuthCreateWipRequest);
    } catch (Exception ex) {
      Assertions.assertTrue(ex.getMessage().contains(
          ErrorMessages.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE));
    }
  }

  @Test
  void createAuthWitDocCountExceptionTest() throws Exception {
    brandAuthCreateWipRequest = generateCreateBrandAuthWipRequest();
    brandAuthCreateWipRequest.setDocumentLinks(
        Arrays.asList("document1", "document2", "document3", "document4", "document5",
            "document6"));
    try {
      brandAuthorisationWipService.createBrandAuthRequest(STORE_ID, USERNAME, REQUEST_ID,
          brandAuthCreateWipRequest);
    } catch (Exception ex) {
      Assertions.assertTrue(ex.getMessage().contains(
          ErrorMessages.MAX_FILE_ALLOWED_CROSSED));
    }
  }

  @Test
  void updateBrandAuthWipTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    this.brandAuthUpdateRequest.setIprRegistrationNumber(IPR_REGISTRATION_NUMBER);
    this.brandAuthUpdateRequest.setDocumentLinks(Collections.singletonList("documents"));
    Mockito.when(
            pcbFeign.brandAuthorisationWipUpdate(STORE_ID, USERNAME, brandAuthUpdateRequest))
        .thenReturn(response);
    brandAuthorisationWipService.updateBrandAuthWip(STORE_ID, USERNAME, brandAuthUpdateRequest);
    Mockito.verify(pcbFeign)
        .brandAuthorisationWipUpdate(STORE_ID, USERNAME, brandAuthUpdateRequest);
  }

  @Test
  void updateBrandAuthWipInvalidTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    this.brandAuthUpdateRequest.setIprRegistrationNumber(INVALID_STRING);
    this.brandAuthUpdateRequest.setDocumentLinks(Collections.singletonList("documents"));
    try {
      brandAuthorisationWipService.updateBrandAuthWip(STORE_ID, USERNAME, brandAuthUpdateRequest);
    } catch (Exception ignored) {
    }
  }

  @Test
  void filterSummaryInactiveTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 2);
    Date endDate = calendar1.getTime();
    GdnRestListResponse<BrandAuthorisationWipListResponse> response = new GdnRestListResponse<>();
    brandAuthorisationWipListResponse.setAuthStartDate(startDate);
    brandAuthorisationWipListResponse.setAuthExpireDate(endDate);
    brandAuthorisationWipListResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setSuccess(true);
    response.setContent(Collections.singletonList(brandAuthorisationWipListResponse));
    Mockito.when(pcbFeign.filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest))
        .thenReturn(response);
    brandAuthorisationWipService.getBrandAuthorisationWipList(PAGE, SIZE,
        brandAuthorisationWipListRequest);
    Mockito.verify(pcbFeign).filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest);
  }

  @Test
  void filterSummaryInReviewTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 2);
    Date endDate = calendar1.getTime();
    GdnRestListResponse<BrandAuthorisationWipListResponse> response = new GdnRestListResponse<>();
    brandAuthorisationWipListResponse.setAuthStartDate(startDate);
    brandAuthorisationWipListResponse.setAuthExpireDate(endDate);
    brandAuthorisationWipListResponse.setAuthorisationStatus(IN_REVIEW);
    response.setSuccess(true);
    response.setContent(Collections.singletonList(brandAuthorisationWipListResponse));
    Mockito.when(pcbFeign.filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest))
        .thenReturn(response);
    brandAuthorisationWipService.getBrandAuthorisationWipList(PAGE, SIZE,
        brandAuthorisationWipListRequest);
    Mockito.verify(pcbFeign).filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest);
  }

  @Test
  void filterSummaryNearExpiryTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 3);
    Date endDate = calendar1.getTime();
    GdnRestListResponse<BrandAuthorisationWipListResponse> response = new GdnRestListResponse<>();
    brandAuthorisationWipListResponse.setAuthStartDate(startDate);
    brandAuthorisationWipListResponse.setAuthExpireDate(endDate);
    brandAuthorisationWipListResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setSuccess(true);
    response.setContent(Collections.singletonList(brandAuthorisationWipListResponse));
    Mockito.when(pcbFeign.filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest))
        .thenReturn(response);
    brandAuthorisationWipService.getBrandAuthorisationWipList(PAGE, SIZE,
        brandAuthorisationWipListRequest);
    Mockito.verify(pcbFeign).filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest);
  }

  @Test
  void filterSummaryInactiveEndDateTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -2);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, -1);
    Date endDate = calendar1.getTime();
    GdnRestListResponse<BrandAuthorisationWipListResponse> response = new GdnRestListResponse<>();
    brandAuthorisationWipListResponse.setAuthStartDate(startDate);
    brandAuthorisationWipListResponse.setAuthExpireDate(endDate);
    brandAuthorisationWipListResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setSuccess(true);
    response.setContent(Collections.singletonList(brandAuthorisationWipListResponse));
    Mockito.when(pcbFeign.filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest))
        .thenReturn(response);
    brandAuthorisationWipService.getBrandAuthorisationWipList(PAGE, SIZE,
        brandAuthorisationWipListRequest);
    Mockito.verify(pcbFeign).filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest);
  }

  @Test
  void filterSummaryInactiveStartDateTest() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    Date startDate = calendar.getTime();
    Calendar calendar1 = Calendar.getInstance();
    calendar1.add(Calendar.DAY_OF_MONTH, 1);
    Date endDate = calendar1.getTime();
    GdnRestListResponse<BrandAuthorisationWipListResponse> response = new GdnRestListResponse<>();
    brandAuthorisationWipListResponse.setAuthStartDate(startDate);
    brandAuthorisationWipListResponse.setAuthExpireDate(endDate);
    brandAuthorisationWipListResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE.name());
    response.setSuccess(true);
    response.setContent(Collections.singletonList(brandAuthorisationWipListResponse));
    Mockito.when(pcbFeign.filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest))
        .thenReturn(response);
    brandAuthorisationWipService.getBrandAuthorisationWipList(PAGE, SIZE,
        brandAuthorisationWipListRequest);
    Mockito.verify(pcbFeign).filterSummary(PAGE, SIZE, brandAuthorisationWipListRequest);
  }

  @Test
  void checkEligibilityTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setValue(new SimpleBooleanResponse(true));
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(pcbFeign.creationEligibility(SELLER_CODE))
        .thenReturn(gdnRestSingleResponse);
    boolean response = brandAuthorisationWipService.checkEligibility(SELLER_CODE);
    Assertions.assertTrue(response);
    Mockito.verify(pcbFeign).creationEligibility(SELLER_CODE);
  }

  @Test
  void checkEligibilityTest_nullResponse() {
    GdnRestSingleResponse<SimpleBooleanResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(pcbFeign.creationEligibility(SELLER_CODE))
        .thenReturn(gdnRestSingleResponse);
    boolean response = brandAuthorisationWipService.checkEligibility(SELLER_CODE);
    Assertions.assertFalse(response);
    Mockito.verify(pcbFeign).creationEligibility(SELLER_CODE);
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
}
