package com.gdn.partners.pcu.external.service.impl;

import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.BPServiceFeign;
import com.gdn.partners.pcu.external.client.feign.PickupPointFeign;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductItemBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.BPService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.web.model.request.PickupPointDetailWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.response.L3AndPickupPointStockAvailabilityResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.gdn.partners.pcu.external.model.Constants.CHANNEL_ID;
import static com.gdn.partners.pcu.external.model.Constants.CLIENT_ID;
import static com.gdn.partners.pcu.external.model.Constants.CODE;
import static com.gdn.partners.pcu.external.model.Constants.REQUEST_ID;
import static com.gdn.partners.pcu.external.model.Constants.STORE_ID;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Created by govind on 20/12/2018 AD.
 */
public class PickupPointServiceImplTest {

  private static final String PP_CODE = "123";
  private static final String BP_CODE = "123";
  private static final String USERNAME = "username";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PP_NAME = "ppName";
  private static final String KEYWORD = "keyword";
  private static final String BUSINESS_PARTNER_CODE = "bussinessPartnerCode";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;

  private static final List<String> PP_CODES = Arrays.asList("PP1", "PP2", "PP3");
  public static final String PP_1234 = "PP-1234";

  private PickupPointDetailWebRequest pickupPointDetailWebRequest;

  private final List<String> PP_CODE_LIST = Collections.singletonList(PP_CODE);
  private PickupPointDetailResponse pickupPointDetailResponse;
  private PickupPointResponse pickupPointResponse;

  @Mock
  private BPServiceFeign bpServiceFeign;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private PickupPointFeign pickupPointFeign;

  @Mock
  private BPService bpService;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private UserPicService userPicService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private XInventoryFeign xInventoryFeign;

  @InjectMocks
  private PickupPointServiceImpl pickupPointService;


  private GdnBaseRestResponse gdnBaseRestResponse;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(Boolean.TRUE);
    pickupPointDetailResponse =
      PickupPointDetailResponse.builder().pickupPointCode(PP_CODE).pickupPointName(PP_NAME).build();
    pickupPointResponse = PickupPointResponse.builder().code(PP_CODE).name(PP_NAME).build();
    pickupPointDetailWebRequest =
      PickupPointDetailWebRequest.builder().pickupPointCodes(PP_CODE_LIST)
        .build();
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(bpServiceFeign);
    verifyNoMoreInteractions(xProductFeign);
    verifyNoMoreInteractions(userPicService, xInventoryFeign, mandatoryParameterHelper);
  }

  @Test
  public void testValidateAndSaveDefaultPickupPointForProductCreationRequest_Success()
      throws Exception {
    ProductCreationRequest request = generateProductCreationRequest();
    Mockito.when(pickupPointFeign.markDefaultAddress(request.getBusinessPartnerCode(),
        request.getProductItemRequests().get(0).getPickupPointId()))
        .thenReturn(gdnBaseRestResponse);
    pickupPointService.validateAndSaveDefaultPickupPoint(USERNAME, request);
    Mockito.verify(pickupPointFeign).markDefaultAddress(request.getBusinessPartnerCode(),
        request.getProductItemRequests().get(0).getPickupPointId());
  }

  @Test
  public void testValidateAndSaveDefaultPickupPointForProductCreationRequest_whenClientException()
      throws Exception {
    ProductCreationRequest request = generateProductCreationRequest();
    Mockito.when(pickupPointFeign.markDefaultAddress(request.getBusinessPartnerCode(),
        request.getProductItemRequests().get(0).getPickupPointId())).thenReturn(null);
    try {
      pickupPointService.validateAndSaveDefaultPickupPoint(USERNAME, request);
    }catch(ClientException ex) {
      Mockito.verify(pickupPointFeign).markDefaultAddress(request.getBusinessPartnerCode(),
          request.getProductItemRequests().get(0).getPickupPointId());
    }
  }

  @Test
  public void testValidateAndSaveDefaultPickupPointForProductCreationRequest_emptyItemRequest()
      throws Exception {
    ProductCreationRequest request = generateProductCreationRequest();
    request.setBusinessPartnerCode(BP_CODE);
    request.setProductItemRequests(new ArrayList<>());
    pickupPointService.validateAndSaveDefaultPickupPoint(USERNAME, request);
  }

  @Test
  public void testValidateAndSaveDefaultPickupPointForProductCreationRequest_markDefaultAddressFalse()
      throws Exception {
    ProductCreationRequest request = generateProductCreationRequest();
    request.getProductItemRequests().get(0).setMarkDefaultAddress(false);
    request.setBusinessPartnerCode(BP_CODE);
    pickupPointService.validateAndSaveDefaultPickupPoint(USERNAME, request);
  }

  @Test
  public void testValidateAndSaveDefaultPickupPoint_emptyItemRequest()
      throws Exception {
    ProductBusinessPartnerServiceRequest request = generateProductBusinessPartner();
    request.setProductItemBusinessPartners(new ArrayList<>());
    request.setBusinessPartnerCode(BP_CODE);
    pickupPointService.validateAndSaveDefaultPickupPoint(request);
  }

  @Test
  public void testValidateAndSaveDefaultPickupPoint_markDefaultAddressFalse()
      throws Exception {
    ProductBusinessPartnerServiceRequest request = generateProductBusinessPartner();
    request.getProductItemBusinessPartners().get(0).setMarkDefaultAddress(false);
    request.setBusinessPartnerCode(BP_CODE);
    pickupPointService.validateAndSaveDefaultPickupPoint(request);
  }

  @Test
  public void getPickupPointSummaryFilterCreationTest() throws Exception {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    Mockito.when(businessPartnerService.filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(null)).thenReturn(new HashSet<>());
    pickupPointService.getPickupPointSummaryFilter("10001", 0, 1, pickupPointSummaryWebRequest);
    Mockito.verify(businessPartnerService).filterBusinessPartner(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(null);
    Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(pickupPointSummaryWebRequest.getPickupPointCodes(), null);
  }

  @Test
  public void getPickupPointSummaryFilterEditTest() throws Exception {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(xProductFeign.getBusinessPartnerPickupPointSummary(Mockito.eq(0), Mockito.eq(1),
        Mockito.any(PickupPointSummaryRequest.class)))
      .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(null)).thenReturn(new HashSet<>());
    pickupPointService.getPickupPointSummaryFilter("10001", 0, 1, pickupPointSummaryWebRequest);
    Mockito.verify(xProductFeign).getBusinessPartnerPickupPointSummary(Mockito.eq(0), Mockito.eq(1),
      Mockito.any(PickupPointSummaryRequest.class));
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(null);
  }
  @Test
  public void getPickupPointSummaryFilterEditShouldRestrictAccessTrueTest() throws Exception {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(xProductFeign.getBusinessPartnerPickupPointSummary(Mockito.eq(0), Mockito.eq(1),
        Mockito.any(PickupPointSummaryRequest.class)))
      .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(null)).thenReturn(new HashSet<>());
    pickupPointService.getPickupPointSummaryFilter("10001", 0, 1, pickupPointSummaryWebRequest);
    Mockito.verify(xProductFeign).getBusinessPartnerPickupPointSummary(Mockito.eq(0), Mockito.eq(1),
      Mockito.any(PickupPointSummaryRequest.class));
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(null);
  }

  @Test
  public void fetchPickupPointDetailsByRequestTest() {
    SimpleListStringRequest simpleListStringRequest =
      new SimpleListStringRequest(PP_CODE_LIST);
    Mockito.when(this.xProductFeign.getPickupPointDetailByCodes(simpleListStringRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(pickupPointDetailResponse), null, null));
    List<PickupPointDetailWebResponse> response =
      pickupPointService.fetchPickupPointDetailsByRequest(pickupPointDetailWebRequest);
    Mockito.verify(this.xProductFeign).getPickupPointDetailByCodes(simpleListStringRequest);
    Assertions.assertEquals(PP_NAME, response.get(0).getPickupPointName());
    Assertions.assertEquals(PP_CODE, response.get(0).getPickupPointCode());
  }

  @Test
  public void fetchAccessiblePickupPointDetailsTest() {
    pickupPointDetailWebRequest.setKeyword(KEYWORD);
    pickupPointDetailWebRequest.setPickupPointCodes(PP_CODE_LIST);
    PickupPointFilterRequest pickupPointFilterRequest = PickupPointFilterRequest.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .codes(new HashSet<>(pickupPointDetailWebRequest.getPickupPointCodes()))
        .name(pickupPointDetailWebRequest.getKeyword())
        .sortedBy(CODE)
        .sortDirection(Sort.Direction.DESC.name())
        .build();
    Mockito.when(this.pickupPointFeign.fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointFilterRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            Collections.singletonList(pickupPointResponse), new PageMetaData(SIZE, PAGE, 1), null));
    Page<PickupPointDetailWebResponse> response =
        pickupPointService.fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointDetailWebRequest,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pickupPointFeign).fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointFilterRequest);
  }

  @Test
  public void fetchAccessiblePickupPointDetailsTest_emptyRecords() {
    pickupPointDetailWebRequest.setKeyword(KEYWORD);
    pickupPointDetailWebRequest.setPickupPointCodes(new ArrayList<>());
    PickupPointFilterRequest pickupPointFilterRequest = PickupPointFilterRequest.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .codes(new HashSet<>(pickupPointDetailWebRequest.getPickupPointCodes()))
        .name(pickupPointDetailWebRequest.getKeyword())
        .sortedBy(CODE)
        .sortDirection(Sort.Direction.DESC.name())
        .build();
    Mockito.when(this.pickupPointFeign.fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointFilterRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            new ArrayList<>(), new PageMetaData(0, PAGE, 0), null));
    Page<PickupPointDetailWebResponse> response =
        pickupPointService.fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointDetailWebRequest,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pickupPointFeign).fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointFilterRequest);
  }


  private ProductBusinessPartnerServiceRequest generateProductBusinessPartner() {
    ProductBusinessPartnerServiceRequest request = new ProductBusinessPartnerServiceRequest();
    ProductItemBusinessPartnerServiceRequest itemBusinessPartnerRequest =
        new ProductItemBusinessPartnerServiceRequest();
    itemBusinessPartnerRequest.setPickupPointId(PP_CODE);
    itemBusinessPartnerRequest.setMarkDefaultAddress(true);
    request.setProductItemBusinessPartners(Arrays.asList(itemBusinessPartnerRequest));
    return request;
  }

  private ProductCreationRequest generateProductCreationRequest() {
    ProductCreationRequest request = new ProductCreationRequest();
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setPickupPointId(PP_CODE);
    productItemCreationRequest.setMarkDefaultAddress(true);
    request.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    request.setBusinessPartnerCode(BP_CODE);
    return request;
  }

  @Test
  public void fetchStockAndInBoundStatusByProductSkuAndPPCode_SuccessfulResponse() {
    ReflectionTestUtils.setField(pickupPointService, "maxFetchSizeForStockStatusByPPCode", 10);
    GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    L3AndPickupPointStockAvailabilityResponse stockResponse = new L3AndPickupPointStockAvailabilityResponse();
    response.setValue(stockResponse);

    Mockito.when(xInventoryFeign.getStockAvailabilityByL3AndPickupPoint(
      Constants.STORE_ID, REQUEST_ID,
      CHANNEL_ID, CLIENT_ID, USERNAME, PRODUCT_SKU, PP_CODE)).thenReturn(response);

    Page<PickupPointStockAndInBoundStatusWebResponse> result = pickupPointService.fetchStockAndInBoundStatusByProductSkuAndPPCode(
      BUSINESS_PARTNER_CODE, STORE_ID, pickupPointDetailWebRequest, PRODUCT_SKU, PAGE, SIZE);

    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.getContent().isEmpty());
    Mockito.verify(xInventoryFeign, Mockito.times(PP_CODE_LIST.size()))
      .getStockAvailabilityByL3AndPickupPoint(Constants.STORE_ID, REQUEST_ID,
        CHANNEL_ID, CLIENT_ID, USERNAME, PRODUCT_SKU, PP_CODE);
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
    Mockito.verify(mandatoryParameterHelper).getChannelId();
    Mockito.verify(mandatoryParameterHelper).getUsername();


  }

  @Test
  public void fetchStockAndInBoundStatusByProductSkuAndPPCode_FailedResponse_ShouldSkip() {
    PickupPointSummaryWebRequest pointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pointSummaryWebRequest.setPickupPointCodes(Set.of(PP_CODE, PP_1234, "pp123"));
    ReflectionTestUtils.setField(pickupPointService, "maxFetchSizeForStockStatusByPPCode", 1);
    GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    L3AndPickupPointStockAvailabilityResponse stockResponse = new L3AndPickupPointStockAvailabilityResponse();
    response.setValue(stockResponse);
    Mockito.when(
        xInventoryFeign.getStockAvailabilityByL3AndPickupPoint(Constants.STORE_ID, REQUEST_ID,
          CHANNEL_ID, CLIENT_ID, USERNAME, PRODUCT_SKU, PP_1234))
      .thenThrow(new ClientException("API Failure"));
    Mockito.when(xInventoryFeign.getStockAvailabilityByL3AndPickupPoint(
      Constants.STORE_ID, REQUEST_ID,
      CHANNEL_ID, CLIENT_ID, USERNAME, PRODUCT_SKU, PP_CODE)).thenReturn(response);
    Page<PickupPointStockAndInBoundStatusWebResponse> result = null;
    try {
      result =
        pickupPointService.fetchStockAndInBoundStatusByProductSkuAndPPCode(BUSINESS_PARTNER_CODE,
          STORE_ID, pickupPointDetailWebRequest, PRODUCT_SKU, PAGE, SIZE);
    }
    finally {
      Assertions.assertNotNull(result);
      Assertions.assertFalse(result.getContent().isEmpty());
      Mockito.verify(xInventoryFeign)
        .getStockAvailabilityByL3AndPickupPoint(Constants.STORE_ID, REQUEST_ID,
          CHANNEL_ID, CLIENT_ID, USERNAME, PRODUCT_SKU, PP_CODE);
      Mockito.verify(mandatoryParameterHelper).getClientId();
      Mockito.verify(mandatoryParameterHelper).getRequestId();
      Mockito.verify(mandatoryParameterHelper).getChannelId();
      Mockito.verify(mandatoryParameterHelper).getUsername();
    }
  }

  @Test
  public void fetchStockAndInBoundStatusByProductSkuAndPPCode_LimitLessThanPickupPointCodes() {
    // Set maxFetchSizeForStockStatusByPPCode to a smaller value than PP_CODE_LIST size
    ReflectionTestUtils.setField(pickupPointService, "maxFetchSizeForStockStatusByPPCode", 1);

    // Ensure there are more pickup point codes than maxFetchSizeForStockStatusByPPCode
    List<String> ppCodes = Arrays.asList(PP_CODE, PP_1234, "pp123");
    PickupPointDetailWebRequest request = new PickupPointDetailWebRequest();
    request.setPickupPointCodes(ppCodes);

    GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    L3AndPickupPointStockAvailabilityResponse stockResponse = new L3AndPickupPointStockAvailabilityResponse();
    response.setValue(stockResponse);

    Mockito.when(xInventoryFeign.getStockAvailabilityByL3AndPickupPoint(
      anyString(), anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(response);

    Page<PickupPointStockAndInBoundStatusWebResponse> result =
      pickupPointService.fetchStockAndInBoundStatusByProductSkuAndPPCode(
        BUSINESS_PARTNER_CODE,
        STORE_ID,
        request,
        PRODUCT_SKU,
        PAGE,
        SIZE);
    Assertions.assertNotNull(result);
    Mockito.verify(xInventoryFeign).getStockAvailabilityByL3AndPickupPoint(
      anyString(), anyString(), anyString(), anyString(), anyString(), anyString(), anyString());
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
    Mockito.verify(mandatoryParameterHelper).getChannelId();
    Mockito.verify(mandatoryParameterHelper).getUsername();
  }

  @Test
  public void fetchStockAndInBoundStatusByProductSkuAndPPCode_InvalidValidation() {

    ReflectionTestUtils.setField(pickupPointService,"maxFetchSizeForStockStatusByPPCode",10);

    List<String>ppCodes=Arrays.asList(PP_CODE);
    PickupPointDetailWebRequest request=new PickupPointDetailWebRequest();
    request.setPickupPointCodes(ppCodes);
    GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    Mockito.when(xInventoryFeign.getStockAvailabilityByL3AndPickupPoint(
      anyString(), anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(response);
    Page<PickupPointStockAndInBoundStatusWebResponse> result =
      pickupPointService.fetchStockAndInBoundStatusByProductSkuAndPPCode(
        BUSINESS_PARTNER_CODE,
        STORE_ID,
        request,
        PRODUCT_SKU,
        PAGE,
        SIZE);
    Mockito.verify(xInventoryFeign).getStockAvailabilityByL3AndPickupPoint(
      anyString(), anyString(), anyString(), anyString(), anyString(), anyString(), anyString());
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
    Mockito.verify(mandatoryParameterHelper).getChannelId();
    Mockito.verify(mandatoryParameterHelper).getUsername();

  }

}
