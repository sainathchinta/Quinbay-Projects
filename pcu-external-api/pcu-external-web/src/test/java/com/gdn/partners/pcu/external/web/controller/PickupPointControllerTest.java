package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.PickupPointApiPath;
import com.gdn.partners.pcu.external.service.impl.PickupPointServiceImpl;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.PickupPointDetailWebRequest;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


public class PickupPointControllerTest extends TestHelper {

  public static final String PRODUCT_SKU = "SKU123";
  public static final String PICKUP_POINT_CODE = "pickupPointCode";
  private PickupPointDetailWebResponse
    pickupPointDetailWebResponse = new PickupPointDetailWebResponse();
  private PickupPointDetailWebRequest pickupPointDetailWebRequest = new PickupPointDetailWebRequest();
  private static final String KEYWORD = "keyword";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private final Integer PAGE = 0;
  private final Integer SIZE = 10;

  @Mock
  private PickupPointServiceImpl pickupPointService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private PickupPointController pickupPointController;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(pickupPointController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(mandatoryParameterHelper);
    verifyNoMoreInteractions(pickupPointService);
  }

  @Test
  public void getPickupPointDetailsByRequestTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(pickupPointService.fetchPickupPointDetailsByRequest(pickupPointDetailWebRequest)).thenReturn(
      Collections.singletonList(pickupPointDetailWebResponse));

    MockHttpServletRequestBuilder requestBuilder =
      post(PickupPointApiPath.BASE_PATH + PickupPointApiPath.FETCH_PICKUP_POINTS_BY_CODES).accept(
          MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(pickupPointDetailWebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(pickupPointService).fetchPickupPointDetailsByRequest(pickupPointDetailWebRequest);
  }

  @Test
  public void fetchAccessiblePickupPointsTest() throws Exception {
    pickupPointDetailWebRequest.setPickupPointCodes(new ArrayList<>());
    pickupPointDetailWebRequest.setKeyword(KEYWORD);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(pickupPointService.fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointDetailWebRequest,
        BUSINESS_PARTNER_CODE)).thenReturn(
        new PageImpl<>(Collections.singletonList(pickupPointDetailWebResponse)) {});

    MockHttpServletRequestBuilder requestBuilder =
        post(PickupPointApiPath.BASE_PATH).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(pickupPointDetailWebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(pickupPointService).fetchAccessiblePickupPoints(PAGE, SIZE, pickupPointDetailWebRequest,
        BUSINESS_PARTNER_CODE);
  }

  @Test
  public void validateDeletionByProductSkuAndPPCodeTest() throws Exception {
    String productSku = PRODUCT_SKU;
    pickupPointDetailWebRequest.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Page<PickupPointStockAndInBoundStatusWebResponse> responsePage =
      getPickupPointStockAndInBoundStatusWebResponses();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(pickupPointService.fetchStockAndInBoundStatusByProductSkuAndPPCode(BUSINESS_PARTNER_CODE,
      Constants.STORE_ID, pickupPointDetailWebRequest, productSku, PAGE, SIZE)).thenReturn(
      responsePage);
    MockHttpServletRequestBuilder requestBuilder = post(PickupPointApiPath.BASE_PATH
      + PickupPointApiPath.VALIDATE_DELETION_BY_PRODUCT_SKU_AND_PICK_UP_POINT, PRODUCT_SKU).accept(
        MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
      .content(toJson(pickupPointDetailWebRequest)).param("page", String.valueOf(PAGE))
      .param("size", String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(pickupPointService).fetchStockAndInBoundStatusByProductSkuAndPPCode(
      BUSINESS_PARTNER_CODE, Constants.STORE_ID, pickupPointDetailWebRequest, productSku, PAGE,
      SIZE);
  }


  private static Page<PickupPointStockAndInBoundStatusWebResponse> getPickupPointStockAndInBoundStatusWebResponses() {
    PickupPointStockAndInBoundStatusWebResponse pickupPointStockAndInBoundStatusWebResponse
      = new PickupPointStockAndInBoundStatusWebResponse();
    pickupPointStockAndInBoundStatusWebResponse.setProductSku(PRODUCT_SKU);
    pickupPointStockAndInBoundStatusWebResponse.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointStockAndInBoundStatusWebResponse.setPendingInBounds(true);
    pickupPointStockAndInBoundStatusWebResponse.setWarehouseStockAvailable(true);
    List<PickupPointStockAndInBoundStatusWebResponse> responseList =
      Collections.singletonList(pickupPointStockAndInBoundStatusWebResponse);
    return new PageImpl<>(responseList);
  }
}
