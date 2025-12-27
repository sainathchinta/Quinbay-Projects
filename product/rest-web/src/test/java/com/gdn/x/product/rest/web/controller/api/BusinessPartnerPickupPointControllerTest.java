package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.BusinessPartnerPickupPointApiPath;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.util.ModelConverter;

public class BusinessPartnerPickupPointControllerTest {

  private static final String BUSINESS_PARTNER_CODE = "bpCode1";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "request-id";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String STORE_ID = "store-id";
  private static final String OTHER_BP_PP = "pickupPointCode2";
  private static final String MERCHANT_CODE_1 = "merchantCode1";
  private static final String MERCHANT_CODE_2 = "merchantCode2";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper = new ObjectMapper();
  private SimpleListStringRequest businessPartnerPickupPointCodes;
  private PickupPointSummaryRequest pickupPointSummaryRequest;

  @InjectMocks
  private BusinessPartnerPickupPointController businessPartnerPickupPointController;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private ModelConverter modelConverter;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.businessPartnerPickupPointController).build();
    businessPartnerPickupPointCodes = new SimpleListStringRequest();
    businessPartnerPickupPointCodes.setValue(Arrays.asList(BUSINESS_PARTNER_CODE, OTHER_BP_PP));
    pickupPointSummaryRequest = new PickupPointSummaryRequest();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerPickupPointService);
    Mockito.verifyNoMoreInteractions(modelConverter);
  }

  @Test
  public void getBusinessPartnerPickupPointDetailsTest() throws Exception {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode(MERCHANT_CODE_1);
    BusinessPartnerPickupPoint businessPartnerPickupPoint2 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint2.setBusinessPartnerCode(MERCHANT_CODE_2);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
            Arrays.asList(BUSINESS_PARTNER_CODE, OTHER_BP_PP)))
        .thenReturn(Arrays.asList(businessPartnerPickupPoint2, businessPartnerPickupPoint));
    ReflectionTestUtils.setField(businessPartnerPickupPointController, "validateBusinessPartnerCodeForSecurity", true);
    String bodyJson = objectMapper.writeValueAsString(businessPartnerPickupPointCodes);
    this.mockMvc.perform(post(BusinessPartnerPickupPointApiPath.BASE_PATH
            + BusinessPartnerPickupPointApiPath.GET_PICKUP_POINT_DETAILS_BY_PICKUP_POINT_CODES).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", BusinessPartnerPickupPointControllerTest.STORE_ID)
            .param("channelId", BusinessPartnerPickupPointControllerTest.CHANNEL_ID)
            .param("clientId", BusinessPartnerPickupPointControllerTest.CLIENT_ID)
            .param("requestId", BusinessPartnerPickupPointControllerTest.REQUEST_ID)
            .param("username", BusinessPartnerPickupPointControllerTest.USERNAME)
            .param("merchantCode", BusinessPartnerPickupPointControllerTest.MERCHANT_CODE_1).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true))
        .andExpect(MockMvcResultMatchers.jsonPath("$.content.length()").value(0));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, businessPartnerPickupPointCodes.getValue());
    Mockito.verify(modelConverter).convertListToResponse(Mockito.anyList(), Mockito.any());
  }

  @Test
  public void getBusinessPartnerPickupPointDetailsExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(modelConverter)
        .convertListToResponse(new ArrayList<>(), BusinessPartnerPickupPointResponse.class);
    String bodyJson = objectMapper.writeValueAsString(businessPartnerPickupPointCodes);
    this.mockMvc.perform(post(BusinessPartnerPickupPointApiPath.BASE_PATH
            + BusinessPartnerPickupPointApiPath.GET_PICKUP_POINT_DETAILS_BY_PICKUP_POINT_CODES)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", BusinessPartnerPickupPointControllerTest.STORE_ID)
            .param("channelId", BusinessPartnerPickupPointControllerTest.CHANNEL_ID)
            .param("clientId", BusinessPartnerPickupPointControllerTest.CLIENT_ID)
            .param("requestId", BusinessPartnerPickupPointControllerTest.REQUEST_ID)
            .param("username", BusinessPartnerPickupPointControllerTest.USERNAME).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, businessPartnerPickupPointCodes.getValue());
    Mockito.verify(modelConverter).convertListToResponse(new ArrayList<>(), BusinessPartnerPickupPointResponse.class);
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryTest() throws Exception {
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
        pickupPointSummaryRequest)).thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(BusinessPartnerPickupPointApiPath.BASE_PATH
            + BusinessPartnerPickupPointApiPath.GET_PICKUP_POINT_DETAILS_SUMMARY).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", BusinessPartnerPickupPointControllerTest.STORE_ID)
            .param("channelId", BusinessPartnerPickupPointControllerTest.CHANNEL_ID)
            .param("clientId", BusinessPartnerPickupPointControllerTest.CLIENT_ID)
            .param("requestId", BusinessPartnerPickupPointControllerTest.REQUEST_ID)
            .param("username", BusinessPartnerPickupPointControllerTest.USERNAME).param("page", "0").param("size", "1")
            .content(objectMapper.writeValueAsString(pickupPointSummaryRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1, pickupPointSummaryRequest);
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryExceptionTest() throws Exception {
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
        pickupPointSummaryRequest)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(BusinessPartnerPickupPointApiPath.BASE_PATH
            + BusinessPartnerPickupPointApiPath.GET_PICKUP_POINT_DETAILS_SUMMARY).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", BusinessPartnerPickupPointControllerTest.STORE_ID)
            .param("channelId", BusinessPartnerPickupPointControllerTest.CHANNEL_ID)
            .param("clientId", BusinessPartnerPickupPointControllerTest.CLIENT_ID)
            .param("requestId", BusinessPartnerPickupPointControllerTest.REQUEST_ID)
            .param("username", BusinessPartnerPickupPointControllerTest.USERNAME).param("page", "0").param("size", "1")
            .content(objectMapper.writeValueAsString(pickupPointSummaryRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1, pickupPointSummaryRequest);
  }
}