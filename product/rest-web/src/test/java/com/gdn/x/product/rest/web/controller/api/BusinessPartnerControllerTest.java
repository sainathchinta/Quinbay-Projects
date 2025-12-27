package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.rest.web.model.BusinessPartnerApiPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.util.ModelConverter;

public class BusinessPartnerControllerTest {

  private static final String BUSINESS_PARTNER_CODE = "bpCode1";
  private static final String MERCHANT_STATUS = "ACTIVE";
  private static final String MERCHANT_TYPE = "CM";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "request-id";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String STORE_ID = "store-id";

  @InjectMocks
  private BusinessPartnerController businessPartnerController;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ModelConverter modelConverter;

  private MockMvc mockMvc;

  private SimpleListStringRequest businessPartnerCodes;
  private List<BusinessPartner> businessPartnerList;
  private List<BusinessPartnerResponse> businessPartnerResponseList;

  @Test
  public void getBusinessPartnerDetailsTest() throws Exception {
    String bodyJson = new ObjectMapper().writeValueAsString(businessPartnerCodes);
    Mockito.when(businessPartnerService.findByStoreIdAndBusinessPartnerCodes(STORE_ID, businessPartnerCodes))
        .thenReturn(businessPartnerList);
    Mockito.when(modelConverter.convertListToResponse(businessPartnerList, BusinessPartnerResponse.class))
        .thenReturn(businessPartnerResponseList);
    this.mockMvc.perform(post(BusinessPartnerApiPath.BASE_PATH + BusinessPartnerApiPath.GET_BUSINESS_PARTNER_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", BusinessPartnerControllerTest.STORE_ID)
        .param("channelId", BusinessPartnerControllerTest.CHANNEL_ID)
        .param("clientId", BusinessPartnerControllerTest.CLIENT_ID)
        .param("requestId", BusinessPartnerControllerTest.REQUEST_ID)
        .param("username", BusinessPartnerControllerTest.USERNAME).content(bodyJson)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(this.businessPartnerService)
        .findByStoreIdAndBusinessPartnerCodes(BusinessPartnerControllerTest.STORE_ID, businessPartnerCodes);
    verify(this.modelConverter).convertListToResponse(businessPartnerList, BusinessPartnerResponse.class);
  }

  @Test
  public void getBusinessPartnerDetailsExceptionTest() throws Exception {
    String bodyJson = new ObjectMapper().writeValueAsString(businessPartnerCodes);
    Mockito.when(businessPartnerService.findByStoreIdAndBusinessPartnerCodes(STORE_ID, businessPartnerCodes))
        .thenReturn(businessPartnerList);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.modelConverter)
        .convertListToResponse(businessPartnerList, BusinessPartnerResponse.class);
    this.mockMvc.perform(post(BusinessPartnerApiPath.BASE_PATH + BusinessPartnerApiPath.GET_BUSINESS_PARTNER_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", BusinessPartnerControllerTest.STORE_ID)
        .param("channelId", BusinessPartnerControllerTest.CHANNEL_ID)
        .param("clientId", BusinessPartnerControllerTest.CLIENT_ID)
        .param("requestId", BusinessPartnerControllerTest.REQUEST_ID)
        .param("username", BusinessPartnerControllerTest.USERNAME).content(bodyJson)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
    verify(this.businessPartnerService)
        .findByStoreIdAndBusinessPartnerCodes(BusinessPartnerControllerTest.STORE_ID, businessPartnerCodes);
    verify(this.modelConverter).convertListToResponse(businessPartnerList, BusinessPartnerResponse.class);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.businessPartnerController).build();

    businessPartnerCodes = new SimpleListStringRequest();
    businessPartnerCodes.setValue(Arrays.asList(BUSINESS_PARTNER_CODE));

    businessPartnerList = new ArrayList<>();
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartner.setMerchantStatus(MERCHANT_STATUS);
    businessPartner.setMerchantType(MERCHANT_TYPE);

    businessPartnerResponseList = new ArrayList<>();
    BusinessPartnerResponse businessPartnerResponse = new BusinessPartnerResponse();
    businessPartnerResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerResponse.setMerchantStatus(MERCHANT_STATUS);
    businessPartnerResponse.setMerchantType(MERCHANT_TYPE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.businessPartnerService);
    verifyNoMoreInteractions(this.modelConverter);
  }
}
