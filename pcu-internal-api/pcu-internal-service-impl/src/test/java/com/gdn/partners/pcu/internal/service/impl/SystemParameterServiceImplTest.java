package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductSystemParameterResponse;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.gdn.partners.pcu.internal.model.Constants.CHANNEL_ID;
import static com.gdn.partners.pcu.internal.model.Constants.CLIENT_ID;
import static com.gdn.partners.pcu.internal.model.Constants.REQUEST_ID;
import static com.gdn.partners.pcu.internal.model.Constants.STORE_ID;
import static com.gdn.partners.pcu.internal.model.Constants.USER_NAME;

@ExtendWith(MockitoExtension.class)
class SystemParameterServiceImplTest {

  private static final String VALUE = "value";
  private static final String VARIABLE = "variable";
  private static final String DESCRIPTION = "description";

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PDTFeign pdtFeign;

  @InjectMocks
  private SystemParameterServiceImpl systemParameterService;


  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(pbpFeign, pdtFeign);
  }


  @Test
  void updateSystemParameterTest() {
    List<SystemParameterRequest> systemParameterRequests = new ArrayList<>();
    SystemParameterRequest productSystemParameterRequest = new SystemParameterRequest();
    systemParameterRequests.add(productSystemParameterRequest);
    Mockito.when(pbpFeign.updateSystemParameter(STORE_ID, CHANNEL_ID,
            CLIENT_ID, REQUEST_ID, USER_NAME, productSystemParameterRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    systemParameterService.updateSystemParameter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USER_NAME, systemParameterRequests);
    Mockito.verify(pbpFeign).updateSystemParameter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USER_NAME, productSystemParameterRequest);
  }

  @Test
  void fetchSystemParameterTest() {
    ReflectionTestUtils.setField(systemParameterService, "valuesEnglishMap",
      new HashMap<>());
    ReflectionTestUtils.setField(systemParameterService, "valuesIndonesiaMap",
      new HashMap<>());
    List<ProductSystemParameterResponse> productSystemParameterResponses = new ArrayList<>();
    ProductSystemParameterResponse productSystemParameterResponse = new ProductSystemParameterResponse();
    productSystemParameterResponse.setDescription(DESCRIPTION);
    productSystemParameterResponse.setValue(VALUE);
    productSystemParameterResponse.setVariable(VARIABLE);
    productSystemParameterResponses.add(productSystemParameterResponse);
    Mockito.when(pbpFeign.fetchSystemParameterShowOnUI(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID))
        .thenReturn(new GdnRestListResponse<>(productSystemParameterResponses,
            new PageMetaData(productSystemParameterResponses.size(), 0,
                productSystemParameterResponses.size()), REQUEST_ID));
    systemParameterService.fetchSystemParameterShowOnUI(STORE_ID, CHANNEL_ID, CLIENT_ID,
        REQUEST_ID);
    Mockito.verify(pbpFeign)
        .fetchSystemParameterShowOnUI(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);
  }

  @Test
  void getSystemParameterSwitchesTest() {
    Map<String, String> switchMap = new HashMap<>();
    switchMap.put(STORE_ID, Boolean.TRUE.toString());
    Mockito.when(pdtFeign.fetchInternalSystemParameter(STORE_ID, REQUEST_ID))
      .thenReturn(new GdnRestSingleResponse<>(new SystemParameterResponse(switchMap), REQUEST_ID));
    SystemParameterResponse systemParameterResponse =
      systemParameterService.getSystemParameterSwitches(STORE_ID, REQUEST_ID);
    Assertions.assertEquals(
      systemParameterResponse.getProductSystemParameterSwitchValues().get(STORE_ID),
      Boolean.TRUE.toString());
  }
}
