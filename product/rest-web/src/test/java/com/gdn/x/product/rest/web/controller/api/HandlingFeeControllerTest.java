package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.rest.web.model.ApiPath;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.request.HandlingFeeRequestRestWeb;
import com.gdn.x.product.rest.web.model.response.HandlingFeeResponseRestWeb;
import com.gdn.x.product.service.api.HandlingFeeService;
import com.gdn.x.product.service.util.ModelConverter;

public class HandlingFeeControllerTest {

  private static final String DESCRIPTION = "description";
  private static final String VALUE = "value";
  private static final String VARIABLE = "variable";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "request-id";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String STORE_ID = "store-id";

  private static final String PATH_FILE_UPDATE_SYSTEM_PARAMETER_JSON =
      "src/test/resources/updateSystemParameter.json";
  private static final String PATH_FILE_CALCULATE_HANDLING_FEE_JSON =
      "src/test/resources/calculateHandlingFee.json";
  private static final String ITEM_SKU = "item-sku";
  private static final int QUANTITY = 2;

  @InjectMocks
  private HandlingFeeController handlingFeeController;

  @Mock
  private HandlingFeeService handlingFeeService;

  @Mock
  private ModelConverter modelConverter;

  private MockMvc mockMvc;

  private String updateSystemParameter;
  private String handlingFeeRequestString;

  private SystemParameter systemParameter;
  private SystemParameterRequest systemParameterRequest;
  private List<HandlingFeeRequestRestWeb> handlingFeeRequestRestWebList;

  private HandlingFeeResponse handlingFeeResponse;

  @Captor
  private ArgumentCaptor<List<HandlingFeeRequest>> handlingFeeRequestCaptor;
  private List<HandlingFeeRequest> handlingFeeRequestList;
  private HandlingFeeRequest handlingFeeRequest;
  private HandlingFeeResponseRestWeb handlingFeeResponseRestWeb;
  private SystemParameterResponse systemParameterResponse;

  @Test
  public void calculateHandlingFeeTest() throws Exception {
    this.mockMvc.perform(
        post(ApiPath.HANDLING_FEE_PATH + ApiPath.HANDLING_FEE_CALCULATE_HANDLING_FEE).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.handlingFeeRequestString).param("storeId", HandlingFeeControllerTest.STORE_ID)
          .param("channelId", HandlingFeeControllerTest.CHANNEL_ID)
          .param("clientId", HandlingFeeControllerTest.CLIENT_ID)
          .param("requestId", HandlingFeeControllerTest.REQUEST_ID)
          .param("username", HandlingFeeControllerTest.USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.handlingFeeService).calculateHandlingFee(eq(HandlingFeeControllerTest.STORE_ID),
        this.handlingFeeRequestCaptor.capture());

    verify(this.modelConverter).convertToHandlingFeeRequestList(this.handlingFeeRequestRestWebList);

    verify(this.modelConverter).convertToHandlingFeeResponseRestWeb(this.handlingFeeResponse);

    List<HandlingFeeRequest> handlingFeeRequest = this.handlingFeeRequestCaptor.getValue();
    Assertions.assertNotNull(handlingFeeRequest.get(0));
    Assertions.assertEquals(handlingFeeRequest.get(0).getItemSku(),(ITEM_SKU));
    Assertions.assertEquals(handlingFeeRequest.get(0).getQuantity(), (2));
  }

  @Test
  public void getAllSettingOfHandlingFeeTest() throws Exception {
    this.mockMvc.perform(
        post(ApiPath.HANDLING_FEE_PATH + ApiPath.HANDLING_FEE_GET_SETTING_OF_HANDLING_FEE).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", HandlingFeeControllerTest.STORE_ID)
          .param("channelId", HandlingFeeControllerTest.CHANNEL_ID)
          .param("clientId", HandlingFeeControllerTest.CLIENT_ID)
          .param("requestId", HandlingFeeControllerTest.REQUEST_ID)
          .param("username", HandlingFeeControllerTest.USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.value.variable", equalTo(HandlingFeeControllerTest.VARIABLE)))
      .andExpect(jsonPath("$.value.value", equalTo(HandlingFeeControllerTest.VALUE)))
      .andExpect(jsonPath("$.value.description", equalTo(HandlingFeeControllerTest.DESCRIPTION)));

    verify(this.handlingFeeService).getAllSettingOfHandlingFee(HandlingFeeControllerTest.STORE_ID);

    verify(this.modelConverter).convertToSystemParameterResponse(this.systemParameter);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.handlingFeeController).build();

    ObjectMapper mapper = new ObjectMapper();

    this.updateSystemParameter =
        FileUtils.readFileToString(new File(
            HandlingFeeControllerTest.PATH_FILE_UPDATE_SYSTEM_PARAMETER_JSON));

    this.systemParameterRequest =
        mapper.readValue(this.updateSystemParameter,
            mapper.getTypeFactory().constructType(SystemParameterRequest.class));
    Assertions.assertNotNull(this.systemParameterRequest);

    this.handlingFeeRequestString =
        FileUtils.readFileToString(new File(
            HandlingFeeControllerTest.PATH_FILE_CALCULATE_HANDLING_FEE_JSON));

    this.handlingFeeRequestRestWebList =
        mapper.readValue(this.handlingFeeRequestString, mapper.getTypeFactory()
            .constructCollectionType(List.class, HandlingFeeRequestRestWeb.class));
    Assertions.assertNotNull(this.handlingFeeRequestRestWebList);

    this.systemParameter = new SystemParameter();
    this.systemParameter.setVariable(HandlingFeeControllerTest.VARIABLE);
    this.systemParameter.setValue(HandlingFeeControllerTest.VALUE);
    this.systemParameter.setDescription(HandlingFeeControllerTest.DESCRIPTION);
    this.systemParameter.setStoreId(HandlingFeeControllerTest.STORE_ID);

    when(this.handlingFeeService.getAllSettingOfHandlingFee(HandlingFeeControllerTest.STORE_ID))
        .thenReturn(this.systemParameter);

    this.handlingFeeResponse = new HandlingFeeResponse(new BigDecimal(50000));

    this.handlingFeeRequestList = new ArrayList<HandlingFeeRequest>();
    this.handlingFeeRequest = new HandlingFeeRequest();
    this.handlingFeeRequest.setItemSku(HandlingFeeControllerTest.ITEM_SKU);
    this.handlingFeeRequest.setQuantity(HandlingFeeControllerTest.QUANTITY);
    this.handlingFeeRequestList.add(this.handlingFeeRequest);

    this.handlingFeeResponseRestWeb = new HandlingFeeResponseRestWeb();
    this.handlingFeeResponseRestWeb.setTotalHandlingFee(this.handlingFeeResponse
        .getTotalHandlingFee());

    this.systemParameterResponse = new SystemParameterResponse();
    this.systemParameterResponse.setVariable(this.systemParameter.getVariable());
    this.systemParameterResponse.setValue(this.systemParameter.getValue());
    this.systemParameterResponse.setDescription(this.systemParameter.getDescription());
    this.systemParameterResponse.setStoreId(this.systemParameter.getStoreId());


    when(
        this.handlingFeeService.calculateHandlingFee(eq(HandlingFeeControllerTest.STORE_ID),
            this.handlingFeeRequestCaptor.capture())).thenReturn(this.handlingFeeResponse);

    when(this.modelConverter.convertToHandlingFeeRequestList(this.handlingFeeRequestRestWebList))
        .thenReturn(this.handlingFeeRequestList);

    when(this.modelConverter.convertToHandlingFeeResponseRestWeb(this.handlingFeeResponse))
        .thenReturn(this.handlingFeeResponseRestWeb);

    when(this.modelConverter.convertToSystemParameterResponse(this.systemParameter)).thenReturn(
        this.systemParameterResponse);

    when(
        this.modelConverter.convertToSystemParameter(this.systemParameterRequest,
            this.systemParameter)).thenReturn(this.systemParameter);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.handlingFeeService);
    verifyNoMoreInteractions(this.modelConverter);
  }

  @Test
  public void updateAllSettingOfHandlingFeeTest() throws Exception {
    this.mockMvc.perform(
        post(ApiPath.HANDLING_FEE_PATH + ApiPath.HANDLING_FEE_UPDATE_SETTING_OF_HANDLING_FEE).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.updateSystemParameter).param("storeId", HandlingFeeControllerTest.STORE_ID)
          .param("channelId", HandlingFeeControllerTest.CHANNEL_ID)
          .param("clientId", HandlingFeeControllerTest.CLIENT_ID)
          .param("requestId", HandlingFeeControllerTest.REQUEST_ID)
          .param("username", HandlingFeeControllerTest.USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));

    this.systemParameter = new SystemParameter();
    this.systemParameter.setStoreId(HandlingFeeControllerTest.STORE_ID);

    verify(this.modelConverter).convertToSystemParameter(this.systemParameterRequest,
        this.systemParameter);

    verify(this.handlingFeeService).updateAllSettingOfHandlingFee(this.systemParameter);
  }

}
