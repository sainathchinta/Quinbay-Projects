package com.gdn.partners.pcu.master.web.controller;


import static com.gdn.partners.pcu.master.model.Constants.USER_NAME;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.partners.pcu.master.web.model.request.DimensionMappingDTO;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.AttributeApiPath;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.attribute.AttributeFilterRequest;
import com.gdn.partners.pcu.master.model.attribute.AttributeValue;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.service.AttributeService;
import com.gdn.partners.pcu.master.web.helper.TestHelper;
import com.gdn.partners.pcu.master.web.model.AttributeValueUpdateWebModel;
import com.gdn.partners.pcu.master.web.model.request.AllowedAttributeValueDTO;
import com.gdn.partners.pcu.master.web.model.request.AttributeSortTypeDTO;
import com.gdn.partners.pcu.master.web.model.request.AttributeTypeDTO;
import com.gdn.partners.pcu.master.web.model.request.AttributeValueAddWebRequest;
import com.gdn.partners.pcu.master.web.model.request.AttributeValuesUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.partners.pcu.master.web.model.request.PredefinedAllowedAttributeValueDTO;
import com.gdn.partners.pcu.master.web.model.response.AttributeDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValuesWebResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.partners.pcu.master.web.controller.util.ConverterUtil;

@ExtendWith(MockitoExtension.class)
public class AttributeControllerTest extends TestHelper {

  private static final String ATTRIBUTE_VALUES_API_PATH = "/values";
  private static final String ALL_ATTRIBUTE_VALUES_API_PATH = "/allValues";
  private static final String SEPERATOR = "/";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String ATTRIBUTE_CODE = "ATT-";
  private static final String PREDEFINED_ATTRIBUTE_CODE = "predefinedAttributeCode";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final Integer TOTAL_ELEMENTS = 1;
  private static final String VALUE = "value";
  private static final String VALUE_TYPE = "valueType";
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String ID = "id";
  private static final String NAME = "name";
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String NAME_ENGLISH = "name-english";
  private static final String DESCRIPTION_ENGLISH = "description-english";
  private static final String ATTRIBUTE_VALUE_UPDATE_ID_1 = "id1";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_1 = "value1";
  private static final String ATTRIBUTE_VALUE_UPDATE_CODE_1 = "code2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1 = 1;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_2 = "value2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2 = 2;
  private static final String ATTRIBUTE_VALUE_UPDATE_ID_3 = "id3";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_3 = "value3";
  private static final String ATTRIBUTE_VALUE_UPDATE_CODE_3 = "code3";
  private static final String INVALID_ATTRIBUTE_TYPE = "code3";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowedAttributeCode";
  private static final Integer ATTRIBUTE_VALUE_ADD_SEQUENCE = 1;
  private static final int DEFAULT_SEQUENCE = 0;
  private static final int SEQUENCE = 1;
  private static final boolean VARIANT_CREATION = true;
  Date createdDate = new Date();
  byte[] bytes = DESCRIPTION_ENGLISH.getBytes();

  private List<AttributeValue> attributeValueList;
  private AttributeValue attributeValue;
  private Page<AttributeValue> attributeValuePage;
  private Pageable pageable;
  private List<String> attributeCodes;
  private List<AttributeValuesWebResponse> attributeValuesWebResponses;
  private List<AttributeValueWebResponse> attributeValueRespons;
  private AttributeValueWebResponse attributeValueWebResponse;
  private AttributeValuesWebResponse attributeValuesWebResponse;
  private AttributeDetailWebResponse attributeDetailWebResponse;

  @Mock
  private AttributeService attributeService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Captor
  private ArgumentCaptor<AttributeValuesUpdateServiceRequest> attributeValuesUpdateServiceRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<AttributeValueAddServiceRequest> attributeValueAddServiceRequestArgumentCaptor;

  @InjectMocks
  private AttributeController attributeController;

  private MasterAttributeRequest masterAttributeRequest;



  private void setUp(MasterAttributeDTO attributeRequest) {
    attributeRequest.setName(NAME);
    attributeRequest.setNameEnglish(NAME_ENGLISH);
    attributeRequest.setDescriptionEnglish(bytes);
    attributeRequest.setAttributeType(AttributeTypeDTO.DEFINING_ATTRIBUTE);
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(createdDate);
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(ID);
    attributeRequest.setSortType(AttributeSortTypeDTO.MANUAL);
    attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueDTO>());
    attributeRequest.getAllowedAttributeValues()
        .add(new AllowedAttributeValueDTO(ATTRIBUTE_CODE, VALUE, VALUE_TYPE, 1));
    attributeRequest.getAllowedAttributeValues()
        .add(new AllowedAttributeValueDTO(ATTRIBUTE_CODE, VALUE, VALUE_TYPE, 2));
    attributeRequest.setPredefinedAllowedAttributeValues(
        new ArrayList<PredefinedAllowedAttributeValueDTO>());
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueDTO(ATTRIBUTE_CODE, VALUE, 1));
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueDTO(ATTRIBUTE_CODE, VALUE, 2));
    attributeRequest.setVariantCreation(VARIANT_CREATION);
    attributeRequest.getDimensionMapping().add(new DimensionMappingDTO());
    attributeRequest.setDsExtraction(true);
    attributeRequest.setHideForCustomer(true);
    attributeRequest.setHideForSeller(true);
  }

  @BeforeEach
  void setup() {
    mockMvc = MockMvcBuilders.standaloneSetup(attributeController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    attributeValue = new AttributeValue();
    attributeValue.setPredefinedAllowedAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    attributeValueList = new ArrayList<>();
    attributeValueList.add(attributeValue);
    pageable = PageRequest.of(PAGE, SIZE);
    attributeValuePage = new PageImpl<>(attributeValueList, pageable, TOTAL_ELEMENTS);

    this.mockMvc = standaloneSetup(this.attributeController).build();
    masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setName(NAME);
    masterAttributeRequest.setNameEnglish(NAME_ENGLISH);
    masterAttributeRequest.setDescriptionEnglish(bytes);
    masterAttributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE.DEFINING_ATTRIBUTE);
    masterAttributeRequest.setCreatedBy("system");
    masterAttributeRequest.setCreatedDate(createdDate);
    masterAttributeRequest.setStoreId(STORE_ID);
    masterAttributeRequest.setSearchAble(true);
    masterAttributeRequest.setId(ID);
    masterAttributeRequest.setSortType(AttributeSortTypeRequest.MANUAL);
    masterAttributeRequest.setVariantCreation(VARIANT_CREATION);
    masterAttributeRequest.setDsExtraction(true);
    masterAttributeRequest.setHideForCustomer(true);
    masterAttributeRequest.getDimensionMapping().add(new DimensionMappingRequest());
    masterAttributeRequest.setHideForSeller(true);
    masterAttributeRequest.setAllowedAttributeValues(
        new ArrayList<com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest>());

    com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest
        allowedAttributeValueRequest1 =
        new com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest();
    allowedAttributeValueRequest1.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueRequest1.setValue(VALUE);
    allowedAttributeValueRequest1.setSequence(1);

    com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest
        allowedAttributeValueRequest2 =
        new com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest();
    allowedAttributeValueRequest2.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueRequest2.setValue(VALUE);
    allowedAttributeValueRequest2.setSequence(2);

    masterAttributeRequest.getAllowedAttributeValues().add(allowedAttributeValueRequest1);
    masterAttributeRequest.getAllowedAttributeValues().add(allowedAttributeValueRequest2);

    com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest
        predefinedAllowedAttributeValueRequest1 =
        new com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest1.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest1.setSequence(1);
    predefinedAllowedAttributeValueRequest1.setValue(VALUE);

    com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest
        predefinedAllowedAttributeValueRequest2 =
        new com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest2.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest2.setSequence(2);
    predefinedAllowedAttributeValueRequest2.setValue(VALUE);

    masterAttributeRequest.setPredefinedAllowedAttributeValues(
        new ArrayList<com.gdn.x.productcategorybase.dto.request
            .PredefinedAllowedAttributeValueRequest>());
    masterAttributeRequest.getPredefinedAllowedAttributeValues()
        .add(predefinedAllowedAttributeValueRequest1);
    masterAttributeRequest.getPredefinedAllowedAttributeValues()
        .add(predefinedAllowedAttributeValueRequest2);
    attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    attributeValuesWebResponse = new AttributeValuesWebResponse();
    attributeValuesWebResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeValuesWebResponses = Arrays.asList(attributeValuesWebResponse);

    attributeDetailWebResponse = new AttributeDetailWebResponse();
    attributeDetailWebResponse.setAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  void getAttributeValuesTest() throws Exception {

    when(attributeService.getAttributeValues(ATTRIBUTE_CODE, pageable, false, false)).thenReturn(attributeValuePage);

    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + SEPERATOR + ATTRIBUTE_CODE + ATTRIBUTE_VALUES_API_PATH)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());


    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true))).andExpect(
        jsonPath("$.content[0].predefinedAllowedAttributeCode", is(PREDEFINED_ATTRIBUTE_CODE)));

    verify(attributeService).getAttributeValues(ATTRIBUTE_CODE, pageable, false, false);
  }

  @Test
  void attributeInfoTest() throws Exception {

    MasterAttributeResponse masterAttributeResponse = new MasterAttributeResponse();
    masterAttributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    masterAttributeResponse.setSortType(AttributeSortTypeRequest.MANUAL);
    masterAttributeResponse.setAttributeType(String.valueOf(AttributeType.DEFINING_ATTRIBUTE));
    when(attributeService.getAttributeDetail(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);

    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_ATTRIBUTE_INFO, ATTRIBUTE_CODE)
            .contentType(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(attributeService).getAttributeDetail(ATTRIBUTE_CODE);
  }

  @Test
  void attributeInfoTest_EmptySortAndAttributeType() throws Exception {

    MasterAttributeResponse masterAttributeResponse = new MasterAttributeResponse();
    masterAttributeResponse.setAttributeType(String.valueOf(AttributeType.DEFINING_ATTRIBUTE));
    when(attributeService.getAttributeDetail(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);

    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_ATTRIBUTE_INFO, ATTRIBUTE_CODE)
            .contentType(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(attributeService).getAttributeDetail(ATTRIBUTE_CODE);
  }

  @Test
  void filter_Valid_Success() throws Exception {

    Mockito.when(this.attributeService
        .findByFilter(any(MasterAttributeFilterRequest.class),
            any(Pageable.class))).thenReturn(this.generateAttributes());
    MockHttpServletRequestBuilder requestBuilder =
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER)
            .param(Constants.PAGE, String.valueOf(PAGE))
            .param(Constants.SIZE, String.valueOf(SIZE))
            .content(toJson(AttributeFilterRequest.builder().build()))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.attributeService)
        .findByFilter(any(MasterAttributeFilterRequest.class), any(Pageable.class));
  }

  private Page<MasterAttributeResponse> generateAttributes() throws Exception {
    return new PageImpl<MasterAttributeResponse>(Arrays.asList(new MasterAttributeResponse()));
  }

  @Test
  void addMasterAttribute() throws Exception {
    MasterAttributeDTO attributeRequest = new MasterAttributeDTO();
    setUp(attributeRequest);
    attributeRequest.setAttributeType(AttributeTypeDTO.DEFINING_ATTRIBUTE);
    attributeRequest.setSortType(AttributeSortTypeDTO.MANUAL);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    GdnBaseRestResponse response = new GdnBaseRestResponse(Boolean.TRUE);

    MasterAttributeRequest expectedRequest =
        ConverterUtil.convertMasterAttributeRequestToAttribute(attributeRequest, STORE_ID);
    when(attributeService.addAttribute(expectedRequest)).thenReturn(response);

    this.mockMvc.perform(post(com.gdn.partners.pcu.master.model.AttributeApiPath.BASE_PATH).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());

    verify(this.attributeService).addAttribute(expectedRequest);
  }

  @Test
  void addMasterAttribute_EmptySortAndAttributeType() throws Exception {
    MasterAttributeDTO attributeRequest = new MasterAttributeDTO();
    attributeRequest.setName(NAME);
    attributeRequest.setNameEnglish(NAME_ENGLISH);
    attributeRequest.setDescriptionEnglish(DESCRIPTION_ENGLISH.getBytes());
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(new Date());
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(ID);
    MasterAttributeRequest request = new MasterAttributeRequest();
    request.setName(NAME);
    request.setNameEnglish(NAME_ENGLISH);
    request.setDescriptionEnglish(DESCRIPTION_ENGLISH.getBytes());
    request.setCreatedBy("system");
    request.setCreatedDate(new Date());
    request.setSearchAble(true);
    request.setId(ID);
    String contentRequest = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    GdnBaseRestResponse response = new GdnBaseRestResponse(Boolean.TRUE);
    when(attributeService.addAttribute(request)).thenReturn(response);
    this.mockMvc.perform(post(com.gdn.partners.pcu.master.model.AttributeApiPath.BASE_PATH)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(contentRequest)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());
    verify(this.attributeService).addAttribute(request);
  }

  @Test
  void updateMasterAttribute() throws Exception {
    MasterAttributeDTO attributeRequest = new MasterAttributeDTO();
    setUp(attributeRequest);
    String request = toJson(attributeRequest);
    GdnBaseRestResponse response = new GdnBaseRestResponse(Boolean.TRUE);
    when(attributeService.updateAttribute(attributeRequest)).thenReturn(response);
    this.mockMvc.perform(put(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE_ATTRIBUTE_DETAIL, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request))
        .andExpect(status().isOk());
    verify(this.attributeService).updateAttribute(attributeRequest);
  }

  @Test
  void updateMasterAttribute_EmptySortAndAttributeType() throws Exception {
    MasterAttributeDTO attributeRequest = new MasterAttributeDTO();
    attributeRequest.setId(ID);
    MasterAttributeRequest request = new MasterAttributeRequest();
    request.setId(ID);
    String contentRequest = toJson(attributeRequest);
    GdnBaseRestResponse response = new GdnBaseRestResponse(Boolean.TRUE);
    when(attributeService.updateAttribute(attributeRequest)).thenReturn(response);
    this.mockMvc.perform(put(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE_ATTRIBUTE_DETAIL, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(contentRequest))
        .andExpect(status().isOk());
    verify(this.attributeService).updateAttribute(attributeRequest);
  }

  @Test
  void updateAttributeValuesTest() throws Exception {
    AttributeValueUpdateWebModel attributeValueUpdateWebModel1 = AttributeValueUpdateWebModel.builder()
        .id(ATTRIBUTE_VALUE_UPDATE_ID_1).allowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_CODE_1)
        .value(ATTRIBUTE_VALUE_UPDATE_VALUE_1).sequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1).build();
    AttributeValueUpdateWebModel attributeValueUpdateWebModel2 = AttributeValueUpdateWebModel.builder()
        .value(ATTRIBUTE_VALUE_UPDATE_VALUE_2).sequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2).build();
    AttributeValueUpdateWebModel attributeValueUpdateWebModel3 = AttributeValueUpdateWebModel.builder()
        .id(ATTRIBUTE_VALUE_UPDATE_ID_3).allowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_CODE_3)
        .value(ATTRIBUTE_VALUE_UPDATE_VALUE_3).build();
    AttributeValuesUpdateWebRequest attributeValuesUpdateWebRequest =
        AttributeValuesUpdateWebRequest.builder()
            .attributeValues(Collections.singletonList(attributeValueUpdateWebModel1))
            .addedAttributeValues(Collections.singletonList(attributeValueUpdateWebModel2))
            .deletedAttributeValues(Collections.singletonList(attributeValueUpdateWebModel3))
            .build();
    when(attributeService.updateAttributeValues(eq(ATTRIBUTE_CODE),
        any(AttributeValuesUpdateServiceRequest.class))).thenReturn(new GdnBaseRestResponse(true));

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        put(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE_ATTRIBUTE_VALUES, ATTRIBUTE_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(toJson(attributeValuesUpdateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(attributeService).updateAttributeValues(eq(ATTRIBUTE_CODE),
        attributeValuesUpdateServiceRequestArgumentCaptor.capture());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_ID_1, attributeValuesUpdateServiceRequestArgumentCaptor.getValue()
        .getAttributeValues().get(0).getId());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_VALUE_2, attributeValuesUpdateServiceRequestArgumentCaptor
        .getValue().getAddedAttributeValues().get(0).getValue());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_ID_3, attributeValuesUpdateServiceRequestArgumentCaptor.getValue()
        .getDeletedAttributeValues().get(0).getId());
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getAttributeListByAttributeTypeSuccess() throws Exception {

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    Mockito.when(this.attributeService
        .findByFilter(any(MasterAttributeFilterRequest.class),
            any(Pageable.class))).thenReturn(this.generateAttributes());
    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + AttributeApiPath.ATTRIBUTES_LIST_BY_ATTRIBUTE_TYPE,
            AttributeTypeDTO.PREDEFINED_ATTRIBUTE.name())
            .content(toJson(AttributeFilterRequest.builder().build()))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.attributeService)
        .findByFilter(any(MasterAttributeFilterRequest.class), any(Pageable.class));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getAttributeListByAttributeTypeSuccessFalse() throws Exception {

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + AttributeApiPath.ATTRIBUTES_LIST_BY_ATTRIBUTE_TYPE,
            INVALID_ATTRIBUTE_TYPE).content(toJson(AttributeFilterRequest.builder().build()))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getAllAttributeValues_EmptyAttributeValueTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(attributeService.getAttributeValues(ATTRIBUTE_CODE, PageRequest.of(0, Integer.MAX_VALUE), true,
        true)).thenReturn(null);

    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + SEPERATOR + ATTRIBUTE_CODE + ALL_ATTRIBUTE_VALUES_API_PATH)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(Integer.MAX_VALUE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());


    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(clientParameterHelper).getRequestId();
    verify(attributeService).getAttributeValues(ATTRIBUTE_CODE, PageRequest.of(0, Integer.MAX_VALUE), true, true);
  }

  @Test
  void getAllAttributeValuesTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(attributeService.getAttributeValues(ATTRIBUTE_CODE, PageRequest.of(0, Integer.MAX_VALUE), true,
        true)).thenReturn(attributeValuePage);

    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + SEPERATOR + ATTRIBUTE_CODE + ALL_ATTRIBUTE_VALUES_API_PATH)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(Integer.MAX_VALUE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());


    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true))).andExpect(
        jsonPath("$.content[0].predefinedAllowedAttributeCode", is(PREDEFINED_ATTRIBUTE_CODE)));
    verify(clientParameterHelper).getRequestId();
    verify(attributeService).getAttributeValues(ATTRIBUTE_CODE, PageRequest.of(0, Integer.MAX_VALUE), true, true);
  }

  @Test
  void getAllAttributeValues_EmptyAttributeCodeTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + SEPERATOR + " " + ALL_ATTRIBUTE_VALUES_API_PATH)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(Integer.MAX_VALUE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());


    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getAllAttributeValuesByAttributeCodesTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    when(attributeService.getAttributeValuesByAttributeCodes(attributeCodes)).thenReturn(attributeValuesWebResponses);
    MockHttpServletRequestBuilder requestBuilder =
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_ALL_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODES)
            .content(toJson(attributeCodes))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());


    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(attributeService).getAttributeValuesByAttributeCodes(attributeCodes);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getAllAttributeValuesByEmptyAttributeCodesTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_ALL_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODES)
            .content(toJson(new ArrayList<String>()))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getPredefinedAllowedAttributesByAttributeIdAndValueTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(this.attributeService.getPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE))
        .thenReturn(attributeValueRespons);
    ListBaseResponse<AttributeValueWebResponse> response =
        attributeController.getPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    Mockito.verify(attributeService).getPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getSpecificPredefinedAllowedAttributesByAttributeIdAndValueTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(this.attributeService.getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE))
        .thenReturn(Arrays.asList(attributeValueWebResponse));
    MockHttpServletRequestBuilder requestBuilder = get(AttributeApiPath.BASE_PATH
        + AttributeApiPath.GET_SPECIFIC_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_BY_ATTRIBUTE_ID_AND_VALUE)
        .param(ATTRIBUTE_ID, ATTRIBUTE_ID).param(VALUE, VALUE).contentType(MediaType.APPLICATION_JSON)
        .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(attributeService).getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void addAttributeValueTest() throws Exception {
    AttributeValueAddWebRequest attributeValueAddWebRequest =
        AttributeValueAddWebRequest.builder().value(VALUE).sequence(SEQUENCE).build();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(attributeService.addAttributeValue(eq(ATTRIBUTE_CODE), any(AttributeValueAddServiceRequest.class)))
        .thenReturn(new AttributeValueResponse());

    MockHttpServletRequestBuilder requestBuilder =
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.ADD_ATTRIBUTE_VALUE_BY_ATTRIBUTE_CODE, ATTRIBUTE_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(attributeValueAddWebRequest)).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(attributeService)
        .addAttributeValue(eq(ATTRIBUTE_CODE), attributeValueAddServiceRequestArgumentCaptor.capture());
    assertNotNull(attributeValueAddServiceRequestArgumentCaptor.getValue().getCreatedDate());
    assertEquals(USER_NAME, attributeValueAddServiceRequestArgumentCaptor.getValue().getCreatedBy());
    assertEquals(VALUE, attributeValueAddServiceRequestArgumentCaptor.getValue().getValue());
    assertEquals(SEQUENCE, attributeValueAddServiceRequestArgumentCaptor.getValue().getSequence());
  }

  @Test
  void addAttributeValueTest_withNullValue() throws Exception {
    AttributeValueAddWebRequest attributeValueAddWebRequest =
        AttributeValueAddWebRequest.builder().value(VALUE).build();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(attributeService.addAttributeValue(eq(ATTRIBUTE_CODE), any(AttributeValueAddServiceRequest.class)))
        .thenReturn(new AttributeValueResponse());

    MockHttpServletRequestBuilder requestBuilder =
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.ADD_ATTRIBUTE_VALUE_BY_ATTRIBUTE_CODE, ATTRIBUTE_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(attributeValueAddWebRequest)).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(attributeService)
        .addAttributeValue(eq(ATTRIBUTE_CODE), attributeValueAddServiceRequestArgumentCaptor.capture());
    assertNotNull(attributeValueAddServiceRequestArgumentCaptor.getValue().getCreatedDate());
    assertEquals(USER_NAME, attributeValueAddServiceRequestArgumentCaptor.getValue().getCreatedBy());
    assertEquals(VALUE, attributeValueAddServiceRequestArgumentCaptor.getValue().getValue());
    assertEquals(DEFAULT_SEQUENCE, attributeValueAddServiceRequestArgumentCaptor.getValue().getSequence());
  }

  @Test
  void getAttributesTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(attributeService.getAttributeDetails(eq(ATTRIBUTE_CODE))).thenReturn(attributeDetailWebResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_DETAIL_BY_ATTRIBUTE_CODE, ATTRIBUTE_CODE)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(attributeService).getAttributeDetails(eq(ATTRIBUTE_CODE));
  }
  @AfterEach
  void tearDown() {
    verifyNoMoreInteractions(attributeService);
    verifyNoMoreInteractions(clientParameterHelper);
  }
}