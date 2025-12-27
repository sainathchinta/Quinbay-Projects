package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
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

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeApiPath;
import com.gdn.x.productcategorybase.AttributeFilter;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.MasterAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeValueUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.MasterAttributeService;

/**
 * @author BhagwatiMalav - created on 30/10/18
 */
public class MasterAttributeControllerTest {

  private static final String VALUE = "Value";
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
  private static final String SORT_DIRECTTION = "ASC";
  private static final String PAGE = "1";
  private static final String SIZE = "10";
  private static final String DS_ATTRIBUTE_NAME = "dsAttributeName";

  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "PRE-101";
  private static final Integer SEQUENCE =0;
  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 10;
  private static final String ATTRIBUTE_CODE = "ATT";
  private static final String VALUES = "/values";
  private static final String SEPARATOR = "/";

  private static final String INFO = "/info/";
  private static final String DETAIL_BY_ATTRIBUTE_CODE = "/detail";

  private static final Boolean GET_ALL_VALUES = Boolean.FALSE;

  @InjectMocks
  private MasterAttributeController controller;

  @Mock
  private MasterAttributeService masterAttributeService;

  @Captor
  private ArgumentCaptor<MasterAttributeUpdateDTO> masterAttributeUpdateDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<AttributeValueUpdateDTO> attributeValueUpdateDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<Attribute> attributeArgumentCaptor;

  private Page<AttributeValueDTO> attributeValueDTOPage;

  private final Pageable pageable =
      PageRequest.of(MasterAttributeControllerTest.PAGE_NUMBER, MasterAttributeControllerTest.PAGE_SIZE);
  private MockMvc mockMvc;
  private Attribute attribute;
  private List<AttributeValueDTO> attributeValueDTOS;
  private AttributeValueDTO attributeValueDTO;


  private MasterAttributeFilterRequest masterAttributeFilterRequest;

  private Page<Attribute> attributePage;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();

    this.attribute = new Attribute();
    this.attribute.setName(NAME);
    this.attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    this.attribute.setSearchAble(true);
    this.attribute.getAllowedAttributeValues().add(
        new AllowedAttributeValue(this.attribute, VALUE, STORE_ID, 1));
    this.attribute.getAllowedAttributeValues().add(
        new AllowedAttributeValue(this.attribute, VALUE, STORE_ID, 2));
    this.attribute.getAllowedAttributeValues().get(1).setMarkForDelete(true);
    this.attribute.getPredefinedAllowedAttributeValues().add(
        new PredefinedAllowedAttributeValue(this.attribute, VALUE,
            STORE_ID, 1));
    this.attribute.getPredefinedAllowedAttributeValues().add(
        new PredefinedAllowedAttributeValue(this.attribute, VALUE,
            STORE_ID, 2));
    this.attribute.getPredefinedAllowedAttributeValues().get(1).setMarkForDelete(true);
    this.attribute.setId(ID);
    this.attribute.setSortType(AttributeSortType.ALPHABETICAL);
    this.attributeValueDTO = new AttributeValueDTO();
    this.attributeValueDTO.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    this.attributeValueDTO.setValue(VALUE);
    this.attributeValueDTO.setSequence(SEQUENCE);
    this.attributeValueDTOS = new ArrayList<>();
    this.attributeValueDTOS.add(attributeValueDTO);
    this.attributeValueDTOPage = new PageImpl<>(attributeValueDTOS);

    masterAttributeFilterRequest =
        MasterAttributeFilterRequest.builder().attributeType(AttributeType.DEFINING_ATTRIBUTE.name())
            .name(NAME).sortDirection(SORT_DIRECTTION).sortedBy(NAME).build();
    attributePage =
        new PageImpl<Attribute>(Collections.singletonList(attribute), PageRequest.of(0, 10), 1);
  }

  private void setUp(MasterAttributeRequest attributeRequest) {
    attributeRequest.setName(NAME);
    attributeRequest.setNameEnglish(NAME_ENGLISH);
    attributeRequest.setDescriptionEnglish(DESCRIPTION_ENGLISH.getBytes());
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(new Date());
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(ID);
    attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
    attributeRequest.getAllowedAttributeValues().add(new AllowedAttributeValueRequest(
        VALUE, 1, STORE_ID));
    attributeRequest.getAllowedAttributeValues().add(new AllowedAttributeValueRequest(
        VALUE, 1, ""));
    attributeRequest.setPredefinedAllowedAttributeValues(
        new ArrayList<PredefinedAllowedAttributeValueRequest>());
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueRequest(VALUE, 1,
            STORE_ID));
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueRequest(VALUE, 1,
            ""));
  }

  private void getAttributeRequestWithoutAttributeValues(MasterAttributeRequest attributeRequest) {
    attributeRequest.setName(NAME);
    attributeRequest.setNameEnglish(NAME_ENGLISH);
    attributeRequest.setDescriptionEnglish(DESCRIPTION_ENGLISH.getBytes());
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(new Date());
    attributeRequest.setUpdatedBy("system");
    attributeRequest.setUpdatedDate(new Date());
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(ID);
  }

  @Test
  public void saveMasterAttributeTest() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());
    verify(this.masterAttributeService).insertMasterAttribute(attributeArgumentCaptor.capture(),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
    Assertions.assertEquals(NAME, attributeArgumentCaptor.getValue().getName());
    Assertions.assertEquals(NAME_ENGLISH, attributeArgumentCaptor.getValue().getNameEnglish());
    Assertions.assertEquals(DESCRIPTION_ENGLISH, new String(attributeArgumentCaptor.getValue().getDescriptionEnglish()));
  }

  @Test
  public void saveMasterAttributeTest_dsAttributeTrue() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    attributeRequest.setDsExtraction(true);
    attributeRequest.setDsAttributeName(DS_ATTRIBUTE_NAME);
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());
    verify(this.masterAttributeService).insertMasterAttribute(attributeArgumentCaptor.capture(),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
    Assertions.assertEquals(NAME, attributeArgumentCaptor.getValue().getName());
    Assertions.assertEquals(NAME_ENGLISH, attributeArgumentCaptor.getValue().getNameEnglish());
    Assertions.assertEquals(DESCRIPTION_ENGLISH, new String(attributeArgumentCaptor.getValue().getDescriptionEnglish()));
  }

  @Test
  public void saveMasterAttributeTest_dsAttributeTrueInvalidAttributeType() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    attributeRequest.setDsExtraction(true);
    attributeRequest.setDsAttributeName(DS_ATTRIBUTE_NAME);
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(
            ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_DS_EXTRACTION_ERROR_CODE.getMessage().toString())));
  }

  @Test
  public void saveMasterAttributeTest_multiLanguageTrue() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    attributeRequest.setDsExtraction(true);
    attributeRequest.setDsAttributeName(DS_ATTRIBUTE_NAME);
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setMultiLanguage(true);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());
    verify(this.masterAttributeService).insertMasterAttribute(attributeArgumentCaptor.capture(),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
    Assertions.assertEquals(NAME, attributeArgumentCaptor.getValue().getName());
    Assertions.assertEquals(NAME_ENGLISH, attributeArgumentCaptor.getValue().getNameEnglish());
    Assertions.assertEquals(DESCRIPTION_ENGLISH, new String(attributeArgumentCaptor.getValue().getDescriptionEnglish()));
  }

  @Test
  public void saveMasterAttributeTest_multiLanguageTrue_exception() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    attributeRequest.setDsAttributeName(DS_ATTRIBUTE_NAME);
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setMultiLanguage(true);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(
            ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_MULTI_LANGUAGE_ERROR_CODE.getMessage().toString())));
  }

  @Test
  public void saveMasterAttributeTest_multiValuedMultiLanguageTrue() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    attributeRequest.setDsExtraction(true);
    attributeRequest.setDsAttributeName(DS_ATTRIBUTE_NAME);
    attributeRequest.setAttributeType(
        com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_MULTIVALUE);
    attributeRequest.setMultiLanguage(true);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());
    verify(this.masterAttributeService).insertMasterAttribute(attributeArgumentCaptor.capture(),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
    Assertions.assertEquals(NAME, attributeArgumentCaptor.getValue().getName());
    Assertions.assertEquals(NAME_ENGLISH, attributeArgumentCaptor.getValue().getNameEnglish());
    Assertions.assertEquals(DESCRIPTION_ENGLISH, new String(attributeArgumentCaptor.getValue().getDescriptionEnglish()));
  }

  @Test
  public void saveMasterAttribute_throwsApplicationException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    org.springframework.beans.BeanUtils.copyProperties(attribute, attributeRequest, "attributeType");
    when(masterAttributeService.insertMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()), eq(new ArrayList<>()))).thenThrow(
        new ApplicationException(ErrorCategory.VALIDATION));
    this.mockMvc.perform(
        post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.VALIDATION.toString())));
    verify(masterAttributeService).insertMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
  }

  @Test
  public void saveMasterAttribute_throwsException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    org.springframework.beans.BeanUtils.copyProperties(attribute, attributeRequest, "attributeType");
    when(masterAttributeService.insertMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()), eq(new ArrayList<>()))).thenThrow(new Exception());
    this.mockMvc.perform(
        post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.toString())));
    verify(masterAttributeService).insertMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
  }

  @Test
  public void saveMasterAttribute_throwsValidationException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    setUp(attributeRequest);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    org.springframework.beans.BeanUtils.copyProperties(attribute, attributeRequest,
        "attributeType");
    when(masterAttributeService.insertMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()), eq(new ArrayList<>()))).thenThrow(
        new ValidationException(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage(),
            ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage()));
    this.mockMvc.perform(
            post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))).andExpect(
            jsonPath("$.errorCode", equalTo(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage())));
    verify(masterAttributeService).insertMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()), eq(new ArrayList<>()));
  }

  @Test
  public void getAttributeValuesBasedOnAttributeCode() throws Exception {
    Mockito.doReturn(attributeValueDTOPage).when(this.masterAttributeService)
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, this.pageable, GET_ALL_VALUES, true);
    StringBuilder path = new StringBuilder();
    path = path.append(SEPARATOR + ATTRIBUTE_CODE + VALUES);
    this.mockMvc.perform(
        get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + path.toString()).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("page", String.valueOf(PAGE_NUMBER)).param("size", String.valueOf(PAGE_SIZE))
            .param("getAllValues", String.valueOf(GET_ALL_VALUES))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(PAGE_NUMBER)));
    Mockito.verify(this.masterAttributeService)
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, this.pageable, GET_ALL_VALUES, true);
  }

  @Test
  public void getAttributeValuesBasedOnAttributeCodeWithException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.masterAttributeService)
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, pageable, GET_ALL_VALUES, true);
    StringBuilder path = new StringBuilder();
    path = path.append(SEPARATOR + ATTRIBUTE_CODE + VALUES);
    this.mockMvc.perform(
        get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + path.toString()).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("page", String.valueOf(PAGE_NUMBER)).param("size", String.valueOf(PAGE_SIZE))
            .param("getAllValues", String.valueOf(GET_ALL_VALUES))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(PAGE_NUMBER)));
    Mockito.verify(this.masterAttributeService)
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, pageable, GET_ALL_VALUES, true);
  }


  @Test
  public void getAttributeByAttributeFilterTest() throws Exception {
    String request = OBJECT_MAPPER.writeValueAsString(masterAttributeFilterRequest);
    Mockito.when(this.masterAttributeService.getAttributeByAttributeFilter(eq(STORE_ID), any(
        AttributeFilter.class), any(Pageable.class))).thenReturn(attributePage);
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.FILTER_LIST)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()));
    Mockito.verify(this.masterAttributeService).getAttributeByAttributeFilter(eq(STORE_ID), any(
        AttributeFilter.class), any(Pageable.class));
  }

  @Test
  public void getAttributeByAttributeFilter_whenRuntimeExceptionTest() throws Exception {
    String request = OBJECT_MAPPER.writeValueAsString(masterAttributeFilterRequest);
    Mockito.doThrow(new RuntimeException()).when(this.masterAttributeService)
        .getAttributeByAttributeFilter(eq(STORE_ID), any(AttributeFilter.class),
            any(Pageable.class));
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.FILTER_LIST)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.masterAttributeService).getAttributeByAttributeFilter(eq(STORE_ID), any(
        AttributeFilter.class), any(Pageable.class));
  }

  @Test
  public void getAttributeByAttributeFilter_whenApplicationRuntimeExceptionTest() throws Exception {
    String request = OBJECT_MAPPER.writeValueAsString(masterAttributeFilterRequest);
    Page<Attribute> attributePage =
        new PageImpl<Attribute>(Collections.singletonList(attribute), PageRequest.of(0, 10), 1);
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.masterAttributeService)
        .getAttributeByAttributeFilter(eq(STORE_ID), any(AttributeFilter.class),
            any(Pageable.class));
    this.mockMvc
        .perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.FILTER_LIST)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.masterAttributeService).getAttributeByAttributeFilter(eq(STORE_ID), any(
        AttributeFilter.class), any(Pageable.class));
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.masterAttributeService);
  }

  @Test
  public void getAttributeDetailByAttributeCodeTest() throws Exception {
    Attribute result = new Attribute();
    result.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE)).thenReturn(result);

    this.mockMvc.perform(
        get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + MasterAttributeControllerTest.INFO
            + MasterAttributeControllerTest.ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", MasterAttributeControllerTest.STORE_ID)
            .param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
            .param("clientId", MasterAttributeControllerTest.CLIENT_ID)
            .param("requestId", MasterAttributeControllerTest.REQUEST_ID)
            .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void getAttributeDetailByAttributeCodeSizeValueTypesTestTest() throws Exception {
    List<String> valueTypes = List.of(STORE_ID);
    Attribute result = new Attribute();
    result.setAttributeCode(ATTRIBUTE_CODE);
    result.setValueTypes(new ObjectMapper().writeValueAsString(valueTypes));
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE)).thenReturn(result);

    this.mockMvc.perform(
        get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + MasterAttributeControllerTest.INFO
          + MasterAttributeControllerTest.ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).param("storeId", MasterAttributeControllerTest.STORE_ID)
          .param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
          .param("clientId", MasterAttributeControllerTest.CLIENT_ID)
          .param("requestId", MasterAttributeControllerTest.REQUEST_ID)
          .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
          .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
      .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }


  @Test
  public void getAttributeDetailByAttributeCodeException_Test() throws Exception {
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE)).thenThrow(RuntimeException.class);

    this.mockMvc.perform(
        get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + MasterAttributeControllerTest.INFO
            + MasterAttributeControllerTest.ATTRIBUTE_CODE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", MasterAttributeControllerTest.STORE_ID).param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
            .param("clientId", MasterAttributeControllerTest.CLIENT_ID).param("requestId", MasterAttributeControllerTest.REQUEST_ID)
            .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void getAttributeDetailByAttributeCodeApplicationRuntimeException_Test() throws Exception {
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE)).thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(
        get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + MasterAttributeControllerTest.INFO
            + MasterAttributeControllerTest.ATTRIBUTE_CODE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", MasterAttributeControllerTest.STORE_ID).param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
            .param("clientId", MasterAttributeControllerTest.CLIENT_ID).param("requestId", MasterAttributeControllerTest.REQUEST_ID)
            .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void updateMasterAttributeValuesTest() throws Exception{
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    MasterAttributeUpdateRequest attributeUpdateRequest = new MasterAttributeUpdateRequest();
    AttributeValueUpdateRequest attributeValueUpdateRequest = new AttributeValueUpdateRequest();
    attributeValueUpdateRequest.setAllowedAttributeCode(ATTRIBUTE_CODE);
    attributeValueUpdateRequest.setValue(VALUE);
    attributeUpdateRequest.setAddedAttributeValues(Arrays.asList(attributeValueUpdateRequest));
    attributeUpdateRequest.setAttributeValues(Arrays.asList(attributeValueUpdateRequest));
    attributeUpdateRequest.setDeletedAttributeValues(Arrays.asList(attributeValueUpdateRequest));
    attributeUpdateRequest.setSortType(AttributeSortTypeRequest.MANUAL);
    attributeUpdateRequest.setUpdatedBy(DEFAULT_USERNAME);
    attributeUpdateRequest.setUpdatedDate(new Date());
    String request = OBJECT_MAPPER.writeValueAsString(attributeUpdateRequest);
    doNothing().when(masterAttributeService).updateAttributeValues(eq(STORE_ID), eq(attribute.getAttributeCode()),
       any(MasterAttributeUpdateDTO.class));
    this.mockMvc.perform(
        put(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.UPDATE_VALUES,
            attribute.getAttributeCode())
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(masterAttributeService).updateAttributeValues(eq(STORE_ID), eq(attribute.getAttributeCode()),
        masterAttributeUpdateDTOArgumentCaptor.capture());
    assertEquals(attributeUpdateRequest.getSortType().toString(),
        masterAttributeUpdateDTOArgumentCaptor.getValue().getSortType().toString());
    assertEquals(attributeUpdateRequest.getAddedAttributeValues().size(),
        masterAttributeUpdateDTOArgumentCaptor.getValue().getAddedAttributeValues().size());
    assertEquals(attributeUpdateRequest.getUpdatedBy(),
        masterAttributeUpdateDTOArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void updateMasterAttributeValues_throwsApplicationExceptionTest() throws Exception{
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    MasterAttributeUpdateRequest attributeUpdateRequest = new MasterAttributeUpdateRequest();
    attributeUpdateRequest.setAddedAttributeValues(new ArrayList<>());
    attributeUpdateRequest.setAttributeValues(new ArrayList<>());
    attributeUpdateRequest.setDeletedAttributeValues(new ArrayList<>());
    attributeUpdateRequest.setSortType(AttributeSortTypeRequest.MANUAL);
    attributeUpdateRequest.setUpdatedBy(DEFAULT_USERNAME);
    attributeUpdateRequest.setUpdatedDate(new Date());
    String request = OBJECT_MAPPER.writeValueAsString(attributeUpdateRequest);
    doThrow(new ApplicationException(ErrorCategory.DATA_NOT_FOUND))
        .when(masterAttributeService).updateAttributeValues(eq(STORE_ID), eq(attribute.getAttributeCode()),
       any(MasterAttributeUpdateDTO.class));
    this.mockMvc.perform(
        put(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.UPDATE_VALUES,
            attribute.getAttributeCode())
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(masterAttributeService).updateAttributeValues(eq(STORE_ID), eq(attribute.getAttributeCode()),
       any(MasterAttributeUpdateDTO.class));
  }

  @Test
  public void updateMasterAttributeValues_throwsApplicationRuntimeExceptionTest() throws Exception{
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    MasterAttributeUpdateRequest attributeUpdateRequest = new MasterAttributeUpdateRequest();
    String request = OBJECT_MAPPER.writeValueAsString(attributeUpdateRequest);
    this.mockMvc.perform(
        put(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.UPDATE_VALUES,
            attribute.getAttributeCode())
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void updateMasterAttribute_throwsApplicationException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    getAttributeRequestWithoutAttributeValues(attributeRequest);
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    BeanUtils.copyProperties(attribute, attributeRequest, "attributeType");
    when(masterAttributeService.updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()))).thenThrow(new ApplicationException(ErrorCategory.DATA_NOT_FOUND));
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.UPDATE_MASTER_ATTRIBUTE)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.DATA_NOT_FOUND.toString())));
    verify(masterAttributeService).updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()));
  }

  @Test
  public void updateMasterAttribute_throwsException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    getAttributeRequestWithoutAttributeValues(attributeRequest);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    BeanUtils.copyProperties(attribute, attributeRequest, "attributeType");
    when(masterAttributeService.updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()))).thenThrow(new Exception());
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.UPDATE_MASTER_ATTRIBUTE)
        .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.toString())));
    verify(masterAttributeService).updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()));
  }

  @Test
  public void updateMasterAttribute_throwsApplicationRuntimeException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    getAttributeRequestWithoutAttributeValues(attributeRequest);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    BeanUtils.copyProperties(attribute, attributeRequest, "attributeType");
    when(masterAttributeService.updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()))).thenThrow(
        new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT));
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.UPDATE_MASTER_ATTRIBUTE)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.INVALID_FORMAT.toString())));
    verify(masterAttributeService).updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()));
  }

  @Test
  public void updateMasterAttribute_throwsValidationException() throws Exception {
    MasterAttributeRequest attributeRequest = new MasterAttributeRequest();
    getAttributeRequestWithoutAttributeValues(attributeRequest);
    String request = OBJECT_MAPPER.writeValueAsString(attributeRequest);
    BeanUtils.copyProperties(attribute, attributeRequest, "attributeType");
    when(masterAttributeService.updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()))).thenThrow(
        new ValidationException(ErrorCategory.DATA_NOT_FOUND.getCode(),
            ErrorCategory.DATA_NOT_FOUND.getMessage()));
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH
            + AttributeApiPath.UPDATE_MASTER_ATTRIBUTE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.DATA_NOT_FOUND.getCode())));
    verify(masterAttributeService).updateMasterAttribute(any(Attribute.class),
        eq(new ArrayList<>()));
  }

  @Test
  public void addMasterAttributeValueTest() throws Exception {
    MasterAttributeAddRequest attributeAddRequest = new MasterAttributeAddRequest();
    attributeAddRequest.setValue(VALUE.concat(StringUtils.SPACE));
    Date date = new Date();
    attributeAddRequest.setCreatedDate(date);
    attributeAddRequest.setCreatedBy(DEFAULT_USERNAME);
    attributeAddRequest.setSequence(SEQUENCE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeAddRequest);
    Mockito.doReturn(new AttributeValueResponse()).when(masterAttributeService)
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date));
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.ADD_VALUE, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(masterAttributeService)
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), attributeValueUpdateDTOArgumentCaptor.capture(),
            eq(DEFAULT_USERNAME), eq(date));
    assertEquals(attributeAddRequest.getValue().trim(), attributeValueUpdateDTOArgumentCaptor.getValue().getValue());
    assertEquals(attributeAddRequest.getSequence(),
        (int) attributeValueUpdateDTOArgumentCaptor.getValue().getSequence());
  }

  @Test
  public void addMasterAttributeValueEmptyValueTest() throws Exception {
    MasterAttributeAddRequest attributeAddRequest = new MasterAttributeAddRequest();
    attributeAddRequest.setValue(StringUtils.SPACE);
    Date date = new Date();
    attributeAddRequest.setCreatedDate(date);
    attributeAddRequest.setCreatedBy(DEFAULT_USERNAME);
    attributeAddRequest.setSequence(SEQUENCE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeAddRequest);
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.ADD_VALUE, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void addMasterAttributeValue_throwsApplicationExceptionTest() throws Exception {
    MasterAttributeAddRequest attributeAddRequest = new MasterAttributeAddRequest();
    attributeAddRequest.setValue(VALUE);
    Date date = new Date();
    attributeAddRequest.setCreatedDate(date);
    attributeAddRequest.setCreatedBy(DEFAULT_USERNAME);
    attributeAddRequest.setSequence(SEQUENCE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeAddRequest);
    doThrow(new ApplicationException(ErrorCategory.DATA_NOT_FOUND)).when(masterAttributeService)
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date));
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.ADD_VALUE, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.DATA_NOT_FOUND.toString())));
    verify(masterAttributeService)
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date));
  }

  @Test
  public void addMasterAttributeValue_throwsApplicationRuntimeExceptionTest() throws Exception {
    MasterAttributeAddRequest attributeAddRequest = new MasterAttributeAddRequest();
    attributeAddRequest.setValue(VALUE);
    Date date = new Date();
    attributeAddRequest.setCreatedDate(date);
    attributeAddRequest.setCreatedBy(DEFAULT_USERNAME);
    attributeAddRequest.setSequence(SEQUENCE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeAddRequest);
    Mockito.when(masterAttributeService
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date))).thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.ADD_VALUE, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(masterAttributeService)
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date));
  }

  @Test
  public void addMasterAttributeValue_throwsExceptionTest() throws Exception {
    MasterAttributeAddRequest attributeAddRequest = new MasterAttributeAddRequest();
    attributeAddRequest.setValue(VALUE);
    Date date = new Date();
    attributeAddRequest.setCreatedDate(date);
    attributeAddRequest.setCreatedBy(DEFAULT_USERNAME);
    attributeAddRequest.setSequence(SEQUENCE);
    String request = OBJECT_MAPPER.writeValueAsString(attributeAddRequest);
    when(masterAttributeService
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date))).thenThrow(new Exception());
    this.mockMvc.perform(post(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + AttributeApiPath.ADD_VALUE, ATTRIBUTE_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.toString())));
    verify(masterAttributeService)
        .addAttributeValue(eq(STORE_ID), eq(ATTRIBUTE_CODE), any(AttributeValueUpdateDTO.class),
            eq(DEFAULT_USERNAME), eq(date));
  }

  @Test
  public void getAttributeByAttributeCodeTest() throws Exception {
    Attribute result = new Attribute();
    result.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE)).thenReturn(result);

    this.mockMvc.perform(get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH
            + MasterAttributeControllerTest.DETAIL_BY_ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", MasterAttributeControllerTest.STORE_ID)
            .param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
            .param("clientId", MasterAttributeControllerTest.CLIENT_ID)
            .param("requestId", MasterAttributeControllerTest.REQUEST_ID)
            .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void getAttributeByAttributeCode_Test() throws Exception {
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE)).thenThrow(RuntimeException.class);

    this.mockMvc.perform(get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH
            + MasterAttributeControllerTest.DETAIL_BY_ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", MasterAttributeControllerTest.STORE_ID)
            .param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
            .param("clientId", MasterAttributeControllerTest.CLIENT_ID)
            .param("requestId", MasterAttributeControllerTest.REQUEST_ID)
            .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void getAttributeByAttributeCodeApplicationRuntimeException_Test() throws Exception {
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(get(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH
            + MasterAttributeControllerTest.DETAIL_BY_ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", MasterAttributeControllerTest.STORE_ID)
            .param("channelId", MasterAttributeControllerTest.CHANNEL_ID)
            .param("clientId", MasterAttributeControllerTest.CLIENT_ID)
            .param("requestId", MasterAttributeControllerTest.REQUEST_ID)
            .param("username", MasterAttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", MasterAttributeControllerTest.ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(MasterAttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
  }
}
