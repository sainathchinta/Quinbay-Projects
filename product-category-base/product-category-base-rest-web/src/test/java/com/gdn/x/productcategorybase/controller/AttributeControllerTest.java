package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.AttributeApiPath;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.AttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.ProductItemService;

public class AttributeControllerTest {

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  private static final String VALUE = "Value";
  private static final String ID = "id";
  private static final String NAME = "name";
  private static final String ATTRIBUTE_CODE = "ATT";
  private static final String CATEGORY_CODE = "CAT-123";
  private static final String CATEGORY_CODE_2 = "CAT-123-2";
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 10;
  private static final int TOTAL_RECORDS = 0;
  private static final String ATTRIBUTE1_NAME = "Attribute1";
  private static final String ATTRIBUTE2_NAME = "Attribute2";
  private static final String ATTRIBUTE3_NAME = "Attribute3";
  private static final String PREDEFINED_ATTRIBUTE_CODE = "PDALCODE";
  private static final String PREDEFINED_ATTRIBUTE_CODE_DEFAULT = "PDALCODE-DEFAULT";
  private static final String PRODUCT_CODE = "MTA-123456";
  private static final String ATTRIBUTE_VALUE = "attributeValue";

  private final Pageable pageable =
      PageRequest.of(AttributeControllerTest.PAGE_NUMBER, AttributeControllerTest.PAGE_SIZE);

  @InjectMocks
  private AttributeController controller;
  @Mock
  private AttributeService service;
  @Mock
  private ProductItemService productItemService;
  @Mock
  private Page<Attribute> page;

  @Captor
  private ArgumentCaptor<Attribute> attributeArgumentCaptor;

  private MockMvc mockMvc;
  private List<Attribute> attributeList;
  private Attribute attribute;

  @Test
  public void deleteAllowedAttributeValueTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder = new SimpleRequestHolder(AttributeControllerTest.ID);
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(simpleRequestHolder);
    this.mockMvc.perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.DELETE_VALUE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
        .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
        .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(this.service).deleteAllowedAttributeValue(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID);
  }
  
  @Test
  public void getAttributeDetailByCategoryCodeTest() throws Exception {
    CategoryAttributeDetailDTO categoryAttributeDetailDTO = new CategoryAttributeDetailDTO();
    categoryAttributeDetailDTO.setEnglishName("category-english");
    categoryAttributeDetailDTO.setAttributes(Arrays.asList(AttributeDetailDTO.builder()
        .englishName("attribute-english")
        .variantCreatingUi(true)
        .build()));
    Mockito.when(service.getAttributeDetailByCategoryCode(CATEGORY_CODE, true))
        .thenReturn(categoryAttributeDetailDTO);

    this.mockMvc
        .perform(get(
            AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("categoryCode", AttributeControllerTest.CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(service).getAttributeDetailByCategoryCode(CATEGORY_CODE, true);
  }

  @Test
  public void getAttributeDetailByCategoryCodeWhenError() throws Exception {
    Mockito.when(service.getAttributeDetailByCategoryCode(CATEGORY_CODE, true)).thenThrow(Exception.class);
    
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("categoryCode", AttributeControllerTest.CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(service).getAttributeDetailByCategoryCode(CATEGORY_CODE, true);
  }

  @Test
  public void findByAttributeCode() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", AttributeControllerTest.ATTRIBUTE_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));

    verify(this.service).findByAttributeCode(AttributeControllerTest.STORE_ID, AttributeControllerTest.ATTRIBUTE_CODE,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void findByAttributeType() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeType", AttributeType.DEFINING_ATTRIBUTE.toString()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));

    verify(this.service).findByAttributeType(AttributeControllerTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }
  
//  @Ignore
  @Test
  public void getAttributeByNameLikeAndPageable() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_NAME_LIKE)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("name", NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));


    verify(this.service).findByNameLikeIgnoreCase(STORE_ID, NAME, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getAttributeByNameStartingWithAndPageable() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_NAME).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME).param("name", AttributeControllerTest.NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));

    verify(this.service).findByNameStartingWith(AttributeControllerTest.STORE_ID, AttributeControllerTest.NAME,
        this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getAttributeBySearchAbleFalseAndPageable() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_SEARCHABLE_FALSE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));

    verify(this.service).findBySearchAbleFalse(AttributeControllerTest.STORE_ID, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getAttributeBySearchAbleTrueAndPageable() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_SEARCHABLE_TRUE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));

    verify(this.service).findBySearchAbleTrue(AttributeControllerTest.STORE_ID, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }
  
  @Test
  public void getAttributeDetailByAttributeCodesTest() throws Exception {
    AttributeCodesRequest attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(AttributeControllerTest.ATTRIBUTE_CODE));
    this.attribute
        .setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    List<Attribute> attributes = Arrays.asList(this.attribute);
    when(this.service.findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(),
        false)).thenReturn(attributes);
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODES)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(request)
                .param("storeId", AttributeControllerTest.STORE_ID)
                .param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID)
                .param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    verify(this.service)
        .findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(), false);
  }
  
  @Test
  public void getAttributeDetailByAttributeCodesEmptyRequestTest() throws Exception {
    AttributeCodesRequest attributeCodesRequest = new AttributeCodesRequest();
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODES)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(request)
                .param("storeId", AttributeControllerTest.STORE_ID)
                .param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID)
                .param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    verify(this.service, times(0))
        .findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(), false);
  }
  
  @Test
  public void getAttributeDetailByAttributeCodesEmptyPredefinedAndAllowedTest() throws Exception {
    AttributeCodesRequest attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(AttributeControllerTest.ATTRIBUTE_CODE));
    this.attribute
        .setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute
    .setAllowedAttributeValues(new ArrayList<AllowedAttributeValue>());
    List<Attribute> attributes = Arrays.asList(this.attribute);
    when(this.service.findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(),
        false)).thenReturn(attributes);
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODES)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(request)
                .param("storeId", AttributeControllerTest.STORE_ID)
                .param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID)
                .param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    verify(this.service)
        .findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(), false);
  }
  
  @Test
  public void getAttributeDetailByAttributeCodesEmptyResponseTest() throws Exception {
    AttributeCodesRequest attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(AttributeControllerTest.ATTRIBUTE_CODE));
    this.attribute
        .setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute
    .setAllowedAttributeValues(new ArrayList<AllowedAttributeValue>());
    when(this.service.findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(),
        false)).thenReturn(null);
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODES)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(request)
                .param("storeId", AttributeControllerTest.STORE_ID)
                .param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID)
                .param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true))).andReturn();
    verify(this.service)
        .findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(), false);
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void getAttributeDetailByAttributeCodeErrorTest() throws Exception {
    AttributeCodesRequest attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(AttributeControllerTest.ATTRIBUTE_CODE));
    this.attribute
        .setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    
    when(this.service.findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(),
        false)).thenThrow(RuntimeException.class);
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc
    .perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODES)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME))
    .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))).andReturn();
    verify(this.service)
        .findDetailByStoreIdAndAttributeCodes(attributeCodesRequest.getAttributeCodes(), false);
  }

  @Test
  public void getAttributeDetailTest() throws Exception {
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
//    this.mockMvc.perform(get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID)

    MvcResult mvcResult = this.mockMvc.perform(get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
        .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
        .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response =
        OBJECT_MAPPER.readValue(result, new TypeReference<GdnRestSingleResponse<AttributeResponse>>() {});
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID, Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailMarkForDeleteTest() throws Exception {
    ReflectionTestUtils.setField(this.controller, "valueTypeAdditionForDefiningAttributes", true);
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    this.attribute.getAllowedAttributeValues().get(0).setMarkForDelete(true);

    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("allowedAttributeValuesTrim", String.valueOf(true))).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response = OBJECT_MAPPER.readValue(result, new TypeReference<>() {
    });
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID,
        Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailMarkForDeleteConcatTest() throws Exception {
    ReflectionTestUtils.setField(this.controller, "valueTypeAdditionForDefiningAttributes", true);
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    this.attribute.getAllowedAttributeValues().get(0).setMarkForDelete(true);

    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("allowedAttributeValuesTrim", String.valueOf(true))
            .param("sortValues", String.valueOf(true))
            .param("concatenateValueWithValueType", String.valueOf(true))).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response = OBJECT_MAPPER.readValue(result, new TypeReference<>() {
    });
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID,
        Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailMarkForDeleteConcatValueNotEmptyTest() throws Exception {
    ReflectionTestUtils.setField(this.controller, "valueTypeAdditionForDefiningAttributes", true);
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    this.attribute.getAllowedAttributeValues().get(0).setValue(StringUtils.EMPTY);

    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("allowedAttributeValuesTrim", String.valueOf(true))
            .param("concatenateValueWithValueType", String.valueOf(true))).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response = OBJECT_MAPPER.readValue(result, new TypeReference<>() {
    });
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID,
        Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailMarkForDeleteConcatValueNotTest() throws Exception {
    ReflectionTestUtils.setField(this.controller, "valueTypeAdditionForDefiningAttributes", true);
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    this.attribute.getAllowedAttributeValues().get(0).setValue(StringUtils.EMPTY);

    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("allowedAttributeValuesTrim", String.valueOf(true))
            .param("concatenateValueWithValueType", String.valueOf(false))).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response = OBJECT_MAPPER.readValue(result, new TypeReference<>() {
    });
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID,
        Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailMarkForDeleteConcatValueTypeNotEmptyTest() throws Exception {
    ReflectionTestUtils.setField(this.controller, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(this.controller, "sizeChartValueTypeDelimiter", PREDEFINED_ATTRIBUTE_CODE);
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    this.attribute.getAllowedAttributeValues().get(0).setValue(PRODUCT_CODE);
    this.attribute.getAllowedAttributeValues().get(0).setValueType(PRODUCT_CODE);

    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("allowedAttributeValuesTrim", String.valueOf(true))
            .param("concatenateValueWithValueType", String.valueOf(true))).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response = OBJECT_MAPPER.readValue(result, new TypeReference<>() {
    });
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID,
        Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailTest_predefinedAllowedAttributeValueDeleted() throws Exception {

    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    this.attribute.getPredefinedAllowedAttributeValues().get(0).setMarkForDelete(true);

    MvcResult mvcResult = this.mockMvc.perform(get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
            .andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response =
            OBJECT_MAPPER.readValue(result, new TypeReference<GdnRestSingleResponse<AttributeResponse>>() {});
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
            .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID, Boolean.FALSE);
  }

  @Test
  public void getAttributeDetailExceptionTest() throws Exception {
    this.attribute.setMandatory(false);
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.service)
        .findDetailByStoreIdAndId(eq(AttributeControllerTest.STORE_ID), eq(AttributeControllerTest.ID),
            Mockito.anyBoolean());
    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("allowedAttributeValuesTrim", Boolean.TRUE.toString())).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response =
        OBJECT_MAPPER.readValue(result, new TypeReference<GdnRestSingleResponse<AttributeResponse>>() {
        });
    verify(this.service)
        .findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID, Boolean.FALSE);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getAttributeSummary() throws Exception {
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.requestId", equalTo(AttributeControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(AttributeControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(AttributeControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(AttributeControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(AttributeControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].attributeType", equalTo(AttributeType.DEFINING_ATTRIBUTE.toString())))
        .andExpect(jsonPath("$.content[0].searchAble", equalTo(true)));

    verify(this.service).findByStoreId(AttributeControllerTest.STORE_ID, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void saveAttributeTest() throws Exception {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setName(AttributeControllerTest.NAME);
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(new Date());
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(AttributeControllerTest.ID);
    attributeRequest.setAllowedAttributeValues(new ArrayList<>());
    attributeRequest.getAllowedAttributeValues().add(new AllowedAttributeValueRequest(
        AttributeControllerTest.VALUE, 1, AttributeControllerTest.STORE_ID));
    attributeRequest.getAllowedAttributeValues().add(new AllowedAttributeValueRequest(
        AttributeControllerTest.VALUE, 2, STORE_ID));
    attributeRequest.setPredefinedAllowedAttributeValues(
        new ArrayList<>());
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1,
            AttributeControllerTest.STORE_ID));
    attributeRequest.getPredefinedAllowedAttributeValues()
    .add(new PredefinedAllowedAttributeValueRequest(AttributeControllerTest.VALUE, 2,
        STORE_ID));
    attributeRequest.setBasicView(true);
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc
        .perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.SAVE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk());
    attribute.setBasicView(true);
    verify(this.service).save(any(Attribute.class));
  }

  @Test
  public void saveAttributeWithEmptyCreatedByTest() throws Exception {
    try {
      AttributeRequest attributeRequest = new AttributeRequest();
      attributeRequest.setName(AttributeControllerTest.NAME);
      attributeRequest.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
      attributeRequest.setCreatedDate(new Date());
      attributeRequest.setSearchAble(true);
      attributeRequest.setId(AttributeControllerTest.ID);
      attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
      attributeRequest.getAllowedAttributeValues()
          .add(new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1, AttributeControllerTest.STORE_ID));
      String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
      this.mockMvc.perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.SAVE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", AttributeControllerTest.STORE_ID)
          .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
          .param("requestId", AttributeControllerTest.REQUEST_ID)
          .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void saveAttributeWithEmptyCreatedDateTest() throws Exception {
    try {
      AttributeRequest attributeRequest = new AttributeRequest();
      attributeRequest.setName(AttributeControllerTest.NAME);
      attributeRequest.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
      attributeRequest.setCreatedBy("system");
      attributeRequest.setSearchAble(true);
      attributeRequest.setId(AttributeControllerTest.ID);
      attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
      attributeRequest.getAllowedAttributeValues()
          .add(new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1, AttributeControllerTest.STORE_ID));
      String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
      this.mockMvc.perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.SAVE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", AttributeControllerTest.STORE_ID)
          .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
          .param("requestId", AttributeControllerTest.REQUEST_ID)
          .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }
  @Test
  public void saveAttributeWithPrefinedAttributeAndIsBasic() throws Exception {

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setBasicView(Boolean.FALSE);
    attributeRequest.setName(AttributeControllerTest.NAME);
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(new Date());
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(AttributeControllerTest.ID);
    attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
    attributeRequest.getAllowedAttributeValues().add(
        new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1,
            AttributeControllerTest.STORE_ID));
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc.perform(
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.SAVE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void saveAttributeWithDefiningAttributeAndIsBasic() throws Exception {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setBasicView(Boolean.FALSE);
    attributeRequest.setName(AttributeControllerTest.NAME);
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setCreatedBy("system");
    attributeRequest.setCreatedDate(new Date());
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(AttributeControllerTest.ID);
    attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
    attributeRequest.getAllowedAttributeValues().add(
        new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1,
            AttributeControllerTest.STORE_ID));
    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc.perform(
        post(AttributeApiPath.BASE_PATH + AttributeApiPath.SAVE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    this.mockMvc = standaloneSetup(this.controller).build();
    this.attribute = new Attribute();
    this.attribute.setName(AttributeControllerTest.NAME);
    this.attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    this.attribute.setSearchAble(true);
    this.attribute.setAttributeCode(ATTRIBUTE_CODE);
    this.attribute.setMandatory(true);
    this.attribute.getAllowedAttributeValues().add(
        new AllowedAttributeValue(this.attribute, AttributeControllerTest.VALUE, AttributeControllerTest.STORE_ID, 1));
    this.attribute.getAllowedAttributeValues().add(
        new AllowedAttributeValue(this.attribute, AttributeControllerTest.VALUE, AttributeControllerTest.STORE_ID, 2));
    this.attribute.getAllowedAttributeValues().get(1).setMarkForDelete(false);
    this.attribute.getPredefinedAllowedAttributeValues().add(
        new PredefinedAllowedAttributeValue(this.attribute, AttributeControllerTest.VALUE,
            AttributeControllerTest.STORE_ID, 1));
    this.attribute.getPredefinedAllowedAttributeValues().add(
        new PredefinedAllowedAttributeValue(this.attribute, AttributeControllerTest.VALUE,
            AttributeControllerTest.STORE_ID, 2));
    this.attribute.getPredefinedAllowedAttributeValues().get(0)
        .setPredefinedAllowedAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    this.attribute.getPredefinedAllowedAttributeValues().get(1)
        .setPredefinedAllowedAttributeCode(PREDEFINED_ATTRIBUTE_CODE_DEFAULT);
    this.attribute.getPredefinedAllowedAttributeValues().get(1).setMarkForDelete(false);
    this.attribute.setId(AttributeControllerTest.ID);
    this.attributeList = new ArrayList<>();
    this.attributeList.add(this.attribute);
    when(this.page.getContent()).thenReturn(this.attributeList);
    when(this.page.getTotalElements()).thenReturn((long) 0);

    when(this.service.findByAttributeType(AttributeControllerTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE,
        this.pageable)).thenReturn(this.page);

    when(this.service.findByAttributeCode(AttributeControllerTest.STORE_ID, AttributeControllerTest.ATTRIBUTE_CODE,
        this.pageable)).thenReturn(this.page);

    when(this.service.findByNameStartingWith(AttributeControllerTest.STORE_ID, AttributeControllerTest.NAME,
        this.pageable)).thenReturn(this.page);

    when(this.service.findBySearchAbleFalse(AttributeControllerTest.STORE_ID, this.pageable)).thenReturn(this.page);

    when(this.service.findBySearchAbleTrue(AttributeControllerTest.STORE_ID, this.pageable)).thenReturn(this.page);

    when(this.service.findDetailByStoreIdAndId(eq(AttributeControllerTest.STORE_ID), eq(AttributeControllerTest.ID), Mockito.anyBoolean()))
        .thenReturn(this.attribute);

    when(this.service
        .findDetailByStoreIdAndAttributeCode(eq(AttributeControllerTest.STORE_ID), eq(ATTRIBUTE_CODE)))
        .thenReturn(attribute);

    when(this.service.findByStoreId(AttributeControllerTest.STORE_ID, this.pageable)).thenReturn(this.page);
    
    when(this.service.findByNameLikeIgnoreCase(AttributeControllerTest.STORE_ID, NAME, this.pageable)).thenReturn(this.page);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.service);
    verifyNoMoreInteractions(this.productItemService);
    verifyNoMoreInteractions(this.page);
  }

  @Test
  public void updateAttributeTest() throws Exception {
    when(this.service.findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID,
        AttributeControllerTest.ID, true)).thenReturn(this.attribute);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setName(AttributeControllerTest.NAME);
    attributeRequest.setUpdatedBy("system");
    attributeRequest.setUpdatedDate(new Date());
    attributeRequest
        .setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest.setSearchAble(true);
    attributeRequest.setId(AttributeControllerTest.ID);
    attributeRequest.setAllowedAttributeValues(new ArrayList<>());
    attributeRequest.getAllowedAttributeValues().add(new AllowedAttributeValueRequest(
        AttributeControllerTest.VALUE, 1, AttributeControllerTest.STORE_ID));
    attributeRequest.getAllowedAttributeValues()
        .add(new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 2, STORE_ID));
    attributeRequest.setPredefinedAllowedAttributeValues(
        new ArrayList<>());
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1,
            AttributeControllerTest.STORE_ID));
    attributeRequest.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValueRequest(AttributeControllerTest.VALUE, 2, STORE_ID));

    String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
    this.mockMvc.perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", AttributeControllerTest.STORE_ID)
        .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
        .param("requestId", AttributeControllerTest.REQUEST_ID)
        .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    verify(this.service).findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID, Boolean.TRUE);
    verify(this.service).regenerateAllowedAttributeValue(eq(STORE_ID), eq(attribute),
        attributeArgumentCaptor.capture());
    Assertions.assertEquals(VALUE,
        attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).getValue());
    Assertions.assertEquals(VALUE,
        attributeArgumentCaptor.getValue().getPredefinedAllowedAttributeValues().get(0).getValue());
  }

  @Test
  public void updateAttributeWithEmptyUpdatedByTest() throws Exception {
    try {
      AttributeRequest attributeRequest = new AttributeRequest();
      attributeRequest.setName(AttributeControllerTest.NAME);
      attributeRequest.setUpdatedDate(new Date());
      attributeRequest.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
      attributeRequest.setSearchAble(true);
      attributeRequest.setId(AttributeControllerTest.ID);
      attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
      attributeRequest.getAllowedAttributeValues()
          .add(new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1, AttributeControllerTest.STORE_ID));
      String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
      this.mockMvc.perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", AttributeControllerTest.STORE_ID)
          .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
          .param("requestId", AttributeControllerTest.REQUEST_ID));
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void updateAttributeWithEmptyUpdatedDateTest() throws Exception {
    try {
      AttributeRequest attributeRequest = new AttributeRequest();
      attributeRequest.setName(AttributeControllerTest.NAME);
      attributeRequest.setUpdatedBy("system");
      attributeRequest.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
      attributeRequest.setSearchAble(true);
      attributeRequest.setId(AttributeControllerTest.ID);
      attributeRequest.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueRequest>());
      attributeRequest.getAllowedAttributeValues()
          .add(new AllowedAttributeValueRequest(AttributeControllerTest.VALUE, 1, AttributeControllerTest.STORE_ID));
      String request = AttributeControllerTest.OBJECT_MAPPER.writeValueAsString(attributeRequest);
      this.mockMvc.perform(post(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", AttributeControllerTest.STORE_ID)
          .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
          .param("requestId", AttributeControllerTest.REQUEST_ID));
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }


  @Test
  public void uploadAttributeDefiningAttributeTest() throws Exception {
    Attribute savedAttributeWarna =
        new Attribute("Warna", AttributeType.DEFINING_ATTRIBUTE, true, AttributeControllerTest.STORE_ID);
    savedAttributeWarna.setAllowedAttributeValues(new ArrayList<>());
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue(savedAttributeWarna, VALUE, STORE_ID, 1);
    savedAttributeWarna.getAllowedAttributeValues().add(allowedAttributeValue);
    String id1 = "ID1";
    savedAttributeWarna.setId(id1);

    when(this.service.save(any(Attribute.class))).thenReturn(id1);
    when(this.service.findDetailByStoreIdAndId(eq(AttributeControllerTest.STORE_ID), anyString(), eq(false)))
        .thenReturn(savedAttributeWarna);
    InputStream in = this.getClass().getResourceAsStream("/Attribute.csv");
    MockMultipartFile file = new MockMultipartFile("attributeCsvFile", in);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(AttributeApiPath.BASE_PATH + "/upload").file(file)
        .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
        .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
        .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());

    verify(this.service).save(attributeArgumentCaptor.capture());
    verify(this.service).findDetailByStoreIdAndId(eq(AttributeControllerTest.STORE_ID), eq(id1), eq(false));
    Assertions.assertEquals("Warna", attributeArgumentCaptor.getValue().getName());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE, attributeArgumentCaptor.getValue().getAttributeType());
    Assertions.assertEquals("Merah", attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).getValue());
    Assertions.assertTrue(attributeArgumentCaptor.getValue().isSearchAble());
  }

  @Test
  public void getAttributeDetailByIdTest() throws Exception {
    when(this.service.findDetailByStoreIdAndIdAndValue(STORE_ID, ID, VALUE, false)).thenReturn(new AttributeResponse());
    this.mockMvc.perform(
            get(AttributeApiPath.BASE_PATH + AttributeApiPath.FIND_ATTRIBUTE_DETAIL_BY_ID_AND_VALUE, ID, VALUE).accept(MediaType.APPLICATION_JSON)
                .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.service).findDetailByStoreIdAndIdAndValue(STORE_ID, ID, VALUE, false);
  }

  @Test
  public void getAttributeDetailByIdErrorTest() throws Exception {
    when(this.service.findDetailByStoreIdAndIdAndValue(STORE_ID, ID, VALUE, false)).thenThrow(
        ApplicationRuntimeException.class);
    this.mockMvc.perform(
            get(AttributeApiPath.BASE_PATH + AttributeApiPath.FIND_ATTRIBUTE_DETAIL_BY_ID_AND_VALUE, ID, VALUE).accept(MediaType.APPLICATION_JSON)
                .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).findDetailByStoreIdAndIdAndValue(STORE_ID, ID, VALUE, false);
  }

  @Test
  public void getAttributeDetailByCategoryCodeWithoutOptionsTest() throws Exception {
    when(this.service.getAttributeDetailByCategoryCodeWithoutOptions(STORE_ID, CATEGORY_CODE))
        .thenReturn(this.listAttributeSummaryBuilder());
    this.mockMvc
        .perform(get(AttributeApiPath.BASE_PATH+ AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE_WITHOUT_OPTIONS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("categoryCode", CATEGORY_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.service).getAttributeDetailByCategoryCodeWithoutOptions(STORE_ID, CATEGORY_CODE);
  }
  
  @Test
  public void getAttributeDetailByCategoryCodeWithoutOptionsTest_Exception_Fail() throws Exception{
    when(this.service.getAttributeDetailByCategoryCodeWithoutOptions(STORE_ID, CATEGORY_CODE)).thenThrow(Exception.class);
      this.mockMvc
      .perform(get(AttributeApiPath.BASE_PATH+ AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE_WITHOUT_OPTIONS)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", AttributeControllerTest.STORE_ID)
          .param("channelId", AttributeControllerTest.CHANNEL_ID)
          .param("clientId", AttributeControllerTest.CLIENT_ID)
          .param("requestId", AttributeControllerTest.REQUEST_ID)
          .param("username", AttributeControllerTest.DEFAULT_USERNAME)
          .param("categoryCode", CATEGORY_CODE))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
      verify(this.service).getAttributeDetailByCategoryCodeWithoutOptions(STORE_ID, CATEGORY_CODE);
    
  }
  
  private List<AttributeSummaryDTO> listAttributeSummaryBuilder() {
    AttributeSummaryDTO attr =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, true, true, false,
            false, false, true);
    AttributeSummaryDTO attr2 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE2_NAME, false, true, false,
            false, false, false);
    AttributeSummaryDTO attr3 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.DESCRIPTIVE_ATTRIBUTE, ATTRIBUTE3_NAME, true, true, false,
            false, false, false);
    List<AttributeSummaryDTO> attrList = new ArrayList<>();
    attrList.add(attr);
    attrList.add(attr2);
    attrList.add(attr3);
    return attrList;
  }

  @Test
  public void uploadAttributePreDefiningAttributeTest() throws Exception {
    Attribute savedAttributeWarna =
        new Attribute("size", AttributeType.PREDEFINED_ATTRIBUTE, true, AttributeControllerTest.STORE_ID);
    String id1 = "ID1";
    savedAttributeWarna.setId(id1);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue(savedAttributeWarna, VALUE, STORE_ID, 1);
    savedAttributeWarna.setPredefinedAllowedAttributeValues(new ArrayList<>());
    savedAttributeWarna.getPredefinedAllowedAttributeValues().add(predefinedAllowedAttributeValue);
    when(this.service.save(any(Attribute.class))).thenReturn(id1);
    when(this.service.findDetailByStoreIdAndId(eq(AttributeControllerTest.STORE_ID), eq(id1), eq(false))).thenReturn(savedAttributeWarna);

    InputStream in = this.getClass().getResourceAsStream("/Attribute2.csv");
    MockMultipartFile file = new MockMultipartFile("attributeCsvFile", in);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(AttributeApiPath.BASE_PATH + "/upload").file(file)
        .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
        .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
        .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());

    verify(this.service).save(attributeArgumentCaptor.capture());
    verify(this.service).findDetailByStoreIdAndId(eq(AttributeControllerTest.STORE_ID), eq(id1), eq(Boolean.FALSE));
    Assertions.assertEquals("size", attributeArgumentCaptor.getValue().getName());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE, attributeArgumentCaptor.getValue().getAttributeType());
    Assertions.assertEquals("Merah",
        attributeArgumentCaptor.getValue().getPredefinedAllowedAttributeValues().get(0).getValue());
    Assertions.assertTrue(attributeArgumentCaptor.getValue().isSearchAble());
  }

  @Test
  public void getAttributeDetailAndValuesByAttributeCodeTest() throws Exception {
    this.attribute.setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValue>());
    this.attribute.getPredefinedAllowedAttributeValues().add(new PredefinedAllowedAttributeValue());
    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/attributeCode/" + ATTRIBUTE_CODE + "/detail")
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    GdnRestSingleResponse<AttributeResponse> response =
        OBJECT_MAPPER.readValue(result, new TypeReference<GdnRestSingleResponse<AttributeResponse>>() {
        });
    for (AllowedAttributeValueResponse allowedValue : response.getValue().getAllowedAttributeValues()) {
      Assertions.assertFalse(allowedValue.isMarkForDelete());
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : response.getValue()
        .getPredefinedAllowedAttributeValues()) {
      Assertions.assertFalse(predefinedAllowedValue.isMarkForDelete());
    }
    verify(service)
        .findDetailByStoreIdAndAttributeCode(AttributeControllerTest.STORE_ID, ATTRIBUTE_CODE) ;
  }

  @Test
  public void getMandatoryPredefinedAttributeDetailExceptionTest() throws Exception {
    when(this.service
        .findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID, Boolean.FALSE)).thenThrow(RuntimeException.class);
    this.attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    MvcResult mvcResult = this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + "/" + AttributeControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID).param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk()).andReturn();
    String result = mvcResult.getResponse().getContentAsString();
    verify(this.service)
        .findDetailByStoreIdAndId(AttributeControllerTest.STORE_ID, AttributeControllerTest.ID, Boolean.FALSE);
  }

  @Test
  public void getAttributeValuesByProductCodeAndAttributeCodeTest() throws Exception {
    when(this.service.findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        ATTRIBUTE_CODE)).thenReturn(ATTRIBUTE_VALUE);
    this.mockMvc.perform(
            get(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_ATTRIBUTE_VALUES).accept(MediaType.APPLICATION_JSON)
                .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME).param("productCode", PRODUCT_CODE)
                .param("attributeCode", ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.service).findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        ATTRIBUTE_CODE);
  }

  @Test
  public void getAttributeValuesByProductCodeAndAttributeCodeExceptionTest() throws Exception {
    when(this.service.findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        ATTRIBUTE_CODE)).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
            get(AttributeApiPath.BASE_PATH + AttributeApiPath.GET_ATTRIBUTE_VALUES).accept(MediaType.APPLICATION_JSON)
                .param("storeId", AttributeControllerTest.STORE_ID).param("channelId", AttributeControllerTest.CHANNEL_ID)
                .param("clientId", AttributeControllerTest.CLIENT_ID).param("requestId", AttributeControllerTest.REQUEST_ID)
                .param("username", AttributeControllerTest.DEFAULT_USERNAME).param("productCode", PRODUCT_CODE)
                .param("attributeCode", ATTRIBUTE_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        ATTRIBUTE_CODE);
  }

  @Test
  public void getCategoryCodesByAttributeCodeTest() throws Exception {
    List<String> stringList = Arrays.asList(CATEGORY_CODE, CATEGORY_CODE_2);
    when(this.service.findCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(
        stringList);
    this.mockMvc.perform(
        get(AttributeApiPath.BASE_PATH + AttributeApiPath.CATEGORIES_BY_ATTRIBUTE_CODE).param(
                "storeId", AttributeControllerTest.STORE_ID)
            .param("channelId", AttributeControllerTest.CHANNEL_ID)
            .param("clientId", AttributeControllerTest.CLIENT_ID)
            .param("requestId", AttributeControllerTest.REQUEST_ID)
            .param("username", AttributeControllerTest.DEFAULT_USERNAME)
            .param("attributeCode", ATTRIBUTE_CODE)).andExpect(status().isOk());
    verify(this.service).findCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
  }
}
