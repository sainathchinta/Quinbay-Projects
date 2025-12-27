package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ProductOptionalParameterRequest;
import com.gdn.x.productcategorybase.dto.response.ProductOptionalParameterResponse;
import com.gdn.x.productcategorybase.entity.ProductOptionalParameter;
import com.gdn.x.productcategorybase.service.ProductOptionalParameterService;


public class ProductOptionalParameterControllerTest {
  private static final String PRODUCT_CODE = "GDN-01";
  private static final String NAME = "name";
  private static final boolean UNIQUE_TRUE = true;
  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final int TOTAL_RECORDS = 1;
  private static final String ID = "ID";
  private static final Date DATE_NOW = new Date(142604);
  private static final String USER = "user";
  private final ObjectMapper objectMapper = new ObjectMapper();

  private final String RESULT_JSON_RESPONSE =
      "{\"id\":\"ID\",\"storeId\":\"10001\",\"createdDate\":null,\"createdBy\":null,\"updatedDate\":null,\"updatedBy\":null,\"version\":null,\"markForDelete\":false,\"productCode\":\"GDN-01\",\"name\":\"name\",\"unique\":true}";

  private final String RESULT_JSON_REQUEST =
      "{\"id\":\"ID\",\"storeId\":\"10001\",\"version\":null,\"createdDate\":142604,\"createdBy\":\"user\",\"updatedDate\":null,\"updatedBy\":null,\"markForDelete\":false,\"productCode\":\"GDN-01\",\"name\":\"name\",\"unique\":true}";

  private ProductOptionalParameter productOptionalParameter;
  private ProductOptionalParameterRequest request;
  private ProductOptionalParameterResponse response;
  private MockMvc mockMvc;
  private String jsonReq;
  private Pageable pageable;

  @InjectMocks
  private ProductOptionalParameterController controller;

  @Mock
  ProductOptionalParameterService service;

  private Page<ProductOptionalParameter> page;

  @Test
  public void getProductOptionalParameterByName() throws Exception {
    Mockito.when(this.service.findByNameLike(ProductOptionalParameterControllerTest.STORE_ID,
        ProductOptionalParameterControllerTest.NAME, this.pageable)).thenReturn(this.page);
    this.mockMvc
        .perform(get(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.FILTER_NAME)
            .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
            .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
            .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
            .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
            .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME)
            .param("name", ProductOptionalParameterControllerTest.NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductOptionalParameterControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductOptionalParameterControllerTest.PAGE_NUMBER)))
        .andExpect(
            jsonPath("$.pageMetaData.totalRecords", equalTo(ProductOptionalParameterControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductOptionalParameterControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductOptionalParameterControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].unique", equalTo(ProductOptionalParameterControllerTest.UNIQUE_TRUE)));
    verify(this.service).findByNameLike(ProductOptionalParameterControllerTest.STORE_ID,
        ProductOptionalParameterControllerTest.NAME, this.pageable);
  }

  @Test
  public void getProductOptionalParameterByPrdocutCode() throws Exception {
    Mockito.when(this.service.findByProductCodeLike(ProductOptionalParameterControllerTest.STORE_ID,
        ProductOptionalParameterControllerTest.PRODUCT_CODE, this.pageable)).thenReturn(this.page);
    this.mockMvc
        .perform(
            get(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.FILTER_PRODUCT_CODE)
                .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
                .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
                .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
                .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
                .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME)
                .param("productCode", ProductOptionalParameterControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductOptionalParameterControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductOptionalParameterControllerTest.PAGE_NUMBER)))
        .andExpect(
            jsonPath("$.pageMetaData.totalRecords", equalTo(ProductOptionalParameterControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductOptionalParameterControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductOptionalParameterControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].unique", equalTo(ProductOptionalParameterControllerTest.UNIQUE_TRUE)));
    verify(this.service).findByProductCodeLike(ProductOptionalParameterControllerTest.STORE_ID,
        ProductOptionalParameterControllerTest.PRODUCT_CODE, this.pageable);
  }

  @Test
  public void getProductOptionalParameterSummary() throws Exception {
    Mockito.when(this.service.findByStoreId(ProductOptionalParameterControllerTest.STORE_ID, this.pageable))
        .thenReturn(this.page);
    this.mockMvc
        .perform(get(ProductOptionalParameterController.BASE_PATH)
            .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
            .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
            .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
            .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
            .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductOptionalParameterControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductOptionalParameterControllerTest.PAGE_NUMBER)))
        .andExpect(
            jsonPath("$.pageMetaData.totalRecords", equalTo(ProductOptionalParameterControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].productCode", equalTo(ProductOptionalParameterControllerTest.PRODUCT_CODE)))
        .andExpect(jsonPath("$.content[0].name", equalTo(ProductOptionalParameterControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].unique", equalTo(ProductOptionalParameterControllerTest.UNIQUE_TRUE)));
    verify(this.service).findByStoreId(ProductOptionalParameterControllerTest.STORE_ID, this.pageable);
  }

  @Test
  public void markFordeleteOptionalParameterTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder = new SimpleRequestHolder(ProductOptionalParameterControllerTest.ID);
    String request = this.objectMapper.writeValueAsString(simpleRequestHolder);
    this.mockMvc
        .perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.DELETE)
            .contentType(MediaType.APPLICATION_JSON).content(request).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
            .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
            .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
            .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
            .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductOptionalParameterControllerTest.REQUEST_ID))).andReturn();

    verify(this.service, Mockito.times(1))
        .markForDeleteProductOptionalParameter(ProductOptionalParameterControllerTest.ID);
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();
    this.request = new ProductOptionalParameterRequest();
    this.response = new ProductOptionalParameterResponse();
    this.productOptionalParameter = new ProductOptionalParameter(ProductOptionalParameterControllerTest.PRODUCT_CODE,
        ProductOptionalParameterControllerTest.NAME, ProductOptionalParameterControllerTest.UNIQUE_TRUE,
        ProductOptionalParameterControllerTest.STORE_ID);
    this.productOptionalParameter.setId(ProductOptionalParameterControllerTest.ID);
    BeanUtils.copyProperties(this.productOptionalParameter, this.request);
    BeanUtils.copyProperties(this.productOptionalParameter, this.response);
    this.request.setCreatedBy(ProductOptionalParameterControllerTest.USER);
    this.request.setCreatedDate(ProductOptionalParameterControllerTest.DATE_NOW);
    this.jsonReq = this.objectMapper.writeValueAsString(this.request);
    this.pageable = PageRequest.of(ProductOptionalParameterControllerTest.PAGE_NUMBER,
        ProductOptionalParameterControllerTest.PAGE_SIZE);
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    this.page = new PageImpl<ProductOptionalParameter>(productOptionalParameters);
  }

  @Test
  public void testCopyPropertiesRequest() throws Exception {
    Assertions.assertEquals(this.RESULT_JSON_REQUEST, this.objectMapper.writeValueAsString(this.request));
  }

  @Test
  public void testCopyPropertiesResponse() throws Exception {
    Assertions.assertEquals(this.RESULT_JSON_RESPONSE, this.objectMapper.writeValueAsString(this.response));
  }

  @Test
  public void testSavingProductOptionalParameter() throws Exception {
    Mockito.when(this.service.save(this.productOptionalParameter)).thenReturn("");
    this.mockMvc
        .perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.SAVE)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
            .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
            .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
            .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
            .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductOptionalParameterControllerTest.REQUEST_ID))).andReturn();
    verify(this.service, Mockito.times(1)).save(Mockito.any(ProductOptionalParameter.class));
  }

  @Test
  public void testSavingProductOptionalParameterWithEmptyCreatedBy() throws Exception {
    this.request.setCreatedBy(null);
    this.jsonReq = this.objectMapper.writeValueAsString(this.request);
    try {
      this.mockMvc
          .perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.SAVE)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
              .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
              .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
              .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
              .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
              .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductOptionalParameterControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void testSavingProductOptionalParameterWithEmptyCreatedDate() throws Exception {
    this.request.setCreatedDate(null);
    this.jsonReq = this.objectMapper.writeValueAsString(this.request);
    try {
      this.mockMvc
          .perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.SAVE)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq).accept(MediaType.APPLICATION_JSON)
              .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
              .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
              .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
              .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
              .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(ProductOptionalParameterControllerTest.REQUEST_ID))).andReturn();
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void TestUpdateProductOptionalParameter() throws Exception {
    this.request.setUpdatedBy(ProductOptionalParameterControllerTest.USER);
    this.request.setUpdatedDate(ProductOptionalParameterControllerTest.DATE_NOW);
    Mockito.when(this.service.findById(ProductOptionalParameterControllerTest.ID))
        .thenReturn(this.productOptionalParameter);
    this.mockMvc.perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.UPDATE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.objectMapper.writeValueAsString(this.request))
        .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
        .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
        .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
        .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
        .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).findById(ProductOptionalParameterControllerTest.ID);
    verify(this.service, Mockito.times(1)).update(this.productOptionalParameter);
  }

  @Test
  public void TestUpdateProductOptionalParameterWithEmptyUpdatedBy() throws Exception {
    this.request.setUpdatedDate(ProductOptionalParameterControllerTest.DATE_NOW);
    Mockito.when(this.service.findById(ProductOptionalParameterControllerTest.ID))
        .thenReturn(this.productOptionalParameter);
    try {
      this.mockMvc
          .perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.UPDATE)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(this.request))
              .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
              .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
              .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
              .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
              .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk());
      Mockito.verify(this.service).findById(ProductOptionalParameterControllerTest.ID);
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void TestUpdateProductOptionalParameterWithEmptyUpdatedDate() throws Exception {
    this.request.setUpdatedBy(ProductOptionalParameterControllerTest.USER);
    Mockito.when(this.service.findById(ProductOptionalParameterControllerTest.ID))
        .thenReturn(this.productOptionalParameter);
    try {
      this.mockMvc
          .perform(post(ProductOptionalParameterController.BASE_PATH + ProductOptionalParameterController.UPDATE)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(this.request))
              .param("storeId", ProductOptionalParameterControllerTest.STORE_ID)
              .param("channelId", ProductOptionalParameterControllerTest.CHANNEL_ID)
              .param("clientId", ProductOptionalParameterControllerTest.CLIENT_ID)
              .param("requestId", ProductOptionalParameterControllerTest.REQUEST_ID)
              .param("username", ProductOptionalParameterControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk());
      Mockito.verify(this.service).findById(ProductOptionalParameterControllerTest.ID);
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }
}
