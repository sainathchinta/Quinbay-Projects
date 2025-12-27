package com.gdn.partners.pbp.controller.tools;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.service.tools.ProductCollectionToolsService;
import com.gdn.x.base.controller.GlobalControllerAdvice;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;

public class ProductCollectionToolsControllerTest {

  @InjectMocks
  private ProductCollectionToolsController productCollectionToolsController;
  @Mock
  private ProductCollectionToolsService productCollectionToolsService;
  
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String SPECIFIED_STATE = "DRAFT";
  private static final String PRODUCT_CODE = "MTA-123456";
  private static final String CATEGORY_CODE = "CAT-1234";
  
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private List<String> reqBody;
  
  @BeforeEach
  public void setup() throws Exception {
    initMocks(this);
    
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productCollectionToolsController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).setControllerAdvice(new GlobalControllerAdvice()).build();
    this.objectMapper = new ObjectMapper();
    
    reqBody = Arrays.asList(PRODUCT_CODE);
    
    Mockito.doNothing().when(productCollectionToolsService)
      .syncState(this.reqBody, SPECIFIED_STATE, STORE_ID, USERNAME);
  }
  
  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(productCollectionToolsService);
  }

  @Test
  public void syncState_Valid_Success() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.UPDATE_STATE)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("specifiedState", SPECIFIED_STATE).build();
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .content(this.objectMapper.writeValueAsString(this.reqBody)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    
    Mockito.verify(productCollectionToolsService)
      .syncState(this.reqBody, SPECIFIED_STATE, STORE_ID, USERNAME);
  }
  
  @Test
  public void syncState_NullProductCodes_ReturnFalse() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.UPDATE_STATE)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("specifiedState", SPECIFIED_STATE).build();
    
    this.reqBody = new ArrayList<>();
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .content(this.objectMapper.writeValueAsString(this.reqBody)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    
    Mockito.verify(productCollectionToolsService, Mockito.never())
      .syncState(this.reqBody, SPECIFIED_STATE, STORE_ID, USERNAME);
  }
  
  @Test
  public void moveCategoryByProductCode_Valid_Success() throws Exception {
    Mockito.doNothing().when(productCollectionToolsService).moveProductCollectionCategory(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.MOVE_CATEGORY)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("productCode", PRODUCT_CODE)
            .addParameter("categoryCode", CATEGORY_CODE)
            .build();
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    
    Mockito.verify(productCollectionToolsService).moveProductCollectionCategory(REQUEST_ID, 
        USERNAME, STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
  }
  
  @Test
  public void moveCategoryByProductCode_ProductCodeEmpty_Success() throws Exception {
    Mockito.doNothing().when(productCollectionToolsService).moveProductCollectionCategory(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.MOVE_CATEGORY)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("productCode", "")
            .addParameter("categoryCode", CATEGORY_CODE)
            .build();
    try{
      this.mockMvc
          .perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch(Exception e){
      Mockito.verify(productCollectionToolsService, Mockito.never()).moveProductCollectionCategory(REQUEST_ID, 
          USERNAME, STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      throw e;
    }
  }
  
  @Test
  public void moveCategoryByProductCode_CategoryCodeEmpty_Success() throws Exception {
    Mockito.doNothing().when(productCollectionToolsService).moveProductCollectionCategory(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.MOVE_CATEGORY)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("productCode", PRODUCT_CODE)
            .addParameter("categoryCode", "")
            .build();
    try{
      this.mockMvc
          .perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch(Exception e){
      Mockito.verify(productCollectionToolsService, Mockito.never()).moveProductCollectionCategory(REQUEST_ID, 
          USERNAME, STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      throw e;
    }
  }
  
  @Test
  public void addProductAttributes_Valid_Success() throws Exception {
    Mockito.when(productCollectionToolsService.addProductAttributesByProductCode(Mockito.anyString(), 
        Mockito.anyString(), Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.ADD_PRD_ATTRIBUTES_BY_PRD_CODE)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .build();
    AddProductAttributesRequest response = new AddProductAttributesRequest();
    response.setProductCode(PRODUCT_CODE);
    this.mockMvc
      .perform(post(uri)
          .accept(MediaType.APPLICATION_JSON_VALUE)
          .contentType(MediaType.APPLICATION_JSON)
          .content(this.objectMapper.writeValueAsString(response)))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(jsonPath("$.success", equalTo(true)));
    
    
    Mockito.verify(productCollectionToolsService).addProductAttributesByProductCode(
        Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }
  
  @Test
  public void addProductAttributes_ProductCodeIsNull_False() throws Exception {
    Mockito.when(productCollectionToolsService.addProductAttributesByProductCode(Mockito.anyString(), 
        Mockito.anyString(), Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    URI uri =
        new URIBuilder()
            .setPath(ProductCollectionToolsPath.ADD_PRD_ATTRIBUTES_BY_PRD_CODE)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .build();
    AddProductAttributesRequest response = new AddProductAttributesRequest();
    this.mockMvc
      .perform(post(uri)
          .accept(MediaType.APPLICATION_JSON_VALUE)
          .contentType(MediaType.APPLICATION_JSON)
          .content(this.objectMapper.writeValueAsString(response)))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(jsonPath("$.success", equalTo(false)));
    
    
    Mockito.verify(productCollectionToolsService, Mockito.never()).addProductAttributesByProductCode(
        Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }
  
}
