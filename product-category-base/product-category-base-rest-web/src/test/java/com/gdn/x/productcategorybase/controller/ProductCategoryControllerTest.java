package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ProductCategoryApiPath;
import com.gdn.x.productcategorybase.dto.AddProductAttributesDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryDTO;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.service.MapperService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;

public class ProductCategoryControllerTest {
  
  @InjectMocks
  private ProductCategoryController prdCategoryController;
  
  @Mock
  private ProductCategoryService prdCategoryService;

  @Mock
  private MapperService mapperService;

  
  private MockMvc mockMvc;
  private static ObjectMapper mapper = new ObjectMapper();
  
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";
  
  private static final String PRODUCT_CODE = "MTA-1234";
  private static final String CATEGORY_CODE = "CAT-1234";
  
  
  @BeforeEach
  public void setup() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.prdCategoryController).build();
  }
  
  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(prdCategoryService);
  }
  
  @Test
  public void movePrdCategoryByPrdCode_Valid_Success() throws Exception {
    CategorySummaryDTO response = new CategorySummaryDTO();
    Mockito.when(prdCategoryService.movePrdCategoryByProductCode(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    
    this.mockMvc.perform(get(ProductCategoryApiPath.MOVE_CATEGORY_BY_PRD_CODE)
        .accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("channelId", CHANNEL_ID)
          .param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID)
          .param("username", DEFAULT_USERNAME)
          .param("productCode", PRODUCT_CODE)
          .param("categoryCode", CATEGORY_CODE))
          .andExpect(status().isOk());
    Mockito.verify(prdCategoryService).movePrdCategoryByProductCode(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString());
  }
  
  @Test
  public void movePrdCategoryByPrdCode_NullProductCode_False() throws Exception {
    CategorySummaryDTO response = new CategorySummaryDTO();
    Mockito.when(prdCategoryService.movePrdCategoryByProductCode(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    
    try{
      this.mockMvc.perform(get(ProductCategoryApiPath.MOVE_CATEGORY_BY_PRD_CODE)
          .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME)
            .param("productCode", "")
            .param("categoryCode", CATEGORY_CODE))
            .andExpect(status().isOk());
    } catch(Exception e){
      Mockito.verify(prdCategoryService, Mockito.never()).movePrdCategoryByProductCode(Mockito.anyString(), 
          Mockito.anyString(), Mockito.anyString());
    }
  }
  
  @Test
  public void movePrdCategoryByPrdCode_NulLCategory_False() throws Exception {
    CategorySummaryDTO response = new CategorySummaryDTO();
    Mockito.when(prdCategoryService.movePrdCategoryByProductCode(Mockito.anyString(), 
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    try{
      this.mockMvc.perform(get(ProductCategoryApiPath.MOVE_CATEGORY_BY_PRD_CODE)
          .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", DEFAULT_USERNAME)
            .param("productCode", PRODUCT_CODE)
            .param("categoryCode", ""))
            .andExpect(status().isOk());
    } catch(Exception e){
      Mockito.verify(prdCategoryService, Mockito.never()).movePrdCategoryByProductCode(Mockito.anyString(), 
          Mockito.anyString(), Mockito.anyString());
    }
  }
  
  @Test
  public void addProductAttributesByProductCode() throws Exception {
    ProductAttribute prdAttrSummary = new ProductAttribute();
    Attribute attr = new Attribute();
    attr.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    prdAttrSummary.setAttribute(attr);
    prdAttrSummary.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    ProductAttributeValue prdAttrValue = new ProductAttributeValue();
    prdAttrValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    prdAttrValue.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    prdAttrSummary.getProductAttributeValues().add(prdAttrValue);
    
    Mockito.when(prdCategoryService.addProductAttributes(Mockito.anyString(), 
        (AddProductAttributesDTO) Mockito.any())).thenReturn(Arrays.asList(prdAttrSummary));
    
    AddProductAttributesRequest request = new AddProductAttributesRequest();
    request.setProductCode(PRODUCT_CODE);
    Mockito.when(mapperService.mapBean(Mockito.any(), Mockito.eq(AddProductAttributesDTO.class)))
      .thenReturn(new AddProductAttributesDTO());
    
    this.mockMvc.perform(post(ProductCategoryApiPath.ADD_PRD_ATTRIBUTES_BY_PRD_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(mapper.writeValueAsString(request))
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk());
    
    Mockito.verify(prdCategoryService).addProductAttributes(Mockito.anyString(), 
        (AddProductAttributesDTO) Mockito.any());
    Mockito.verify(mapperService).mapBean(Mockito.any(), Mockito.eq(AddProductAttributesDTO.class));
  }

  @Test
  public void getMasterParentCategoriesByProductCodeTest() throws Exception {

    Mockito.when(this.prdCategoryService
        .getMasterParentCategoryByProductCode(STORE_ID, ProductCategoryControllerTest.PRODUCT_CODE))
        .thenReturn(new ArrayList<Category>());
    this.mockMvc.perform(
        get(ProductCategoryApiPath.GET_MASTER_PARENT_CATEGORY_RESPONSE_BY_PRODUCT_CODE)
            .param("storeId", ProductCategoryControllerTest.STORE_ID)
            .param("channelId", ProductCategoryControllerTest.CHANNEL_ID)
            .param("clientId", ProductCategoryControllerTest.CLIENT_ID)
            .param("requestId", ProductCategoryControllerTest.REQUEST_ID)
            .param("username", ProductCategoryControllerTest.DEFAULT_USERNAME)
            .param("productCode", ProductCategoryControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(StringUtils.EMPTY)))
        .andExpect(jsonPath("$.errorCode", equalTo(StringUtils.EMPTY)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductCategoryControllerTest.REQUEST_ID)));
    Mockito.verify(this.prdCategoryService)
        .getMasterParentCategoryByProductCode(STORE_ID, ProductCategoryControllerTest.PRODUCT_CODE);
  }

  @Test
  public void getMasterParentCategoriesByProductCode_whenExceptionTest() throws Exception {

    Mockito.doThrow(new RuntimeException()).when(this.prdCategoryService)
        .getMasterParentCategoryByProductCode(STORE_ID, ProductCategoryControllerTest.PRODUCT_CODE);
    this.mockMvc.perform(
        get(ProductCategoryApiPath.GET_MASTER_PARENT_CATEGORY_RESPONSE_BY_PRODUCT_CODE)
            .param("storeId", ProductCategoryControllerTest.STORE_ID)
            .param("channelId", ProductCategoryControllerTest.CHANNEL_ID)
            .param("clientId", ProductCategoryControllerTest.CLIENT_ID)
            .param("requestId", ProductCategoryControllerTest.REQUEST_ID)
            .param("username", ProductCategoryControllerTest.DEFAULT_USERNAME)
            .param("productCode", ProductCategoryControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductCategoryControllerTest.REQUEST_ID)));
    Mockito.verify(this.prdCategoryService)
        .getMasterParentCategoryByProductCode(STORE_ID, ProductCategoryControllerTest.PRODUCT_CODE);
  }

  @Test
  public void getMasterParentCategoriesByProductCode_whenApplicationRuntimeExceptionTest() throws Exception {

    Mockito.doThrow(new ApplicationRuntimeException()).when(this.prdCategoryService)
        .getMasterParentCategoryByProductCode(STORE_ID, ProductCategoryControllerTest.PRODUCT_CODE);
    this.mockMvc.perform(
        get(ProductCategoryApiPath.GET_MASTER_PARENT_CATEGORY_RESPONSE_BY_PRODUCT_CODE)
            .param("storeId", ProductCategoryControllerTest.STORE_ID)
            .param("channelId", ProductCategoryControllerTest.CHANNEL_ID)
            .param("clientId", ProductCategoryControllerTest.CLIENT_ID)
            .param("requestId", ProductCategoryControllerTest.REQUEST_ID)
            .param("username", ProductCategoryControllerTest.DEFAULT_USERNAME)
            .param("productCode", ProductCategoryControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductCategoryControllerTest.REQUEST_ID)));
    Mockito.verify(this.prdCategoryService)
        .getMasterParentCategoryByProductCode(STORE_ID, ProductCategoryControllerTest.PRODUCT_CODE);
  }

}
