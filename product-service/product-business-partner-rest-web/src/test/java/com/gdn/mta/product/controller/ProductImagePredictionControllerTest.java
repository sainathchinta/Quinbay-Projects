package com.gdn.mta.product.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.PredictionCategoryMappingUpdateRequest;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionRequest;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.service.ProductImagePredictionService;
import com.gdn.mta.product.service.ProductImagePredictionServiceWrapper;
import com.gdn.mta.product.web.model.ProductImagePredictionControllerPath;

public class ProductImagePredictionControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String PREDICTION_TYPE = "predictionType";
  private static final String PRODUCT_CODE = "productCode";
  private static final String DISPLAY_NAME = "name";
  private static final String DISPLAY_NAME_IN = "nameIn";
  private static final String ROOT = "/";
  private static final String BRAND_CODE = "BRD-007";
  private static final String BRAND_APPROVAL_STATUS = BrandApprovalStatus.APPROVED.name();
  private static final int CONFIDENCE_THRESHOLD = 30;
  private static final int TEXT_CONFIDENCE_THRESHOLD = 30;
  private static final String CATEGORY_CODE = "categoryCode";

  private ProductImagePredictionRequest productImagePredictionRequest;
  private ProductImagePredictionResponse productImagePredictionResponse;
  private ProductImagePrediction productImagePrediction;
  private ImageQcProcessedResponse imageQcProcessedResponse;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductImagePredictionController productImagePredictionController;

  @Mock
  private ProductImagePredictionService productImagePredictionService;

  @Mock
  private ProductImagePredictionServiceWrapper productImagePredictionServiceWrapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.productImagePredictionController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));

    productImagePredictionRequest = new ProductImagePredictionRequest();
    productImagePredictionRequest.setConfidenceThreshold(10);
    productImagePredictionRequest.setPredictionWeightage(64);
    productImagePredictionRequest.setPredictionType(PREDICTION_TYPE);
    productImagePredictionRequest.setDisplayName(DISPLAY_NAME);
    productImagePredictionRequest.setDisplayNameIn(DISPLAY_NAME_IN);
    productImagePredictionRequest.setForceReview(true);
    productImagePredictionRequest.setNeedRevisionConfidenceThreshold(53);
    productImagePredictionRequest.setNeedRevisionEnabled(true);

    productImagePrediction = new ProductImagePrediction();
    BeanUtils.copyProperties(productImagePredictionRequest, productImagePrediction);

    productImagePredictionResponse = new ProductImagePredictionResponse();
    productImagePredictionResponse.setForceReview(true);
    BeanUtils.copyProperties(productImagePredictionRequest, productImagePredictionResponse);

    imageQcProcessedResponse = new ImageQcProcessedResponse();
    imageQcProcessedResponse.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponse.setForceReview(true);

    imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(imageQcProcessedResponse);
    imageQcProcessedAndBrandResponse.setBrandCode(BRAND_CODE);
    imageQcProcessedAndBrandResponse.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }


  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productImagePredictionService);
  }

  @Test
  public void insertImagePredictionTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductImagePredictionControllerPath.BASE_PATH + ProductImagePredictionControllerPath.IMAGE_PREDICTION_INSERT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(productImagePredictionRequest))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService).insert(productImagePrediction);
  }

  @Test
  public void updateImagePredictionTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductImagePredictionControllerPath.BASE_PATH + ProductImagePredictionControllerPath.IMAGE_PREDICTION_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(productImagePredictionRequest))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionServiceWrapper).update(productImagePrediction);
    assertEquals(53, productImagePrediction.getNeedRevisionConfidenceThreshold());
    assertTrue(productImagePrediction.isNeedRevisionEnabled());
  }

  @Test
  public void deleteImagePredictionTest() throws Exception {
    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH + ROOT + PREDICTION_TYPE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService).delete(DEFAULT_STORE_ID, PREDICTION_TYPE);
  }

  @Test
  public void findImagePredictionTest() throws Exception {
    Mockito.when(productImagePredictionService.findByStoreIdAndPredictionType(DEFAULT_STORE_ID, PREDICTION_TYPE))
        .thenReturn(productImagePrediction);
    URI uri = new URIBuilder().setPath(
        ProductImagePredictionControllerPath.BASE_PATH + ProductImagePredictionControllerPath.IMAGE_PREDICTION_FIND)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("predictionType", PREDICTION_TYPE).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService).findByStoreIdAndPredictionType(DEFAULT_STORE_ID, PREDICTION_TYPE);
    assertTrue(response.getResponse().getContentAsString()
        .contains(getObjectMapper().writeValueAsString(productImagePredictionResponse)));
  }

  @Test
  public void getImageQcPredictionResponseTest() throws Exception {
    Mockito.when(productImagePredictionService
        .findProductImagePredictionResponseByStoreIdAndProductCode(DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(imageQcProcessedResponse);
    URI uri = new URIBuilder().setPath(
        ProductImagePredictionControllerPath.BASE_PATH + ProductImagePredictionControllerPath.GET_IMAGE_QC_PREDICTION_RESPONSE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productCode", PRODUCT_CODE).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService)
        .findProductImagePredictionResponseByStoreIdAndProductCode(DEFAULT_STORE_ID, PRODUCT_CODE);
    assertTrue(response.getResponse().getContentAsString()
        .contains(getObjectMapper().writeValueAsString(imageQcProcessedResponse)));
  }

  @Test
  public void getImageQcPredictionAndBrandResponseTest() throws Exception {
    Mockito.when(productImagePredictionService
        .findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
        + ProductImagePredictionControllerPath.GET_IMAGE_QC_PREDICTION_AND_BRAND_RESPONSE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productCode", PRODUCT_CODE).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService)
        .findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(DEFAULT_STORE_ID, PRODUCT_CODE);
    assertTrue(response.getResponse().getContentAsString()
        .contains(getObjectMapper().writeValueAsString(imageQcProcessedAndBrandResponse)));
  }

  @Test
  public void getDifferentPredictionTypeTest() throws Exception {
    Mockito.when(productImagePredictionService.getDifferentPredictionType(DEFAULT_STORE_ID))
        .thenReturn(Collections.singletonList(new PredictionTypeResponse(DISPLAY_NAME, DISPLAY_NAME_IN)));
    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
        + ProductImagePredictionControllerPath.GET_DIFFERENT_PREDICTION_TYPE).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("productCode", PRODUCT_CODE).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService).getDifferentPredictionType(DEFAULT_STORE_ID);
  }

  @Test
  public void getPredictionListTest() throws Exception {
    Mockito.when(productImagePredictionService.findByStoreId(Mockito.anyString()))
        .thenReturn(Arrays.asList(productImagePredictionResponse));
    URI uri = new URIBuilder().setPath(
        ProductImagePredictionControllerPath.BASE_PATH + ProductImagePredictionControllerPath.GET_PREDICTION_LIST)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productImagePredictionService).findByStoreIdAndPredictionConsideredTrue(DEFAULT_STORE_ID);
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    request.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();

    Mockito.verify(productImagePredictionServiceWrapper)
        .updateImagePredictionAndCategoryMappingAndCacheEvict(DEFAULT_STORE_ID, request);
  }

  @Test
  public void updateImagePredictionAndCategoryMappingApplicationRuntimeExceptionTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    request.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    Mockito.doThrow(new ApplicationRuntimeException()).when(productImagePredictionServiceWrapper)
        .updateImagePredictionAndCategoryMappingAndCacheEvict(DEFAULT_STORE_ID, request);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(productImagePredictionServiceWrapper)
        .updateImagePredictionAndCategoryMappingAndCacheEvict(DEFAULT_STORE_ID, request);
  }

  @Test
  public void updateImagePredictionAndCategoryMappingExceptionTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    request.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    Mockito.doThrow(new Exception()).when(productImagePredictionServiceWrapper)
        .updateImagePredictionAndCategoryMappingAndCacheEvict(DEFAULT_STORE_ID, request);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(productImagePredictionServiceWrapper)
        .updateImagePredictionAndCategoryMappingAndCacheEvict(DEFAULT_STORE_ID, request);
  }

  @Test
  public void updateImagePredictionAndCategoryMappingPredictionTypeBlankTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(StringUtils.EMPTY);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    request.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void updateImagePredictionAndCategoryMappingCategoryListNullTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    request.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(null);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void updateImagePredictionAndCategoryMappingCategoryCodeBlankTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    request.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(StringUtils.EMPTY);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void getImagePredictionAndCategoryMappingTest() throws Exception {
    GenericStringListRequest request = new GenericStringListRequest();
    List<String> predictionType = new ArrayList<>();
    predictionType.add(PREDICTION_TYPE);
    request.setStringList(predictionType);
    List<ProductImagePredictionAndCategoryMappingResponse> productImagePredictionAndCategoryMappingResponses = new ArrayList<>();

    Mockito.when(this.productImagePredictionService.getImagePredictionAndCategoryMapping(DEFAULT_STORE_ID,
        request.getStringList())).thenReturn(productImagePredictionAndCategoryMappingResponses);

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.GET_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult response = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();

    Mockito.verify(this.productImagePredictionService).getImagePredictionAndCategoryMapping(DEFAULT_STORE_ID,
        request.getStringList());
  }

  @Test
  public void getImagePredictionAndCategoryMappingApplicationRuntimeExceptionTest() throws Exception {
    GenericStringListRequest request = new GenericStringListRequest();
    List<String> predictionType = new ArrayList<>();
    predictionType.add(PREDICTION_TYPE);
    request.setStringList(predictionType);
    List<ProductImagePredictionAndCategoryMappingResponse> productImagePredictionAndCategoryMappingResponses = new ArrayList<>();

    Mockito.when(this.productImagePredictionService.getImagePredictionAndCategoryMapping(DEFAULT_STORE_ID,
        request.getStringList())).thenThrow(new ApplicationRuntimeException());

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.GET_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult response = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false))).andReturn();

    Mockito.verify(this.productImagePredictionService).getImagePredictionAndCategoryMapping(DEFAULT_STORE_ID,
        request.getStringList());
  }

  @Test
  public void getImagePredictionAndCategoryMappingExceptionTest() throws Exception {
    GenericStringListRequest request = new GenericStringListRequest();
    List<String> predictionType = new ArrayList<>();
    predictionType.add(PREDICTION_TYPE);
    request.setStringList(predictionType);
    List<ProductImagePredictionAndCategoryMappingResponse> productImagePredictionAndCategoryMappingResponses = new ArrayList<>();

    Mockito.when(this.productImagePredictionService.getImagePredictionAndCategoryMapping(DEFAULT_STORE_ID,
        request.getStringList())).thenThrow(new Exception());

    URI uri = new URIBuilder().setPath(ProductImagePredictionControllerPath.BASE_PATH
            + ProductImagePredictionControllerPath.GET_IMAGE_PREDICTION_AND_CATEGORY_MAPPING)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult response = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.success", equalTo(false))).andReturn();

    Mockito.verify(this.productImagePredictionService).getImagePredictionAndCategoryMapping(DEFAULT_STORE_ID,
        request.getStringList());
  }
}