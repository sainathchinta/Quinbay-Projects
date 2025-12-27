package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import java.util.ArrayList;
import java.util.List;

import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Mock;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ProductImagePredictionApiPath;
import com.gdn.partners.pcu.internal.service.ProductImagePredictionService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.PredictionCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionAndCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;


@AutoConfigureMockMvc
public class ProductImagePredictionControllerTest extends TestHelper {

  @InjectMocks
  private ProductImagePredictionController productImagePredictionController;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private List<ProductImagePredictionWebResponse> productImagePredictionWebResponseList;
  private List<ProductImagePredictionAndCategoryMappingWebResponse> productImagePredictionAndCategoryMappingWebResponseList;
  private MockMvc mockMvc;

  @Mock
  private ProductImagePredictionService productImagePredictionService;

  private ProductImagePredictionWebRequest productImagePredictionWebRequest;
  private ProductImagePredictionAndCategoryMappingWebRequest productImagePredictionAndCategoryMappingWebRequest;
  private static final String PREDICTION_TYPE = "Prediction Type";
  private static final String DISPLAY_NAME = "Display Name";
  private static final String DISPLAY_NAME_IN = "Display Name In";
  private static final int CONFIDENCE_THRESHOLD = 53;
  private static final int PREDICTION_WEIGHTAGE = 53;
  private static final boolean MARK_FOR_DELETE = false;
  private static final int NEED_REVISION_CONFIDENCE_THRESHOLD = 53;
  private static final String RULE_TYPE = "RuleType";
  private static final boolean RULE_ENABLED = true;
  private static final int TEXT_CONFIDENCE_THRESHOLD = 50;
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String REQUEST_ID = "requestId";

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(productImagePredictionController).build();
    productImagePredictionWebRequest = new ProductImagePredictionWebRequest();
    productImagePredictionWebRequest.setPredictionType(PREDICTION_TYPE);
    productImagePredictionWebRequest.setDisplayName(DISPLAY_NAME);
    productImagePredictionWebRequest.setDisplayNameIn(DISPLAY_NAME_IN);
    productImagePredictionWebRequest.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    productImagePredictionWebRequest.setPredictionWeightage(PREDICTION_WEIGHTAGE);
    productImagePredictionWebRequest.setMarkForDelete(MARK_FOR_DELETE);
    productImagePredictionWebRequest.setNeedRevisionConfidenceThreshold(NEED_REVISION_CONFIDENCE_THRESHOLD);
    productImagePredictionWebRequest.setRuleType(RULE_TYPE);
    productImagePredictionWebRequest.setRuleEnabled(RULE_ENABLED);

    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.productImagePredictionController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    mockMvc = MockMvcBuilders.standaloneSetup(productImagePredictionController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    ProductImagePredictionWebResponse productImagePredictionWebResponse=new ProductImagePredictionWebResponse();
    productImagePredictionWebResponse.setConfidenceThreshold(50);
    productImagePredictionWebResponseList=new ArrayList<>();
    productImagePredictionWebResponseList.add(productImagePredictionWebResponse);

    productImagePredictionAndCategoryMappingWebRequest = new ProductImagePredictionAndCategoryMappingWebRequest();
    productImagePredictionAndCategoryMappingWebRequest.setPredictionType(PREDICTION_TYPE);
    productImagePredictionAndCategoryMappingWebRequest.setRuleEnabled(RULE_ENABLED);
    productImagePredictionAndCategoryMappingWebRequest.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingWebRequest.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    List<PredictionCategoryMappingWebRequest> predictionCategoryMappingWebRequestList = new ArrayList<>();
    PredictionCategoryMappingWebRequest predictionCategoryMappingWebRequest = new PredictionCategoryMappingWebRequest();
    predictionCategoryMappingWebRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingWebRequest.setMarkForDelete(MARK_FOR_DELETE);
    predictionCategoryMappingWebRequestList.add(predictionCategoryMappingWebRequest);
    productImagePredictionAndCategoryMappingWebRequest.setPredictionCategoryMappingWebRequestList(
        predictionCategoryMappingWebRequestList);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productImagePredictionService);
  }

  @Test
  public void updateImagePredictionTest() throws Exception {
    Mockito.when(this.productImagePredictionService.update(productImagePredictionWebRequest))
        .thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductImagePredictionApiPath.BASE_PATH + ProductImagePredictionApiPath.UPDATE_PREDICTIONS)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productImagePredictionWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productImagePredictionService).update(productImagePredictionWebRequest);
  }

  @Test
  public void getListOfPredictionsTest() throws Exception{
    Mockito.when(productImagePredictionService.getListOfPredictions()).thenReturn(productImagePredictionWebResponseList);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductImagePredictionApiPath.BASE_PATH + ProductImagePredictionApiPath.GET_PREDICTION_LIST)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(productImagePredictionService).getListOfPredictions();
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTest() throws Exception {
    Mockito.when(productImagePredictionService.updateImagePredictionAndCategoryMapping(
        productImagePredictionAndCategoryMappingWebRequest)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder = put(ProductImagePredictionApiPath.BASE_PATH
        + ProductImagePredictionApiPath.UPDATE_PREDICTION_AND_CATEGORY_MAPPING).contentType(
            MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(productImagePredictionAndCategoryMappingWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productImagePredictionService)
        .updateImagePredictionAndCategoryMapping(productImagePredictionAndCategoryMappingWebRequest);
  }
  @Test
  public void getThresholdDetailAndCategoryMappingTest() throws Exception{
    PredictionTypeListWebRequest request = new PredictionTypeListWebRequest();
    Mockito.when(productImagePredictionService.getThresholdDetailAndCategoryMapping(request)).thenReturn(productImagePredictionAndCategoryMappingWebResponseList);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductImagePredictionApiPath.BASE_PATH + ProductImagePredictionApiPath.GET_THRESHOLD_DETAIL_AND_CATEGORY_MAPPING)
            .content(toJson(request)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(productImagePredictionService).getThresholdDetailAndCategoryMapping(request);
  }

}
