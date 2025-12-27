package com.gdn.partners.pcu.internal.service.impl;

import org.apache.commons.lang.StringUtils;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.CategoryCodeAndCategoryNameResponse;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.security.Credential;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.web.model.request.PredictionCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionAndCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductImagePredictionServiceImplTest {

  private static final String PREDICTION_TYPE = "predictionType";
  private static final String DISPLAY_NAME = "Blur";
  private static final int CONFIDNECE_THRESHOLD = 50;
  private static final int NEED_REVISION_CONFIDENCE_THRESHOLD = 80;
  private static final String REQUEST_ID = "RequestId";
  private static final String AUTO_NEED_REVISION = "Auto need revision";
  private static final String FORCE_REVIEW = "Force review";
  private static final int RULE_THRESHOLD = 47;
  private static final int TEXT_CONFIDENCE_THRESHOLD = 50;
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final boolean RULE_ENABLED = true;
  private static final int CONFIDENCE_THRESHOLD = 53;
  private static final boolean MARK_FOR_DELETE = false;

  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private ProductImagePredictionServiceImpl productImagePredictionService;

  private ProductImagePredictionWebRequest productImagePredictionWebRequest;
  private ProductImagePredictionAndCategoryMappingWebRequest productImagePredictionAndCategoryMappingWebRequest;
  private List<ProductImagePredictionResponse> productImagePredictionResponseList;
  private GdnRestListResponse restListResponse;
  private List<ProductImagePredictionAndCategoryMappingWebResponse> productImagePredictionAndCategoryMappingWebResponseList;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productImagePredictionWebRequest = new ProductImagePredictionWebRequest();
    productImagePredictionWebRequest.setDisplayName(DISPLAY_NAME);
    productImagePredictionWebRequest.setRuleThreshold(RULE_THRESHOLD);

    ProductImagePredictionResponse productImagePredictionResponse = new ProductImagePredictionResponse();
    productImagePredictionResponse.setPredictionType(PREDICTION_TYPE);
    productImagePredictionResponse.setConfidenceThreshold(CONFIDNECE_THRESHOLD);
    productImagePredictionResponse.setDisplayName(DISPLAY_NAME);
    productImagePredictionResponse.setNeedRevisionConfidenceThreshold(NEED_REVISION_CONFIDENCE_THRESHOLD);
    productImagePredictionResponseList = new ArrayList<>();
    productImagePredictionResponseList.add(productImagePredictionResponse);
    restListResponse = new GdnRestListResponse(productImagePredictionResponseList, new PageMetaData(), REQUEST_ID);

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

  @Test
  public void updateProductImagePredictionServiceTest_AutoNeedRevisionTrue() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    productImagePredictionWebRequest.setRuleType(AUTO_NEED_REVISION);
    productImagePredictionWebRequest.setRuleEnabled(true);
    Mockito.when(
        pbpFeign.updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest)))
        .thenReturn(new GdnBaseRestResponse(true));
    boolean response = productImagePredictionService.update(productImagePredictionWebRequest);
    Assertions.assertTrue(response);
    Mockito.verify(pbpFeign)
        .updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest));
  }

  @Test
  public void updateProductImagePredictionServiceTest_AutoNeedRevisionFalse() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    productImagePredictionWebRequest.setRuleType(AUTO_NEED_REVISION);
    productImagePredictionWebRequest.setRuleEnabled(false);
    Mockito.when(
        pbpFeign.updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest)))
        .thenReturn(new GdnBaseRestResponse(true));
    boolean response = productImagePredictionService.update(productImagePredictionWebRequest);
    Assertions.assertTrue(response);
    Mockito.verify(pbpFeign)
        .updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest));
  }

  @Test
  public void updateProductImagePredictionServiceTest_ForceReviewTrue() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    productImagePredictionWebRequest.setRuleType(FORCE_REVIEW);
    productImagePredictionWebRequest.setRuleEnabled(true);
    Mockito.when(
        pbpFeign.updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest)))
        .thenReturn(new GdnBaseRestResponse(true));
    boolean response = productImagePredictionService.update(productImagePredictionWebRequest);
    Assertions.assertTrue(response);
    Mockito.verify(pbpFeign)
        .updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest));
  }

  @Test
  public void updateProductImagePredictionServiceTest_ForceReviewFalse() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    productImagePredictionWebRequest.setRuleType(FORCE_REVIEW);
    productImagePredictionWebRequest.setRuleEnabled(false);
    Mockito.when(
        pbpFeign.updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest)))
        .thenReturn(new GdnBaseRestResponse(true));
    boolean response = productImagePredictionService.update(productImagePredictionWebRequest);
    Assertions.assertTrue(response);
    Mockito.verify(pbpFeign)
        .updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest));
  }

  @Test
  public void updateProductImagePredictionServiceTest_Empty() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    productImagePredictionWebRequest.setRuleType(StringUtils.EMPTY);
    productImagePredictionWebRequest.setRuleEnabled(false);
    Mockito.when(
        pbpFeign.updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest)))
        .thenReturn(new GdnBaseRestResponse(true));
    boolean response = productImagePredictionService.update(productImagePredictionWebRequest);
    Assertions.assertTrue(response);
    Mockito.verify(pbpFeign)
        .updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest));
  }

  @Test
  public void updateProductImagePredictionServiceTest_CheckAccessibility() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = StringUtils.EMPTY;
    Credential.setAccessibilities(accessibilities);
    productImagePredictionWebRequest.setRuleType(AUTO_NEED_REVISION);
    productImagePredictionWebRequest.setRuleEnabled(true);
    Mockito.when(
        pbpFeign.updateImagePrediction(RequestHelper.toProductImagePredictionRequest(productImagePredictionWebRequest)))
        .thenReturn(new GdnBaseRestResponse(true));
    Exception exception = new Exception();
    try {
      boolean response = productImagePredictionService.update(productImagePredictionWebRequest);
    } catch (ApplicationRuntimeException e) {
      exception = e;
    } finally {
      Assertions.assertTrue(exception.getMessage().contains(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE));
    }
  }

  @Test
  public void getListOfPredictions() throws Exception {
    Mockito.when(pbpFeign.getListOfPredictions()).thenReturn(restListResponse);
    List<ProductImagePredictionWebResponse> response = productImagePredictionService.getListOfPredictions();
    Mockito.verify(pbpFeign).getListOfPredictions();
    Assertions.assertEquals(productImagePredictionResponseList.size(), response.size());
    Assertions.assertEquals(DISPLAY_NAME, response.get(0).getDisplayName());
    Assertions.assertEquals(NEED_REVISION_CONFIDENCE_THRESHOLD, response.get(0).getNeedRevisionConfidenceThreshold());
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTest() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    Mockito.when(pbpFeign.updateImagePredictionAndCategoryMapping(
        RequestHelper.toProductImagePredictionAndCategoryMappingRequest(
            productImagePredictionAndCategoryMappingWebRequest))).thenReturn(new GdnBaseRestResponse(true));
    boolean response = productImagePredictionService.updateImagePredictionAndCategoryMapping(
        productImagePredictionAndCategoryMappingWebRequest);
    Assertions.assertTrue(response);
    Mockito.verify(pbpFeign).updateImagePredictionAndCategoryMapping(
        RequestHelper.toProductImagePredictionAndCategoryMappingRequest(
            productImagePredictionAndCategoryMappingWebRequest));
  }

  @Test
  public void updateImagePredictionAndCategoryMappingListNullErrorTest() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = Constants.INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY;
    Credential.setAccessibilities(accessibilities);
    Mockito.when(pbpFeign.updateImagePredictionAndCategoryMapping(
        RequestHelper.toProductImagePredictionAndCategoryMappingRequest(
            productImagePredictionAndCategoryMappingWebRequest))).thenReturn(new GdnBaseRestResponse(true));
    boolean response = false;
    try {
      productImagePredictionAndCategoryMappingWebRequest.setPredictionCategoryMappingWebRequestList(null);
      response = productImagePredictionService.updateImagePredictionAndCategoryMapping(
          productImagePredictionAndCategoryMappingWebRequest);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Assertions.assertFalse(response);
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingAccessibilityTest() throws Exception {
    String accessibilities[] = new String[1];
    accessibilities[0] = StringUtils.EMPTY;
    Credential.setAccessibilities(accessibilities);
    boolean response = false;
    Exception exception = new Exception();
    try {
      response = productImagePredictionService.updateImagePredictionAndCategoryMapping(
          productImagePredictionAndCategoryMappingWebRequest);
    } catch (ApplicationRuntimeException e) {
      exception = e;
    } finally {
      Assertions.assertFalse(response);
      Assertions.assertTrue(exception.getMessage().contains(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE));
    }
  }

  @Test
  public void getThresholdDetailAndCategoryMappingTest() throws Exception {
    List<CategoryCodeAndCategoryNameResponse> categoryCodeAndCategoryNameResponseList = new ArrayList<>();
    CategoryCodeAndCategoryNameResponse categoryCodeAndCategoryNameResponse = new CategoryCodeAndCategoryNameResponse();
    categoryCodeAndCategoryNameResponse.setCategoryCode(CATEGORY_CODE);
    categoryCodeAndCategoryNameResponse.setCategoryName(CATEGORY_NAME);
    categoryCodeAndCategoryNameResponseList.add(categoryCodeAndCategoryNameResponse);
    PredictionTypeListWebRequest predictionTypeListWebRequest = new PredictionTypeListWebRequest();

    List<ProductImagePredictionAndCategoryMappingResponse> predictionList = new ArrayList<>();
    ProductImagePredictionAndCategoryMappingResponse productImagePredictionAndCategoryMappingResponse = new ProductImagePredictionAndCategoryMappingResponse();
    productImagePredictionAndCategoryMappingResponse.setPredictionType(PREDICTION_TYPE);
    productImagePredictionAndCategoryMappingResponse.setRuleEnabled(RULE_ENABLED);
    productImagePredictionAndCategoryMappingResponse.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingResponse.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingResponse.setCategoryCodeAndCategoryNameResponseList(categoryCodeAndCategoryNameResponseList);
    predictionList.add(productImagePredictionAndCategoryMappingResponse);
    GenericStringListRequest request = new GenericStringListRequest();

    Mockito.when(pbpFeign.getImagePredictionAndCategoryMapping(request))
        .thenReturn(new GdnRestListResponse<>(null, null, true, predictionList, null, null));
    List<ProductImagePredictionAndCategoryMappingWebResponse> response =
        productImagePredictionService.getThresholdDetailAndCategoryMapping(predictionTypeListWebRequest);
    Mockito.verify(pbpFeign).getImagePredictionAndCategoryMapping(request);
    Assertions.assertEquals(PREDICTION_TYPE, response.get(0).getPredictionType());
    Assertions.assertEquals(RULE_ENABLED, response.get(0).isRuleEnabled());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.get(0).getConfidenceThreshold());
    Assertions.assertEquals(TEXT_CONFIDENCE_THRESHOLD, response.get(0).getTextConfidenceThreshold());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getPredictionCategoryMappingWebResponseList().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, response.get(0).getPredictionCategoryMappingWebResponseList().get(0).getCategoryName());
  }

  @Test
  public void getThresholdDetailAndCategoryMappingFalseTest() throws Exception {
    PredictionTypeListWebRequest predictionTypeListWebRequest = new PredictionTypeListWebRequest();
    List<ProductImagePredictionAndCategoryMappingResponse> predictionList = new ArrayList<>();
    GenericStringListRequest request = new GenericStringListRequest();
    Mockito.when(pbpFeign.getImagePredictionAndCategoryMapping(request))
        .thenReturn(new GdnRestListResponse<>(null, null, false, predictionList, null, null));
    try {
      List<ProductImagePredictionAndCategoryMappingWebResponse> response =
          productImagePredictionService.getThresholdDetailAndCategoryMapping(
              predictionTypeListWebRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    }
  }
}
