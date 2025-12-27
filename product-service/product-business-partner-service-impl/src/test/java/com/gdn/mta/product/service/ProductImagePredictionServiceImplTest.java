package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.context.ApplicationContext;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.PredictionCategoryMappingUpdateRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionRequest;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.repository.ProductImagePredictionRepository;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionServiceBean;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeAndNameResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;

public class ProductImagePredictionServiceImplTest {

  private static final String PREDICTION_TYPE = "predictionType";
  private static final String DISPLAY_NAME = "name";
  private static final String STORE_ID = "10001";
  private static final String ID = "id";
  private static final String PRODUCT_CODE = "productCode";
  private static final String NAME_IN = "inName";
  private static final String NAME_EN = "enName";
  private static final String BRAND_CODE = "BRD-007";
  private static final String BLUR = "blur";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String IMAGE = "IMAGE";
  private static final String TEXT = "TEXT";
  private static final String IMAGE_AND_TEXT = "IMAGE_AND_TEXT";
  private List<String> ACTIVE_PREDICTIONS =
      Arrays.asList("watermark_predictions", "blur_predictions", "text_predictions", "nsfw_predictions");
  private List<String> ACTIVE_PREDICTIONS_1 =
      Arrays.asList("watermark_predictions", "text_predictions", "nsfw_predictions");
  private static final String RESPONSE_WITHOUR_BLUR =
      "{\"timestamp\":1595003740556,\"productCode\":\"MTA-0451837\","
        + "\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837"
        + "/nike_test_image_qc_full01_uedfzhqd.jpeg\","
        + "\"hashCode\":\"2246ef20d75fd39de86a15adb39fc74b\","
        + "\"predictions\":[{\"predictionType\":\"watermark_predictions\","
        + "\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},"
        + "{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,"
        + "\"confidence\":22},{\"predictionType\":\"nsfw_predictions\","
        + "\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}],\"edited\":false,\"markForDelete\":false},{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full02_gqm86hzf.jpeg\",\"hashCode\":\"d8ed7c1e732a350f9af0ffe0a2e28bca\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":5},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[],\"brandModels\":[],\"keywordRestrictionModels\":[],\"categoryModels\":[]}";
  private static final String IMAGE_QC_RESPONSE =
      "{\"timestamp\":1595003740556,\"productCode\":\"MTA-0451837\","
        + "\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837"
        + "/nike_test_image_qc_full01_uedfzhqd.jpeg\","
        + "\"hashCode\":\"2246ef20d75fd39de86a15adb39fc74b\","
        + "\"predictions\":[{\"predictionType\":\"watermark_predictions\","
        + "\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},"
        + "{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,"
        + "\"confidence\":78},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\","
        + "\"present\":false,\"confidence\":22},{\"predictionType\":\"nsfw_predictions\","
        + "\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}],\"edited\":false,"
        + "\"markForDelete\":false},{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837"
        + "/nike_test_image_qc_full02_gqm86hzf.jpeg\","
        + "\"hashCode\":\"d8ed7c1e732a350f9af0ffe0a2e28bca\","
        + "\"predictions\":[{\"predictionType\":\"watermark_predictions\","
        + "\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":96},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":5},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[],\"brandModels\":[],\"keywordRestrictionModels\":[],\"categoryModels\":[]}";

  private ProductImagePredictionRequest productImagePredictionRequest;
  private ProductImagePredictionResponse productImagePredictionResponse;
  private ProductImagePrediction productImagePrediction;
  private ProductImageQcProcessingResponse productImageQcProcessingResponse;
  private ProductCollection productCollection;

  @InjectMocks
  private ProductImagePredictionServiceImpl productImagePredictionService;

  @Mock
  private ProductImagePredictionRepository productImagePredictionRepository;

  @Mock
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private ProductLevel1CollectionServiceBean productLevel1CollectionServiceBean;

  @Captor
  private ArgumentCaptor<ProductImagePrediction> productImagePredictionArgumentCaptor;

  @Mock
  private ProductOutbound productOutbound;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productImagePredictionRequest = new ProductImagePredictionRequest();
    productImagePredictionRequest.setConfidenceThreshold(10);
    productImagePredictionRequest.setPredictionWeightage(64);
    productImagePredictionRequest.setPredictionType(PREDICTION_TYPE);
    productImagePredictionRequest.setDisplayName(DISPLAY_NAME);

    productImagePrediction = new ProductImagePrediction();
    BeanUtils.copyProperties(productImagePredictionRequest, productImagePrediction);
    productImagePrediction.setStoreId(STORE_ID);
    productImagePrediction.setId(ID);
    productImagePrediction.setForceReview(true);
    productImagePrediction.setDisplayNameIn(NAME_IN);

    productImagePredictionResponse = new ProductImagePredictionResponse();
    BeanUtils.copyProperties(productImagePrediction, productImagePredictionResponse);

    productImageQcProcessingResponse = new ProductImageQcProcessingResponse();
    productImageQcProcessingResponse.setProductCode(PRODUCT_CODE);
    productImageQcProcessingResponse.setForceReview(true);
    productImageQcProcessingResponse.setImageQcResponse("{\"timestamp\":1595003740556,\"productCode\":\"MTA-0451837\",\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full01_uedfzhqd.jpeg\",\"hashCode\":\"2246ef20d75fd39de86a15adb39fc74b\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":78},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":22},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}]},{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full02_gqm86hzf.jpeg\",\"hashCode\":\"d8ed7c1e732a350f9af0ffe0a2e28bca\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":96},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":5},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}]}],\"success\":true,\"errorMessage\":\"\"}");

    productCollection = new ProductCollection();
    productCollection.setBrandCode(BRAND_CODE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);

    Mockito.when(applicationContext.getBean(ProductImagePredictionService.class))
        .thenReturn(productImagePredictionService);
    Mockito.when(
        productLevel1CollectionServiceBean.getProductTypeBasedOnProductCodeOrId(Mockito.any(), Mockito.any()))
        .thenReturn(1);

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productImagePredictionRepository);
    Mockito.verifyNoMoreInteractions(productImageQcProcessingResponseService);
    Mockito.verifyNoMoreInteractions(applicationContext);
    Mockito.verifyNoMoreInteractions(productLevel1CollectionServiceBean);
  }

  @Test
  public void insertTest() {
    productImagePredictionService.insert(productImagePrediction);
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
    Mockito.verify(productImagePredictionRepository).save(productImagePredictionArgumentCaptor.capture());
    Assertions.assertEquals(PREDICTION_TYPE, productImagePredictionArgumentCaptor.getValue().getPredictionType());
    Assertions.assertEquals(DISPLAY_NAME, productImagePredictionArgumentCaptor.getValue().getDisplayName());
  }

  @Test
  public void insertStoreIdBlankTest() {
    productImagePrediction.setStoreId(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productImagePredictionService.insert(productImagePrediction);
    });
  }

  @Test
  public void insertPredictionTypeBlankTest() {
    productImagePrediction.setPredictionType(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productImagePredictionService.insert(productImagePrediction);
    });
  }

  @Test
  public void insertNameBlankTest() {
    productImagePrediction.setDisplayName(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productImagePredictionService.insert(productImagePrediction);
    });
  }

  @Test
  public void updateTest() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE))
        .thenReturn(productImagePrediction);
    productImagePrediction.setConfidenceThreshold(75);
    productImagePrediction.setPredictionWeightage(1);
    productImagePrediction.setNeedRevisionConfidenceThreshold(53);
    productImagePrediction.setNeedRevisionEnabled(true);
    productImagePrediction.setForceReview(false);
    productImagePredictionService.update(productImagePrediction);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE);
    Mockito.verify(productImagePredictionRepository).save(productImagePredictionArgumentCaptor.capture());
    Assertions.assertEquals(75, productImagePredictionArgumentCaptor.getValue().getConfidenceThreshold());
    Assertions.assertEquals(1, productImagePredictionArgumentCaptor.getValue().getPredictionWeightage());
    Assertions.assertEquals(NAME_IN, productImagePredictionArgumentCaptor.getValue().getDisplayNameIn());
    Assertions.assertEquals(53, productImagePredictionArgumentCaptor.getValue().getNeedRevisionConfidenceThreshold());
    Assertions.assertTrue(productImagePredictionArgumentCaptor.getValue().isNeedRevisionEnabled());
    Assertions.assertFalse(productImagePredictionArgumentCaptor.getValue().isForceReview());
  }

  @Test
  public void updateTestBothForceReviewAndNeedRevisionEnabledTrue() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE))
        .thenReturn(productImagePrediction);
    productImagePrediction.setConfidenceThreshold(75);
    productImagePrediction.setPredictionWeightage(1);
    productImagePrediction.setNeedRevisionConfidenceThreshold(53);
    productImagePrediction.setNeedRevisionEnabled(true);
    productImagePrediction.setForceReview(true);
    Exception exception = new Exception();
    try {
      productImagePredictionService.update(productImagePrediction);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertTrue(exception instanceof ApplicationRuntimeException);
      Assertions.assertTrue(
          exception.getMessage().contains(ErrorMessages.BOTH_FORCE_REVIEW_AND_NEED_REVISION_ENABLED_CANNOT_BE_TRUE));
    }
  }

  @Test
  public void updateTestBothForceReviewAndNeedRevisionEnabledFalse() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE))
        .thenReturn(productImagePrediction);
    productImagePrediction.setConfidenceThreshold(75);
    productImagePrediction.setPredictionWeightage(1);
    productImagePrediction.setNeedRevisionConfidenceThreshold(53);
    productImagePrediction.setNeedRevisionEnabled(false);
    productImagePrediction.setForceReview(false);
    productImagePredictionService.update(productImagePrediction);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE);
    Mockito.verify(productImagePredictionRepository).save(productImagePredictionArgumentCaptor.capture());
    Assertions.assertEquals(75, productImagePredictionArgumentCaptor.getValue().getConfidenceThreshold());
    Assertions.assertEquals(1, productImagePredictionArgumentCaptor.getValue().getPredictionWeightage());
    Assertions.assertEquals(NAME_IN, productImagePredictionArgumentCaptor.getValue().getDisplayNameIn());
    Assertions.assertEquals(53, productImagePredictionArgumentCaptor.getValue().getNeedRevisionConfidenceThreshold());
    Assertions.assertFalse(productImagePredictionArgumentCaptor.getValue().isNeedRevisionEnabled());
    Assertions.assertFalse(productImagePredictionArgumentCaptor.getValue().isForceReview());
  }

  @Test
  public void deleteTest() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE))
        .thenReturn(productImagePrediction);
    productImagePredictionService.delete(STORE_ID, PREDICTION_TYPE);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE);
    Mockito.verify(productImagePredictionRepository).deleteById(ID);
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
  }

  @Test
  public void deleteExceptionTest() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.delete(STORE_ID, PREDICTION_TYPE);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository).findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE);
    }
  }

  @Test
  public void findByStoreIdAndPredictionTypeTest() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE))
        .thenReturn(productImagePrediction);
    ProductImagePrediction response =
        productImagePredictionService.findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndPredictionType(STORE_ID, PREDICTION_TYPE);
    Assertions.assertEquals(response, productImagePrediction);
  }

  @Test
  public void findByStoreIdAndForceReviewTrue() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndForceReviewTrue(Mockito.any())).thenReturn(Arrays.asList(productImagePrediction));
    List<ProductImagePrediction> response=productImagePredictionService.findByStoreIdAndForceReviewTrue(STORE_ID);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndForceReviewTrue(STORE_ID);
    Assertions.assertEquals(productImagePrediction.getDisplayName(),response.get(0).getDisplayName());
    Assertions.assertEquals(Arrays.asList(productImagePrediction),response);
  }

  @Test
  public void findByStoreIdAndForceReviewTrueAndPredictionConsideredTrueTest() {
    Mockito.when(productImagePredictionRepository
        .findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(Mockito.any()))
        .thenReturn(Arrays.asList(productImagePrediction));
    List<ProductImagePrediction> response =
        productImagePredictionService.findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(STORE_ID);
    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(STORE_ID);
    Assertions.assertEquals(productImagePrediction.getDisplayName(), response.get(0).getDisplayName());
    Assertions.assertEquals(Arrays.asList(productImagePrediction), response);
  }

  @Test
  public void findProductImagePredictionResponseByStoreAndProductCodeTest() {
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(STORE_ID,
        PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    ImageQcProcessedResponse response =
        productImagePredictionService.findProductImagePredictionResponseByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCodeDb(STORE_ID
      , PRODUCT_CODE);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertTrue(response.isForceReview());
  }

  @Test
  public void findProductImagePredictionAndBrandResponseByStoreAndProductCodeTest() throws Exception {
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(STORE_ID,
        PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(productLevel1CollectionServiceBean.findByProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    ImageQcProcessedAndBrandResponse response = productImagePredictionService
        .findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCodeDb(STORE_ID
      , PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionServiceBean).findByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Mockito.verify(productLevel1CollectionServiceBean)
        .getProductTypeBasedOnProductCodeOrId(Mockito.any(), Mockito.any());
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
    Assertions.assertEquals(PRODUCT_CODE, response.getImageQcProcessedResponse().getProductCode());
    Assertions.assertTrue(response.getImageQcProcessedResponse().isForceReview());
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BrandApprovalStatus.APPROVED.name(), response.getBrandApprovalStatus());
  }

  @Test
  public void findProductImagePredictionAndBrandResponseByStoreAndProductCodeTest1() throws Exception {
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(STORE_ID,
        PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(productLevel1CollectionServiceBean.findByProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productImagePredictionRepository.findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID))
        .thenReturn(ACTIVE_PREDICTIONS);
    ImageQcProcessedAndBrandResponse response = productImagePredictionService
        .findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCodeDb(STORE_ID
      , PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionServiceBean).findByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Mockito.verify(productLevel1CollectionServiceBean)
        .getProductTypeBasedOnProductCodeOrId(Mockito.any(), Mockito.any());
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
    Assertions.assertEquals(PRODUCT_CODE, response.getImageQcProcessedResponse().getProductCode());
    Assertions.assertTrue(response.getImageQcProcessedResponse().isForceReview());
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BrandApprovalStatus.APPROVED.name(), response.getBrandApprovalStatus());
    Assertions.assertEquals(IMAGE_QC_RESPONSE, response.getImageQcProcessedResponse().getImageQcResponse());
  }

  @Test
  public void
  findProductImagePredictionAndBrandResponseByStoreAndProductCodeTestWithoutBlur() throws Exception {
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(STORE_ID,
        PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(productLevel1CollectionServiceBean.findByProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productImagePredictionRepository.findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID))
        .thenReturn(ACTIVE_PREDICTIONS_1);
    ImageQcProcessedAndBrandResponse response = productImagePredictionService
        .findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCodeDb(STORE_ID
      , PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionServiceBean).findByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Mockito.verify(productLevel1CollectionServiceBean)
        .getProductTypeBasedOnProductCodeOrId(Mockito.any(), Mockito.any());
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
    Assertions.assertEquals(PRODUCT_CODE, response.getImageQcProcessedResponse().getProductCode());
    Assertions.assertTrue(response.getImageQcProcessedResponse().isForceReview());
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BrandApprovalStatus.APPROVED.name(), response.getBrandApprovalStatus());
    Assertions.assertEquals(RESPONSE_WITHOUR_BLUR, response.getImageQcProcessedResponse().getImageQcResponse());
  }

  @Test
  public void findProductImagePredictionResponseByStoreAndProductCodeNullTest() {
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(STORE_ID,
        PRODUCT_CODE))
        .thenReturn(null);
    ImageQcProcessedResponse response =
        productImagePredictionService.findProductImagePredictionResponseByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCodeDb(STORE_ID
      , PRODUCT_CODE);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
    Assertions.assertNull(response);
  }

  @Test
  public void getDifferentPredictionTypeTest() {
    Mockito.when(productImagePredictionRepository.findDistinctPredictionTypeNameWhereStoreIdAndMarkForDeleteFalse(STORE_ID))
        .thenReturn(Collections.singletonList(new PredictionTypeResponse(NAME_EN, NAME_IN)));
    List<PredictionTypeResponse> response = productImagePredictionService.getDifferentPredictionType(STORE_ID);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeNameWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(NAME_IN, response.get(0).getInName());
    Assertions.assertEquals(NAME_EN, response.get(0).getEnName());
    Assertions.assertEquals(1, response.size());
  }

  @Test
  public void getListOfActivePredictionTypesTest() {
    Mockito.when(productImagePredictionRepository.findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID))
        .thenReturn(Collections.singletonList(BLUR));
    List<String> listOfActivePredictionTypes = productImagePredictionService.getListOfActivePredictionTypes(STORE_ID);
    Mockito.verify(productImagePredictionRepository)
        .findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(BLUR, listOfActivePredictionTypes.get(0));
  }

  @Test
  public void findByStoreIdTest() {
    Mockito.when(productImagePredictionRepository.findByStoreId(Mockito.any()))
        .thenReturn(Arrays.asList(productImagePrediction));
    List<ProductImagePredictionResponse> response = productImagePredictionService.findByStoreId(STORE_ID);
    Mockito.verify(productImagePredictionRepository).findByStoreId(STORE_ID);
    Assertions.assertEquals(1, response.size());
    Assertions.assertEquals(PREDICTION_TYPE, response.get(0).getPredictionType());
    Assertions.assertEquals(DISPLAY_NAME, response.get(0).getDisplayName());
  }

  @Test
  public void findByStoreIdAndMarkForDeleteTest() {
    Mockito.when(productImagePredictionRepository.findByStoreIdAndMarkForDeleteFalse(Mockito.any()))
        .thenReturn(Collections.singletonList(productImagePrediction));
    List<ProductImagePrediction> response =
        productImagePredictionService.findByStoreIdAndMarkForDeleteFalse(STORE_ID);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(1, response.size());
    Assertions.assertEquals(PREDICTION_TYPE, response.get(0).getPredictionType());
    Assertions.assertEquals(DISPLAY_NAME, response.get(0).getDisplayName());
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);

    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    Mockito.verify(productImagePredictionRepository).save(productImagePrediction);
    Mockito.verify(productOutbound).upsertPredictionCategoryMapping(Mockito.anyList());
  }

  @Test
  public void updateImagePredictionAndCategoryMappingListEmptyTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(new ArrayList<>());

    productImagePrediction.setType(IMAGE);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);

    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    Mockito.verify(productImagePredictionRepository).save(productImagePrediction);
  }

  @Test
  public void updateImagePredictionAndCategoryMappingConfidenceThresholdZeroForImageTypeTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTextConfidenceThresholdNotZeroForImageTypeTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(20);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingImageTypeInvalidTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(30);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingImageTypeMaximumThresholdErrorTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(150);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingImageTypeTextTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(30);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);

    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    Mockito.verify(productImagePredictionRepository).save(productImagePrediction);
    Mockito.verify(productOutbound).upsertPredictionCategoryMapping(Mockito.anyList());
  }

  @Test
  public void updateImagePredictionAndCategoryMappingConfidenceThresholdNotZeroForTypeTextTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(30);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTextConfidenceThresholdZeroForTypeTextTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTextTypeInvalidTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTextTypeMaximumThresholdErrorTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(150);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingImageTypeImageAndTextTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(30);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);

    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    Mockito.verify(productImagePredictionRepository).save(productImagePrediction);
    Mockito.verify(productOutbound).upsertPredictionCategoryMapping(Mockito.anyList());
  }

  @Test
  public void updateImagePredictionAndCategoryMappingConfidenceThresholdZeroTypeImageAndTextTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(30);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTextConfidenceThresholdZeroTypeImageAndTextTest()
      throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingInvalidThresholdForTypeImageAndTextTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(0);
    request.setTextConfidenceThreshold(0);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingInvalidThresholdForTypeImageAndTextMaximumThresholdTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(150);
    request.setTextConfidenceThreshold(150);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingInvalidThresholdForTypeImageAndTextMaximumConfidenceThresholdTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(150);
    request.setTextConfidenceThreshold(5);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingInvalidThresholdForTypeImageAndTextMaximumTextConfidenceThresholdTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(5);
    request.setTextConfidenceThreshold(150);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(true);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void updateImagePredictionAndCategoryMappingCompareCategoryFalseTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    request.setPredictionType(PREDICTION_TYPE);
    request.setRuleEnabled(true);
    request.setConfidenceThreshold(30);
    request.setTextConfidenceThreshold(30);
    List<PredictionCategoryMappingUpdateRequest> predictionCategoryMappingUpdateRequests = new ArrayList<>();
    PredictionCategoryMappingUpdateRequest predictionCategoryMappingUpdateRequest =
        new PredictionCategoryMappingUpdateRequest();
    predictionCategoryMappingUpdateRequest.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingUpdateRequest.setMarkForDelete(false);
    predictionCategoryMappingUpdateRequests.add(predictionCategoryMappingUpdateRequest);
    request.setCategoryMappings(predictionCategoryMappingUpdateRequests);

    productImagePrediction.setType(IMAGE_AND_TEXT);
    productImagePrediction.setCompareCategory(false);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType()))
        .thenReturn(productImagePrediction);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productImagePredictionService.updateImagePredictionAndCategoryMapping(STORE_ID, request);
      });
    } finally {
      Mockito.verify(productImagePredictionRepository)
          .findByStoreIdAndPredictionType(STORE_ID, request.getPredictionType());
    }
  }

  @Test
  public void getImagePredictionAndCategoryMappingTest() throws Exception {
    List<String> predictionTypeList = new ArrayList<>();
    predictionTypeList.add(PREDICTION_TYPE);
    List<ProductImagePrediction> productImagePredictionList = new ArrayList<>();
    productImagePrediction.setId(ID);
    productImagePredictionList.add(productImagePrediction);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionTypeInAndMarkForDeleteFalse(STORE_ID,
        predictionTypeList)).thenReturn(productImagePredictionList);
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(ID);
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponses = new ArrayList<>();
    PredictionIdAndCategoryCodeResponse predictionIdAndCategoryCodeResponse = new PredictionIdAndCategoryCodeResponse();
    predictionIdAndCategoryCodeResponse.setPredictionId(ID);
    List<CategoryCodeAndNameResponse> categoryCodeAndNameResponseList = new ArrayList<>();
    CategoryCodeAndNameResponse categoryCodeAndNameResponse = new CategoryCodeAndNameResponse();
    categoryCodeAndNameResponse.setCategoryCode(CATEGORY_CODE);
    categoryCodeAndNameResponse.setCategoryName(CATEGORY_NAME);
    categoryCodeAndNameResponseList.add(categoryCodeAndNameResponse);
    predictionIdAndCategoryCodeResponse.setCategoryCodeAndNameResponseList(categoryCodeAndNameResponseList);
    predictionIdAndCategoryCodeResponses.add(predictionIdAndCategoryCodeResponse);
    Mockito.when(productOutbound.getPredictionIdAndCategoryCode(predictionIdsRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, predictionIdAndCategoryCodeResponses, null, null));

    List<ProductImagePredictionAndCategoryMappingResponse> imagePredictionAndCategoryMapping =
        this.productImagePredictionService.getImagePredictionAndCategoryMapping(STORE_ID, predictionTypeList);

    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndPredictionTypeInAndMarkForDeleteFalse(STORE_ID, predictionTypeList);
    Mockito.verify(productOutbound).getPredictionIdAndCategoryCode(predictionIdsRequest);
    Assertions.assertEquals(PREDICTION_TYPE, imagePredictionAndCategoryMapping.get(0).getPredictionType());
    Assertions.assertEquals(CATEGORY_CODE,
        imagePredictionAndCategoryMapping.get(0).getCategoryCodeAndCategoryNameResponseList().get(0).getCategoryCode());
  }

  @Test
  public void getImagePredictionAndCategoryMappingMapEmptyTest() throws Exception {
    List<String> predictionTypeList = new ArrayList<>();
    predictionTypeList.add(PREDICTION_TYPE);
    List<ProductImagePrediction> productImagePredictionList = new ArrayList<>();
    productImagePrediction.setId(ID);
    productImagePredictionList.add(productImagePrediction);
    Mockito.when(productImagePredictionRepository.findByStoreIdAndPredictionTypeInAndMarkForDeleteFalse(STORE_ID,
        predictionTypeList)).thenReturn(productImagePredictionList);
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(ID);
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponses = new ArrayList<>();
    Mockito.when(productOutbound.getPredictionIdAndCategoryCode(predictionIdsRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, predictionIdAndCategoryCodeResponses, null, null));

    List<ProductImagePredictionAndCategoryMappingResponse> imagePredictionAndCategoryMapping =
        this.productImagePredictionService.getImagePredictionAndCategoryMapping(STORE_ID, predictionTypeList);

    Mockito.verify(productImagePredictionRepository)
        .findByStoreIdAndPredictionTypeInAndMarkForDeleteFalse(STORE_ID, predictionTypeList);
    Mockito.verify(productOutbound).getPredictionIdAndCategoryCode(predictionIdsRequest);
    Assertions.assertEquals(PREDICTION_TYPE, imagePredictionAndCategoryMapping.get(0).getPredictionType());
  }

  @Test
  public void getImagePredictionAndCategoryMappingStoreIdBlankTest() throws Exception {
    List<String> predictionTypeList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      List<ProductImagePredictionAndCategoryMappingResponse> imagePredictionAndCategoryMapping =
          this.productImagePredictionService.getImagePredictionAndCategoryMapping(StringUtils.EMPTY, predictionTypeList);
    });
  }

  @Test
  public void getImagePredictionAndCategoryMappingPredictionTypeListEmptyTest() throws Exception {
    List<String> predictionTypeList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      List<ProductImagePredictionAndCategoryMappingResponse> imagePredictionAndCategoryMapping =
          this.productImagePredictionService.getImagePredictionAndCategoryMapping(STORE_ID, predictionTypeList);
    });
  }

  @Test
  public void getImagePredictionAndCategoryMappingPredictionTypeBlankTest() throws Exception {
    List<String> predictionTypeList = new ArrayList<>();
    predictionTypeList.add(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      List<ProductImagePredictionAndCategoryMappingResponse> imagePredictionAndCategoryMapping =
          this.productImagePredictionService.getImagePredictionAndCategoryMapping(STORE_ID, predictionTypeList);
    });
  }

  @Test
  public void cacheEvictForUpdateImagePredictionAndCategoryMappingTest() throws Exception {
    productImagePredictionService.cacheEvictForUpdateImagePredictionAndCategoryMapping(new ProductImagePrediction());
    Mockito.verify(applicationContext).getBean(ProductImagePredictionService.class);
  }

  @Test
  public void findByStoreIdAndPredictionConsideredTrueTest() {
    productImagePredictionService.findByStoreIdAndPredictionConsideredTrue(STORE_ID);
    Mockito.verify(productImagePredictionRepository).findByStoreIdAndPredictionConsideredTrue(STORE_ID);
  }
}
