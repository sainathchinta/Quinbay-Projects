package com.gdn.mta.product.controller;

import java.util.List;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionRequest;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.service.ProductImagePredictionService;
import com.gdn.mta.product.service.ProductImagePredictionServiceWrapper;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.mta.product.web.model.ProductImagePredictionControllerPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping(value = ProductImagePredictionControllerPath.BASE_PATH)
@Tag(name = "ProductImagePredictionController", description = "Controller for image prediction")
@Slf4j
public class ProductImagePredictionController {

  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  @Autowired
  private ProductImagePredictionServiceWrapper productImagePredictionServiceWrapper;

  @RequestMapping(value = ProductImagePredictionControllerPath.IMAGE_PREDICTION_INSERT, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "inset image prediction", description = "saveHistory image prediction")
  @ResponseBody
  public GdnBaseRestResponse insertImagePrediction(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestBody ProductImagePredictionRequest productImagePredictionRequest) {
    log.info("Adding new image prediction : {}", productImagePredictionRequest.getPredictionType());
    ProductImagePrediction productImagePrediction = new ProductImagePrediction();
    BeanUtils.copyProperties(productImagePredictionRequest, productImagePrediction);
    productImagePrediction.setStoreId(storeId);
    this.productImagePredictionService.insert(productImagePrediction);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.IMAGE_PREDICTION_UPDATE, method = RequestMethod.PUT,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update image prediction", description = "update image prediction")
  @ResponseBody
  public GdnBaseRestResponse updateImagePrediction(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestBody ProductImagePredictionRequest productImagePredictionRequest) {
    log.info("Updating image prediction : {}", productImagePredictionRequest.getPredictionType());
    ProductImagePrediction productImagePrediction = new ProductImagePrediction();
    BeanUtils.copyProperties(productImagePredictionRequest, productImagePrediction);
    productImagePrediction.setStoreId(storeId);
    this.productImagePredictionServiceWrapper.update(productImagePrediction);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING, method =
      RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update image prediction and category mapping", description = "update image prediction and category mapping")
  @ResponseBody
  public GdnBaseRestResponse updateImagePredictionAndCategoryMapping(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @RequestBody ProductImagePredictionAndCategoryMappingRequest request)
      throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      log.info("Updating image prediction and category mapping. ProductImagePredictionAndCategoryMappingRequest : {}",
          request);
      ValidationUtil.validateImagePredictionAndCategoryMappingRequest(request);
      this.productImagePredictionServiceWrapper.updateImagePredictionAndCategoryMappingAndCacheEvict(storeId, request);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Exception caught while updating ImagePredictionAndCategoryMapping. ProductImagePredictionAndCategoryMappingRequest : {} , error - ",
          request, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error(
          "Exception caught while updating ImagePredictionAndCategoryMapping. ProductImagePredictionAndCategoryMappingRequest : {} , error - ",
          request, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.GET_IMAGE_PREDICTION_AND_CATEGORY_MAPPING,
      method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get image prediction and category mapping", description = "get image prediction and category mapping")
  @ResponseBody
  public GdnRestListResponse<ProductImagePredictionAndCategoryMappingResponse> getImagePredictionAndCategoryMapping(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody GenericStringListRequest predictionTypeList) {
    log.info("Getting imagePrediction and categoryMapping. PredictionTypeListRequest : {} ", predictionTypeList);
    try {
      List<ProductImagePredictionAndCategoryMappingResponse> imagePredictionAndCategoryMapping =
          this.productImagePredictionService.getImagePredictionAndCategoryMapping(storeId,
              predictionTypeList.getStringList());
      return new GdnRestListResponse<>(null, null, true, imagePredictionAndCategoryMapping,
          null, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Exception caught while getting image prediction and category mapping. predictionIdList : {} , error - ",
          predictionTypeList, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error(
          "Exception caught while getting image prediction and category mapping. predictionIdList : {} , error - ",
          predictionTypeList, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.IMAGE_PREDICTION_DELETE, method = RequestMethod.DELETE,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete image prediction", description = "delete image prediction")
  @ResponseBody
  public GdnBaseRestResponse deleteImagePrediction(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId, @PathVariable String predictionType) {
    log.info("Deleting image predictionType : {}", predictionType);
    this.productImagePredictionService.delete(storeId, predictionType);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.IMAGE_PREDICTION_FIND, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "Find image prediction", description = "Find image prediction")
  public GdnRestSingleResponse<ProductImagePredictionResponse> findImagePrediction(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String predictionType) {
    log.info("Fetching image predictionType : {}", predictionType);
    ProductImagePredictionResponse productImagePredictionResponse = new ProductImagePredictionResponse();
    ProductImagePrediction response =
        this.productImagePredictionService.findByStoreIdAndPredictionType(storeId, predictionType);
    BeanUtils.copyProperties(response, productImagePredictionResponse);
    return new GdnRestSingleResponse<>(productImagePredictionResponse, requestId);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.GET_IMAGE_QC_PREDICTION_RESPONSE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get ImageQc prediction response", description = "get ImageQc prediction response")
  @ResponseBody
  public GdnRestSingleResponse<ImageQcProcessedResponse> getImageQcPredictionResponse(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String productCode) {
    log.info("Fetching image prediction response by productCode : {}", productCode);
    return new GdnRestSingleResponse<>(
        productImagePredictionService.findProductImagePredictionResponseByStoreIdAndProductCode(storeId, productCode),
        requestId);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.GET_IMAGE_QC_PREDICTION_AND_BRAND_RESPONSE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get ImageQc Prediction and brand response", description = "get ImageQc Prediction and brand response")
  @ResponseBody
  public GdnRestSingleResponse<ImageQcProcessedAndBrandResponse> getImageQcPredictionAndBrandResponse(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String productCode) throws Exception {
    log.info("Fetching image prediction and brand response by productCode : {}", productCode);
    return new GdnRestSingleResponse<>(productImagePredictionService
        .findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(storeId, productCode), requestId);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.GET_DIFFERENT_PREDICTION_TYPE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get different prediction type", description = "get different prediction type")
  @ResponseBody
  public GdnRestListResponse<PredictionTypeResponse> getDifferentPredictionType(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId) {
    log.info("Get list of different prediction types: requestId{}", requestId);
    List<PredictionTypeResponse> response = productImagePredictionService.getDifferentPredictionType(storeId);
    return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
  }

  @RequestMapping(value = ProductImagePredictionControllerPath.GET_PREDICTION_LIST, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get list of predictions", description = "get list of predictions")
  @ResponseBody
  public GdnRestListResponse<ProductImagePredictionResponse> getPredictionList(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId) {
    log.info("Fetching predictions with need revision threshold");
    List<ProductImagePredictionResponse> response =
        productImagePredictionService.findByStoreIdAndPredictionConsideredTrue(storeId);
    return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
  }
}