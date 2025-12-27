package com.gdn.partners.pcu.internal.web.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ProductImagePredictionApiPath;
import com.gdn.partners.pcu.internal.service.ProductImagePredictionService;
import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionAndCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Product Image Prediction API")
@RestController
@RequestMapping(value = ProductImagePredictionApiPath.BASE_PATH)
@Validated
public class ProductImagePredictionController {

  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Update Product Image Prediction")
  @PutMapping(value = ProductImagePredictionApiPath.UPDATE_PREDICTIONS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateImagePrediction(@RequestBody ProductImagePredictionWebRequest request)
      throws Exception {
    log.info("Product Image Prediction Web Request: {}", request);
    return new BaseResponse(null, null, this.productImagePredictionService.update(request),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "getting auto approval rules")
  @GetMapping(value = ProductImagePredictionApiPath.GET_PREDICTION_LIST, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductImagePredictionWebResponse> getListOfPredictions() throws Exception {
    log.info("get list of product image predictions");
    List<ProductImagePredictionWebResponse> response = productImagePredictionService.getListOfPredictions();
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response, new Metadata(0, response.size(), Long.valueOf(response.size())));
  }

  @Operation(summary = "Update Product Image Prediction and Category Mapping")
  @PutMapping(value = ProductImagePredictionApiPath.UPDATE_PREDICTION_AND_CATEGORY_MAPPING, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateImagePredictionAndCategoryMapping(
      @RequestBody ProductImagePredictionAndCategoryMappingWebRequest request) throws Exception {
    log.info("Product Image Prediction And Category Mapping Web Request : {} ", request);
    return new BaseResponse(null, null,
        this.productImagePredictionService.updateImagePredictionAndCategoryMapping(request),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "get threshold detail and category mapping")
  @PostMapping(value = ProductImagePredictionApiPath.GET_THRESHOLD_DETAIL_AND_CATEGORY_MAPPING,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductImagePredictionAndCategoryMappingWebResponse> getThresholdDetailAndCategoryMapping(
      @RequestBody PredictionTypeListWebRequest request) throws Exception {
    log.info("Threshold Detail And Category Mapping Web Request : {} ", request);
    List<ProductImagePredictionAndCategoryMappingWebResponse> response =
        productImagePredictionService.getThresholdDetailAndCategoryMapping(request);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response, null);
  }
}
