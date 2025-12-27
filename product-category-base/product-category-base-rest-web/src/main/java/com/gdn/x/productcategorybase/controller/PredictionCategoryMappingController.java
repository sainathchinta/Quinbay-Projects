package com.gdn.x.productcategorybase.controller;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.PredictionCategoryMappingApiPath;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.service.PredictionCategoryMappingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = PredictionCategoryMappingApiPath.BASE_PATH)
@Tag(name = "PredictionCategoryMappingController", description = "PredictionCategoryMappingService")
public class PredictionCategoryMappingController {

  @Autowired
  private PredictionCategoryMappingService predictionCategoryMappingService;

  @RequestMapping(value = PredictionCategoryMappingApiPath.UPSERT, method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update or insert prediction category mapping", description = "update or insert prediction category mapping")
  
  public GdnBaseRestResponse upsertPredictionCategoryMapping(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<PredictionCategoryMappingRequest> requestList)
      throws Exception {
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(requestList),
          ErrorMessage.REQUEST_LIST_MUST_NOT_BE_EMPTY.getMessage());
      predictionCategoryMappingService.upsertPredictionCategoryMapping(storeId, requestList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while updating or adding predictionCategoryMapping. requestId : {} , request : {} , error - ",
          requestId, requestList, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("Error while updating or adding predictionCategoryMapping. requestId : {} , request : {} , error - ",
          requestId, requestList, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          Boolean.FALSE, requestId);
    }
  }

  @RequestMapping(value = PredictionCategoryMappingApiPath.GET_PREDICTION_ID_AND_CATEGORY_CODE, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get predictionId and categoryCode", description = "get predictionId and categoryCode")
  
  public GdnRestListResponse<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody PredictionIdsRequest predictionIdsRequest) throws Exception {
    log.info("Fetching predictionId and CategoryCode for predictionIdList : {} ", predictionIdsRequest);
    try {
      List<PredictionIdAndCategoryCodeResponse> responseList =
          predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(storeId,
              predictionIdsRequest.getPredictionIdList());
      return new GdnRestListResponse<>(null, null, true, responseList, null, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while getting predictionId and categoryCode. predictionIdsRequest : {} , error - ",
          predictionIdsRequest, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting predictionId and categoryCode. predictionIdsRequest : {} , error - ",
          predictionIdsRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = PredictionCategoryMappingApiPath.GET_PREDICTION_LIST_BY_CATEGORY_CODE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "API to validate product's prediction and category", description = "API to validate product's prediction and category")
  
  public GdnRestListResponse<ProductPredictionCategoryMappingResponse> getPredictionListByCategoryCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("categoryCode") String categoryCode) throws Exception {
    try {
      List<ProductPredictionCategoryMappingResponse> predictionListByCategoryCode =
          predictionCategoryMappingService.getPredictionListByCategoryCode(storeId, categoryCode);
      return new GdnRestListResponse<>(null, null, true, predictionListByCategoryCode, new PageMetaData(), requestId);
    } catch (Exception e) {
      log.error("Error while fetching predictionCategoryMapping for categoryCode : {}, error - ", categoryCode, e);
      return new GdnRestListResponse<>(null, null, false, null, new PageMetaData(), requestId);
    }
  }
}
