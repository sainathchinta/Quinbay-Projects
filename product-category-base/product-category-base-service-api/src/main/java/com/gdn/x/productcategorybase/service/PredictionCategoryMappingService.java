package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;

import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;

import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;


public interface PredictionCategoryMappingService {

  /**
   * update or insert prediction category mapping
   *
   * @param storeId
   * @param requestList
   * @throws Exception
   */
  void upsertPredictionCategoryMapping(String storeId, List<PredictionCategoryMappingRequest> requestList)
      throws Exception;

  /**
   * get predictionId and categoryCode List
   *
   * @param storeId
   * @param predictionIdList
   * @return
   * @throws Exception
   */
  List<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCodeResponse(String storeId, List<String> predictionIdList) throws Exception;

  /**
   * Validate product prediction and category
   *
   * @param storeId
   * @param categoryCode
   */
  List<ProductPredictionCategoryMappingResponse> getPredictionListByCategoryCode(String storeId, String categoryCode);
}
