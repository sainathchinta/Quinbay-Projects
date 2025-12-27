package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionAndCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImagePredictionWebRequest;

import java.util.List;

import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;

public interface ProductImagePredictionService {

  /**
   * Update product image prediction
   *
   * @param productImagePredictionWebRequest
   * @return
   */
  boolean update(ProductImagePredictionWebRequest productImagePredictionWebRequest) throws Exception;


  /** to get list of product image predictions
   *
   * @return
   */
  List<ProductImagePredictionWebResponse> getListOfPredictions();

  /**
   * update product image prediction and category mapping
   *
   * @param request
   * @return
   * @throws Exception
   */
  boolean updateImagePredictionAndCategoryMapping(ProductImagePredictionAndCategoryMappingWebRequest request) throws Exception;

  /**
   * To get Threshold detail and category mapping
   *
   * @param request
   * @return
   */
  List<ProductImagePredictionAndCategoryMappingWebResponse> getThresholdDetailAndCategoryMapping(
      PredictionTypeListWebRequest request);
}

