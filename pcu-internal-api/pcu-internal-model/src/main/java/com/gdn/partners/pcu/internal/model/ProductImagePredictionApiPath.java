package com.gdn.partners.pcu.internal.model;

public interface ProductImagePredictionApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/product-image-prediction";
  String UPDATE_PREDICTIONS = "/update-predictions";
  String GET_PREDICTION_LIST = "/prediction-list";
  String UPDATE_PREDICTION_AND_CATEGORY_MAPPING = "/update-prediction-and-category-mapping";
  String GET_THRESHOLD_DETAIL_AND_CATEGORY_MAPPING = "/get-threshold-detail-and-category-mapping";
}
