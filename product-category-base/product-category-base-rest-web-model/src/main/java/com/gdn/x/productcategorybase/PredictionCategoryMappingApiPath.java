package com.gdn.x.productcategorybase;

public interface PredictionCategoryMappingApiPath {
  String BASE_PATH = "/api/predictionCategoryMapping";
  String UPSERT = "/upsert";
  String GET_PREDICTION_ID_AND_CATEGORY_CODE = "/getPredictionIdAndCategoryCode";
  String GET_PREDICTION_LIST_BY_CATEGORY_CODE = "/predictionList/{categoryCode}";
}
