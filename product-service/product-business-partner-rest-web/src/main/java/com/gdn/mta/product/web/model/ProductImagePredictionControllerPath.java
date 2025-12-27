package com.gdn.mta.product.web.model;

public interface ProductImagePredictionControllerPath {
  String BASE_PATH = "/api/productImagePrediction";
  String IMAGE_PREDICTION_INSERT = "/";
  String IMAGE_PREDICTION_UPDATE = "/update";
  String UPDATE_IMAGE_PREDICTION_AND_CATEGORY_MAPPING = "/updatePredictionAndCategoryMapping";
  String GET_IMAGE_PREDICTION_AND_CATEGORY_MAPPING = "/getImagePredictionAndCategoryMapping";
  String IMAGE_PREDICTION_FIND = "/find";
  String IMAGE_PREDICTION_DELETE = "/{predictionType}";
  String GET_IMAGE_QC_PREDICTION_RESPONSE = "/getImageQcPredictionResponse";
  String GET_IMAGE_QC_PREDICTION_AND_BRAND_RESPONSE = "/getImageQcPredictionAndBrandResponse";
  String GET_DIFFERENT_PREDICTION_TYPE = "/getDifferentPredictionType";
  String GET_PREDICTION_LIST = "/prediction-list";
}
