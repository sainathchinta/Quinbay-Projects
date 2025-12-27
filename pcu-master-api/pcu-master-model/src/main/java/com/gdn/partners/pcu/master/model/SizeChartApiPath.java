package com.gdn.partners.pcu.master.model;

public interface SizeChartApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/size-chart";
  String UPSERT = "/upsert";
  String FETCH_SIZE_CHART = "/{sizeChartCode}/detail";
  String DELETE = "/{sizeChartCode}/delete";
  String FILTER = "/filter";
  String VALIDATE = "/validate";
  String VALID_CATEGORY = "/validCategory";

}
