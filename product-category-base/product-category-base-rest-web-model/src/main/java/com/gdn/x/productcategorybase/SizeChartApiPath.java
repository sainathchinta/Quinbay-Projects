package com.gdn.x.productcategorybase;

public interface SizeChartApiPath {
  String BASE_PATH = "/api/size-chart";
  String FILTER = "/filter";
  String UPSERT = "/upsert";
  String DETAIL = "/{sizeChartCode}/detail";
  String UPDATE_SIZE_CHART_STATUS = "/{sizeChartCode}/update-size-chart-status";
  String FIND_BY_NAME_AND_BUSINESS_PARTNER_CODE = "/findByNameAndBusinessPartnerCode";
  String VALIDATE_SIZE_CHART_CODE = "/validateSizeChartCode/{sizeChartCode}";
  String VALIDATE_CATEGORY_CODE = "/validCategory";
  String GET_SIZE_CHART_CODE = "/get-size-chart-name";
}
