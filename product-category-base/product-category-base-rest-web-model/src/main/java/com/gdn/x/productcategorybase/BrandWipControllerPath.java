package com.gdn.x.productcategorybase;

public interface BrandWipControllerPath {
   String BASE_PATH = "/api/brand-wip";
   String CREATE = "/create";
   String DETAIL = "/detail";
   String HISTORY_FILTER_SUMMARY = "/history/filter/summary";
   String FILTER_SUMMARY = "/filter/summary";
   String REJECTED_REASON = "/{brandRequestCode}/rejectedReason";
   String UPDATE = "/update";
   String APPROVE = "/approve";
   String REJECT = "/reject";
   String DETAIL_BRAND_CODE = "/detailByBrandCode";
   String FILTER_BRAND_REQUEST_CODE = "/filter/brand-request-code";
   String FILTER_BRAND_NAME = "/filter/brand-name";
   String IN_REVIEW_BRANDS = "/inReviewBrands";
}
