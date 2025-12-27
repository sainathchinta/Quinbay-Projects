package com.gdn.x.productcategorybase;

public interface RestrictedKeywordApiPath {
  String BASE_PATH = "/api/restricted-keywords";
  String SEARCH_RESTRICTED_KEYWORDS = "/search";
  String HISTORY = "/{keywordId}/history";
  String LISTING = "/listing";
  String UI_VALIDATION_LIST = "/ui-validation-list";
  String UPDATE_RESTRICTED_KEYWORDS = "/update-restricted-keywords";
}
