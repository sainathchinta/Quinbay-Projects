package com.gdn.partners.pcu.master.model;

public interface RestrictedKeywordApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/restricted-keywords";
  String SEARCH = "/search";
  String HISTORY = "/{keywordId}/history";
  String UI_VALIDATION = "/ui-validation-list";
  String LISTING = "/listing";
  String UPSERT = "/upsert";
}
