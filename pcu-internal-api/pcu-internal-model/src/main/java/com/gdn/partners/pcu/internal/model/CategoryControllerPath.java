package com.gdn.partners.pcu.internal.model;

public interface CategoryControllerPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/categories";
  String FETCH_CATEGORY_TREE = "/category-tree-with-review-config";
  String RESTRICTED_KEYWORD_BULK_UPLOAD = "/{processType}/restricted-keyword-bulk-upload";
  String HISTORY = "/history";
}
