package com.gdn.partners.pcu.master.model;

public interface CatalogApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/catalogs";
  String GET_SUB_CATEGORIES_BY_CATEGORY_ID = "/{catalogType}/{catalogId}/categories/{parentId}/subCategories";
  String GET_CATALOG_INFO_BY_CATALOG_TYPE = "/{catalogType}";
  String FILTER_BY_CATALOG_ID_AND_CATEGORY_NAME_AND_STATE = "/{catalogId}/categories/filter";
}
