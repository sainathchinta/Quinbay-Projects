package com.gdn.partners.pcu.internal.model;

public interface BrandApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/brands";
  String DELETE = "/{brandCode}";
  String IMAGE = "/brandImage";
  String UPDATE = "/update";
  String DETAIL = "/{brandCode}";
}
