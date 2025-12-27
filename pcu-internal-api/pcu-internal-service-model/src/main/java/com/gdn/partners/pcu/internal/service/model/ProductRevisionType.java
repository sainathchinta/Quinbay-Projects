package com.gdn.partners.pcu.internal.service.model;

/**
 * Created by govind on 14/01/2019 AD.
 */
public enum ProductRevisionType {

  REVISION_TYPE_SCREENING("productDraftInternal");

  private String name;

  ProductRevisionType(String type) {
    this.name = type;
  }

  public String getName() {
    return name;
  }
}
