package com.gdn.x.productcategorybase;

/**
 * @author parvej - created on 12/03/21
 */
public enum DocumentType {
  ALL("ALL"), DOCUMENT_REQUIRED("documentRequired"), DOCUMENT_NOT_REQUIRED("documentNotRequired");

  private String value;

  DocumentType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
