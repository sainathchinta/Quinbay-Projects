package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CatalogRequest extends BaseDTORequest {

  private static final long serialVersionUID = 1034945429089797765L;

  private String name;
  private String catalogCode;
  private String catalogType;

  public CatalogRequest() {
    // nothing to do here
  }

  public CatalogRequest(String name, String catalogCode, String catalogType, String storeId) {
    this.name = name;
    this.catalogCode = catalogCode;
    this.catalogType = catalogType;
    this.setStoreId(storeId);
  }

  public String getCatalogCode() {
    return this.catalogCode;
  }

  public String getCatalogType() {
    return this.catalogType;
  }

  public String getName() {
    return this.name;
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public void setCatalogType(String catalogType) {
    this.catalogType = catalogType;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return String.format("Catalog [name=%s, catalogCode%s, catalogType=%s, toString()=%s]", this.name,
        this.getCatalogCode(), this.getCatalogType(), super.toString());
  }

}
