package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CatalogResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 7079252461520108861L;

  private String name;
  private String catalogCode;
  private String catalogType;

  public CatalogResponse() {
    // nothing to do here
  }

  public CatalogResponse(String name, String catalogCode, String catalogType) {
    super();
    this.name = name;
    this.catalogCode = catalogCode;
    this.catalogType = catalogType;
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
    return String.format("CatalogResponse [name=%s, catalogCode=%s, catalogType=%s, toString()=%s]", this.name,
        this.catalogCode, this.catalogType, super.toString());
  }

}
