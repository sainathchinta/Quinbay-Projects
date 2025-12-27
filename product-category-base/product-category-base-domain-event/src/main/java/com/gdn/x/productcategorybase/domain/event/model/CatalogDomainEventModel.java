package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CatalogDomainEventModel {

  private String name;
  private String catalogCode;
  private String catalogType;

  public CatalogDomainEventModel() {
    // do nothing
  }

  public CatalogDomainEventModel(String name, String catalogCode, String catalogType) {
    super();
    this.name = name;
    this.catalogCode = catalogCode;
    this.catalogType = catalogType;
  }

  public String getCatalogCode() {
    return catalogCode;
  }

  public String getCatalogType() {
    return catalogType;
  }

  public String getName() {
    return name;
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
    return String
        .format(
            "CatalogDomainEventModel [name=%s, catalogCode=%s, catalogType=%s, getName()=%s, getCatalogCode()=%s, getCatalogType()=%s]",
            name, catalogCode, catalogType, getName(), getCatalogCode(), getCatalogType());
  }

}
