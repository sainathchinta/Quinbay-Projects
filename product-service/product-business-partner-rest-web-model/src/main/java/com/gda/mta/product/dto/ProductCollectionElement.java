package com.gda.mta.product.dto;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCollectionElement implements Serializable {

  private static final long serialVersionUID = -7152655055882076957L;

  public ProductCollectionElement(String productCode, String productName, String createdDate,
      String uploader) {
    this.productCode = productCode;
    this.productName = productName;
    this.createdDate = createdDate;
    this.uploader = uploader;
  }

  public ProductCollectionElement() {
  }

  private String productCode;

  private String productName;

  private String createdDate;

  private String uploader;

  public String getCreatedDate() {
    return this.createdDate;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public String getProductName() {
    return this.productName;
  }

  public String getUploader() {
    return this.uploader;
  }

  public void setCreatedDate(String createdDate) {
    this.createdDate = createdDate;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setUploader(String uploader) {
    this.uploader = uploader;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode)
        .append("productName", productName).append("createdDate", createdDate)
        .append("uploader", uploader).toString();
  }
}
