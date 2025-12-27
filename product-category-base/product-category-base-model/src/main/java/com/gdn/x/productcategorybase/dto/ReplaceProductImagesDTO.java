package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ReplaceProductImagesDTO {
  
  private static final long serialVersionUID = -6079570469982828094L;
  private String productCode;
  private int generatedImageCount;
  private List<ReplaceProductItemImagesDTO> productItem;
  
  public ReplaceProductImagesDTO() {
    super();
  }

  public ReplaceProductImagesDTO(String productCode, int generatedImageCount,
      List<ReplaceProductItemImagesDTO> productItem) {
    super();
    this.productCode = productCode;
    this.generatedImageCount = generatedImageCount;
    this.productItem = productItem;
  }

  public List<ReplaceProductItemImagesDTO> getProductItem() {
    return productItem;
  }

  public void setProductItem(List<ReplaceProductItemImagesDTO> productItem) {
    this.productItem = productItem;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public int getGeneratedImageCount() {
    return generatedImageCount;
  }

  public void setGeneratedImageCount(int generatedImageCount) {
    this.generatedImageCount = generatedImageCount;
  }
  
}
