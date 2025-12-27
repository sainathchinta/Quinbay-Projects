package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ReplaceProductItemImagesDTO {

  private static final long serialVersionUID = 5245831824515476659L;
  private String skuCode;
  private int generatedImageCount;
  
  public ReplaceProductItemImagesDTO() {
    super();
  }

  public ReplaceProductItemImagesDTO(String skuCode, int generatedImageCount) {
    super();
    this.skuCode = skuCode;
    this.generatedImageCount = generatedImageCount;
  }

  public String getSkuCode() {
    return skuCode;
  }


  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }


  public int getGeneratedImageCount() {
    return generatedImageCount;
  }

  public void setGeneratedImageCount(int generatedImageCount) {
    this.generatedImageCount = generatedImageCount;
  }
  
}
