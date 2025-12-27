package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ReplaceProductItemImagesRequest extends BaseDTORequest {

  private static final long serialVersionUID = 5245831824515476659L;
  private String skuCode;
  private int generatedImageCount;
  
  public ReplaceProductItemImagesRequest() {
    super();
  }
  
  public ReplaceProductItemImagesRequest(String skuCode, int generatedImageCount) {
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
