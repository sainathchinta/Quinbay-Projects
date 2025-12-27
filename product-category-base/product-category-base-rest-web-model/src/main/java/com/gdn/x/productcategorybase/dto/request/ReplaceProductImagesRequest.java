package com.gdn.x.productcategorybase.dto.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ReplaceProductImagesRequest extends BaseDTORequest {
  
  private static final long serialVersionUID = -6079570469982828094L;
  private String productCode;
  private int generatedImageCount;
  private List<ReplaceProductItemImagesRequest> productItem;
  
  public ReplaceProductImagesRequest() {
    super();
  }
  
  public ReplaceProductImagesRequest(String productCode, int generatedImageCount,
      List<ReplaceProductItemImagesRequest> productItem) {
    super();
    this.productCode = productCode;
    this.generatedImageCount = generatedImageCount;
    this.productItem = productItem;
  }

  public List<ReplaceProductItemImagesRequest> getProductItem() {
    return productItem;
  }

  public void setProductItem(List<ReplaceProductItemImagesRequest> productItem) {
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
