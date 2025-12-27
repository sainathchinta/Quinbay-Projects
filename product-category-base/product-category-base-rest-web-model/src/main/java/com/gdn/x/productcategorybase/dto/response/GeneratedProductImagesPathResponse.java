package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GeneratedProductImagesPathResponse extends BaseResponse {
  private static final long serialVersionUID = 3400256588990070460L;
  private String productCode;
  private List<ImagePathResponse> images;
  private List<GeneratedProductItemImagesPathResponse> productItems;
  
  public GeneratedProductImagesPathResponse() {
    super();
  }
  
  public GeneratedProductImagesPathResponse(String productCode, List<ImagePathResponse> images,
      List<GeneratedProductItemImagesPathResponse> productItems) {
    super();
    this.productCode = productCode;
    this.images = images;
    this.productItems = productItems;
  }

  public List<GeneratedProductItemImagesPathResponse> getProductItems() {
    return productItems;
  }

  public void setProductItems(List<GeneratedProductItemImagesPathResponse> productItems) {
    this.productItems = productItems;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<ImagePathResponse> getImages() {
    return images;
  }

  public void setImages(List<ImagePathResponse> images) {
    this.images = images;
  }
  
}
