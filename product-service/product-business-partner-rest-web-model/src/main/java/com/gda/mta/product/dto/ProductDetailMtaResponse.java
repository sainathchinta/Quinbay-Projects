package com.gda.mta.product.dto;

import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ProductDetailMtaResponse extends ProductDetailResponse {
  private static final long serialVersionUID = 6294879428242510614L;
  private Boolean isReviewedContent;
  private Boolean isReviewedImage;

  public ProductDetailMtaResponse() {};

  public ProductDetailMtaResponse(ProductResponse product) {
    super(product);
  }

  public Boolean getIsReviewedContent() {
    return isReviewedContent;
  }

  public Boolean getIsReviewedImage() {
    return isReviewedImage;
  }

  public void setIsReviewedContent(Boolean isReviewedContent) {
    this.isReviewedContent = isReviewedContent;
  }

  public void setIsReviewedImage(Boolean isReviewedImage) {
    this.isReviewedImage = isReviewedImage;
  }


}
