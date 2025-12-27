package com.gdn.x.productcategorybase.dto.response;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemDetailResponse extends ProductItemResponse {

  private static final long serialVersionUID = -831070345017868333L;

  private ProductResponse productResponse;

  private List<ProductItemAttributeValueResponse> productItemAttributeValueResponses;

  public ProductItemDetailResponse() {}

  public ProductItemDetailResponse(ProductItemResponse productItem) {
    super(productItem.getGeneratedItemName(), productItem.getItemLength(), productItem.getItemWidth(),
        productItem.getItemHeight(), productItem.getItemWeight(), productItem.getItemDeliveryWeight(),
        productItem.getUpcCode(), productItem.getSkuCode(), productItem.isActivated(), productItem.isViewable(),
        productItem.getHash(), productItem.getImages());
  }

  public List<ProductItemAttributeValueResponse> getProductItemAttributeValueResponses() {
    return this.productItemAttributeValueResponses;
  }

  public ProductResponse getProductResponse() {
    return this.productResponse;
  }

  public void setProductItemAttributeValueResponses(
      List<ProductItemAttributeValueResponse> productItemAttributeValueResponses) {
    this.productItemAttributeValueResponses = productItemAttributeValueResponses;
  }

  public void setProductResponse(ProductResponse productResponse) {
    this.productResponse = productResponse;
  }
}
