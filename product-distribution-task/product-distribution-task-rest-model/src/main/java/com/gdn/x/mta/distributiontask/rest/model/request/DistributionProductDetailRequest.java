package com.gdn.x.mta.distributiontask.rest.model.request;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by virajjasani on 21/09/16.
 */
public class DistributionProductDetailRequest extends DistributionProductRequest {

  private static final long serialVersionUID = -5359789219860338481L;

  private List<DistributionProductImageRequest> productImages = new ArrayList<>();
  private List<DistributionProductItemRequest> productItems = new ArrayList<>();
  private List<DistributionProductAttributeRequest> productAttributes = new ArrayList<>();

  public DistributionProductDetailRequest() {
    // no implementation
  }

  public DistributionProductDetailRequest(
      List<DistributionProductImageRequest> productImages,
      List<DistributionProductItemRequest> productItems,
      List<DistributionProductAttributeRequest> productAttributes) {
    this.productImages = productImages;
    this.productItems = productItems;
    this.productAttributes = productAttributes;
  }

  public List<DistributionProductImageRequest> getProductImages() {
    return productImages;
  }

  public void setProductImages(
      List<DistributionProductImageRequest> productImages) {
    this.productImages = productImages;
  }

  public List<DistributionProductItemRequest> getProductItems() {
    return productItems;
  }

  public void setProductItems(
      List<DistributionProductItemRequest> productItems) {
    this.productItems = productItems;
  }

  public List<DistributionProductAttributeRequest> getProductAttributes() {
    return productAttributes;
  }

  public void setProductAttributes(
      List<DistributionProductAttributeRequest> productAttributes) {
    this.productAttributes = productAttributes;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("DistributionProductDetailRequest{");
    sb.append("productImages=").append(productImages);
    sb.append(", productItems=").append(productItems);
    sb.append(", productAttributes=").append(productAttributes);
    sb.append('}');
    return sb.toString();
  }
}
