package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.ItemAndPickupPointBasicDetailResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ActiveProductResponse extends BaseResponse {
  private static final long serialVersionUID = 1105402601234366733L;

  private String productSku;

  private String productCode;

  private String productName;

  private String imageUrl;

  private String merchantCode;

  private String status;

  private String masterCatalog;

  private Integer itemCount;

  private List<ItemDetailResponse> itemDetailResponses;

  private ItemAndPickupPointBasicDetailResponse itemAndPickupPointBasicDetailResponse;

  private int l5Count;
  private Boolean bundleProduct;

  public Boolean getBundleProduct() {
    return bundleProduct;
  }

  public void setBundleProduct(Boolean bundleProduct) {
    this.bundleProduct = bundleProduct;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public String getProductName() {
    return productName;
  }

  public String getProductSku() {
    return productSku;
  }

  public String getStatus() { return status; }

  public String getMasterCatalog() { return masterCatalog; }

  public Integer getItemCount() { return itemCount; }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setStatus(String status) { this.status = status; }

  public void setMasterCatalog(String masterCatalog) { this.masterCatalog = masterCatalog; }

  public void setL5Count(int l5Count) { this.l5Count = l5Count; }

  public int getL5Count() {
    return l5Count;
  }

  public List<ItemDetailResponse> getItemDetailResponses() {
    return itemDetailResponses;
  }

  public void setItemDetailResponses(List<ItemDetailResponse> itemDetailResponses) {
    this.itemDetailResponses = itemDetailResponses;
  }

  public void setItemCount(Integer itemCount) { this.itemCount = itemCount; }

  public ItemAndPickupPointBasicDetailResponse getItemAndPickupPointBasicDetailResponse() {
    return itemAndPickupPointBasicDetailResponse;
  }

  public void setItemAndPickupPointBasicDetailResponse(
      ItemAndPickupPointBasicDetailResponse itemAndPickupPointBasicDetailResponse) {
    this.itemAndPickupPointBasicDetailResponse = itemAndPickupPointBasicDetailResponse;
  }

  @Override
  public boolean equals(Object obj) {

    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {

    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductDetailResponse{");
    sb.append("productSku='").append(productSku).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", imageUrl='").append(imageUrl).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemDetailResponses='").append(itemDetailResponses).append('\'');
    sb.append(", status='").append(status).append('\'');
    sb.append(", masterCatalog='").append(masterCatalog).append('\'');
    sb.append(", itemCount='").append(itemCount).append('\'');
    sb.append(", bundleProduct='").append(bundleProduct).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
