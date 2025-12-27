package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.ItemAndPickupPointBasicDetailVo;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ActiveProductDetailVo implements Serializable {
  private static final long serialVersionUID = 2504748362708202743L;

  private String productName;

  private String productSku;

  private String productCode;

  private String imageUrl;

  private String merchantCode;

  private String status;

  private String masterCatalog;

  private Integer itemCount;

  private List<ItemNameSkuVO> itemNameSkuVOS;

  private ItemAndPickupPointBasicDetailVo itemAndPickupPointBasicDetailVo;

  private int l5Count;

  private boolean bundleProduct;


  public String getProductName() {
    return productName;
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

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setStatus(String status) { this.status = status; }

  public void setMasterCatalog(String masterCatalog) { this.masterCatalog = masterCatalog;}

  public List<ItemNameSkuVO> getItemDetailVOList() {
    return itemNameSkuVOS;
  }

  public void setItemDetailVOList(List<ItemNameSkuVO> itemNameSkuVOS) {
    this.itemNameSkuVOS = itemNameSkuVOS;
  }

  public void setItemCount(Integer itemCount) { this.itemCount = itemCount; }

  public void setL5Count(int l5Count) { this.l5Count = l5Count; }

  public int getL5Count() { return l5Count; }

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
    final StringBuilder sb = new StringBuilder("ProductDetailVo{");
    sb.append("productName='").append(productName).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", imageUrl='").append(imageUrl).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", status='").append(status).append('\'');
    sb.append(", itemNameSkuVOS='").append(itemNameSkuVOS).append('\'');
    sb.append(", itemCount='").append(itemCount).append('\'');
    sb.append(", l5Count ='").append(l5Count).append('\'');
    sb.append(",  bundleProduct='").append(bundleProduct).append('\'');
    sb.append('}');
    return sb.toString();
  }

  public boolean isBundleProduct() {
    return bundleProduct;
  }

  public void setBundleProduct(boolean bundleProduct) {
    this.bundleProduct = bundleProduct;
  }

  public ItemAndPickupPointBasicDetailVo getItemAndPickupPointBasicDetailVo() {
    return itemAndPickupPointBasicDetailVo;
  }

  public void setItemAndPickupPointBasicDetailVo(ItemAndPickupPointBasicDetailVo itemAndPickupPointBasicDetailVo) {
    this.itemAndPickupPointBasicDetailVo = itemAndPickupPointBasicDetailVo;
  }
}
