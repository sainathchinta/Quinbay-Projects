package com.gdn.x.productcategorybase.mixin;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public interface ProductItemLevel1Mixin {

  @JsonProperty
  String getCreatedBy();

  @JsonProperty
  Date getCreatedDate();

  @JsonProperty
  String getGeneratedItemName();

  @JsonProperty
  String getId();

  @JsonProperty
  Double getItemDeliveryWeight();

  @JsonProperty
  Double getItemHeight();

  @JsonProperty
  Double getItemLength();

  @JsonProperty
  Double getItemWeight();

  @JsonProperty
  Double getItemWidth();

  @JsonIgnore
  Product getProduct();

  @JsonProperty
  List<ProductItemAttributeValue> getProductItemAttributeValues();

  // @JsonProperty("images")
  @JsonIgnore
  List<ProductItemImage> getProductItemImages();

  @JsonProperty
  String getSkuCode();

  @JsonProperty
  String getStoreId();

  @JsonProperty
  String getUpcCode();

  @JsonProperty
  boolean isActivated();

  @JsonProperty
  boolean isMarkForDelete();
}
