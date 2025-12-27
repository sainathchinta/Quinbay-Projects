package com.gdn.x.productcategorybase.mixin;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductMixin {

  @JsonProperty
  String getBrand();

  @JsonProperty
  byte[] getDescription();

  @JsonProperty
  Double getHeight();

  @JsonProperty
  Double getLength();

  @JsonProperty
  byte[] getLongDescription();

  @JsonProperty
  String getName();

  @JsonProperty
  List<ProductAttribute> getProductAttributes();

  @JsonProperty
  List<ProductCategory> getProductCategories();

  @JsonProperty
  String getProductCode();

  // @JsonProperty("images")
  @JsonIgnore
  List<ProductImage> getProductImages();

  @JsonIgnore
  List<ProductItem> getProductItems();

  @JsonProperty
  Double getShippingWeight();

  @JsonProperty
  String getUniqueSellingPoint();

  @JsonProperty
  String getUom();

  @JsonProperty
  Double getWeight();

  @JsonProperty
  Double getWidth();

  @JsonProperty
  boolean isActivated();
}
