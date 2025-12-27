package com.gdn.x.productcategorybase.mixin;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductAttributeMixin {

  @JsonProperty
  Attribute getAttribute();

  @JsonIgnore
  Product getProduct();

  @JsonProperty
  String getProductAttributeName();

  @JsonProperty
  List<ProductAttributeValue> getProductAttributeValues();

  @JsonProperty
  Integer getSequence();

  @JsonProperty
  boolean isOwnByProductItem();
}
