package com.gdn.x.productcategorybase.mixin;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.ProductItem;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductItemAttributeValueMixin {

  @JsonProperty
  Attribute getAttribute();

  @JsonIgnore
  ProductItem getProductItem();

  @JsonProperty
  String getValue();
}
