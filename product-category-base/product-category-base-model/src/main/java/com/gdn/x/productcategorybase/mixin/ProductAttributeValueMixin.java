package com.gdn.x.productcategorybase.mixin;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttribute;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductAttributeValueMixin {

  @JsonProperty
  AllowedAttributeValue getAllowedAttributeValue();

  @JsonProperty
  String getDescriptiveAttributeValue();

  @JsonProperty
  DescriptiveAttributeValueType getDescriptiveAttributeValueType();

  @JsonProperty
  PredefinedAllowedAttributeValue getPredefinedAllowedAttributeValue();

  @JsonIgnore
  ProductAttribute getProductAttribute();
}
