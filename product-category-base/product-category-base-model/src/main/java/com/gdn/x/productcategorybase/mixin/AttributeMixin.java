package com.gdn.x.productcategorybase.mixin;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;


@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface AttributeMixin {

  @JsonIgnore
  List<AllowedAttributeValue> getAllowedAttributeValues();

  @JsonProperty
  AttributeType getAttributeType();

  @JsonIgnore
  List<CategoryAttribute> getCategoryTypes();

  @JsonProperty
  String getName();

  @JsonProperty
  boolean isSearchAble();

  @JsonProperty
  boolean isSkuValue();
}
