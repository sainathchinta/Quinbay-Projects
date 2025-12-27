package com.gdn.x.productcategorybase.mixin;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Attribute;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface PredefinedAllowedAttributeValueMixin {

  @JsonIgnore
  Attribute getAttribute();

  @JsonProperty
  String getPredefinedAllowedAttributeCode();

  @JsonProperty
  Integer getSequence();

  @JsonProperty
  String getValue();

}
