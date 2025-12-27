package com.gdn.x.productcategorybase.mixin;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Product;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductImageMixin {
  @JsonProperty
  String getCreatedBy();

  @JsonProperty
  Date getCreatedDate();

  @JsonProperty
  String getId();

  @JsonProperty
  String getLocationPath();

  @JsonIgnore
  Product getProduct();

  @JsonProperty
  public Integer getSequence();

  @JsonProperty
  String getStoreId();

  @JsonProperty
  String getUpdatedBy();

  @JsonProperty
  Date getUpdatedDate();

  @JsonProperty
  long getVersion();

  @JsonProperty
  boolean isMainImages();

  @JsonProperty
  boolean isMarkForDelete();
}
