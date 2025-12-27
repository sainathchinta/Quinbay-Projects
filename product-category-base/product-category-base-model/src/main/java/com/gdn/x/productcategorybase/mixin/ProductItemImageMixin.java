package com.gdn.x.productcategorybase.mixin;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.ProductItem;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductItemImageMixin {

  @JsonProperty
  public String getCreatedBy();

  @JsonProperty
  public Date getCreatedDate();

  @JsonProperty
  public String getId();

  @JsonProperty
  public String getLocationPath();

  @JsonIgnore
  ProductItem getProductItem();

  @JsonProperty
  public Integer getSequence();

  @JsonProperty
  public String getStoreId();

  @JsonProperty
  public String getUpdatedBy();

  @JsonProperty
  public Date getUpdatedDate();

  @JsonProperty
  public long getVersion();

  @JsonProperty
  public boolean isMainImages();
}
