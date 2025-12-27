package com.gdn.x.productcategorybase.mixin;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface CategoryMixin {

  @JsonProperty
  Catalog getCatalog();

  @JsonIgnore
  List<CategoryAttribute> getCategoryAttributes();

  @JsonProperty
  String getCategoryCode();

  @JsonProperty
  byte[] getDescription();

  @JsonProperty
  String getId();

  @JsonProperty
  String getName();

  @JsonProperty
  Category getParentCategory();

  @JsonIgnore
  List<ProductCategory> getProductCategories();

  @JsonProperty
  Integer getSequence();

  @JsonProperty
  String getShortDescription();

  @JsonProperty
  String getStoreId();

  @JsonProperty
  boolean isActivated();

  @JsonProperty
  boolean isDisplay();

  @JsonProperty
  boolean isMarkForDelete();

  @JsonProperty
  boolean isNeedIdentity();

  @JsonProperty
  boolean isWarranty();

}
