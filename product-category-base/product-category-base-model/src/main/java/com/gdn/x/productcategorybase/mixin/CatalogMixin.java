package com.gdn.x.productcategorybase.mixin;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.entity.Category;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface CatalogMixin {

  @JsonProperty
  CatalogType getCatalogType();

  @JsonIgnore
  List<Category> getCategories();

  @JsonProperty
  String getName();
}
