package com.gdn.x.productcategorybase.mixin;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;

@JsonAutoDetect(getterVisibility = Visibility.NONE, isGetterVisibility = Visibility.NONE)
public interface ProductCategoryMixin {

  @JsonProperty
  Category getCategory();

  @JsonIgnore
  Product getProduct();

}
