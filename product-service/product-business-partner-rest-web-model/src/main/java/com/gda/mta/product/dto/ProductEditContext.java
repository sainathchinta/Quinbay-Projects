package com.gda.mta.product.dto;

import java.util.List;

public interface ProductEditContext {
  String getOldCategoryName();

  void setOldCategoryName(String oldCategoryName);

  boolean isAutoCategoryChange();

  void setAutoCategoryChange(boolean autoCategoryChange);

  RestrictedKeywordsByFieldAndActionType getRestrictedKeywordsByFieldAndActionType();

  void setRestrictedKeywordsByFieldAndActionType(
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType);

  List<RestrictedKeywordsByField> getRestrictedKeywordsByFieldList();

  void setRestrictedKeywordsByFieldList(
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList);

  boolean isBrandChanged();

}
