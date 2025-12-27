package com.gdn.x.productcategorybase.service.categorytree;

import java.util.List;

import com.gdn.x.productcategorybase.entity.categorytree.CategoryNode;

public interface CategoryTreeService {

  List<CategoryNode> findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(String catalogCode,
      List<String> categoryCodes, boolean active) throws Exception;

  List<CategoryNode> findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(String catalogCode,
      String parentCategoryCode, boolean active) throws Exception;

  List<CategoryNode> findByCatalogCodeAndCategoryCodesAndActive(String catalogCode, List<String> categoryCodes,
      boolean active) throws Exception;

}
