package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public interface CachedService {

  /**
   * @param requestId must not be blank
   * @param username is optional
   * @param categoryCode must not be blank
   * @return list of categories from current category to top level category
   */
  List<CategoryResponse> getParentCategoriesFromMasterData(String requestId, String username,
      String categoryCode);

  /**
   * Get Parent category for Master catlog by productCode
   * @param requestId
   * @param username
   * @param productCode
   * @return
   */
  List<CategoryResponse> getMasterParentCategoryResponseByProductCode(String requestId,
      String username, String productCode);

  /**
   * Get Parent Categories From DB and Cache
   * @param requestId
   * @param username
   * @param categoryCodes
   * @return
   */
  Map<String, List<CategoryResponse>> getParentCategoriesFromDbAndCache(String requestId, String username, Set<String> categoryCodes);
}
