package com.gdn.mta.product.repository;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.Category;

public interface CategoryRepository {
  
  GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategoryCached(String requestId, 
      String username, String categoryCode) throws Exception;
  
  Category findOne(String id) throws Exception;
  
  List<CategoryResponse> findHierarchyByCategoryCode(String categoryCode) throws Exception;

  List<CategoryHierarchyResponse> findHierarchyByCategoryCodes(CategoryCodeRequest request)
      throws Exception;

  /**
   * Get all child categories by c1 category code
   *
   * @param requestId
   * @param username
   * @param categoryCode
   * @return
   * @throws Exception
   */
  List<String> getAllChildCategoriesByC1CategoryCode(String requestId,
      String username, String categoryCode) throws Exception;

}
