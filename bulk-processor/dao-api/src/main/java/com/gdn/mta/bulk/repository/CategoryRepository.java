package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public interface CategoryRepository {

  CategoryDetailResponse findByStoreIdAndCategoryCodeAndMarkDeleteFalse(String storeId, String categoryCode)
      throws Exception;
  
  boolean validateIsCnCategory(String storeId, String categoryCode) throws Exception;

  /**
   * get category Hierarchy response by category Code
   *
   * @param storeId must not blank
   * @param categoryCode must not blank
   * @return list of categoryResponse
   * @throws Exception
   */
  List<CategoryResponse> filterCategoryHierarchyByCategoryCode(String storeId, String categoryCode)
      throws Exception;
  
  /**
   * get allowed attribute values for predefined and defining attribute
   * 
   * @param username
   * @param requestId
   * @param storeId
   * @param request
   * @return
   * @throws Exception
   */
  List<AllowedAttributeValueDtoResponse> getPredefinedAndDefiningAllowedAttributeValue(
      String username, String requestId, String storeId, List<AllowedAttributeValueDtoRequest> request) throws Exception;
  
  /**
   * get brand detail
   * 
   * @param storeId
   * @param requestId
   * @param brandName
   * @return
   * @throws Exception
   */
  BrandResponse getBrandDetail(String storeId, String requestId, String brandName)
      throws Exception;
}
