package com.gdn.partners.pcu.master.service;

import java.util.List;

import com.gdn.partners.pcu.master.model.request.GetSubCategoriesServiceRequest;
import com.gdn.partners.pcu.master.web.model.response.CatalogDetailResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySearchWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.master.model.request.CategoryHierarchyServiceRequest;

public interface CatalogService {

  /**
   * Get list of category responses based on catalog Id and parent Id
   *
   * @param getSubCategoriesServiceRequest
   * @return
   */
  List<CategoryWebResponse> getSubCategoriesByCatalogIdAndParentCategoryId(
      GetSubCategoriesServiceRequest getSubCategoriesServiceRequest);

  /**
   * Get catalog summary by catalog Type
   *
   * @param catalogType
   * @return
   */
  List<CatalogDetailResponse> getCatalogSummaryByCatalogType(String catalogType);

  /**
   * Get list of category ids (List<List<String>>) searched by category name
   *
   * @param categoryHierarchyServiceRequest
   * @return List of list of categoryIds
   */
  List<List<CategorySearchWebResponse>> getListOfCategoryHierarchyByCategoryName(
      CategoryHierarchyServiceRequest categoryHierarchyServiceRequest);
}
