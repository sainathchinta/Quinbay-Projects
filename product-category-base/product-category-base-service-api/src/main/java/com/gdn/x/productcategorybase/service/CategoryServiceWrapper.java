package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.dto.CategoryAndHierarchyDto;
import com.gdn.x.productcategorybase.dto.CategoryDetailDTO;
import com.gdn.x.productcategorybase.dto.CategoryErrorDto;
import com.gdn.x.productcategorybase.dto.CategoryInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryServiceDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.Category;

public interface CategoryServiceWrapper {

  /**
   * Update category info
   *
   * @param storeId
   * @param categoryInfoUpdateDTO
   * @param statusChangeFlag
   * @return CategoryHistoryEventModel list
   * @throws Exception
   */
  List<CategoryHistoryEventModel> updateCategoryInfo(String storeId,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO, boolean statusChangeFlag) throws Exception;

  /**
   *
   * @param storeId
   * @param categoryMappingsUpdateDTO
   */
  CategoryUpdateHistoryDTO updateCategoryMappings(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO) throws Exception;

  /**
   *
   * @param storeId
   * @param categoryMappingsUpdateDTO
   */
  void updateCategoryMappingsAndPublishHistory(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO) throws Exception;

  /**
   *
   * @param storeId
   * @param categoryDetailDTO
   * @return Pair of Category DTO & History List
   * @throws Exception
   */
  Pair<CategoryAndHierarchyDto, List<RestrictedKeywordHistoryEventModel>> createCategoryWithoutEventPublish(
    String storeId, CategoryDetailDTO categoryDetailDTO) throws Exception;

  /**
   * set child count by filter type
   *
   * @param storeId
   * @param categoryPage
   * @param filterType
   * @return
   * @throws Exception
   */
  List<CategoryServiceDTO> setChildCountByFilterType(String storeId, Page<Category> categoryPage, String filterType)
      throws Exception;

  /**
   * publish all categories of a catalog asynchronously
   *
   * @param catalogName
   * @param storeId
   * @return
   */
  void publishAllCategories(String catalogName, String storeId);

  /**
   * To add and delete the restricted keywords in category till Cn level
   * @param categoryCode
   * @param categoryKeywordsUpdateListDTO
   * @return List<RestrictedKeywordHistoryEventModel>
   * @throws Exception
   */
  List<RestrictedKeywordHistoryEventModel> updateCategoriesWithRestrictedKeywords(String categoryCode,
      CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO) throws Exception;

  /**
   * To get restricted keywords for category code or keyword or both
   *  @param storeId
   *  @param categoryRestrictedKeywordsRequest
   *  @return
   */
  Page<RestrictedKeywordsResponse> findCategoryRestrictedKeywords(String storeId,
      CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest, Pageable pageable);

  /**
   * To update
   *  @param storeId
   *  @param categoryId
   *  @param wholesaleMappingDTO
   *  @return
   */
  CategoryUpdateHistoryDTO updateCategoriesWithWholesaleConfig(String storeId, String categoryId,
      WholesaleMappingDTO wholesaleMappingDTO) throws Exception;

  /**
   * publish history for wholesale config update
   *
   * @param storeId
   * @param CategoryUpdateHistoryDTO
   */
  void publishHistoryEventForWholesaleConfigUpdate(String storeId,
      CategoryUpdateHistoryDTO CategoryUpdateHistoryDTO);

  /**
   * @param storeId
   * @param categoryId
   * @return
   * @throws Exception
   */
  String getFinalParentCategoryCached(String storeId, String categoryId) throws Exception;

  List<CategoryTreeDTO> getActiveCategoryTree(String catalogName, String storeId, String nonInventoryCode);

  /**
   * get category detail by id and error code/error message
   * @param storeId
   * @param categoryId
   * @return
   */
  CategoryErrorDto getCategoryDetailByCategoryId(String storeId, String categoryId);

  /**
   * Get category Attributes by Store Id and category Codes
   * @param storeId
   * @param categoryCodes
   * @return
   */
  List<Category> getCategoryAndDefiningAttributes(String storeId, List<String> categoryCodes);

  /**
   *
   * @param storeId
   * @param categoryId
   * @return
   */
  CategoryErrorDto validateCategory(String storeId, String categoryId);

  /**
   * find category restricted keyword list by categoryCode
   * @param storeId
   * @param categoryCode
   * @return
   */
  List<RestrictedKeywordsMappedToCategoryResponse> getRestrictedKeywordMappedToCategory(String storeId, String categoryCode);

  /**
   * validate destination category for restricted keyword category change
   * @param storeId
   * @param categoryCodes
   * @return
   */
  List<CategoryErrorResponse> validateCategoryForRestrictedKeywordCategoryChange(String storeId,
      List<String> categoryCodes);

  /**
   * publish category change and clear cache
   * @param category
   * @throws Exception
   */
  void publishCategoryChangeAndClearCache(CategoryAndHierarchyDto category) throws Exception;
}
