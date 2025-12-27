package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndNameDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.entity.ProductCategory;

public interface CategoryService {

  /**
   * adjust category while update
   *
   * @param savedCategory
   * @param category
   * @param parentCategoryId
   * @param savedInternalActivationInterval
   * @throws Exception
   */
  void adjustCategory(Category savedCategory, Category category, String parentCategoryId,
      Integer savedInternalActivationInterval) throws Exception;

  /**
   * Save edited category
   * @param category
   * @param previousActivationInterval
   * @param parentCategoryIds
   * @param eventTypes
   * @throws Exception
   */
  void saveUpdatedCategory(Category category, Integer previousActivationInterval, List<String> parentCategoryIds,
      List<CategoryChangeEventType> eventTypes) throws Exception;

  /**
   * Save edited category
   * @param category
   * @param previousActivationInterval
   * @param parentCategoryIds
   * @param eventTypes
   * @param categoryChangeEventTypesV2
   * @param masterCategoryIds
   * @param salesCategoryIds
   * @throws Exception
   */
  void saveUpdatedCategoryInfo(Category category, Integer previousActivationInterval, List<String> parentCategoryIds,
      List<CategoryChangeEventType> eventTypes, Set<String> categoryChangeEventTypesV2, List<String> masterCategoryIds, List<String> salesCategoryIds) throws Exception;

  /**
   *  get categories based on name and state
   *
   * @param storeId
   * @param name
   * @param pageable
   * @param state
   * @param documentFilterType
   * @return
   */
  Page<Category> findByName(String storeId, String name, Pageable pageable, String state, String documentFilterType);

  Category findByStoreIdAndCategoryCode(String storeId, String categoryCode);

  /**
   * to get category code
   *
   * @param storeId
   * @param categoryCode
   * @return
   * @throws Exception
   */
  Category findBasicInfoByStoreIdAndCategoryCode(String storeId, String categoryCode) throws Exception;

  List<CategoryShipping> findShippingInfoByStoreIdCategoryCode(String storeId, String categoryCode);

  Page<Category> findByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes,
      Boolean activated,
      Pageable pageable);

  List<Category> findByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes);

  Category findByStoreIdAndId(String storeId, String id);

  /**
   * to fetch count of Halal categories having parent category
   *
   * @param storeId
   * @param parentCategory
   * @param halalCategory
   * @return
   */
  long findCountByParentCategoryAndHalalCategory(String storeId, Category parentCategory, boolean halalCategory);

  Category findByStoreIdAndIdInitCategoryAttribute(String storeId, String id);

  /**
   * Get category Attributes by Store Id and category Codes
   * @param storeId
   * @param categoryCode
   * @return
   */
  List<Category> getCategoryAttributesByStoreIdAndCategoryCodes(String storeId, List<String> categoryCode);

  /**
   *
   * @param storeId
   * @param id
   * @return
   */
  Category findByStoreIdAndIdInitAllCategoryAttribute(String storeId, String id);

  List<Category> findCategoryHierarchyByCategoryCode(String storeId, String categoryCode) throws
      Exception;

  /**
   * get category hierarchy based on category code from caffeine cache.
   * @param storeId
   * @param categoryCode
   * @return
   * @throws Exception
   */
  List<Category> findCategoryHierarchyByCategoryCodeFromCaffeine(String storeId, String categoryCode)
      throws Exception;

  /**
   * get category hierarchy based on category code from redis cache.
   * @param storeId
   * @param categoryCode
   * @return
   * @throws Exception
   */
  List<Category> findCategoryHierarchyByCategoryCodeFromRedis(String storeId, String categoryCode)
      throws Exception;

  /**
   * get category hierarchy based on category ID.
   *
   * @param storeId
   * @param categoryId
   * @return
   */
  List<Category> findCategoryHierarchyByCategoryId(String storeId, String categoryId) throws Exception;

  List<Category> findChildForParent(String storeId, Category parentCategory);

  Page<Category> findChildForParent(String storeId, Category parentCategory, Pageable pageable);

  /**
   * Get active child count by parent category id
   *
   * @param storeId
   * @param parentCategory
   * @return
   */
  long findActiveChildCountForParent(String storeId, Category parentCategory);

  /**
   * Get overall child count by parent category id
   *
   * @param storeId
   * @param parentCategory
   * @return
   */
  long findOverAllChildCountForParent(String storeId, Category parentCategory);

  /**
   * Get inActive child count by parent category
   *
   * @param storeId
   * @param parentCategory
   * @return
   */
  long findInActiveChildCountForParent(String storeId, Category parentCategory);

  Page<Category> findChildForParentByCatalogId(String storeId, String parentCategoryId, String catalogId,
      String documentFilterType, boolean ignoreB2bExclusive, boolean filterHalalCategory, Pageable pageable);

  List<Category> findChildForParentWithCatalogType(String storeId, Category parentCategory,
      CatalogType catalogType,
      Boolean activated);

  Page<Category> findChildForParentWithCatalogType(String storeId, Category parentCategory,
      CatalogType catalogType,
      Boolean activated, Pageable pageable);

  String getSequence(String categoryCode);

  void markForDeleteCategory(String storeId, String categoryId) throws Exception;

  void markForDeleteCategoryAttribute(List<CategoryAttribute> categoryAttributes) throws Exception;

  void markForDeleteCategoryAttribute(String categoryAttributeId) throws Exception;

  void markForDeleteCategoryReference(String storeId, List<CategoryReference> categoryReferences)
      throws Exception;

  void markForDeleteCategoryReference(String storeId, String id) throws Exception;

  void markForDeleteProductCategory(String storeId, List<ProductCategory> productCategories)
      throws Exception;

  void markForDeleteProductCategory(String storeId, String productCategoryId) throws Exception;

  /**
   * save and update product category
   * @param storeId
   * @param category
   * @return
   * @throws Exception
   */
  Category saveAndUpdateProductCategory(String storeId, Category category) throws Exception;

  /**
   * Used only for update displayable in category
   *
   * @param categoryId
   * @param isAble
   */
  void setCategoryDisplayable(String categoryId, boolean isAble) throws Exception;

  Page<Category> findByCategoryNameAndCatalogType(String storeId, String categoryName, CatalogType catalogType,
      Pageable pageable) throws Exception;

  Map<String, String> getCategoryToFinalParent();

  /**
   * API to get final parent category from given categoryID
   *
   * @param categoryId
   * @return
   */
  String getFinalParentCategory(String categoryId) throws Exception;

  /**
   * API to get parent category hierarchy for given categoryID
   *
   * @param categoryId
   * @return
   */
  List<String> getParentCategoryHierarchyByCategoryId(String categoryId) throws Exception;

  /**
   * API to get all categories for particular store
   *
   * @param storeId
   * @return
   */
  List<Category> findByStoreId(String storeId);

  /**
   * Retrieve all categories associated with given storeId and catalogType
   *
   * @param storeId
   * @param catalogType
   * @return
   */
  List<CustomCategoryDto> getCategoriesFromCatalogType(String storeId, CatalogType catalogType)
      throws Exception;

  
  /**
   * Get all active parent categories
   * @return
   */
  List<String> getParentCategories();

  /**
   * GET all category tree
   * @param catalogName
   * @param storeId
   * @param ignoreB2bExclusive
   * @return
   * @throws Exception
   */
  List<CategoryTreeDTO> getAllCategoryTree(String catalogName, String storeId, boolean ignoreB2bExclusive) throws Exception;

  /**
   * get active category tree
   * @param catalogName calalog name default master
   * @param storeId storeId
   * @return
   */
  List<CategoryTreeDTO> getActiveCategoryTree(String catalogName, String storeId);

  /**
   * publish all categories from parents to children sequentially
   * @param catalogName
   * @param storeId
   * @return
   * @throws Exception
   */
  Boolean publishAllCategories(String catalogName, String storeId) throws Exception;

  /**
   *
   * @param source
   * @param parentCategoryId
   * @return
   */
  List<CategoryTreeDTO> buildCategoryTree(List<CategoryTreeDTO> source, String parentCategoryId);

  List<CategoryTreeDTO> getCategoryTree(List<String> categoryCodes, String catalogName, String storeId) throws Exception;
  
  Page<Category> findCategorySummaryByStoreIdAndCatalogIdAndDisplay(String storeId, String catalogId, Boolean display, Pageable pageable);

  /**
   * API to fetch category names for a list of category codes
   * @param storeId
   * @param categoryCodeList list of categoryCodes
   * @param pageable
   * @return
   * @throws Exception
   */

  Map<String, String> findCategoryNamesByCategoryCodes(String storeId,
      List<String> categoryCodeList, Pageable pageable) throws Exception;

  /**
   * Get all child categoryCodes from C1 categoryCode
   * @param storeId
   * @param categoryCodes - C1 category codes
   * @param filterOutInactiveCn - Filter Only Active Categories
   * @return
   */
  List<String> findAllChildForC1CategoryCodes(String storeId, List<String> categoryCodes, boolean filterOutInactiveCn)
      throws Exception;
  
  boolean validateIsCategoryCn(String storeId, String parentCategoryCode) throws Exception;

  /**
   * Find Categories by ids and storeId and markForDelete false
   * @param storeId
   * @param categoryIds
   * @return
   */
  List<Category> findByCategoryIds(String storeId, List<String> categoryIds);

  /**
   * Find all C1 level categories
   * @param storeId
   * @return
   */
  List<Category> findAllParentCategories(String storeId);

  /**
   * Return complete category tree with its review config
   *
   * @return
   * @param storeId
   */
  List<CategoryTreeNodeResponse> getCategoryTree(String storeId);

  /**
   *
   * @param storeId
   * @param categoryId
   * @return
   */
  Category getCategoryByStoreIdAndIdCached(String storeId, String categoryId);

  /**
   *
   * @param storeId
   * @param categoryCode
   * @return
   */
  Category getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(String storeId, String categoryCode);

  /**
   *
   * @param storeId
   * @param categoryCodes
   * @return
   * @throws Exception
   */
  List<Category> findAllChildForC1CategoryCodesTree(String storeId, String categoryCodes) throws Exception;


  /**
   * list of categories in the tree with leaves categories
   * @param storeId
   * @param categoryCodes
   * @return
   */
  List<String> findIdsByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes);

  /**
   * Return list of category which are eligible for generic template
   *
   * @param storeId
   * @param genericTemplateEligible
   * @param ignoreB2bExclusive
   * @return
   */
  List<CategoryTreeResponse> getAllCategoryTreeforGenericTemplate(String storeId,
      boolean genericTemplateEligible, boolean ignoreB2bExclusive) throws Exception;

  /**
   * Evict generic category tree cache
   *
   * @param storeId
   * @param genericTemplateEligible
   * @param ignoreB2bExclusive
   */
  void evictGenericCategoryTreeCache(String storeId, boolean genericTemplateEligible,
    boolean ignoreB2bExclusive);

  /**
   * Evict the child category cache for given parent category Ids.
   *
   * @param parentCategoryIds
   */
  void evictChildCategoryCache(List<String> parentCategoryIds);

  /**
   * Api to save original sales category
   *
   * @param originalSalesCategoryRequest
   */
  String saveOSC(OriginalSalesCategoryRequest originalSalesCategoryRequest) throws Exception;

  /**
   * Evict the child category cache for Active Categories by parent category Ids.
   *
   * @param parentCategoryIds list of parentCategoryId
   */
  void evictActiveChildCategoryCache(List<String> parentCategoryIds);

  /**
   * Find category name and english name by category codes
   *  @param storeId
   * @param categoryCodes
   */
  List<CategoryCodeAndNameDTO> findNameByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes);

  /**
   * get catgeory attributes by category id
   * @param storeId
   * @param categoryId
   * @return
   */
  List<CategoryAttribute> getCategoryAttributes(String storeId, String categoryId);


  /**
   * get catgeory attributes by category id and mark for delete false
   * @param storeId non null
   * @param categoryId non null
   * @return list of categoryAttributes
   */

  List<CategoryAttribute> getCategoryAttributesMarkForDeleteFalse(String storeId,
    String categoryId);

  /**
   * get category by categoryCodes
   * @param storeId
   * @param categoryCodes
   * @return
   */
  List<Category> findCategoriesByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes);

  /**
   * update b2b exclusive and halal category flag for all the child categories of a category
   *
   * @param storeId
   * @param category
   * @param b2bExclusive
   * @param halalCategory
   * @throws Exception
   */
  void updateB2bExclusiveOrHalalCategoryFlagForChildCategories(String storeId, Category category, boolean b2bExclusive,
      boolean halalCategory) throws Exception;

  /**
   * Save category details
   *
   * @param category
   * @return
   * @throws Exception
   */
  Category save(Category category) throws Exception;

  /**
   * Update category details
   *
   * @param category
   * @throws Exception
   */
  void update(Category category) throws Exception;

  /**
   * Fetch category details by id
   * @param id
   * @return
   */
  Category findById(String id) throws Exception;

  /**
   * Fetch paginated category details by store id
   *
   * @param storeId
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<Category> findByStoreId(String storeId, Pageable pageable) throws Exception;

  /**
   * GET categoryCodes mapped to attribute
   * @param storeId
   * @param attributeCode
   * @return
   */
  List<String> findCategoryCodesByAttributeCode(String storeId, String attributeCode);

}
