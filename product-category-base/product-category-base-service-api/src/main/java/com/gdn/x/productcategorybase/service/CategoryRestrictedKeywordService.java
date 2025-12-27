package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;

public interface CategoryRestrictedKeywordService {

  /**
   * find category restricted keyword by categoryCode
   *
   * @param storeId
   * @param categoryCode
   * @return
   */
  List<CategoryRestrictedKeyword> findByStoreIdAndCategoryCode(String storeId, String categoryCode);

  /**
   * save the category restricted keyword list and evit cache
   * @param storeId
   * @param categoryCode
   * @param categoryRestrictedKeywordList
   */
  void updateCategoryRestrictedKeywordAndEvictCache(String storeId, String categoryCode,
      List<CategoryRestrictedKeyword> categoryRestrictedKeywordList);

  /**
   * find category restricted keyword page by categoryCode
   *
   * @param storeId
   * @param categoryCode
   * @return
   */
  Page<CategoryRestrictedKeyword> findByStoreIdAndCategoryCode(String storeId, String categoryCode, Pageable pageable);

  /**
   * Updating category restricted keyword mapping
   *
   * @param storeId
   * @param restrictedKeywordsMap
   * @param category
   * @param deletedRestrictedKeyword
   * @param addedRestrictedKeywordAndRequestMap
   * @throws Exception
   */
  void addAndDeleteCategoryRestrictedKeywordsMappings(String storeId, List<RestrictedKeyword> restrictedKeywordsMap,
      Category category,  List<CategoryKeywordsUpdateDTO> deletedRestrictedKeyword,
      Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap) throws Exception;

  /**
   * Updating category restricted keyword mapping async
   *
   * @param storeId
   * @param restrictedKeywordsMap
   * @param category
   * @param deletedRestrictedKeyword
   * @param addedRestrictedKeywordAndRequestMap
   * @throws Exception
   */
  void addAndDeleteCategoryRestrictedKeywordsForChildCategories(String storeId,
      List<RestrictedKeyword> restrictedKeywordsMap, Category category, List<CategoryKeywordsUpdateDTO> deletedRestrictedKeyword,
      Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap)
      throws Exception;

  /**
   * Api to get category restricted keyword by id
   *
   * @param id
   */
  CategoryRestrictedKeywordResponse getCategoryRestrictedKeywordById(String storeId, String id);

  /**
   * Find CategoryRestrictedKeywordResponse by categoryCode and keywordId in
   *
   * @param storeId
   * @param categoryCode
   * @param keywordIds
   * @return
   */
  List<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(String storeId,
      String categoryCode,List<String> keywordIds);
}
