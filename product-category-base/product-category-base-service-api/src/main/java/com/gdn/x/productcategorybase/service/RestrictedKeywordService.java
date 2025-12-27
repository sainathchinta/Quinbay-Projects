package com.gdn.x.productcategorybase.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;

public interface RestrictedKeywordService {

  /**
   * Saving the restricted keywords
   * @param keywords
   * @return
   */
  List<RestrictedKeyword> saveRestrictedKeywords(List<RestrictedKeyword> keywords);

  /**
   * fetch the restricted keywords
   * @param storeId
   * @param keywords
   * @return
   */
  List<RestrictedKeyword> findByKeywordId(String storeId, List<String> keywords);

  /**
   * Fetching the restricted keyword with ignored case
   * @param storeId
   * @param keyword
   * @return
   */
  RestrictedKeyword findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String keyword);

  /**
   * fetch and set keywordId for a keyword
   *
   * @param categoryKeywordsUpdateListDTO
   */
  void getDeletedRestrictedKeywordId(CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO);

  /**
   * fetch the restricted keywords by keywords
   * @param storeId
   * @param keywords

   */
  Page<RestrictedKeyword> getRestrictedKeywordSuggestions(String storeId, String keywords, Pageable pageable);

  /**
   * Fetching The Restricted Keyword For Listing
   *
   * @param storeId
   * @param keyword
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<RestrictedKeywordsListingResponse> getRestrictedKeywordForListing(String storeId, String keyword,
      Pageable pageable) throws Exception;

  /**
   * Fetching the list of restricted keywords for ui-validation
   *
   * @param storeId
   * @return
   */
  List<UiValidationRestrictedKeywordsResponse> getListOfRestrictedKeywordsForUiValidation(String storeId);
}
