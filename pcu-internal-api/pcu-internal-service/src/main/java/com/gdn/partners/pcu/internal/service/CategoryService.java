package com.gdn.partners.pcu.internal.service;

import java.util.List;
import java.util.Map;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;

/**
 * Created by govind on 15/01/2019 AD.
 */
public interface CategoryService {

  /**
   * find internal activation interval
   * @param categoryCode
   * @return
   */
  String findInternalActivationIntervalInDaysByCategoryCode(String categoryCode);

  /**
   * To find the MAP for category and respective final category
   *
   * @return
   */
  Map<String, String> getCategoryToFinalParentMap();

  /**
   * API to get final parent category and update categoryMap afterwards
   *
   * @param categoryId
   * @return
   */
  String getFinalParentCategoryAndUpdateMap(String categoryId);

  /**
   * Fetches complete category tree with review configs
   *
   * @return
   */
  List<CategoryTreeNodeResponse> fetchCategoryTreeWithReviewConfig();

  /**
   * Bulk product suspension
   *
   * @param multipartFile
   * @param processType
   * @param requestId
   * @param storeId
   * @param username
   */
  void saveRestrictedKeywordFile(MultipartFile multipartFile, String processType, String requestId, String storeId,
      String username) throws Exception;
}
