package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;

public interface RestrictedKeywordServiceWrapper {

  /**
   * Update the restricted keyword mapping
   * @param restrictedKeywordsUpdateDTO
   * @param storeId
   * @throws Exception
   */
  void updateRestrictedKeyword(RestrictedKeywordsUpdateDTO restrictedKeywordsUpdateDTO, String storeId) throws Exception;
}
