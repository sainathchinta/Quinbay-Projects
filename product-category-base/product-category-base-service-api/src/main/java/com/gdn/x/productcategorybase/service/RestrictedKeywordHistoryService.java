package com.gdn.x.productcategorybase.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;

public interface RestrictedKeywordHistoryService {

  /**
   * Fetching Restricted Keyword History by StoreId and KeywordId
   *
   * @param storeId
   * @param keywordId
   * @param pageable
   * @return
   */
  Page<RestrictedKeywordHistoryResponse> getRestrictedKeywordHistory(String storeId, String keywordId,
      Pageable pageable) throws Exception;

  /**
   * Saving Restricted Keyword History
   *
   * @param historyEventModel
   * @throws Exception
   */
  void saveRestrictedKeywordHistory(RestrictedKeywordHistoryEventModel historyEventModel) throws Exception;
}
