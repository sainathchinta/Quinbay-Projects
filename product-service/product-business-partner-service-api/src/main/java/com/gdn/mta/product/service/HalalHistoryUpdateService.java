package com.gdn.mta.product.service;


import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;

public interface HalalHistoryUpdateService {

  /**
   * Fetching Halal Product History by ProductSku
   *
   * @param productSku
   * @param pageable
   * @return
   */
  Page<HalalProductHistoryResponse> getHalalProductHistory(String storeId, String productSku, Pageable pageable)
      throws Exception;

  /**
   * Saving Halal Update History
   *
   * @param historyEventModel
   * @throws Exception
   */
  void saveHalalHistoryUpdate(HalalHistoryUpdateEventModel historyEventModel) throws Exception;
}
