package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.PriceHistory;

public interface PriceHistoryService {
  /**
   * @param priceHistory must not be null
   * @return saved price history
   */
  PriceHistory savePriceHistory(PriceHistory priceHistory);
}
