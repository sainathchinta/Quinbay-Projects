package com.gdn.x.product.service.api;

import com.gdn.x.product.domain.event.model.BackFillFbbFlagRequest;
public interface BackFillFbbFlagService {

  /**
   * @param backFillFbbFlagRequest Request with item sku and pp code
   */
  void backFillFbbFlag(BackFillFbbFlagRequest backFillFbbFlagRequest);
}
