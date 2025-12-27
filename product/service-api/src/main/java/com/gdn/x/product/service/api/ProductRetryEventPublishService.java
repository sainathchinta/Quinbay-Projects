package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.ProductRetryEventPublish;

public interface ProductRetryEventPublishService {

  /**
   * Add product sku to ProductL3SolrReindexStatus
   *
   * @param productRetryEventPublish
   */
  ProductRetryEventPublish insertToRetryPublish(ProductRetryEventPublish productRetryEventPublish);

  /**
   * Scheduler to retry publsih
   *
   * @param storeId
   */
  void schedulerRetryPublish(String storeId);

}
