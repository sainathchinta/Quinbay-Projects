package com.gdn.x.product.service.api;

import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;

public interface ArchiveAndDeleteRejectedMerchantProductDataService {

  /**
   * Archive, Delete And EvictCache For Rejected Merchant ProductData
   *
   * @param productCodeDomainEventModel
   * @throws Exception
   */
  void archiveAndDeleteRejectedMerchantProductData(ProductCodeDomainEventModel productCodeDomainEventModel)
      throws Exception;

  /**
   * Archive, Delete And EvictCache For Rejected Merchant ProductData For Retry
   *
   * @param productCodeDomainEventModel
   * @throws Exception
   */
  void archiveAndDeleteRejectedMerchantProductDataRetry(ProductCodeDomainEventModel productCodeDomainEventModel)
      throws Exception;
}
