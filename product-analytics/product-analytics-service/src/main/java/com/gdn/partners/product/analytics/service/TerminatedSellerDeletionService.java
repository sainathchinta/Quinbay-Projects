package com.gdn.partners.product.analytics.service;

public interface TerminatedSellerDeletionService {

  /**
   * Publish events to permanently delete products of terminated Sellers
   */
  void publishEventsForProductDeletion(String storeId, boolean publishForAGPDeletion);

  void updateStatusForParticularService(String productCode, String sellerCode, String service,
      String result);
}
