package com.gdn.mta.product.service;

import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;

public interface BackFillFbbFlagService {

  /**
   * @param eventModel
   */
  void backFillFbbFlag(ProductFbbMigrationEventModel eventModel);

  /**
   *
   * @param productFbbMigration ProductFbbMigration
   */
  void updateFbbPickupPoint(ProductFbbMigrationEventModel productFbbMigration) throws Exception;
}
