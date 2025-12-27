package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.ItemPickupPointArchive;

public interface ItemPickupPointArchiveService {

  /**
   * Add ItemPickupPoints To ItemPickupPointArchive
   *
   * @param itemPickupPointArchiveList
   * @return
   */
  List<ItemPickupPointArchive> addItemPickupPointsToItemPickupPointArchive(
      List<ItemPickupPointArchive> itemPickupPointArchiveList);
}
