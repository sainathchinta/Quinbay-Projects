package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.ItemArchive;

public interface ItemArchiveService {

  /**
   * Add Items To ItemArchive
   *
   * @param itemArchiveList
   * @return
   */
  List<ItemArchive> addItemsToItemArchive(List<ItemArchive> itemArchiveList);
}
