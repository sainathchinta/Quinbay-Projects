package com.gdn.x.product.dao.api;

import java.util.List;

import com.gdn.x.product.model.entity.PristineDataItemUpdate;

public interface PristineItemUpdateRepositoryCustom {

  /**
   * Update isUpdated flag to true
   *
   * @param pristineDataItemUpdate
   */
  void updateIsUpdatedFlag(List<PristineDataItemUpdate> pristineDataItemUpdate);
}
