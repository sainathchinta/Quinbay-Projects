package com.gdn.mta.product.service;

import com.gdn.x.businesspartner.dto.ProfileResponse;

public interface ProductInventoryService {
  /**
   * Is synced to level 1
   *
   * @param businessPartner
   * @return
   * @throws Exception
   */
  boolean isSyncedToLevel1Inventory(ProfileResponse businessPartner) throws Exception;
}
