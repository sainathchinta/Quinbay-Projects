package com.gdn.mta.product.service;

import org.springframework.stereotype.Service;

import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.x.businesspartner.dto.ProfileResponse;

@Service
public class ProductInventoryServiceBean implements ProductInventoryService {
  
  @Override
  public boolean isSyncedToLevel1Inventory(ProfileResponse businessPartner) throws Exception {
    return GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI.equals(
        businessPartner.getCompany().getInventoryFulfillment());
  }
}
