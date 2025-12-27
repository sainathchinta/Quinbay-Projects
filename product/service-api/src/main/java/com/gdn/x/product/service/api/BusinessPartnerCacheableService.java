package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.BusinessPartner;

public interface BusinessPartnerCacheableService {

  /**
   * Find business partner by storeId and businessPartnerCode
   *
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  BusinessPartner findByStoreIdAndBusinessPartnerCode(String storeId, String businessPartnerCode);

}
