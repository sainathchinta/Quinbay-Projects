package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.BusinessPartnerPromo;

import java.util.List;

public interface BusinessPartnerPromoService {

  /**
   * upsert business partner promo after listening to promo event
   *
   * @param storeId
   * @param promoBundlingActivated
   * @param promoBundlingType
   * @param merchantCode
   */
  void upsertBusinessPartnerPromo(String storeId, boolean promoBundlingActivated, String promoBundlingType,
      String merchantCode);

  List<BusinessPartnerPromo> findByStoreIdAndBusinessPartnerList(String storeId,
    List<String> businessPartnerCodes);

  /**
   *
   * @param businessPartnerCode
   * @return
   */
  BusinessPartnerPromo findByBusinessPartnerCode(String businessPartnerCode);
}
