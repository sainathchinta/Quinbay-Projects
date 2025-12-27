package com.gdn.x.product.service.api;

import java.util.List;

import org.apache.commons.lang3.tuple.Pair;

import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

public interface BusinessPartnerService {

  /**
   * upsert business partner after listening to businessPartner update event
   *
   * @param businessPartnerChange
   */
  void upsertBusinessPartner(BusinessPartnerChange businessPartnerChange);

  /**
   * Get business partner details by business partner code
   *
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  BusinessPartner getBusinessPartnerByBusinessPartnerCode(String storeId, String businessPartnerCode);

  /**
   * Returns true if buisnessPartner is a umkm merchant
   *
   *
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  boolean isBusinessPartnerUmkmMerchant(String storeId, String businessPartnerCode);

  /**
   * Find business partner deatil by storeId and businessPartnerCodes
   *
   * @param storeId
   * @param businessPartnerCodes
   * @return
   */
  List<BusinessPartner> findByStoreIdAndBusinessPartnerCodes(String storeId, SimpleListStringRequest businessPartnerCodes);

  /**
   * Returns true if businessPartner is a B2B merchant
   *
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  boolean isBusinessPartnerB2bMerchant(String storeId, String merchantCode);

  /**
   * Returns pair of umkm flag and b2b flag for a merchant
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  Pair<Boolean, Boolean> isBusinessPartnerUmkmAndB2b(String storeId, String merchantCode);
}
