package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.mta.product.entity.ProductBusinessPartnerConfig;

/**
 * Created by Vishal on 17/05/18.
 */

public interface ProductBusinessPartnerConfigRepository extends
    JpaRepository<ProductBusinessPartnerConfig, String> {

  /**
   * find to top row by passing businessPartnerCode and store Id
   *
   * @param storeId must not blank
   * @param bpCode  businessPartnerCode must not blank
   * @return return ProductBusinessPartnerConfig object
   */
  ProductBusinessPartnerConfig findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(
      String storeId, String bpCode);

}
