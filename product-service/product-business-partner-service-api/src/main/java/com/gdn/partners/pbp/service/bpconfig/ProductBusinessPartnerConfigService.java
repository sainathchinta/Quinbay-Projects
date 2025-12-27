package com.gdn.partners.pbp.service.bpconfig;

import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;

/**
 * Created by Vishal on 17/05/18.
 */
public interface ProductBusinessPartnerConfigService {

  /**
   * return boolean value to show mail option notify visibility on product wip page
   *
   * @param storeId must not blank
   * @param bpCode businessPartnerCode must not blank
   * @return return boolean
   */
  boolean notifyMailVisibilityOptionForProductWip(String storeId, String bpCode);

  /**
   * save ProductBusinessPartnerConfig to database
   * @param storeId must not blank
   * @param username user name optional
   * @param request must not null
   */
  void save(String storeId, String username, ProductBusinessPartnerConfigRequest request);

}
