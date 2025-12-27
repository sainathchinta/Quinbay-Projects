package com.gdn.x.productcategorybase.service.BusinessPartner;

import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;

public interface BusinessPartnerService {

  /**
   * Get BusinessPartnerProfile
   *
   * @param businessPartnerCode
   * @return
   */
  ProfileResponse getBusinessPartnerProfile(String businessPartnerCode) throws Exception;

  /**
   * Send web notification and email
   *
   * @param brandWip
   */
  void createBrandStatusChangeNotification(BrandWip brandWip) throws Exception;
}
