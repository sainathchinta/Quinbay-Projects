package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;

public interface BrandAuthorisationWipServiceWrapper {

  /**
   * Approve Brand Authorization Wip
   *
   * @param storeId                            storeId
   * @param username                           username
   * @param brandAuthorisationWipActionRequest brandAuthorizationWipApproveRequest
   */
  void brandAuthorisationWipAction(String storeId, String username,
      BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest);

  /**
   * check for the eligibility of the seller to create brand-auth based on the number of requests
   * already present for the seller
   *
   * @param storeId
   * @param sellerCode
   * @return
   */
  boolean checkEligibility(String storeId, String sellerCode);

  /**
   *
   * @param storeId Store id
   * @param username User name
   * @param brandAuthUpdateRequest Brand Auth Update Request
   * @throws Exception
   */
  void submitBrandAuthorisationRequest(String storeId,String username,
    BrandAuthUpdateRequest brandAuthUpdateRequest) throws Exception;

  /**
   *
   * @param storeId storeId
   */
  void sendNearExpiryMailNotification(String storeId);
}
