package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthWipDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthCreateWipResponse;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;

import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthorisationWipActionWebRequest;
import org.springframework.data.domain.Page;

public interface BrandAuthorisationWipService {

  /**
   * @param storeId non null store id
   * @param status  non null status
   * @param id      non null id
   * @return Returns BrandAuthWipDetailResponse fetched from brandCode & sellerCode
   */
  BrandAuthWipDetailResponse fetchBrandAuthWipDetails(String storeId, String status, String id,
      String sellerCode);

   /**
   *
   * @param username non null username
   * @param brandAuthorisationWipActionWebRequest non null
   */
  void brandAuthorisationWipAction(String storeId, String username,
      BrandAuthorisationWipActionWebRequest brandAuthorisationWipActionWebRequest);

  /**
   * To verify if the brand and seller code are eligible for brand auth
   *
   * @param storeId    String
   * @param brandCode  String
   * @param sellerCode String
   * @param edited     boolean
   * @return true if eligible for brand auth request
   */
  boolean validateBrandAuthRequest(String storeId, String brandCode, String sellerCode,
    boolean edited);

  /**
   *
   * @param storeId non null store id
   * @param requestId non null request id
   * @param username non null user name
   * @return Returns BrandAuthCreateWipResponse fetched from brandCode & sellerCode
   */
  BrandAuthCreateWipResponse createBrandAuthRequest(String storeId, String username, String requestId,
      BrandAuthCreateWipRequest request);

  /**
   * Update Brand Authorisation Wip
   *
   * @param storeId storeId
   * @param username username
   * @param brandAuthUpdateRequest brandAuthUpdateWipRequest
   */
  void updateBrandAuthWip(String storeId, String username,
      BrandAuthUpdateRequest brandAuthUpdateRequest);

  /**
   * Brand Authorisation Wip List
   *
   * @param page page
   * @param size size
   * @param brandAuthorisationWipListRequest brandAuthorisationWipListRequest
   * @return page of BrandAuthorisationWipListResponse
   */
  Page<BrandAuthorisationWipListResponse> getBrandAuthorisationWipList(int page, int size,
      BrandAuthorisationWipListRequest brandAuthorisationWipListRequest);

  /**
   * Check eligibility to create new brand-auth-wip request
   *
   * @param sellerCode
   * @return
   */
  boolean checkEligibility(String sellerCode);

}
