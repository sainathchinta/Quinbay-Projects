package com.gdn.x.productcategorybase.service.brand;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthActivateEventModel;
import com.gdn.x.productcategorybase.dto.BrandAuthorisationStatusIdDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthorisationWipListResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipListRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;

import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthWipDetailResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Set;

public interface BrandAuthorisationWipService {


  /**
   * Approve Brand Authorization Wip
   * updating/creating brand auth request by seller
   *
   * @param storeId                String
   * @param brandAuthUpdateRequest BrandAuthUpdateRequest
   * @throws Exception Exception
   */
  void submitBrandAuthorisationRequest(String storeId,
    BrandAuthUpdateRequest brandAuthUpdateRequest) throws Exception;


  /**
   * Action for Brand Authorisation Wip
   *
   * @param storeId                            storeId
   * @param username                           username
   * @param brandAuthorisationWipActionRequest brandAuthorisationWipActionRequest
   * @return Pair of List<BrandAuthorisationHistory> and BrandAuthorisationStatusIdDTO
   * @throws Exception
   */
  Pair<List<BrandAuthorisationHistory>, BrandAuthorisationStatusIdDTO> brandAuthorisationWipAction(String storeId, String username,
      BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest);

  /**
   *
   * @param storeId non null storeId
   * @param status non null status
   * @param id non null id
   * @return BrandAuthWipDetailResponse
   */
  BrandAuthWipDetailResponse fetchBrandAuthWipDetails(String storeId, String status, String id);

  /**
   * Create new brand auth for a seller
   *
   * @param brandCreateAuthorisationRequest
   * @param storeId
   * @param username
   * @return
   */
  Pair<BrandAuthCreateWipResponse, BrandAuthorisationWip> brandAuthCreateWipRequest(
    BrandAuthCreateWipRequest brandCreateAuthorisationRequest, String storeId, String username)
    throws Exception;

   /**
    * To verify if the brand and seller code are eligible for brand auth
    *
    * @param storeId    String
    * @param brandCode  String
    * @param sellerCode String
    * @param edited
    * @return true if eligible for brand auth request
    */
  boolean validateBrandAuthRequest(String storeId, String brandCode, String sellerCode,
    boolean edited);

  /**
   * get brand authorisation wip list response
   *
   * @param storeId storeId
   * @param brandAuthorisationWipListRequest brandAuthorisationWipListRequest
   * @param pageable pageable
   * @return page of BrandAuthorisationWipListResponse
   */
  Page<BrandAuthorisationWipListResponse> getBrandAuthorisationWipListResponse(String storeId,
      BrandAuthorisationWipListRequest brandAuthorisationWipListRequest, Pageable pageable);

  /**
   * fetch the number of pending requests for a seller
   *
   *
   * @param storeId
   * @param sellerCode
   * @return
   */
  long fetchCountOfPendingRequestsForSeller(String storeId, String sellerCode);

  /**
   * Publish upcoming brand authorisation
   * @param storeId store id
   * @param daysThreshold Future days to check
   */
  void publishUpcomingBrandAuthorisation(String storeId, int daysThreshold);

  /**
   * Activate upcoming brand authorisation
   * @param brandAuthActivateEventModel Brand Auth Activate Event Model
   */
  List<BrandAuthorisationHistory> activateBrandAuthorisation(
    BrandAuthActivateEventModel brandAuthActivateEventModel);

  /**
   * Create brand auth wip from internal
   *
   * @param brandAuthCreateRequest
   * @param storeId
   * @param username
   * @return
   * @throws Exception
   */
  Pair<BrandAuthCreateResponse, BrandAuthorisation> createWipFromInternal(
      BrandAuthCreateRequest brandAuthCreateRequest, String storeId, String username)
      throws Exception;

  /**
   * Update the wip status as active for an update from upcoming tab with auth-start date as
   * current date
   *
   * @param brandCode
   * @param sellerCode
   */
  void updateWipEntryForActivation(String brandCode, String sellerCode);

  /**
   * Send near expiry mails and notif
   * @param storeId storeId
   */
  List<BrandAuthorisation> fetchBrandAuthorisationForNearExpiry(String storeId);
}
