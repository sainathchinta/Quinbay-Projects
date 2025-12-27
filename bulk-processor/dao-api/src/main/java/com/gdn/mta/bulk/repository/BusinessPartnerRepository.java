package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.data.domain.Page;

public interface BusinessPartnerRepository {

  /**
   * Filter business partner details by businessPartnerCode
   * @param storeId
   * @param businessPartnerCode
   * @return
   * @throws Exception
   */
  ProfileResponse filterByBusinessPartnerCodeV2(String storeId, String businessPartnerCode) throws Exception;

  /**
   * Filter business partner details by businessPartnerCode using cache
   * @param storeId
   * @param businessPartnerCode
   * @return
   * @throws Exception
   */
  ProfileResponse filterByBusinessPartnerCodeV2Cache(String storeId, String businessPartnerCode) throws Exception;

  /**
   * filter business partner pickup point
   * @param page
   * @param size
   * @param request
   * @return
   */
  Page<PickupPointResponse> filterBusinessPartnerPickupPointV2(int page, int size,
    PickupPointFilterRequest request)
    throws ApplicationException;

  /**
   * Filter business partners by a list
   *
   * @param businessPartnerFilterRequest businessPartnerFilterRequest
   * @param page                        Page
   * @param size                        Size
   * @return List of profile response
   * @throws Exception
   */
  List<ProfileResponse> filterByBusinessPartnerCodeList(
    BusinessPartnerFilterRequest businessPartnerFilterRequest, int page, int size) throws Exception;
}
