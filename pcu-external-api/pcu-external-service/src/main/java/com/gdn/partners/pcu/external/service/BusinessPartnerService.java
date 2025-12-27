package com.gdn.partners.pcu.external.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import org.springframework.data.domain.Page;

import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;

/**
 * Created by govind on 08/01/2019 AD.
 */
public interface BusinessPartnerService {

  /**
   * Get BusinessPartner's ProfileResponse by profileId
   *
   * @param profileId
   * @return
   */
  ProfileResponse getProfileDetailById(String profileId);

  /**
   * filter business partner pickup point
   * @param page
   * @param size
   * @param request
   * @return
   */
  Page<PickupPointOutboundResponse> filterBusinessPartner(int page, int size, PickupPointFilterRequest request);

  /**
   * update default pickup points
   * @param request
   */
  void updateDefaultPickupPointCode(MarkPickupPointAsDefaultRequest request);

  /**
   * update default configurations
   * @param profileRequest
   */
  void updateDefaultConfiguration(ProfileRequest profileRequest);

  /**
   * filter business partner by businessPartnerCode
   * @param businessPartnerCode
   * @return
   */
  ProfileResponse filterByBusinessPartnerCode(String businessPartnerCode);

  /**
   * get all pickup points for a business partner
   * @param businessPartnerCode
   * @return
   */
  List<PickupPointOutboundResponse> getAllPickupPointsForBusinessPartner(String businessPartnerCode);

  /**
   * get requested pickup points for a business partner.
   * If pickupPointCodes is null / empty, then all pickup points are fetched
   *
   * @param businessPartnerCode
   * @param pickupPointCodes
   * @return
   */
  List<PickupPointOutboundResponse> getPickupPointsForBusinessPartner(String businessPartnerCode,
    Set<String> pickupPointCodes);

  /**
   *
   * @param code
   * @return
   */
  PickupPointOutboundResponse getPickupPointByCode(String code);

  SimpleMapStringResponse getBusinessPartnerDetailsByList(int page, int size,
      BusinessPartnerFilterRequest request);
}
