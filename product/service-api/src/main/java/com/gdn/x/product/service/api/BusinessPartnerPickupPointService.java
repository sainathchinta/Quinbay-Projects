package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;

import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;

public interface BusinessPartnerPickupPointService {

  /**
   * Save BusinessPartnerPickupPoint
   *
   * @param BusinessPartnerPickupPoint
   * @return
   */
  BusinessPartnerPickupPoint saveBusinessPartnerPickupPoint(BusinessPartnerPickupPoint BusinessPartnerPickupPoint);

  /**
   * Get BusinessPartnerPickupPoint by storeId and businessPartnerCode
   *
   * @param storeId
   * @param businessPartnerCode
   * @param pickupPointCode
   * @return
   */
  BusinessPartnerPickupPoint getBusinessPartnerPickupPoint(String storeId, String businessPartnerCode,
      String pickupPointCode);

  /**
   * Get BusinessPartnerPickupPoint by storeId and businessPartnerCodes
   *
   * @param storeId
   * @param pickupPointCodes
   * @return
   */
  List<BusinessPartnerPickupPoint> getBusinessPartnerPickupPointByPickupPointCodes(String storeId,
      List<String> pickupPointCodes);

  /**
   * Get BusinessPartnerPickupPoint By BusinessPartnerCode
   *
   * @param businessPartnerCode
   * @return
   */
  List<BusinessPartnerPickupPoint> getBusinessPartnerPickupPointByBusinessPartnerCode(String businessPartnerCode);

  /**
   * Get business partner pickup point summary
   * @param storeId
   * @param page
   * @param size
   * @param pickupPointSummaryRequest
   * @return
   */
  Page<BusinessPartnerPickupPointResponse> getBusinessPartnerPickupPointSummary(String storeId, int page,
      int size, PickupPointSummaryRequest pickupPointSummaryRequest);

  /**
   * Get CNC_FLAG_UPDATE activated pickup point codes
   *
   * @param pickupPointCodes
   * @return
   */
  List<String> getCncActivatedPickupPointCodes(Set<String> pickupPointCodes);

  /**
   * Get FBB activated pickup point codes
   *
   * @param pickupPointCodes
   * @return
   */
  List<String> getFbbActivatedPickupPointCodes(Set<String> pickupPointCodes);
}