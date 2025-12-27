package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import org.springframework.data.domain.Page;

import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.response.PickupPointSummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.request.PickupPointDetailWebRequest;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;

import java.util.List;

/**
 * Created by govind on 12/12/2018 AD.
 */
public interface PickupPointService {

  /**
   * mark default address for pickup point after validating product-businessPartner-request
   *
   * @param request must not null
   * @throws Exception
   */
  void validateAndSaveDefaultPickupPoint(ProductBusinessPartnerServiceRequest request)
      throws Exception;

  /**
   * mark default address for pickup point after validating product-creation-request
   * @param username
   * @param request
   * @throws Exception
   */
  void validateAndSaveDefaultPickupPoint(String username, ProductCreationRequest request)
      throws Exception;

  /**
   * mark default address for pickup point
   * @param storeId
   * @param username
   * @param businessPartnerCode
   * @param pickupPointCode
   * @throws Exception
   */
  void markDefaultAddressForBusinessPartner(String storeId, String username, String businessPartnerCode,
      String pickupPointCode) throws Exception;

  /**
   * get business partner pickup point summary
   * @param storeId
   * @param page
   * @param size
   * @param pickupPointSummaryWebRequest
   * @return
   */
  Page<PickupPointSummaryWebResponse> getPickupPointSummaryFilter(String storeId, int page, int size,
      PickupPointSummaryWebRequest pickupPointSummaryWebRequest) throws Exception;

  /**
   * Fetch pickup point details by PickupPointDetailRequest
   *
   * @param pickupPointDetailWebRequest
   * @return
   */
  List<PickupPointDetailWebResponse> fetchPickupPointDetailsByRequest(
    PickupPointDetailWebRequest pickupPointDetailWebRequest);

  /**
   * Fetch pickup point details for the user
   *
   * @param page
   * @param size
   * @param pickupPointDetailWebRequest
   * @param businessPartnerCode
   * @return
   */
  Page<PickupPointDetailWebResponse> fetchAccessiblePickupPoints(int page, int size,
      PickupPointDetailWebRequest pickupPointDetailWebRequest, String businessPartnerCode);

  /**
   * Fetch Stock And InBound Status By ProductSku And PPCode
   *
   * @param businessPartnerCode non null
   * @param storeId non null
   * @param pickupPointDetailWebRequest pickupPointDetailWebRequest
   * @param page int
   * @param size int
   * @return stock status web response
   */
  Page<PickupPointStockAndInBoundStatusWebResponse> fetchStockAndInBoundStatusByProductSkuAndPPCode(
    String businessPartnerCode, String storeId,
    PickupPointDetailWebRequest pickupPointDetailWebRequest, String productSku, int page, int size);
}
