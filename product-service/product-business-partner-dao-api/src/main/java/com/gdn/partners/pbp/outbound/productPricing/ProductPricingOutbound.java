package com.gdn.partners.pbp.outbound.productPricing;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.pbp.dto.promo.request.PromoBundlingPricingSummaryRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSummaryResponse;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.BulkActivateDeactivateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.partners.pbp.model.SortOrder;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;

/**
 * @author nitinmathew - created on 06/02/2020
 **/
public interface ProductPricingOutbound {

  /**
   * Add/update wholesale price
   *
   * @param wholesalePriceRequest
   * @return
   */
  WholesalePriceBulkUpdateResponse upsertWholesalePrice(WholesalePriceRequest wholesalePriceRequest) throws Exception;

  /**
   * Get wholesale price detail
   *
   * @param itemSku
   * @param pickupPointCode
   * @return
   * @throws Exception
   */
  WholesalePriceSkuResponse getWholesalePrice(String itemSku, String pickupPointCode) throws Exception;

  /**
   * Update Activated/deactivated status for wholesale
   *
   * @param itemSku
   * @param updateSkuStatus
   * @param pickupPointCode
   * @return
   * @throws Exception
   */
  boolean setWholesaleActivated(String itemSku, String updateSkuStatus, String pickupPointCode) throws Exception;

  /**
   * Fetch wholesale details for item skus
   *
   * @param itemSku
   * @param itemSkuAndPickupPointCodeMap
   * @return
   * @throws Exception
   */
  List<WholesalePriceSkuResponse> getWholesalePriceList(Set<String> itemSku,
      Map<String, String> itemSkuAndPickupPointCodeMap) throws Exception;

  /**
   * @param bulkActivateDeactivateRequestList
   * @return
   * @throws Exception
   */
  BulkActivateDeactivateResponse bulkActivateOrDeactivateSku(
      List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequestList) throws Exception;

  /**
   * @param wholesalePriceBulkUpdateRequestList
   * @param merchantCode
   * @return
   * @throws Exception
   */
  WholesalePriceBulkUpdateResponse bulkUpdateWholesalePrice(List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList,
      String merchantCode) throws Exception;

  /**
   * @param wholesalePriceBulkUpdateRequestList
   * @param merchantCode
   * @param username
   * @return
   * @throws Exception
   */
  WholesalePriceBulkUpdateResponse bulkUpdateWholesalePriceV2(List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList,
      String merchantCode, String username) throws Exception;

  /**
   * Get Wholesale price details by itemSku and pickupPointCode
   *
   * @param itemInfoDtoList
   * @return
   */
  List<WholesalePriceSkuResponse> getWholesalePriceByItemSkuAndPickupPointCode(
    List<ItemInfoDto> itemInfoDtoList) throws Exception;

  /**
   * @param promoBundlingPricingSummaryRequest
   * @return
   * @throws Exception
   */
  List<PromoBundlingSummaryResponse> getPromoBundlingSummaryResponse (PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest)
    throws Exception;


  /**
   * Get WS price details
   * @param wholesalePriceSkuDetailListRequest
   * @return
   */
  List<WholesalePriceSkuResponse> getWholesalePriceListV2(WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest) throws Exception;

  /**
   * Setting wholesale active flag
   * @param itemSku
   * @param pickupPointCode
   * @param updateSkuStatus
   * @return
   * @throws Exception
   */
  boolean setWholesaleActivatedFlag(String itemSku, String pickupPointCode, String updateSkuStatus) throws Exception;
}
