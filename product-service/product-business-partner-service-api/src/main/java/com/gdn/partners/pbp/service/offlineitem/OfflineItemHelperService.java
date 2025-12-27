package com.gdn.partners.pbp.service.offlineitem;

import java.util.List;
import java.util.Map;

import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

public interface OfflineItemHelperService {

  /**
   * Validate a merchant is registered as a CNC or not
   * 
   * @param merchantCode must not be blank
   * @throws Exception if the merchant is not registered as a CNC
   */
  void validateCncMerchant(String merchantCode) throws Exception;

  List<FailedOfflineItemResponse> mapFailedOfflineItemResponses(
      List<OfflineItem> offlineItems, Map<String, OfflineItemResponseDetail> errorCodeByMerchantSkuMap,
      Map<String, ExternalPickupPointCodeResponseDetail> errorCodeByExternalPickupPointCodeMap);

  Map<String, OfflineItemResponseDetail> mapOfflineItemResponseByMerchantSku(
      List<OfflineItemResponseDetail> offlineItemResponseDetails);

  Map<String, ExternalPickupPointCodeResponseDetail> mapExternalPickupPointResponseByExternalPickupPointCode(
      List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetails);

  void upsertOfflineItemPriceAndAppendSuccessAndFailedOfflineItemFromXProduct(
      String requestId, String username, String merchantCode,
      List<ValidOfflineItem> validOfflineItems,
      List<SuccessOfflineItemResponse> successOfflineItemResponses,
      List<FailedOfflineItemResponse> failedOfflineItemResponses) throws Exception;

  /**
   * map failedOfflineItemResponses from upsertOfflineItems
   *
   * @param upsertOfflineItems
   * @param merchantCode
   * @param validPickupPointCodes
   * @return
   * @throws Exception
   */
  List<UpsertOfflineItemFailedResponse> mapFailedOfflineItemResponsesFromUpsertOfflineItems(
      List<UpsertOfflineItem> upsertOfflineItems, String merchantCode,
      List<String> validPickupPointCodes);

  List<UpsertOfflineItemPriceResponse> upsertOfflineItemAndAppendFailedUpsertOfflineItemsFromXProduct(String requestId,
      String username, String merchantCode, List<ValidOfflineItem> validOfflineItems,
      List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses) throws Exception;

  List<UpsertOfflineItem> upsertL5ItemsInXProduct(String requestId, String username, String merchantCode,
      List<UpsertOfflineItem> upsertOfflineItems, List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses)
      throws Exception;
}
