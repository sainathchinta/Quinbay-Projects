package com.gdn.partners.pbp.converter;

import java.util.List;
import java.util.Map;

import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Request;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

public interface OfflineItemConverter {

  List<ValidOfflineItem> constructValidItems(
      List<OfflineItem> offlineItems, Map<String, OfflineItemResponseDetail> offlineItemResponseByMerchantSkuMap,
      Map<String, ExternalPickupPointCodeResponseDetail> externalPickupPointResponseByExternalPickupPointCodeMap);


  List<OfflineItemRequest> convertOfflineItemsToOfflineItemRequests(
      List<ValidOfflineItem> validOfflineItems);


  FailedOfflineItemResponse convertOfflineItemToFailedItem(OfflineItem offlineItem, String errorCode);

  FailedOfflineItemResponse convertUpdateOfflineItemPriceToFailedItem(
      UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse, String errorCode);

  SuccessOfflineItemResponse convertUpdateOfflineItemPriceToSuccessItem(
      UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse);

  FailedOfflineItemResponse convertOfflineItemToFailedOfflineItemResponse(
      ValidOfflineItem validOfflineItem, String errorMessage);

  SuccessOfflineItemResponse convertOfflineItemToSuccessOfflineItemResponse(
      ValidOfflineItem validOfflineItem);

  ItemSummaryRequest convertOfflineItemInstantPickupRequestToItemSummaryRequest(
      OfflineItemInstantPickupRequest request);

  ItemSummaryRequest convertOfflineItemInstantPickupV2RequestToItemSummaryRequest(
      OfflineItemInstantPickupV2Request request);

  List<String> convertItemSummaryResponseToListOfGdnSku(List<ItemSummaryResponse> source);

  Map<String, List<String>> convertItemSummaryResponseToMapOfPickupPointCodeAndListOfGdnSku(
      List<ItemSummaryResponse> source);

  List<DeleteOfflineItemRequest> convertToDeleteOfflineItemRequest(
      List<DeleteOfflineItem> requests);


  /**
   * convert validOfflineItems To upsertOfflineItemRequests
   *
   * @param validOfflineItems
   * @return
   */
  List<UpsertOfflineItemRequest> convertValidOfflineItemsToUpsertOfflineItemRequests(
      List<ValidOfflineItem> validOfflineItems);

  /**
   * construct validOfflineItems
   *
   * @param upsertOfflineItems
   * @param merchantCode
   * @param validPickupPointCodes
   * @return
   */
  List<ValidOfflineItem> constructValidOfflineItems(List<UpsertOfflineItem> upsertOfflineItems,
      String merchantCode, List<String> validPickupPointCodes);

  /**
   * convert to UpsertOfflineItemFailedResponse
   *
   * @param upsertOfflineItemPriceResponse
   * @param errorCode
   * @return
   */
  UpsertOfflineItemFailedResponse convertToUpsertOfflineItemFailedResponse(
      UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse, String errorCode);

  /**
   * convert upsertOfflineItem to UpsertOfflineItemFailedResponse
   *
   * @param upsertOfflineItem
   * @param errorCode
   * @return
   */
  UpsertOfflineItemFailedResponse convertUpsertOfflineItemToUpsertOfflineItemFailedResponse(
      UpsertOfflineItem upsertOfflineItem, String errorCode);

  /**
   * convert offlineItem to upsertOfflineItemDetailResponse
   *
   * @param validOfflineItem
   * @param errorMessage
   * @return
   */
  UpsertOfflineItemFailedResponse convertOfflineItemToUpsertOfflineItemDetailResponse(
      ValidOfflineItem validOfflineItem, String errorMessage);

}
