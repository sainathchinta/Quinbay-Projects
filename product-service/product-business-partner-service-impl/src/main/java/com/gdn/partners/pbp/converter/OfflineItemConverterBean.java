package com.gdn.partners.pbp.converter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.WeakHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Request;
import com.gdn.x.product.rest.web.model.dto.OfflineItemPriceDTO;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.partners.pbp.util.OfflineItemInventoryUtil;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

@Component
public class OfflineItemConverterBean implements OfflineItemConverter {

  private static final String INVENTORY_UNIQUE_ID_SEPARATOR = "_";
  private static final String DASH_SEPARATOR = "-";
  private static final String FAILED_TO_UPDATE_STOCK_ERROR = "FAILED_TO_UPDATE_STOCK_ERROR";

  @Override
  public List<ValidOfflineItem> constructValidItems(
      List<OfflineItem> offlineItems, Map<String, OfflineItemResponseDetail> offlineItemResponseByMerchantSkuMap,
      Map<String, ExternalPickupPointCodeResponseDetail> externalPickupPointResponseByExternalPickupPointCodeMap) {

    List<ValidOfflineItem> validOfflineItems = new ArrayList<>();
    if (CollectionUtils.isEmpty(offlineItems)) {
      return validOfflineItems;
    }

    for (OfflineItem offlineItem : offlineItems) {
      OfflineItemResponseDetail offlineItemResponseDetail = offlineItemResponseByMerchantSkuMap.get(
          offlineItem.getMerchantSku());
      ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail =
          externalPickupPointResponseByExternalPickupPointCodeMap.get(offlineItem.getExternalPickupPointCode());

      String itemSku = offlineItem.getItemSku();
      if (StringUtils.isBlank(itemSku) && Objects.nonNull(offlineItemResponseDetail) &&
          StringUtils.isBlank(offlineItemResponseDetail.getErrorCode())) {
        itemSku = offlineItemResponseDetail.getItemSku();
      }

      String pickupPointCode = offlineItem.getPickupPointCode();
      if (StringUtils.isBlank(pickupPointCode) && Objects.nonNull(externalPickupPointCodeResponseDetail) &&
          StringUtils.isBlank(externalPickupPointCodeResponseDetail.getErrorCode())) {
        pickupPointCode = externalPickupPointCodeResponseDetail.getCode();
      }

      if (Objects.isNull(itemSku) || Objects.isNull(pickupPointCode)) {
        continue;
      }

      ValidOfflineItem validOfflineItem = new ValidOfflineItem();
      validOfflineItem.setMerchantSku(offlineItem.getMerchantSku());
      validOfflineItem.setExternalPickupPointCode(offlineItem.getExternalPickupPointCode());
      validOfflineItem.setStock(offlineItem.getStock());
      validOfflineItem.setListPrice(
          Optional.ofNullable(offlineItem.getListPrice()).orElse(offlineItem.getPrice()));
      validOfflineItem.setPrice(offlineItem.getPrice());
      validOfflineItem.setItemSku(itemSku);
      validOfflineItem.setPickupPointCode(pickupPointCode);
      validOfflineItem.setFileName(offlineItem.getFileName());
      validOfflineItems.add(validOfflineItem);
    }

    return validOfflineItems;
  }

  @Override
  public List<OfflineItemRequest> convertOfflineItemsToOfflineItemRequests(
      List<ValidOfflineItem> validOfflineItems) {

    List<OfflineItemRequest> offlineItemRequests = new ArrayList<>();

    if (CollectionUtils.isEmpty(validOfflineItems)) {
      return offlineItemRequests;
    }

    for (ValidOfflineItem validOfflineItem : validOfflineItems) {
      String offlineItemId = OfflineItemInventoryUtil.generateL5OfflineItemId(
          validOfflineItem.getItemSku(), validOfflineItem.getPickupPointCode());
      OfflineItemRequest offlineItemRequest = new OfflineItemRequest();
      offlineItemRequest
          .setExternalPickupPointCode(validOfflineItem.getExternalPickupPointCode());
      offlineItemRequest.setOfflineItemId(offlineItemId);
      offlineItemRequest.setItemSku(validOfflineItem.getItemSku());
      offlineItemRequest.setMerchantSku(validOfflineItem.getMerchantSku());
      offlineItemRequest.setPickupPointCode(validOfflineItem.getPickupPointCode());
      offlineItemRequest.setListPrice(
          Optional.ofNullable(validOfflineItem.getListPrice()).orElse(validOfflineItem.getPrice()));
      offlineItemRequest.setOfferPrice(validOfflineItem.getPrice());

      offlineItemRequests.add(offlineItemRequest);
    }

    return offlineItemRequests;
  }

  private String generateL5OfflineItem(ValidOfflineItem validOfflineItem){
    return new StringBuilder()
        .append(validOfflineItem.getItemSku())
        .append(DASH_SEPARATOR)
        .append(validOfflineItem.getPickupPointCode())
        .toString();
  }

  @Override
  public FailedOfflineItemResponse convertOfflineItemToFailedItem(
      OfflineItem offlineItem, String errorCode) {
    FailedOfflineItemResponse offlineItemResponse = new FailedOfflineItemResponse();
    offlineItemResponse.setErrorCode(errorCode);
    offlineItemResponse.setPickupPointCode(offlineItem.getPickupPointCode());
    offlineItemResponse.setExternalPickupPointCode(offlineItem.getExternalPickupPointCode());
    offlineItemResponse.setItemSku(offlineItem.getItemSku());
    offlineItemResponse.setMerchantSku(offlineItem.getMerchantSku());

    return offlineItemResponse;
  }

  @Override
  public FailedOfflineItemResponse convertUpdateOfflineItemPriceToFailedItem(
      UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse, String errorCode) {
    FailedOfflineItemResponse offlineItemResponse = new FailedOfflineItemResponse();
    offlineItemResponse.setItemSku(upsertOfflineItemPriceResponse.getItemSku());
    offlineItemResponse.setMerchantSku(upsertOfflineItemPriceResponse.getMerchantSku());
    offlineItemResponse.setPickupPointCode(upsertOfflineItemPriceResponse.getPickupPointCode());
    offlineItemResponse.setExternalPickupPointCode(upsertOfflineItemPriceResponse.getExternalPickupPointCode());
    offlineItemResponse.setErrorCode(errorCode);

    return offlineItemResponse;
  }

  @Override
  public SuccessOfflineItemResponse convertUpdateOfflineItemPriceToSuccessItem(
      UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse) {
    return SuccessOfflineItemResponse.builder()
        .itemSku(upsertOfflineItemPriceResponse.getItemSku())
        .merchantSku(upsertOfflineItemPriceResponse.getMerchantSku())
        .pickupPointCode(upsertOfflineItemPriceResponse.getPickupPointCode())
        .externalPickupPointCode(upsertOfflineItemPriceResponse.getExternalPickupPointCode())
        .listPrice(upsertOfflineItemPriceResponse.getListPrice())
        .price(upsertOfflineItemPriceResponse.getOfferPrice())
        .build();
  }

  @Override
  public FailedOfflineItemResponse convertOfflineItemToFailedOfflineItemResponse(
      ValidOfflineItem validOfflineItem, String errorMessage){

    FailedOfflineItemResponse failedOfflineItemResponse = new FailedOfflineItemResponse();
    failedOfflineItemResponse.setExternalPickupPointCode(validOfflineItem.getExternalPickupPointCode());
    failedOfflineItemResponse.setMerchantSku(validOfflineItem.getMerchantSku());
    failedOfflineItemResponse.setErrorCode(errorMessage);

    return failedOfflineItemResponse;
  }

  @Override
  public SuccessOfflineItemResponse convertOfflineItemToSuccessOfflineItemResponse(
      ValidOfflineItem validOfflineItem) {
    return SuccessOfflineItemResponse.builder()
        .itemSku(validOfflineItem.getItemSku())
        .merchantSku(validOfflineItem.getMerchantSku())
        .pickupPointCode(validOfflineItem.getPickupPointCode())
        .externalPickupPointCode(validOfflineItem.getExternalPickupPointCode())
        .listPrice(validOfflineItem.getListPrice())
        .price(validOfflineItem.getPrice())
        .build();
  }

  @Override
  public ItemSummaryRequest convertOfflineItemInstantPickupRequestToItemSummaryRequest(
      OfflineItemInstantPickupRequest request) {
    ItemSummaryRequest result = new ItemSummaryRequest();
    result.setMerchantCode(request.getMerchantCode());
    result.setMerchantSku(request.getMerchantSku());
    result.setMasterCategoryCode(request.getCategoryCode());
    result.setCncActivated(true);
    result.setPickupPointCode(request.getPickupPointCode());
    result.setProductItemName(request.getItemName());
    result.setArchived(false);
    return result;
  }

  @Override
  public ItemSummaryRequest convertOfflineItemInstantPickupV2RequestToItemSummaryRequest(
      OfflineItemInstantPickupV2Request request) {
    ItemSummaryRequest result = new ItemSummaryRequest();
    result.setMerchantCode(request.getMerchantCode());
    result.setCncActivated(Boolean.TRUE);
    result.setItemSkus(request.getGdnSkus());
    result.setMerchantSkus(request.getMerchantSkus());
    result.setPickupPointCodes(request.getPickupPointCodes());
    result.setProductItemName(request.getItemName());
    result.setMasterCategoryCode(request.getCategoryCode());
    result.setArchived(Boolean.FALSE);
    return result;
  }

  @Override
  public List<String> convertItemSummaryResponseToListOfGdnSku(List<ItemSummaryResponse> source) {
    if (source == null) {
      return new ArrayList<>();
    }
    return source.stream().map(ItemSummaryResponse::getItemSku).collect(Collectors.toList());
  }

  @Override
  public Map<String, List<String>> convertItemSummaryResponseToMapOfPickupPointCodeAndListOfGdnSku(
      List<ItemSummaryResponse> itemSummaryResponses) {
    Map<String, List<String>> results = new HashMap<>();
    for (ItemSummaryResponse item : itemSummaryResponses) {
      String gdnSku = item.getItemSku();
      for (OfflineItemPriceDTO offlinePrice : item.getOfflinePrices()) {
        List<String> gdnSkus = results.get(offlinePrice.getPickupPointCode());
        if (Objects.isNull(gdnSkus)) {
          gdnSkus = new ArrayList<>();
          gdnSkus.add(gdnSku);
          results.put(offlinePrice.getPickupPointCode(), gdnSkus);
        } else {
          gdnSkus.add(gdnSku);
        }
      }
    }
    return results;
  }

  @Override
  public List<DeleteOfflineItemRequest> convertToDeleteOfflineItemRequest(
      List<DeleteOfflineItem> requests) {
    List<DeleteOfflineItemRequest> deleteOfflineItemRequests = new ArrayList<>();
    for (DeleteOfflineItem deleteOfflineItem : requests) {
      DeleteOfflineItemRequest deleteOfflineItemRequest = new DeleteOfflineItemRequest();
      BeanUtils.copyProperties(deleteOfflineItem, deleteOfflineItemRequest);
      deleteOfflineItemRequests.add(deleteOfflineItemRequest);
    }
    return deleteOfflineItemRequests;
  }

  private String generateOfflineItemKey(String webItemSku, String merchantCode, String pickupPointCode) {
    return new StringBuilder(webItemSku).append("_").append(merchantCode).append("_").append(pickupPointCode).toString();
  }

  @Override
  public List<UpsertOfflineItemRequest> convertValidOfflineItemsToUpsertOfflineItemRequests(
      List<ValidOfflineItem> validOfflineItems) {

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();

    if (CollectionUtils.isEmpty(validOfflineItems)) {
      return upsertOfflineItemRequests;
    }

    for (ValidOfflineItem validOfflineItem : validOfflineItems) {
      String offlineItemId = generateL5OfflineItem(validOfflineItem);
      UpsertOfflineItemRequest upsertOfflineItemRequest = new UpsertOfflineItemRequest();
      BeanUtils.copyProperties(validOfflineItem, upsertOfflineItemRequest);
      upsertOfflineItemRequest.setOfflineItemId(offlineItemId);
      upsertOfflineItemRequest.setListPrice(
          Optional.ofNullable(validOfflineItem.getListPrice()).orElse(validOfflineItem.getPrice()));
      upsertOfflineItemRequest.setOfferPrice(validOfflineItem.getPrice());
      upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    }

    return upsertOfflineItemRequests;
  }

  @Override
  public List<ValidOfflineItem> constructValidOfflineItems(
      List<UpsertOfflineItem> upsertOfflineItems, String merchantCode,
      List<String> validPickupPointCodes) {

    List<UpsertOfflineItem> validUpsertOfflineItems = upsertOfflineItems
        .stream()
        .filter(upsertOfflineItem ->
            validPickupPointCodes.contains(upsertOfflineItem.getPickupPointCode())
                && upsertOfflineItem.getItemSku().contains(merchantCode))
        .collect(Collectors.toList());

    List<ValidOfflineItem> validOfflineItems = new ArrayList<>();
    if (CollectionUtils.isEmpty(validUpsertOfflineItems)) {
      return validOfflineItems;
    }

    for (UpsertOfflineItem validUpsertOfflineItem : validUpsertOfflineItems) {
      ValidOfflineItem validOfflineItem = new ValidOfflineItem();
      BeanUtils.copyProperties(validUpsertOfflineItem, validOfflineItem);
      validOfflineItem.setListPrice(Optional.ofNullable(validUpsertOfflineItem.getListPrice())
          .orElse(validUpsertOfflineItem.getOfferPrice()));
      validOfflineItem.setPrice(validUpsertOfflineItem.getOfferPrice());
      validOfflineItems.add(validOfflineItem);
    }

    return validOfflineItems;
  }

  @Override
  public UpsertOfflineItemFailedResponse convertToUpsertOfflineItemFailedResponse(
      UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse, String errorCode) {
    UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse =
        new UpsertOfflineItemFailedResponse();
    upsertOfflineItemFailedResponse.setItemSku(upsertOfflineItemPriceResponse.getItemSku());
    upsertOfflineItemFailedResponse
        .setPickupPointCode(upsertOfflineItemPriceResponse.getPickupPointCode());
    upsertOfflineItemFailedResponse.setErrorCode(errorCode);

    return upsertOfflineItemFailedResponse;
  }

  @Override
  public UpsertOfflineItemFailedResponse convertUpsertOfflineItemToUpsertOfflineItemFailedResponse(
      UpsertOfflineItem upsertOfflineItem, String errorCode) {
    UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse =
        new UpsertOfflineItemFailedResponse();
    upsertOfflineItemFailedResponse.setErrorCode(errorCode);
    upsertOfflineItemFailedResponse.setPickupPointCode(upsertOfflineItem.getPickupPointCode());
    upsertOfflineItemFailedResponse.setItemSku(upsertOfflineItem.getItemSku());

    return upsertOfflineItemFailedResponse;
  }

  @Override
  public UpsertOfflineItemFailedResponse convertOfflineItemToUpsertOfflineItemDetailResponse(
      ValidOfflineItem validOfflineItem, String errorMessage) {
    UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse =
        new UpsertOfflineItemFailedResponse();
    upsertOfflineItemFailedResponse.setPickupPointCode(validOfflineItem.getPickupPointCode());
    upsertOfflineItemFailedResponse.setItemSku(validOfflineItem.getItemSku());
    upsertOfflineItemFailedResponse.setErrorCode(errorMessage);

    return upsertOfflineItemFailedResponse;
  }
}
