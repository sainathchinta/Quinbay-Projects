package com.gdn.partners.pbp.service.offlineitem;

import static com.gdn.common.helper.GdnCommonHelper.getIfNotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.converter.OfflineItemConverter;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.util.OfflineItemInventoryUtil;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

@Service
public class OfflineItemHelperServiceBean implements OfflineItemHelperService {

  private static final String COMMA_DELIMITER = ",";
  private static final String WHITE_SPACE = " ";
  private static final String NOT_A_CNC_MERCHANT = "Merchant is not a cnc merchant";
  public static final String FAILED_TO_UPDATE_PRICE_ERROR = "FAILED_TO_UPDATE_PRICE_ERROR";
  private static final String PICKUP_POINT_IS_NOT_VALID = "Kode lokasi tidak sesuai";
  private static final String ITEM_SKU_IS_NOT_VALID = "Blibli SKU tidak sesuai";

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;
  
  @Autowired
  private OfflineItemConverter offlineItemConverter;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Override
  public void validateCncMerchant(String merchantCode) throws Exception {
    ProfileResponse profileResponse =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(merchantCode);
    if (!profileResponse.getCompany().isCncActivated()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, NOT_A_CNC_MERCHANT);
    }
  }

  @Override
  public List<FailedOfflineItemResponse> mapFailedOfflineItemResponses(
      List<OfflineItem> offlineItems, Map<String, OfflineItemResponseDetail> offlineItemResponseByMerchantSkuMap,
      Map<String, ExternalPickupPointCodeResponseDetail> externalPickupPointResponseByExternalPickupPointCodeMap) {

    List<FailedOfflineItemResponse> failedOfflineItemResponses = new ArrayList<>();
    for (OfflineItem offlineItem : offlineItems) {
      OfflineItemResponseDetail offlineItemResponseDetail = offlineItemResponseByMerchantSkuMap.get(
          offlineItem.getMerchantSku());
      ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail =
          externalPickupPointResponseByExternalPickupPointCodeMap.get(offlineItem.getExternalPickupPointCode());

      boolean hasErrors =
          StringUtils.isNotBlank(getIfNotNull(offlineItemResponseDetail, e -> e.getErrorCode())) ||
              StringUtils.isNotBlank(getIfNotNull(externalPickupPointCodeResponseDetail, e -> e.getErrorCode()));
      if (!hasErrors) {
        continue;
      }

      StringBuilder errorCode = new StringBuilder();
      if (StringUtils.isNotBlank(offlineItemResponseDetail.getErrorCode())) {
        errorCode.append(offlineItemResponseDetail.getErrorCode())
            .append(COMMA_DELIMITER).append(WHITE_SPACE);
      }

      if (StringUtils.isNotBlank(externalPickupPointCodeResponseDetail.getErrorCode())) {
        errorCode.append(externalPickupPointCodeResponseDetail.getErrorCode())
            .append(COMMA_DELIMITER).append(WHITE_SPACE);
      }

      failedOfflineItemResponses.add(
          offlineItemConverter.convertOfflineItemToFailedItem(
              offlineItem, StringUtils.chop(errorCode.toString().trim())));
    }
    return failedOfflineItemResponses;
  }

  @Override
  public Map<String, OfflineItemResponseDetail> mapOfflineItemResponseByMerchantSku(
      List<OfflineItemResponseDetail> offlineItemResponseDetails) {

    if (CollectionUtils.isNotEmpty(offlineItemResponseDetails)) {
      return offlineItemResponseDetails.stream()
          .collect(HashMap::new, (map, o) -> map.put(o.getMerchantSku(), o), HashMap::putAll);
    }
    return new HashMap<>();
  }

  @Override
  public Map<String, ExternalPickupPointCodeResponseDetail> mapExternalPickupPointResponseByExternalPickupPointCode(
      List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetails) {

    if (CollectionUtils.isNotEmpty(externalPickupPointCodeResponseDetails)) {
      return externalPickupPointCodeResponseDetails.stream()
          .collect(HashMap::new, (map, o) -> map.put(o.getExternalPickupPointCode(), o), HashMap::putAll);
    }
    return new HashMap<>();
  }


  @Override
  public void upsertOfflineItemPriceAndAppendSuccessAndFailedOfflineItemFromXProduct(
      String requestId, String username, String merchantCode,
      List<ValidOfflineItem> validOfflineItems,
      List<SuccessOfflineItemResponse> successOfflineItemResponses,
      List<FailedOfflineItemResponse> failedOfflineItemResponses) throws Exception {

    //update to x-product
    List<OfflineItemRequest> offlineItemRequests = offlineItemConverter
        .convertOfflineItemsToOfflineItemRequests(validOfflineItems);

    List<UpsertOfflineItemPriceResponse> itemUpdateResponses = productOutbound
          .upsertOfflineItemPrice(requestId, username, merchantCode, offlineItemRequests);

    if (CollectionUtils.isNotEmpty(itemUpdateResponses)) {
      for (UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse : itemUpdateResponses) {
        if (!upsertOfflineItemPriceResponse.isSuccess()) {
          failedOfflineItemResponses.add(offlineItemConverter
              .convertUpdateOfflineItemPriceToFailedItem(upsertOfflineItemPriceResponse,
                  new StringBuilder(FAILED_TO_UPDATE_PRICE_ERROR).append("-")
                      .append(upsertOfflineItemPriceResponse.getErrorMessage()).toString()));
        } else {
          successOfflineItemResponses.add(offlineItemConverter
              .convertUpdateOfflineItemPriceToSuccessItem(upsertOfflineItemPriceResponse));
        }
      }
    }
  }

  @Override
  public List<UpsertOfflineItemFailedResponse> mapFailedOfflineItemResponsesFromUpsertOfflineItems(
      List<UpsertOfflineItem> upsertOfflineItems, String merchantCode,
      List<String> validPickupPointCodes) {

    List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses = new ArrayList<>();
    for (UpsertOfflineItem upsertOfflineItem : upsertOfflineItems) {

      boolean hasErrors = !validPickupPointCodes.contains(upsertOfflineItem.getPickupPointCode())
          || !upsertOfflineItem.getItemSku().contains(merchantCode);

      if (!hasErrors) {
        continue;
      }

      StringBuilder errorCode = new StringBuilder();
      if (!validPickupPointCodes.contains(upsertOfflineItem.getPickupPointCode())) {
        errorCode.append(PICKUP_POINT_IS_NOT_VALID).append(COMMA_DELIMITER).append(WHITE_SPACE);
      }

      if (!upsertOfflineItem.getItemSku().contains(merchantCode)) {
        errorCode.append(ITEM_SKU_IS_NOT_VALID).append(COMMA_DELIMITER).append(WHITE_SPACE);
      }

      failedOfflineItemResponses.add(
          offlineItemConverter.convertUpsertOfflineItemToUpsertOfflineItemFailedResponse(
              upsertOfflineItem, StringUtils.chop(errorCode.toString().trim())));
    }
    return failedOfflineItemResponses;
  }

  @Override
  public List<UpsertOfflineItemPriceResponse> upsertOfflineItemAndAppendFailedUpsertOfflineItemsFromXProduct(String requestId,
      String username, String merchantCode, List<ValidOfflineItem> validOfflineItems,
      List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses) throws Exception {

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests =
        offlineItemConverter.convertValidOfflineItemsToUpsertOfflineItemRequests(validOfflineItems);

    List<UpsertOfflineItemPriceResponse> itemUpdateResponses = productOutbound
        .upsertOfflineItem(requestId, username, merchantCode, upsertOfflineItemRequests);

    failedOfflineItemResponses.addAll(itemUpdateResponses.stream()
        .filter(itemUpdateResponse -> !itemUpdateResponse.isSuccess()).map(response -> {
          validOfflineItems.removeIf(
              validOfflineItem -> validOfflineItem.getItemSku().equals(response.getItemSku()));
          return offlineItemConverter.convertToUpsertOfflineItemFailedResponse(response,
              new StringBuilder(FAILED_TO_UPDATE_PRICE_ERROR).append(Constants.DASH_DELIMITER)
                  .append(response.getErrorMessage()).toString());
        }).collect(Collectors.toList()));
    return itemUpdateResponses;
  }

  @Override
  public List<UpsertOfflineItem> upsertL5ItemsInXProduct(String requestId, String username, String merchantCode,
      List<UpsertOfflineItem> upsertOfflineItems, List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses)
      throws Exception {
    Map<String, UpsertOfflineItemRequest> upsertOfflineItemRequests = new HashMap<>();
    Map<String, UpsertOfflineItem> upsertOfflineItemMap = new HashMap<>();
    for (UpsertOfflineItem upsertOfflineItem : upsertOfflineItems) {
      String offlineItemId = new StringBuilder().append(upsertOfflineItem.getItemSku()).append(Constants.HYPHEN)
          .append(upsertOfflineItem.getPickupPointCode()).toString();
      UpsertOfflineItemRequest upsertOfflineItemRequest = new UpsertOfflineItemRequest();
      BeanUtils.copyProperties(upsertOfflineItem, upsertOfflineItemRequest);
      upsertOfflineItemRequest.setOfflineItemId(offlineItemId);
      upsertOfflineItemRequest.setListPrice(
          Optional.ofNullable(upsertOfflineItem.getListPrice()).orElse(upsertOfflineItem.getOfferPrice()));
      upsertOfflineItemRequest.setOfferPrice(upsertOfflineItem.getOfferPrice());
      upsertOfflineItemRequests.put(offlineItemId, upsertOfflineItemRequest);
      upsertOfflineItemMap.put(offlineItemId, upsertOfflineItem);
    }

    List<UpsertOfflineItemPriceResponse> itemUpdateResponses =
        productOutbound.upsertOfflineItem(requestId, username, merchantCode,
            new ArrayList<>(upsertOfflineItemRequests.values()));

    List<UpsertOfflineItem> successResponseList = new ArrayList<>();

    for (UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse : itemUpdateResponses) {
      if (!upsertOfflineItemPriceResponse.isSuccess()) {
        failedOfflineItemResponses.add(
            offlineItemConverter.convertToUpsertOfflineItemFailedResponse(upsertOfflineItemPriceResponse,
                new StringBuilder(FAILED_TO_UPDATE_PRICE_ERROR).append(Constants.DASH_DELIMITER)
                    .append(upsertOfflineItemPriceResponse.getErrorMessage()).toString()));
      } else {
        UpsertOfflineItem upsertOfflineItem =
            upsertOfflineItemMap.get(upsertOfflineItemPriceResponse.getOfflineItemId());
        upsertOfflineItem.setNew(upsertOfflineItemPriceResponse.isNew());
        upsertOfflineItem.setProductSku(upsertOfflineItemPriceResponse.getProductSku());
        upsertOfflineItem.setItemName(upsertOfflineItemPriceResponse.getItemName());
        upsertOfflineItem.setItemCode(upsertOfflineItemPriceResponse.getItemCode());
        upsertOfflineItem.setFbbActivated(upsertOfflineItemPriceResponse.isFbbActivated());
        successResponseList.add(upsertOfflineItem);
      }
    }
    return successResponseList;
  }
}
