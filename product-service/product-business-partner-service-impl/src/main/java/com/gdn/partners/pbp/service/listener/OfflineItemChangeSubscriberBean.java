package com.gdn.partners.pbp.service.listener;

import static com.gdn.common.base.GdnPreconditions.checkState;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.OfflineItemChangeEvent;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductItemBusinessPartnerService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class OfflineItemChangeSubscriberBean {

  private static final String SYSTEM = "System";
  private static final String DASH = "-";
  private static final String FORMAT = "################";
  private static final NumberFormat NUMBER_FORMAT = new DecimalFormat(FORMAT);
  private static final String COMMA_DELIMITER = ",";

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ProductSystemParameterService systemParameterService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductOutbound productOutbound;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_OFFLINE_ITEM_CHANGE, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    OfflineItemChangeEvent event = objectMapper.readValue(message, OfflineItemChangeEvent.class);
    log.info("Processing event from {} : {}", DomainEventName.PRODUCT_OFFLINE_ITEM_CHANGE, event);
    String storeId = event.getStoreId();
    String clientId = event.getClientId();
    String username = event.getUsername();
    String requestId = event.getRequestId();
    String merchantCode = event.getMerchantCode();
    String offlineItemId = event.getUniqueId();
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);

    try {
      if (isMerchantExcludedForOfflineProductHistory(storeId, merchantCode)) {
        log.warn("Merchant {} with offlineItemId {} skipped due to not allowed by system",
            merchantCode, offlineItemId);
        return;
      }

      username = getCleanUsernameForXProductIntegrator(storeId, username);
      List<UpdatedProductHistory> updatedProductHistories = new ArrayList<>();
      if (event.getNewData()) {
        updatedProductHistories.addAll(Arrays.asList(
            constructLogAudit(event, UpdateProductActivity.INITIAL_NORMAL_PRICE.getDesc(), username),
            constructLogAudit(event, UpdateProductActivity.INITIAL_SELLING_PRICE.getDesc(), username)));
      } else {
        if (Objects.nonNull(event.getOldListPrice()) && Objects.nonNull(event.getOldOfferPrice())) {
          boolean isListPriceUpdated = Double.compare(event.getListPrice(), event.getOldListPrice()) != 0;
          boolean isOfferPriceUpdated = Double.compare(event.getOfferPrice(), event.getOldOfferPrice()) != 0;
          if (isListPriceUpdated) {
            String normalPriceActivity = event.getSyncPriceAction() ?
                UpdateProductActivity.SYNC_NORMAL_PRICE.getDesc() :
                UpdateProductActivity.NORMAL_PRICE.getDesc();
            updatedProductHistories.add(constructLogAudit(event, normalPriceActivity, username));
          }
          if (isOfferPriceUpdated) {
            String sellingPriceActivity = event.getSyncPriceAction() ?
                UpdateProductActivity.SYNC_SELLING_PRICE.getDesc() :
                UpdateProductActivity.SELLING_PRICE.getDesc();
            updatedProductHistories.add(constructLogAudit(event, sellingPriceActivity, username));
          }
        }
      }
      if (CollectionUtils.isNotEmpty(updatedProductHistories)) {
        updatedProductHistoryService.createAudit(updatedProductHistories, false);
      }
    } catch (Exception e) {
      log.error("Exception caught while processing event from {} : {}",
          DomainEventName.PRODUCT_OFFLINE_ITEM_CHANGE, event, e);
    }
  }

  private UpdatedProductHistory constructLogAudit(OfflineItemChangeEvent event,
      String activity, String username) {
    UpdatedProductHistory logAudit = new UpdatedProductHistory();
    logAudit.setAccessTime(event.getUpdatedDate());
    logAudit.setActivity(activity);
    logAudit.setBusinessPartnerCode(event.getMerchantCode());
    logAudit.setChangedBy(username);
    logAudit.setClientHost(event.getClientId());
    logAudit.setGdnSku(event.getItemSku());
    logAudit.setPickupPointCode(event.getPickupPointCode());
    logAudit.setOnlineStatus(false);
    logAudit.setRequestId(event.getRequestId());
    if (UpdateProductActivity.NORMAL_PRICE.getDesc().equalsIgnoreCase(activity) ||
        UpdateProductActivity.SYNC_NORMAL_PRICE.getDesc().equalsIgnoreCase(activity)) {
      logAudit.setOldValues(NUMBER_FORMAT.format(event.getOldListPrice()));
      logAudit.setNewValues(NUMBER_FORMAT.format(event.getListPrice()));
    }
    if (UpdateProductActivity.SELLING_PRICE.getDesc().equalsIgnoreCase(activity) ||
        UpdateProductActivity.SYNC_SELLING_PRICE.getDesc().equalsIgnoreCase(activity)) {
      logAudit.setOldValues(NUMBER_FORMAT.format(event.getOldOfferPrice()));
      logAudit.setNewValues(NUMBER_FORMAT.format(event.getOfferPrice()));
    }
    if (UpdateProductActivity.INITIAL_NORMAL_PRICE.getDesc().equalsIgnoreCase(activity)) {
      logAudit.setOldValues(DASH);
      logAudit.setNewValues(NUMBER_FORMAT.format(event.getListPrice()));
    }
    if (UpdateProductActivity.INITIAL_SELLING_PRICE.getDesc().equalsIgnoreCase(activity)) {
      logAudit.setOldValues(DASH);
      logAudit.setNewValues(NUMBER_FORMAT.format(event.getOfferPrice()));
    }
    updateItemNameAndProductSkuFromItemSku(event.getStoreId(), event.getItemSku(), logAudit);
    return logAudit;
  }

  private String getCleanUsernameForXProductIntegrator(String storeId, String username) {
    ProductSystemParameter systemParameter = systemParameterService.findByStoreIdAndVariable(
        storeId, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    checkState(Objects.nonNull(systemParameter), ErrorMessages.VALUE_NOT_FOUND);
    List<String> possibleClientIds = Arrays.asList(systemParameter.getValue().split(COMMA_DELIMITER));
    if(possibleClientIds.contains(username)) {
      username = SYSTEM;
    }
    return username;
  }

  private boolean isMerchantExcludedForOfflineProductHistory(String storeId, String merchantCode) {
    ProductSystemParameter systemParameter = systemParameterService.findByStoreIdAndVariable(
        storeId, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    checkState(Objects.nonNull(systemParameter), ErrorMessages.VALUE_NOT_FOUND);
    Set<String> merchantCodes = StringUtils.commaDelimitedListToSet(systemParameter.getValue());

    return merchantCodes.contains(merchantCode);
  }

  private void updateItemNameAndProductSkuFromItemSku(String storeId,
      String itemSku,
      UpdatedProductHistory auditTrail) {
    ProductItemBusinessPartner productItemBusinessPartner = productItemBusinessPartnerService.findProductItemByItemSku(storeId, itemSku);
    String productSku = productItemBusinessPartner.getProductBusinessPartner().getGdnProductSku();
    String itemName = productOutbound.findProductNameByProductItemId(productItemBusinessPartner.getProductItemId());
    auditTrail.setProductSku(productSku);
    auditTrail.setGdnName(itemName);
  }

}
