package com.gdn.partners.pbp.service.listener;

import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.OfflineInventoryStockUpdatedEvent;
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

@Slf4j
@Service
public class OfflineInventoryStockChangeSubscriberBean {

  private static final String SYSTEM = "System";
  private static final String COMMA_DELIMITER = ",";

  @Autowired
  private ProductSystemParameterService systemParameterService;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductOutbound productOutbound;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.OFFLINE_INVENTORY_STOCK_UPDATED_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    OfflineInventoryStockUpdatedEvent offlineInventoryStockUpdatedEvent =
        objectMapper.readValue(message, OfflineInventoryStockUpdatedEvent.class);
    String storeId = offlineInventoryStockUpdatedEvent.getStoreId();
    String clientId = offlineInventoryStockUpdatedEvent.getClientId();
    String username = offlineInventoryStockUpdatedEvent.getUsername();
    String merchantCode = offlineInventoryStockUpdatedEvent.getMerchantCode();
    String offlineInventoryId = offlineInventoryStockUpdatedEvent.getOfflineInventoryId();
    try {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          offlineInventoryStockUpdatedEvent.getRequestId());
      MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
      log.info("Message received, from topic {}, message :{}",
          DomainEventName.OFFLINE_INVENTORY_STOCK_UPDATED_EVENT_NAME,
          offlineInventoryStockUpdatedEvent);

      boolean prohibited = this.isMerchantProhibitedToSaveHistory(storeId, merchantCode);
      if (prohibited) {
        log.warn("Merchant {} with offlineInventoryId {} skipped due to not allowed by system",
            merchantCode, offlineInventoryId);
        return;
      }

      username = this.getCleanUsernameForXProductIntegrator(storeId, username);

      UpdatedProductHistory auditTrail = new UpdatedProductHistory();
      auditTrail.setRequestId(offlineInventoryStockUpdatedEvent.getRequestId());
      auditTrail.setAccessTime(offlineInventoryStockUpdatedEvent.getUpdatedDate());
      auditTrail.setActivity(UpdateProductActivity.STOCK_VALUE.getDesc());
      auditTrail.setBusinessPartnerCode(merchantCode);
      auditTrail.setChangedBy(username);
      auditTrail.setClientHost(clientId);
      auditTrail.setGdnSku(offlineInventoryStockUpdatedEvent.getItemSku());
      auditTrail.setNewValues(String.valueOf(offlineInventoryStockUpdatedEvent.getOriginalStock()));
      auditTrail.setOldValues(String.valueOf(offlineInventoryStockUpdatedEvent.getOldOriginalStock()));
      auditTrail.setPickupPointCode(offlineInventoryStockUpdatedEvent.getPickupPointCode());
      auditTrail.setOnlineStatus(false);
      updateItemNameAndProductSkuFromItemSku(offlineInventoryStockUpdatedEvent.getStoreId(),
          offlineInventoryStockUpdatedEvent.getItemSku(), auditTrail);

      this.updatedProductHistoryService.createAudit(Arrays.asList(auditTrail), false);
    } catch (Exception e) {
      log.error(
          "Exception caught while offline inventory stock change event , offlineInventoryId:{}",
          offlineInventoryId, e);
    }
  }

  private String getCleanUsernameForXProductIntegrator(String storeId,
      String username) {
    ProductSystemParameter systemParameter = this.systemParameterService
        .findByStoreIdAndVariable(storeId,
            SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    checkState(Objects.nonNull(systemParameter), ErrorMessages.VALUE_NOT_FOUND);
    List<String> possibleClientIds = Arrays.asList(systemParameter.getValue().split(COMMA_DELIMITER));
    if(possibleClientIds.contains(username)) {
      username = SYSTEM;
    }
    return username;
  }

  private boolean isMerchantProhibitedToSaveHistory(String storeId, String merchantCode) {
    ProductSystemParameter systemParameter = this.systemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
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
