package com.gdn.x.product.service.event.listener;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.merchant.voucher.streaming.model.DomainEventName;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.merchant.voucher.merchant.product.detail.event"
    + ".external.listener.enabled", havingValue = "true")
public class MerchantVoucherProductEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.MERCHANT_PRODUCT_DETAIL_EVENT_EXTERNAL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to event {} - {}", DomainEventName.MERCHANT_PRODUCT_DETAIL_EVENT_EXTERNAL,
      message);
    try {
      VoucherItemSkusEventModel voucherItemSkusEventModel =
        this.objectMapper.readValue(message, VoucherItemSkusEventModel.class);
      checkArgument(StringUtils.isNotBlank(voucherItemSkusEventModel.getStoreId()),
        "StoreId not be empty");
      checkArgument(StringUtils.isNotBlank(voucherItemSkusEventModel.getMerchantCode()),
        "Merchant Code should not be empty");
      checkArgument(StringUtils.isNotBlank(voucherItemSkusEventModel.getVoucherCode()),
        "Voucher code not be empty");
      if (Objects.isNull(voucherItemSkusEventModel.getVoucherCreatedDate())) {
        voucherItemSkusEventModel.setVoucherCreatedDate(new Date());
      }
      itemService.publishItemsByMerchantCodeToVoucher(voucherItemSkusEventModel);
    } catch (Exception e) {
      log.error("Error on MerchantVoucherProductEventListener for payload : {} , error - ", message,
        e);
    }
  }
}
