package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.Collections;
import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductDeleteEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
public class ProductDeleteEventListener {

  private final ObjectMapper objectMapper;
  private final ProductService productService;
  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Value("${product.delete.item.fetch.size}")
  private int productDeleteItemFetchSize;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getDeleteProductEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      ProductDeleteEventModel productDeleteEventModel = objectMapper.readValue(message, ProductDeleteEventModel.class);
      setMandatoryParameters(productDeleteEventModel.getStoreId());
      log.info("Consume product delete event : {} message : {}, uuid : {}",
          kafkaTopicPropertiesConsumer.getDeleteProductEvent(), productDeleteEventModel,
          productDeleteEventModel.getIdentifier());
      for (String productCode : productDeleteEventModel.getProductCodeList()) {
        Product product = productService.findProductByProductCode(productCode);
        if (!product.isPickedForDeletion()) {
          log.info("Skipping product deletion for productCode : {} , uuid : {} ", product,
              productDeleteEventModel.getIdentifier());
          continue;
        }
        log.info("Deleting product : {} uuid : {} ", productCode, productDeleteEventModel.getIdentifier());
        productService.deleteProducts(productDeleteEventModel.getStoreId(), Collections.singletonList(product.getId()),
            Collections.singletonList(productCode), productDeleteItemFetchSize);
        log.info("Deleting product success : {} uuid : {} ", productCode, productDeleteEventModel.getIdentifier());
      }
    } catch (Exception e) {
      log.error("Error while consuming delete event : {} message : {} ",
          kafkaTopicPropertiesConsumer.getDeleteProductEvent(), message, e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }

}
