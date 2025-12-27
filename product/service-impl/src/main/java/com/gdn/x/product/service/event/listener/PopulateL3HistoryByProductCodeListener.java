package com.gdn.x.product.service.event.listener;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.AuditTrailDtoEventModel;
import com.gdn.x.product.domain.event.model.AuditTrailListByProductCodeEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.config.KafkaPublisher;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.pbp.product.distribution.history.listener.enabled", havingValue = "true")
public class PopulateL3HistoryByProductCodeListener {

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @KafkaListener(topics = ProductDomainEventName.SEND_EVENT_FOR_DISTRIBUTION_HISTORY, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("ProductSkuToSalesCatalogMappingRequestListener consume event with message : {}", message);
    try {
      AuditTrailListByProductCodeEventModel auditTrailListByProductCodeEventModel =
          this.objectMapper.readValue(message, AuditTrailListByProductCodeEventModel.class);
      if (CollectionUtils.isNotEmpty(auditTrailListByProductCodeEventModel.getAuditTrailResponseList())) {
        List<Product> productCodeList = productService.findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
            auditTrailListByProductCodeEventModel.getProductCode());
        Product defaultProduct = productCodeList.get(0);
        List<Item> basicItemDetailsByItemCodes = getItems(auditTrailListByProductCodeEventModel, defaultProduct);
        Map<String, Item> itemMap =
            basicItemDetailsByItemCodes.stream().collect(Collectors.toMap(Item::getItemCode, Function.identity()));
        List<AuditTrailDto> auditTrailResponseList = new ArrayList<>();
        getAuditTrailListByProductCodeEventModel(auditTrailListByProductCodeEventModel, itemMap, auditTrailResponseList, defaultProduct);
        publishHistoryEvent(auditTrailResponseList, auditTrailListByProductCodeEventModel);
      }
    } catch (Exception ex) {
      log.error("Error while Event listening {} for payload : {}, error is : ",
          ProductDomainEventName.PRODUCT_SKU_TO_SALES_CATALOG_MAPPING_EVENT, message, ex);
    }
  }

  private static void getAuditTrailListByProductCodeEventModel(AuditTrailListByProductCodeEventModel auditTrailListByProductCodeEventModel,
      Map<String, Item> itemMap, List<AuditTrailDto> auditTrailResponseList, Product defaultProduct) {
    for (AuditTrailDtoEventModel auditTrailDtoEventModel : auditTrailListByProductCodeEventModel.getAuditTrailResponseList()) {
      auditTrailDtoEventModel.setBusinessPartnerCode(defaultProduct.getMerchantCode());
      auditTrailDtoEventModel.setProductSku(defaultProduct.getProductSku());
      auditTrailDtoEventModel.setName(defaultProduct.getProductName());
      auditTrailDtoEventModel.setOnlineStatus(true);
      auditTrailDtoEventModel.setPickupPointCode(Constants.HYPHEN);
      if (itemMap.containsKey(auditTrailDtoEventModel.getGdnSku())) {
        Item item = itemMap.get(auditTrailDtoEventModel.getGdnSku());
        auditTrailDtoEventModel.setGdnSku(item.getItemSku());
        auditTrailDtoEventModel.setName(item.getGeneratedItemName());
      } else {
        auditTrailDtoEventModel.setGdnSku(Constants.DEFAULT);
      }
      AuditTrailDto auditTrailDto = new AuditTrailDto();
      BeanUtils.copyProperties(auditTrailDtoEventModel, auditTrailDto);
      auditTrailResponseList.add(auditTrailDto);
    }
  }

  private List<Item> getItems(AuditTrailListByProductCodeEventModel auditTrailListByProductCodeEventModel,
      Product defaultProduct) {
    auditTrailListByProductCodeEventModel.getAuditTrailResponseList()
        .forEach(auditTrailDtoEventModel -> auditTrailDtoEventModel.setName(defaultProduct.getProductName()));
    auditTrailListByProductCodeEventModel.getAuditTrailResponseList()
        .forEach(auditTrailDtoEventModel -> auditTrailDtoEventModel.setProductSku(defaultProduct.getProductSku()));
    Set<String> itemCodeList = auditTrailListByProductCodeEventModel.getAuditTrailResponseList().stream()
        .map(AuditTrailDtoEventModel::getGdnSku).collect(Collectors.toSet());
    return itemHelperService.getBasicItemDetailsByItemCodes(Constants.DEFAULT_STORE_ID, itemCodeList);
  }

  private void publishHistoryEvent(List<AuditTrailDto> auditTrailResponseList,
      AuditTrailListByProductCodeEventModel auditTrailListByProductCodeEventModel) {
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAuditTrailResponseList(auditTrailResponseList);
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(auditTrailListByProductCodeEventModel.getChangedBy());
    auditTrailListResponse.setClientId(auditTrailListByProductCodeEventModel.getClientId());
    auditTrailListResponse.setRequestId(auditTrailListByProductCodeEventModel.getRequestId());
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setUpdateDirectlyToDB(true);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }
}
