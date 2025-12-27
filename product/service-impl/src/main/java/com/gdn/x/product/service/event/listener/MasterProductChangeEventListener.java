package com.gdn.x.product.service.event.listener;


import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.productcategorybase.product.publish.listener.enabled",
                       havingValue = "true")
public class MasterProductChangeEventListener {

  @Value("${productcategorybase.argument.storeId}")
  private String storeId;

  @Autowired
  private MasterDataCacheService cacheService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${product.visibility.switch.enabled}")
  private boolean isPristineSettingEnabled;

  @Value("${delete.l3.l4.switch.enabled}")
  private boolean isDeleteL3AndL4Enabled;

  @Value("${master.data.change.solr.reindex.enabled}")
  private boolean masterDataChangeSolrReindexEnabled;

  @Value("#{${event.to.blacklisted.sellers.map}}")
  private Map<String, Set<String>> eventToBlackListedSellersMap;

  @KafkaListener(topics = DomainEventName.PRODUCT_PUBLISH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    String requestId = UUID.randomUUID().toString();
    Map<String, Double> productAndTotalScoreMap = new HashMap<>();
    log.info("Consume event {} with message : {}", DomainEventName.PRODUCT_PUBLISH, message);
    try {
      ProductDomainEventModel productDomainEventModel =
        this.objectMapper.readValue(message, ProductDomainEventModel.class);
      if (StringUtils.isNotEmpty(productDomainEventModel.getProductCode())) {
        setStoreId(productDomainEventModel);
        updatePoductSalesCategory(productDomainEventModel);
        updateBrandForUnsyncProducts(productDomainEventModel);
        deleteRejectedProductsAndEvictMasterDataCacheAndSolrReindex(productDomainEventModel, eventToBlackListedSellersMap);
        evictProductAndItemCache(productDomainEventModel);
        updateItemIfPristineEnabled(requestId, productDomainEventModel);
      }
    } catch (Exception ex) {
      log.error("Error while Event listening when product update from PCB , payload - {}, error - ",
        message, ex);
      log.error("Error : {}", ex);
    }
  }

  private void updateItemIfPristineEnabled(String requestId, ProductDomainEventModel productDomainEventModel) {
    if (this.isPristineSettingEnabled && productDomainEventModel.isActivated() && productDomainEventModel.isViewable()) {
      log.debug(
          "Event listening when product update from PCB, product code: {} , request id {}, timestamp : {}, mark for delete : {}",
          productDomainEventModel.getProductCode(), requestId, productDomainEventModel.getTimestamp(), productDomainEventModel.isproductMarkForDelete());
      if (!productDomainEventModel.isproductMarkForDelete()) {
        Map<String, Boolean> itemsChanged = productDomainEventModel.getProductItems().stream().collect(Collectors
            .toMap(ProductItemDomainEventModel::getSkuCode, ProductItemDomainEventModel::isContentChanged));
        itemService.publishItemSkus(storeId, itemsChanged, itemToPublish(productDomainEventModel),
            productDomainEventModel);
      }
    } else {
      log.info(
          "Event listening when product update from PCB. Ignore  product : {} as they are not active in xproduct, request id {} ",
          productDomainEventModel.getProductCode(), requestId);
    }
  }

  private void evictProductAndItemCache(ProductDomainEventModel productDomainEventModel) {
    for (ProductItemDomainEventModel productItem : productDomainEventModel.getProductItems()) {
      this.cacheService.evictMasterDataItem(productItem.getSkuCode(), productItem);
      this.cacheService.evictCacheMasterDataForTransaction(productItem.getSkuCode());
    }
  }

  private void setStoreId(ProductDomainEventModel productDomainEventModel) {
    if (StringUtils.isNotEmpty(productDomainEventModel.getStoreId())) {
      storeId = productDomainEventModel.getStoreId();
    }
  }

  private void updateBrandForUnsyncProducts(ProductDomainEventModel productDomainEventModel) {
    try {
      if (productDomainEventModel.isBrandChanged()) {
        log.info("Brand update on consuming the PRODUCT_PUBLISH event for productCode :{}",
            productDomainEventModel.getProductCode());
        productService.updateBrandForUnsyncProducts(storeId, productDomainEventModel);
      }
    } catch (ApplicationRuntimeException ex) {
      log.error("Error in brand update for product : {}, error : {}, error - ",
        productDomainEventModel.getProductCode(), ex.getMessage(), ex);
    }
  }

  private void deleteRejectedProductsAndEvictMasterDataCacheAndSolrReindex(
    ProductDomainEventModel productDomainEventModel,
    Map<String, Set<String>> eventToBlackListedSellersMap) {
    if (productDomainEventModel.isproductMarkForDelete()) {
      itemService.archiveAndDeleteActiveProduct(productDomainEventModel, eventToBlackListedSellersMap);
      this.cacheService.evictMasterDataProductWithoutSolrIndex(productDomainEventModel.getProductCode(),
          productDomainEventModel);
    } else {
      this.cacheService.evictMasterDataProduct(productDomainEventModel.getProductCode(), productDomainEventModel);
      if (productDomainEventModel.isSolrUpdateRequired() && masterDataChangeSolrReindexEnabled) {
        this.productAndItemSolrIndexerService.updateInSolrByProductCode(productDomainEventModel.getStoreId(),
            productDomainEventModel.getProductCode());
      }
    }
  }

  private void updatePoductSalesCategory(ProductDomainEventModel productDomainEventModel) {
    if (Objects.nonNull(productDomainEventModel.getProductSalesCategoryMapping()) && (
        CollectionUtils.isNotEmpty(productDomainEventModel.getProductSalesCategoryMapping().getNewSalesCategoryCodes())
            || CollectionUtils.isNotEmpty(productDomainEventModel.getProductSalesCategoryMapping().getOldSalesCategoryCodes())
            || CollectionUtils
            .isNotEmpty(productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes()))) {
      if (!productDomainEventModel.isIgnoreSalesCategoryPublish()) {
        log.info("Category change detected for productCode : {} and categoryDetails are {}", productDomainEventModel.getProductCode(), productDomainEventModel.getProductSalesCategoryMapping());
        productService.updateProductCatalogByProductCodeOnCategoryChange(storeId, productDomainEventModel.getProductCode(),
            productDomainEventModel.getProductSalesCategoryMapping(), productDomainEventModel.getProductCategories(),
            productDomainEventModel.isBopisEligible());
      }
    }
  }

  private Set<String> itemToPublish(ProductDomainEventModel productDomainEventModel) {
    return Optional.ofNullable(productDomainEventModel).map(ProductDomainEventModel::getProductItems)
        .orElse(new ArrayList<>()).stream().filter(ProductItemDomainEventModel::isPublishL4)
        .map(ProductItemDomainEventModel::getSkuCode).collect(Collectors.toSet());
  }


}
