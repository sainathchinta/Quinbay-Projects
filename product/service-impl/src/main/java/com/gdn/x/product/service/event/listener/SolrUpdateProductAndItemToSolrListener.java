package com.gdn.x.product.service.event.listener;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.google.common.collect.ImmutableSet;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.update.to.solr", havingValue = "true")
public class SolrUpdateProductAndItemToSolrListener {

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ProductService productService;

  @Value("${blacklist.seller.list.l3.reindex}")
  private String blackListSellersForL3Reindex;

  @Value("${l3.solr.atomic.update.enabled}")
  private boolean l3AtomicUpdateEnabled;

  @KafkaListener(topics = ProductDomainEventName.UPDATE_TO_SOLR, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to solrUpdate topic : {} , {}", ProductDomainEventName.UPDATE_TO_SOLR, message);
    try {
      ProductAndItemEventModel productAndItemEventModel =
          this.objectMapper.readValue(message, ProductAndItemEventModel.class);
      checkForRejection(productAndItemEventModel);
      if (Objects.nonNull(productAndItemEventModel)) {
        if (isEligibleForL3Reindexing(productAndItemEventModel)) {
          if (Objects.nonNull(productAndItemEventModel.getProduct()) && CollectionUtils.isNotEmpty(
              productAndItemEventModel.getItems())) {
            this.productAndItemSolrIndexerService.applyProductAndItems(toProductAndItemVo(productAndItemEventModel),
                l3AtomicUpdateEnabled);
          } else if (Objects.nonNull(productAndItemEventModel.getProduct())) {
            this.productAndItemSolrIndexerService.applyProduct(
                objectConverterService.convertToProduct(productAndItemEventModel.getProduct()),
                l3AtomicUpdateEnabled && productAndItemEventModel.isSkipInventoryCallForAtomicUpdate());
          } else if (CollectionUtils.isNotEmpty(productAndItemEventModel.getItems())) {
            this.doIndexItem(productAndItemEventModel.getProductSku(), objectConverterService
                    .convertToListItem(productAndItemEventModel.getItems(), productAndItemEventModel.getProductSku(),
                productAndItemEventModel.isNeedToOverrideL4DetailsFromL5()));
          } else if (MapUtils.isNotEmpty(productAndItemEventModel.getFieldsAndValues())) {
            productAndItemSolrIndexerService.eventBasedAtomicUpdateToSolr(productAndItemEventModel.getProductSku(),
                productAndItemEventModel.getMerchantCode(), productAndItemEventModel.getFieldsAndValues());
          }
          log.info("updated in solr by event : {} for productSku : {} ",
            ProductDomainEventName.UPDATE_TO_SOLR, productAndItemEventModel.getProductSku());
        }
        else {
          log.info("Dropped L3 Solr Reindex for product : {} belonging to merchant : {} ",
            productAndItemEventModel.getProductSku(), productAndItemEventModel.getMerchantCode());
        }
      }
    } catch (Exception e) {
      log.error("Exception caught while updating  documents to solr, payload: {} ", message, e);
    }
  }

  private boolean isEligibleForL3Reindexing(ProductAndItemEventModel productAndItemEventModel) {
    if (productAndItemEventModel.isRejected()) {
      return false;
    }
    String productSku = productAndItemEventModel.getProductSku();
    if (StringUtils.isEmpty(blackListSellersForL3Reindex) || StringUtils.isEmpty(productSku)) {
      return true;
    }
    return Stream.of(blackListSellersForL3Reindex.split(","))
        .noneMatch(blackListedSellerCode -> productSku.startsWith(blackListedSellerCode));
  }


  private ProductAndItemsVO toProductAndItemVo(ProductAndItemEventModel productAndItemEventModel) {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(objectConverterService
        .convertToProduct(productAndItemEventModel.getProduct()));
    productAndItemsVO.setItems(objectConverterService.convertToListItem(productAndItemEventModel.getItems(),
        productAndItemEventModel.getProductSku(), productAndItemEventModel.isNeedToOverrideL4DetailsFromL5()));
    return productAndItemsVO;
  }

  private void doIndexItem(String productSku, List<Item> itemList) throws Exception {
    Product product =
        this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(itemList.get(0).getStoreId(),
            productSku);
    this.productAndItemSolrIndexerService.applyProductAndItems(new ProductAndItemsVO(product, itemList), l3AtomicUpdateEnabled);
  }

  private void checkForRejection(ProductAndItemEventModel productAndItemEventModel) {
    if (productAndItemEventModel.isRejected()) {
      productAndItemSolrIndexerService.deleteProductsFromSolrAfterPostLiveRejection(
          ImmutableSet.of(productAndItemEventModel.getProductSku()));
      productAndItemSolrIndexerService.deleteItemsFromSolrAfterPostLiveRejection(
          productAndItemEventModel.getItems().stream().map(ItemEventModel::getItemSku).distinct()
              .collect(Collectors.toList()));
    }
  }
}
