package com.gdn.aggregate.platform.module.product.listener.service.helper;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.KafkaService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.event.ProductElasticSearchDeletionEvent;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.properties.RepublishProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.PublisherProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.DedicatedProcessingProperties;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

@Component("ProductPublisherService")
@Slf4j
public class PublisherService {

  @Autowired
  private KafkaService kafkaService;

  @Autowired
  private RepublishProperties republishProperties;

  @Autowired
  private DedicatedProcessingProperties dedicatedProcessingProperties;

  private final PublisherProperties publisherProperties;

  public PublisherService(PublisherProperties publisherProperties) {
    this.publisherProperties = publisherProperties;
  }

  /*Publisher*/
  public Mono<Boolean> publishIdTimestampSivaProducts(List<SivaProduct> sivaProducts,
    SaveParam saveParam) {
    Flux<SivaProduct> flux = Flux.fromIterable(sivaProducts).filter(Objects::nonNull);

    Function<SivaProduct, Mono<Boolean>> safePublisher =
      sivaProduct -> publishIdTimestampSivaProduct(sivaProduct, saveParam)
        .onErrorResume(ex -> {
          log.warn("Failed to publish product {}: {}", sivaProduct.getProductSku(),
            ex.getMessage());
          return Mono.just(false); // continue processing other products
        });

    if (publisherProperties.isThrottlingEnabled()) {
      // concat will ensure that the order of processing is maintained and throttling is applied
      // to ensure that we do not overwhelm the Kafka broker
      return flux.flatMap(safePublisher, publisherProperties.getConcurrency())
        .reduce(MainUtil::reduce);
    } else {
      return flux.flatMap(safePublisher).reduce(MainUtil::reduce);
    }
  }


  public Mono<Boolean> publishIdTimestampSivaProduct(SivaProduct sivaProduct, SaveParam saveParam) {
    IdTimestamp idTimestampSivaProduct = ModuleProductUtil.toIdTimestampSivaProduct(sivaProduct,saveParam);

    Mono<Boolean> mainPublish = kafkaService.publish(idTimestampSivaProduct,Topics.ID_TIMESTAMP_SIVA_PRODUCT);
    Mono<Boolean> publishFixPrice = kafkaService.publish(ModuleProductUtil.getFixPriceIdTimestamp(idTimestampSivaProduct,sivaProduct),Topics.FIX_PRICE);
    Mono<Boolean> publishToLkpp = kafkaService.publish(ModuleProductUtil.getFilteredIdTimestamp(idTimestampSivaProduct,saveParam,republishProperties.getLkppTopics()),Topics.LKPP_PRODUCT_ID);

    return Mono.zip(mainPublish, publishFixPrice, publishToLkpp).map(tuple3 -> true);
  }

  public Mono<Boolean> publishIdTimestampSivaItems(List<SivaItem> sivaItems, SaveParam saveParam) {
    Flux<SivaItem> flux = Flux.fromIterable(sivaItems).filter(Objects::nonNull);
    Function<SivaItem, Mono<Boolean>> normalPublish =
      sivaItem -> publishIdTimestampSivaItem(sivaItem, saveParam).onErrorResume(ex -> {
        log.error("Failed to publishIdTimestampSivaItems : item {}: ", sivaItem.getItemSku(), ex);
        return Mono.just(false);
      });
    if (publisherProperties.isThrottlingEnabled()) {
      // concat will ensure that the order of processing is maintained and throttling is applied
      // to ensure that we do not overwhelm the Kafka broker
      return flux.flatMap(normalPublish, publisherProperties.getConcurrency())
        .reduce(MainUtil::reduce);
    } else {
      return flux.flatMap(normalPublish).reduce(MainUtil::reduce);
    }
  }


  public Mono<Boolean> publishIdTimestampSivaItem(SivaItem sivaItem, SaveParam saveParam) {
    IdTimestamp idTimestampSivaItem = ModuleProductUtil.toIdTimestampSivaItem(sivaItem,saveParam);

    Mono<Boolean> mainPublish = kafkaService.publish(idTimestampSivaItem,Topics.ID_TIMESTAMP_SIVA_ITEM);
    Mono<Boolean> publishFixItemCode = kafkaService.publish(ModuleProductUtil.getFixItemCodeIdTimestamp(idTimestampSivaItem,sivaItem),Topics.FIX_ITEM_CODE);
    Mono<Boolean> publishToProductItem = kafkaService.publish(ModuleProductUtil.getFilteredIdTimestamp(idTimestampSivaItem,saveParam,republishProperties.getProductItemTopics()),Topics.PRODUCTFEED_ITEM_ID);
    Mono<Boolean> publishToProductItemMigration = kafkaService.publish(ModuleProductUtil.getFilteredIdTimestamp(idTimestampSivaItem,saveParam,republishProperties.getProductItemMigrationTopics()),Topics.PRODUCTFEED_ITEM_ID_REPUBLISH);

    return Mono.zip(mainPublish, publishFixItemCode, publishToProductItem, publishToProductItemMigration).map(tuple4 -> true);
  }

  public Mono<Boolean> publishCampaignProductTagLabel(CampaignTeaserLive campaignTeaserLive, SaveParam saveParam, boolean allowed) {
    List<String> filters = MainUtil.toList(Topics.CAMPAIGN_TEASER_LIVE,Topics.CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE);
    return kafkaService.publish(ModuleProductUtil.getFilteredCampaignProductTagLabel(campaignTeaserLive,saveParam,filters,allowed),Topics.CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE);
  }

  public Mono<Boolean> publishUpdateQueue(IdTimestamp idTimestamp, SaveParam saveParam) {
    return kafkaService.publish(idTimestamp,Topics.UPDATE_QUEUE_MODULE_PRODUCT);
  }

  public Mono<Boolean> publishSivaProductUpsertCombinedEvent(
    SivaProductCombinedUpsertEventModel sivaProductCombinedUpsertEventModel){
    String targetTopic = determineSivaProductTopic(sivaProductCombinedUpsertEventModel);
    return kafkaService.publish(sivaProductCombinedUpsertEventModel, targetTopic);
  }

  public Mono<Boolean> publishRawProductUpsertCombinedEvent(
    RawProductCombinedUpsertEventModel rawProductCombinedUpsertEventModel){
    String merchantCode = extractMerchantCodeFromRawProduct(rawProductCombinedUpsertEventModel);
    String destinationTopic = determineRawProductTopic(merchantCode);
    return kafkaService.publish(rawProductCombinedUpsertEventModel, destinationTopic);
  }
  
  private String extractMerchantCodeFromRawProduct(RawProductCombinedUpsertEventModel eventModel) {
    if (Objects.isNull(eventModel) || Objects.isNull(eventModel.getProduct())) {
      return StringUtils.EMPTY;
    }
    Product product = eventModel.getProduct();
    if (StringUtils.isNotBlank(product.getMerchantCode())) {
      return product.getMerchantCode();
    }
    return ModuleProductUtil.extractMerchantCodeFromSku(product.getProductSku());
  }

  private String determineRawProductTopic(String merchantCode) {
    if (StringUtils.isNotBlank(merchantCode)
      && dedicatedProcessingProperties.isL3ProcessingEnabled()
      && dedicatedProcessingProperties.isHighVolumeSeller(merchantCode)) {

      return Topics.RAW_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME;
    }
    return Topics.RAW_PRODUCT_COMBINED_UPSERT;
  }

  public Mono<Boolean> publishSivaPickupPointUpsertCombinedEvent(
    PickupPointUpsertCombinedEventModel pickupPointUpsertCombinedEventModel) {
    ModuleProductUtil.setPartitionKeyForPickupPointEvent(pickupPointUpsertCombinedEventModel,
      dedicatedProcessingProperties.getPartitionKey());
    String targetTopic = determinePickupPointTopic(pickupPointUpsertCombinedEventModel);
    return kafkaService.publish(pickupPointUpsertCombinedEventModel, targetTopic);
  }

  private String determinePickupPointTopic(PickupPointUpsertCombinedEventModel eventModel) {
    String merchantCode = ModuleProductUtil.extractMerchantCode(eventModel);
    if (StringUtils.isNotBlank(merchantCode)
      && dedicatedProcessingProperties.isL5ProcessingEnabled()
      && dedicatedProcessingProperties.isHighVolumeSeller(merchantCode)) {
      return Topics.NEW_PICKUP_POINT_UPSERT_EVENT_HIGH_VOLUME;
    }
    return Topics.NEW_PICKUP_POINT_UPSERT_EVENT;
  }

  private String determineSivaProductTopic(SivaProductCombinedUpsertEventModel eventModel) {
    String merchantCode = ModuleProductUtil.extractMerchantCode(eventModel);
    if (StringUtils.isNotBlank(merchantCode)
      && dedicatedProcessingProperties.isL3ProcessingEnabled()
      && dedicatedProcessingProperties.isHighVolumeSeller(merchantCode)) {
      log.debug("Routing siva product event for high-volume seller {} to dedicated queue",
        merchantCode);
      return Topics.SIVA_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME;
    }
    return Topics.SIVA_PRODUCT_COMBINED_UPSERT;
  }

  public Mono<Boolean> publishNewRawItemUpsertCombinedEvent(
      RawItemCombinedUpsertEventModel rawItemCombinedUpsertEventModel) {
    String merchantCode = extractMerchantCodeFromRawItem(rawItemCombinedUpsertEventModel);
    String targetTopic = determineTargetTopicForRawItem(merchantCode);
    return kafkaService.publish(rawItemCombinedUpsertEventModel, targetTopic);
  }
  
  private String extractMerchantCodeFromRawItem(RawItemCombinedUpsertEventModel eventModel) {
    if (Objects.isNull(eventModel) || Objects.isNull(eventModel.getItem())) {
      return null;
    }
    Item item = eventModel.getItem();
    if (StringUtils.isNotBlank(item.getMerchantCode())) {
      return item.getMerchantCode();
    }
    return ModuleProductUtil.extractMerchantCodeFromSku(item.getItemSku());
  }

  private String determineTargetTopicForRawItem(String merchantCode) {
    if (StringUtils.isNotBlank(merchantCode)
      && dedicatedProcessingProperties.isL4ProcessingEnabled()
      && dedicatedProcessingProperties.isHighVolumeSeller(merchantCode)) {
      return Topics.NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME;
    }
    return Topics.NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT;
  }

  public Mono<Boolean> publishSivaItemUpsertCombinedEvent(
    SivaItemCombinedUpsertEventModel sivaItemCombinedUpsertEventModel) {
    String targetTopic = determineSivaItemTopic(sivaItemCombinedUpsertEventModel);
    return kafkaService.publish(sivaItemCombinedUpsertEventModel, targetTopic);
  }

  private String determineSivaItemTopic(SivaItemCombinedUpsertEventModel eventModel) {
    String merchantCode = ModuleProductUtil.extractMerchantCode(eventModel);
    if (StringUtils.isNotBlank(merchantCode)
      && dedicatedProcessingProperties.isL4ProcessingEnabled()
      && dedicatedProcessingProperties.isHighVolumeSeller(merchantCode)) {
      log.debug("Routing siva item event for high-volume seller {} to dedicated queue",
        merchantCode);
      return Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME;
    }
    return Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT;
  }

  public Mono<Boolean> publishProductEsDeletionEvent(ProductElasticSearchDeletionEvent event) {
    return kafkaService.publish(event, Topics.PRODUCT_ES_DELETION_EVENT);
  }
  /*End of Publisher*/

}
