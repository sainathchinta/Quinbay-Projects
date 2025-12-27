package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.KafkaService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductTag;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.raw.BusinessPartnerProfileChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.raw.DenpasarSearchLogisticOptionChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FbbChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.DenpasarShippingLogisticOptionChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.TradeInChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.updater.MainUpdater;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomMerchantService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveRawService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class TagService {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductService productService;

  @Autowired
  private CustomMerchantService customMerchantService;

  @Autowired
  private SaveRawService saveRawService;

  @Autowired
  private MainUpdater mainUpdater;

  @Autowired
  private KafkaService kafkaService;

  private Mono<Boolean> updateTag(List<Item> items, SaveParam saveParam) {
    return saveRawService.saveItems(items,ParamUtil.toSaveParamCustomSaveProcessed(saveParam,false), false)
        .flatMap(val -> mainUpdater.directUpdateSivaBothByTag(items,ParamUtil.toSaveParamCustomSaveProcessed(saveParam,true)))
        .onErrorResume(MainUtil::errorResult);
  }

  /*Start of THD*/
  //Shipping
  public Mono<Boolean> updateShippingThdTagInItem(DenpasarShippingLogisticOptionChange denpasarShippingLogisticOptionChange, SaveParam saveParam) {
    return Optional.ofNullable(denpasarShippingLogisticOptionChange)
        .map(this::fromShippingToItems)
        .filter(CollectionUtils::isNotEmpty)
        .map(item -> updateTag(item,saveParam))
        .orElseGet(MainUtil::failedResult);
  }

  private List<Item> fromShippingToItems(DenpasarShippingLogisticOptionChange denpasarShippingLogisticOptionChange) {
    return Optional.ofNullable(denpasarShippingLogisticOptionChange)
        .map(DenpasarShippingLogisticOptionChange::getItemSku)
        .map(itemSku -> itemService.getExistingItem(itemSku))
        .map(item -> fromShippingToItem(item,denpasarShippingLogisticOptionChange))
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  private Item fromShippingToItem(Item item, DenpasarShippingLogisticOptionChange denpasarShippingLogisticOptionChange) {
    Item.Tag tag = ModuleProductUtil.toTag(item);
    Optional.ofNullable(denpasarShippingLogisticOptionChange)
        .map(DenpasarShippingLogisticOptionChange::getLogisticOptions)
        .filter(logisticOptions -> !CollectionUtils.isEmpty(logisticOptions))
        .map(logisticOptions -> logisticOptions.stream().anyMatch(ProductTag.TWOHD_CODE::equals))
        .ifPresent(tag::setThd);
    item.setTag(tag);
    Optional.ofNullable(denpasarShippingLogisticOptionChange)
        .map(BaseData::getTimestamp)
        .ifPresent(item::setTimestamp);
    return item;
  }

  //Search
  public Mono<Boolean> updateSearchThdTagInItem(DenpasarSearchLogisticOptionChange denpasarSearchLogisticOptionChange, SaveParam saveParam) {
    return Optional.ofNullable(denpasarSearchLogisticOptionChange)
        .map(this::fromSearchToItems)
        .filter(CollectionUtils::isNotEmpty)
        .map(item -> updateTag(item,saveParam))
        .orElseGet(MainUtil::failedResult);
  }

  private List<Item> fromSearchToItems(DenpasarSearchLogisticOptionChange denpasarSearchLogisticOptionChange) {
    return Optional.ofNullable(denpasarSearchLogisticOptionChange)
        .map(DenpasarSearchLogisticOptionChange::getItemSku)
        .map(itemSku -> itemService.getExistingItem(itemSku))
        .map(item -> fromSearchToItem(item,denpasarSearchLogisticOptionChange))
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  private Item fromSearchToItem(Item item, DenpasarSearchLogisticOptionChange denpasarSearchLogisticOptionChange) {
    Item.Tag tag = ModuleProductUtil.toTag(item);
    Optional.ofNullable(denpasarSearchLogisticOptionChange)
        .map(DenpasarSearchLogisticOptionChange::getBadge)
        .map(badge -> badge.get(ProductTag.LOGISTIC_BADGE_KEY))
        .map(ProductTag.LOGISTIC_BADGE_VALUE::equals)
        .ifPresent(tag::setThd);
    item.setTag(tag);
    Optional.ofNullable(denpasarSearchLogisticOptionChange)
        .map(BaseData::getTimestamp)
        .ifPresent(item::setTimestamp);
    return item;
  }
  /*End of THD*/

  /*Start of FBB*/
  //Business Partner
  public Mono<Boolean> selfRepublishFbbTag(BusinessPartnerProfileChangeEvent businessPartnerProfileChangeEvent) {
    long timestamp = Optional.ofNullable(businessPartnerProfileChangeEvent)
        .map(BusinessPartnerProfileChangeEvent::getTimestamp)
        .orElseGet(MainUtil::getCurrentTimestamp);
    boolean fbb = Optional.ofNullable(businessPartnerProfileChangeEvent)
        .map(BusinessPartnerProfileChangeEvent::getId)
        .map(customMerchantService::getExistingCustomMerchant)
        .map(CustomMerchant::getType)
        .map(ProductTag.FBB_CODE::equals)
        .orElseGet(() -> false);
    List<FbbChangeEvent> fbbChangeEvents = Optional.ofNullable(businessPartnerProfileChangeEvent)
        .map(BusinessPartnerProfileChangeEvent::getId)
        .map(productService::getProductSkusByMerchantCode)
        .orElseGet(ArrayList::new).stream()
        .map(productSku -> FbbChangeEvent.builder()
            .timestamp(timestamp)
            .id(productSku)
            .fbb(fbb)
            .build())
        .collect(Collectors.toList());

    if (CollectionUtils.isEmpty(fbbChangeEvents)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(fbbChangeEvents)
          .flatMap(fbbChangeEvent -> kafkaService.publish(fbbChangeEvent,Topics.FBB_CHANGE))
          .reduce(MainUtil::reduce);
    }
  }

  //Fbb Change
  public Mono<Boolean> updateFBBTagInItem(FbbChangeEvent fbbChangeEvent, SaveParam saveParam) {
    return Optional.ofNullable(fbbChangeEvent)
        .map(this::fromFbbChangeToItems)
        .filter(CollectionUtils::isNotEmpty)
        .map(items -> updateTag(items, saveParam))
        .orElseGet(MainUtil::failedResult);
  }

  private List<Item> fromFbbChangeToItems(FbbChangeEvent fbbChangeEvent) {
    return Optional.ofNullable(fbbChangeEvent)
        .map(FbbChangeEvent::getId)
        .map(itemService::getItemsByProductSku)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(item -> toUpdatedItemByFbbChange(item,fbbChangeEvent))
        .collect(Collectors.toList());
  }

  private Item toUpdatedItemByFbbChange(Item item, FbbChangeEvent fbbChangeEvent) {
    Item.Tag tag = ModuleProductUtil.toTag(item);
    Optional.ofNullable(fbbChangeEvent)
        .map(FbbChangeEvent::isFbb)
        .ifPresent(tag::setFbb);
    item.setTag(tag);
    Optional.ofNullable(fbbChangeEvent)
        .map(FbbChangeEvent::getTimestamp)
        .ifPresent(item::setTimestamp);
    return item;
  }
  /*End of FBB*/

  /*Start of Trade In*/
  public Mono<Boolean> updateTradeInTagInProduct(TradeInChangeEvent tradeInChangeEvent, SaveParam saveParam) {
    return Optional.ofNullable(tradeInChangeEvent)
        .map(this::fromTradeInChangeToItems)
        .filter(CollectionUtils::isNotEmpty)
        .map(items -> updateTag(items, saveParam))
        .orElseGet(MainUtil::failedResult);
  }

  private List<Item> fromTradeInChangeToItems(TradeInChangeEvent tradeInChangeEvent) {
    return Optional.ofNullable(tradeInChangeEvent)
        .map(TradeInChangeEvent::getProductSkus)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(itemService::getItemsByProductSku)
        .filter(Objects::nonNull)
        .flatMap(Collection::stream)
        .filter(Objects::nonNull)
        .map(item -> toUpdatedItemByTradeInChange(item, tradeInChangeEvent))
        .collect(Collectors.toList());
  }

  private Item toUpdatedItemByTradeInChange(Item item, TradeInChangeEvent tradeInChangeEvent) {
    Item.Tag tag = ModuleProductUtil.toTag(item);
    Optional.ofNullable(tradeInChangeEvent)
        .map(TradeInChangeEvent::isActive)
        .ifPresent(tag::setTradeIn);
    item.setTag(tag);
    Optional.ofNullable(tradeInChangeEvent)
        .map(TradeInChangeEvent::getTimestamp)
        .ifPresent(item::setTimestamp);
    return item;
  }
  /*End of Trade In*/

}
