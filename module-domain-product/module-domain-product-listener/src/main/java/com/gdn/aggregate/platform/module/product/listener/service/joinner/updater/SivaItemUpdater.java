package com.gdn.aggregate.platform.module.product.listener.service.joinner.updater;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.ProductReviewChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StoreClosingMerchantChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.MainConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaItemConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomMerchantService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomProductReviewService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveProcessedService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Component("ProductSivaItemUpdater")
public class SivaItemUpdater {

  /*Raw*/
  @Autowired
  private ProductService productService;
  @Autowired
  private ItemService itemService;
  @Autowired
  private PickupPointService pickupPointService;

  /*Custom*/
  @Autowired
  private CustomMerchantService customMerchantService;
  @Autowired
  private CustomProductReviewService customProductReviewService;

  /*Processed*/
  @Autowired
  private SivaItemService sivaItemService;
  @Autowired
  private SaveProcessedService saveProcessedService;

  /*Helper*/
  @Autowired
  private MainConstructor mainConstructor;
  @Autowired
  private SivaItemConstructor sivaItemConstructor;

  /*Properties*/
  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private PublisherService publisherService;

  @Value("${module.domain.product.siva.item.combined.upsert}")
  private boolean sivaItemCombinedUpsertEnabled;

  @Value("${new.master.data.flow.enabled}")
  private boolean newMasterDataFlowEnabled;

  /*DirectUpdate*/
  public Mono<Boolean> directUpdateByProductReviewChangeEvent(ProductReviewChangeEvent productReviewChangeEvent, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    CustomProductReview productReview = customProductReviewService.getExistingCustomProductReview(productReviewChangeEvent.getId());
    String productSku = ModuleProductUtil.toReviewId(productReview);
    Review review = ModuleProductUtil.toReview(productReview);

    List<SivaItem> sivaItems = Optional.ofNullable(productSku)
        .map(sivaItemService::getSivaItemsByProductSku)
        .map(svItems -> svItems.stream()
            .map(svItem -> {
              svItem.setTimestamp(productReview.getTimestamp());
              svItem.setReview(review);
              return svItem;
            })
            .collect(Collectors.toList()))
        .orElseGet(ArrayList::new);

    return saveProcessedService.saveSivaItems(sivaItems,saveParam);
  }

  public Mono<Boolean> directUpdateByStoreClosingMerchantChangeEvent(StoreClosingMerchantChangeEvent storeClosingMerchantChangeEvent, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    CustomMerchant merchant = customMerchantService.getExistingCustomMerchant(storeClosingMerchantChangeEvent.getId());
    List<String> productSkus = productService.getProductSkusByMerchantCode(merchant.getId());

    List<SivaItem> sivaItems = productSkus.stream()
        .map(sivaItemService::getSivaItemsByProductSku)
        .filter(vals -> !CollectionUtils.isEmpty(vals))
        .flatMap(Collection::stream)
        .filter(Objects::nonNull)
        .map(svItem -> {
          svItem.setTimestamp(storeClosingMerchantChangeEvent.getTimestamp());
          svItem.setMerchant(merchant);
          return svItem;
        })
        .collect(Collectors.toList());

    return saveProcessedService.saveSivaItems(sivaItems,saveParam);
  }

  public Mono<Boolean> directUpdateByMasterData(MasterData masterData, List<Product> products, List<Item> items, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    Map<String, Product> mapProduct = products.stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toMap(Product::getProductSku, product -> product));

    List<SivaItem> sivaItems = items.stream()
        .filter(Objects::nonNull)
        .map(itm -> {
          String itemSku = itm.getItemSku();
          String productSku = itm.getProductSku();

          return Optional.ofNullable(sivaItemService.getExistingSivaItem(itemSku))
              .map(svItem -> {
                svItem.setTimestamp(masterData.getTimestamp());
                svItem = sivaItemConstructor.constructSivaItemProductRelated(svItem,mapProduct.get(productSku),itm);
                svItem = sivaItemConstructor.constructSivaItemItemRelated(svItem,mapProduct.get(productSku),itm);
                return svItem;
              })
              .orElse(null);
        })
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
    if (newMasterDataFlowEnabled) {
      return publishSivaItems(sivaItems, saveParam);
    } else {
      return saveProcessedService.saveSivaItems(sivaItems, saveParam);
    }
  }

  public Mono<Boolean> directUpdateByProduct(Product product, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    String productSku = product.getProductSku();

    List<SivaItem> sivaItems = Optional.ofNullable(sivaItemService.getSivaItemsByProductSku(productSku))
        .map(svItems -> svItems.stream()
            .map(svItem -> {
              Item item = itemService.getExistingItem(svItem.getItemSku());

              svItem.setTimestamp(product.getTimestamp());
              return sivaItemConstructor.constructSivaItemProductRelated(svItem,product,item);
            })
            .collect(Collectors.toList()))
        .orElseGet(ArrayList::new);

    if (!CollectionUtils.isEmpty(sivaItems)) {
      return saveProcessedService.saveSivaItems(sivaItems,saveParam);
    } else {
      Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      List<Item> items = itemService.getItemsByProductSku(productSku);
      return sivaItemConstructor.joinSivaItem(product,items,saveParam);
    }
  }

  public Mono<Boolean> directUpdateByItem(Item item, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    String itemSku = item.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    Item itemForSivaItem = itemService.getExistingItem(itemSku);
    Product product = productService.getExistingProduct(productSku);

    SivaItem sivaItem = Optional.ofNullable(sivaItemService.getExistingSivaItem(itemSku))
        .map(svItem -> {
          svItem.setTimestamp(item.getTimestamp());
          return sivaItemConstructor.constructSivaItemItemRelated(svItem,product,itemForSivaItem);
        })
        .orElse(null);

    if (Objects.nonNull(sivaItem)) {
      return saveProcessedService.saveSivaItem(sivaItem,saveParam);
    } else {
      Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      return sivaItemConstructor.joinSivaItem(product, MainUtil.toList(itemForSivaItem),saveParam);
    }
  }

  public Mono<Boolean> directUpdateByPickupPoint(PickupPoint pickupPoint, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    String itemSku = pickupPoint.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    Item itemForSivaItem = itemService.getExistingItem(itemSku);
    Product product = productService.getExistingProduct(productSku);

    SivaItem sivaItem = Optional.ofNullable(sivaItemService.getExistingSivaItem(itemSku))
        .map(svItem -> {
          svItem.setTimestamp(pickupPoint.getTimestamp());
          return sivaItemConstructor.constructSivaItemItemRelated(svItem,product,itemForSivaItem);
        })
        .orElse(null);

    if (Objects.nonNull(sivaItem)) {
      if (sivaItemCombinedUpsertEnabled) {
        return publishSingleSivaItem(itemForSivaItem, sivaItem, saveParam);
      } else {
        return saveProcessedService.saveSivaItem(sivaItem, saveParam);
      }
    } else {
      Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      return sivaItemConstructor.joinSivaItem(product, MainUtil.toList(itemForSivaItem),saveParam);
    }
  }

  public Mono<Boolean> directUpdateByTag(List<Item> items, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    long timestamp = ModuleProductUtil.toFirstTimestamp(items);

    List<SivaItem> sivaItems = items.stream()
        .map(itm -> Optional.ofNullable(itm.getItemSku())
            .map(sivaItemService::getExistingSivaItem)
            .map(svItem -> {
              svItem.setTimestamp(timestamp);
              svItem.setTags(ModuleProductUtil.toTags(MainUtil.toList(itm),svItem.isFbbActivated(),svItem.isHasCncActivated(),svItem.getPurchasedType(),svItem.getPurchasedTypeScore(),pickupPointProperties.isOnePartyActivated()));
              return svItem;
            })
            .orElse(null))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());

    return saveProcessedService.saveSivaItems(sivaItems,saveParam);
  }

  public Mono<Boolean> directUpdateByFBBItem(SivaItem sivaItem, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_ITEM);
    if (!MainUtil.isMarkForDelete(sivaItem)) {
      return saveProcessedService.saveSivaItem(sivaItem,saveParam);
    } else {
      return saveProcessedService.deleteSivaItem(sivaItem,saveParam);
    }
  }

  public Mono<Boolean> publishSivaItems(List<SivaItem> sivaItems, SaveParam saveParam) {
    return Flux.fromIterable(sivaItems).doOnNext(
        sivaItem -> log.info("Publishing save of SivaItem with productCode: {} to topic: {}",
          Optional.ofNullable(sivaItem.getProductCode()).orElse(StringUtils.EMPTY),
          Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT))
      .flatMap(sivaItem -> publishSingleSivaItem(null, sivaItem, saveParam)).all(result -> result);
  }


  private Mono<Boolean> publishSingleSivaItem(Item itemForSivaItem, SivaItem sivaItem, SaveParam saveParam) {
    SivaItemCombinedUpsertEventModel eventModel =
      ModuleProductUtil.toSivaItemCombinedUpsertEventModel(itemForSivaItem, saveParam, sivaItem,
        true, new HashSet<>());

    eventModel.setId(Optional.ofNullable(sivaItem.getProductSku()).orElse(StringUtils.EMPTY));

    return publisherService.publishSivaItemUpsertCombinedEvent(eventModel);
  }

}
