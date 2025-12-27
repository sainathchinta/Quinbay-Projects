package com.gdn.aggregate.platform.module.product.listener.service.saver;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.ProductPickupPointInventoryRepository;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointInventoryService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.util.CompleteItemData;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaItemConstructorV2;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaProductConstructorV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomBusinessPartnerPickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomInventoryServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomMerchantServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomPcbCategoryServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomProductReviewServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CampaignProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CheapestPriceDayServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.FlashSaleProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MerchantDiscountPriceServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import reactor.core.publisher.Mono;

import static com.gdn.aggregate.platform.module.product.listener.constants.Collections.SIVA_PRODUCT;

@Component("ProductSaveRawServiceV2")
@Slf4j
public class SaveRawServiceV2 {

  public static final String SUCCESS_SAVE_SIVA_ITEM = "Success Save Siva Item for {} ";
  public static final String ERROR_ON_SAVE_OF_SIVA_ITEM = "Error on save of Siva Item for {} ";

  @Autowired
  private ProductServiceV2 productService;

  @Autowired
  private ItemServiceV2 itemService;

  @Autowired
  private PickupPointServiceV2 pickupPointService;

  @Autowired
  private AdjustmentProductQuotaServiceV2 adjustmentProductQuotaService;

  @Autowired
  private AdjustmentProductServiceV2 adjustmentProductService;

  @Autowired
  private CustomInventoryServiceV2 customInventoryService;

  @Autowired
  private FlashSaleProductServiceV2 flashSaleProductService;

  @Autowired
  private CampaignProductServiceV2 campaignProductService;

  @Autowired
  private CheapestPriceDayServiceV2 cheapestPriceDayService;

  @Autowired
  private MasterDataItemServiceV2 masterDataItemService;

  @Autowired
  private MasterDataProductServiceV2 masterDataProductService;

  @Autowired
  private CustomPcbCategoryServiceV2 customPcbCategoryService;

  @Autowired
  private CustomMerchantServiceV2 customMerchantService;

  @Autowired
  private CustomBusinessPartnerPickupPointServiceV2 customBusinessPartnerPickupPointService;

  @Autowired
  private MerchantDiscountPriceServiceV2 merchantDiscountPriceService;

  @Autowired
  private CustomProductReviewServiceV2 customProductReviewService;

  @Autowired
  private SivaItemServiceV2 sivaItemService;

  @Autowired
  private SivaItemConstructorV2 sivaItemConstructor;

  @Autowired
  private SivaProductConstructorV2 sivaProductConstructor;

  @Autowired
  private SivaProductServiceV2 sivaProductService;

  @Autowired
  private SaveProcessedService saveProcessedService;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private SchedulerHelper schedulerHelper;

  @Autowired
  private PublisherService publisherService;

  @Autowired
  private PickupPointInventoryService pickupPointInventoryService;

  @Autowired
  private ProductPickupPointInventoryRepository pickupPointInventoryRepository;

  @Value("${module.domain.product.siva.product.combined.upsert}")
  private boolean sivaProductCombinedUpsertEnabled;

  @Value("${module.domain.product.siva.item.combined.upsert}")
  private boolean sivaItemCombinedUpsertEnabled;

  @Value("${fetch.flash.sale.products}")
  private boolean  fetchFlashSaleProducts;
  
  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  @Value("${process.pickupPoint.inventory:true}")
  private boolean processPickupPointInventory;

  public void saveItem(Item item, SaveParam saveParam) {
    if(Objects.nonNull(item)){
      processSaveItem(item, saveParam);
    }
  }

  public void savePickupPoint(PickupPoint pickupPoint, SaveParam saveParam) {
      processSavePickupPoint(pickupPoint, saveParam);
  }

  public boolean saveAdjustmentProduct(AdjustmentProduct adjustmentProduct, SaveParam saveParam) {
    if (Objects.isNull(adjustmentProduct)) {
      return false;
    } else {
      return processSaveAdjustmentProduct(adjustmentProduct, saveParam);
    }
  }

  private boolean processSaveAdjustmentProduct(AdjustmentProduct adjustmentProduct, SaveParam saveParam) {
    Optional<AdjustmentProduct> existingAdjustmentProduct = adjustmentProductService.findById(adjustmentProduct.toId());
    List<AdjustmentProductQuota> adjustmentProductQuotaList =
        adjustmentProductQuotaService.findAllByItemSkuIn(Set.of(adjustmentProduct.getItemSku()));
    adjustmentProductService.setMandatory(adjustmentProduct, saveParam,
        existingAdjustmentProduct.map(List::of).orElseGet(ArrayList::new), adjustmentProductQuotaList);
    adjustmentProductService.save(adjustmentProduct, saveParam).block();
    return pickupPointService.getExistingPickupPoint(
            ModuleProductUtil.toPickupPointId(adjustmentProduct.getItemSku(), adjustmentProduct.getPickupPointCode()))
        .map(val -> processSavePickupPoint(val, saveParam)).orElse(true);
  }

  private boolean processSavePickupPoint(PickupPoint pickupPoint, SaveParam saveParam) {
    //fetch all the data at once
    if(Objects.nonNull(pickupPoint)) {
      CompleteItemData completeItemData = getCompleteItemData(pickupPoint);
      pickupPointService.setMandatory(pickupPoint,
        completeItemData.getAllCustomInventoryPickupPointInfos(),
        completeItemData.getAllCustomInventoryInfos(),
        completeItemData.getAllCustomBusinessPartnerPickupPoint(),
        completeItemData.getAllMerchantDiscountPrice(), completeItemData.getAllAdjustmentProducts(),
        completeItemData.getAllAdjustmentProductQuotas(), completeItemData.getAllPickupPointInventory());
      pickupPointService.save(pickupPoint, saveParam).block();

      return updateSivaProductAndItemCollectionsForL5Update(pickupPoint, saveParam, completeItemData);
    }
    return false;
  }

  private void inventoryUpdate(SaveParam saveParam, PickupPointInventory inventory, 
    PickupPointInventory existingInventory) {
    if (processPickupPointInventory) {
      if (Objects.nonNull(existingInventory) && Objects.nonNull(inventory) &&
          existingInventory.getEventTimestamp() > inventory.getEventTimestamp()) {
        log.info("[DEBUG] Found existing inventory with newer timestamp - ID: {}, existing: {}, new: {}, " +
          "skipping save to preserve newer data", inventory.getId(), existingInventory.getEventTimestamp(), inventory.getEventTimestamp());
        return;
      }

      try {
          pickupPointInventoryService.save(inventory, saveParam).doOnSuccess(success -> {
          }).doOnError(error -> log.error("[DEBUG] Failed to save PickupPointInventory with ID: {}",
            inventory.toId(), error)).block();
      } catch (Exception e) {
        log.error("[DEBUG] Failed to save PickupPointInventory, continuing with PickupPoint save",
          e);
      }
    }
  }

  private boolean updateSivaProductAndItemCollectionsForL5Update(PickupPoint pickupPoint, SaveParam saveParam,
    CompleteItemData completeItemData) {
    //remove updated l5s from complete data list and add again
    Optional.ofNullable(completeItemData.getAllPickupPoints()).orElseGet(ArrayList::new).removeIf(
      productPickupPoint -> ModuleProductUtil.toPickupPointId(productPickupPoint.getItemSku(),
        productPickupPoint.getPickupPointCode()).equals(
        ModuleProductUtil.toPickupPointId(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode())));
    Optional.ofNullable(completeItemData.getAllPickupPoints()).orElseGet(ArrayList::new)
      .add(pickupPoint);

    if (ModuleProductUtil.isAvailableDirectUpdateByPickupPoint(pickupPoint,
      pickupPointProperties.getAvailableCampaignTypes(), pickupPointProperties.getRestrictedMerchantCodes())) {
      ParamUtil.toSaveParamDirectUpdate(saveParam, null);
      boolean directUpdateSivaItemByPickupPoint =
        directUpdateItemByPickupPoint(pickupPoint, saveParam, completeItemData);
      boolean directUpdateSivaProductByPickupPoint =
        directUpdateSivaProductByPickupPoint(pickupPoint, saveParam, completeItemData);
      return directUpdateSivaItemByPickupPoint && directUpdateSivaProductByPickupPoint;
    } else {
      return true;
    }
  }

  private CompleteItemData getCompleteItemData(PickupPoint pickupPoint) {
    CompleteItemData completeItemData =
        Optional.ofNullable(pickupPoint).map(PickupPoint::getProductSku).map(this::costuctCompleteItemData)
            .orElse(new CompleteItemData());
    completeItemData.setFetchStockFromInventoryModule(
      Optional.ofNullable(pickupPoint).map(PickupPoint::getNewData).orElse(false));
    fetchAllDataUsingAllItems(completeItemData);
    fetchMerchantDiscountPriceByPickupPoint(completeItemData, pickupPoint);
    fetchCustomBusinessPartnerPickupPointByPickupPoint(completeItemData, pickupPoint);
    return completeItemData;
  }

  private void processSaveItem(Item item, SaveParam saveParam) {
    //fetch all the data at once
    CompleteItemData completeItemData;
    if(sivaItemCombinedUpsertEnabled){
      Item existingItem = itemService.findSingleItemById(item.getId());

      item.setId(item.toId());
      if (Objects.nonNull(existingItem)) {
        item = itemService.toUpdatedItemCode(item, existingItem);
        item = itemService.toUpdatedTag(item, existingItem);
      }
      MasterDataItem masterDataItem = masterDataItemService.getSingleMasterDataItemById(item.getItemCode());
      item = itemService.completingMasterDataItem(item, masterDataItem);
      completeItemData = null;
    }
    else {
      Pair<Item, CompleteItemData> itemCompleteItemDataPair =
        prepareCompleteItemDataAndUpdateItem(item);
      completeItemData = itemCompleteItemDataPair.getSecond();
      item = itemCompleteItemDataPair.getFirst();
    }
    itemService.save(item, saveParam).block();

    if (ParamUtil.isSaveProcessed(saveParam)) {
      ParamUtil.toSaveParamDirectUpdate(saveParam, null);
      if(Objects.nonNull(completeItemData)) {
        Mono<Boolean> directSivaItemUpdateMono =
          processSivaItemDataUpdate(item, saveParam, completeItemData);
        Item finalItem = item;
        Mono<Boolean> directSivaProductUpdateMono = Mono.fromCallable(
          () -> directUpdateSivaProductByItem(finalItem, saveParam, completeItemData));
        directSivaItemUpdateMono.zipWith(directSivaProductUpdateMono,
          (itemResult, productResult) -> itemResult && productResult)
        .block();
      }
      else {
        processSivaItemAndSivaProductCombinedUpdate(item, saveParam);
      }
    }
  }

  private void processSivaItemAndSivaProductCombinedUpdate(Item item, SaveParam saveParam) {
        SivaItemCombinedUpsertEventModel sivaItemCombinedUpsertEventModel =
          ModuleProductUtil.toSivaItemCombinedUpsertEventModel(item, saveParam, null, false,
            Set.of(SIVA_PRODUCT));
        sivaItemCombinedUpsertEventModel.setId(
          Optional.ofNullable(item).map(Item::getProductSku).orElse(StringUtils.EMPTY));
        publishSivaItemUpsertEvent(item, sivaItemCombinedUpsertEventModel);
  }

  private Mono<Boolean> processSivaItemDataUpdate(Item item, SaveParam saveParam,
    CompleteItemData completeItemData) {
      return directUpdateSivaItemByItem(item, saveParam, completeItemData, null,
        false, Collections.emptySet());
  }

  private void publishSivaItemUpsertEvent(Item item,
    SivaItemCombinedUpsertEventModel sivaItemCombinedUpsertEventModel) {
    publisherService.publishSivaItemUpsertCombinedEvent(sivaItemCombinedUpsertEventModel)
      .block();
  }

  public Mono<Boolean> directUpdateSivaItemByItem(Item item, SaveParam sourceSaveParam, CompleteItemData completeItemData,
      SivaItem sivaItemToDirectSave, boolean directSave, Set<String> eligibleDataSourcesFroUpsert) {
    if (directSave) {
      return Boolean.TRUE.equals(
        saveProcessedService.saveSivaItem(sivaItemToDirectSave, sourceSaveParam).block()) ?
        Mono.just(true) : Mono.just(false);
    }
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam, Topics.GROUP_ID_SIVA_ITEM);
    String itemSku = item.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);
    completeItemData = fetchCompleteItemData(item, completeItemData);

    if (CollectionUtils.isNotEmpty(eligibleDataSourcesFroUpsert)) {
      return processEligibleDataSourcesForUpsert(item, saveParam, eligibleDataSourcesFroUpsert, completeItemData);
    }
    return processSivaItemUpdate(item, saveParam, completeItemData, productSku);
  }

  private Mono<Boolean> processEligibleDataSourcesForUpsert(Item item, SaveParam saveParam,
    Set<String> eligibleDataSourcesFroUpsert, CompleteItemData completeItemData) {
    MasterDataItem masterDataItem = masterDataItemService.getExistingMasterDataItem(
      item.getItemCode(), completeItemData.getAllMasterDataItem());
    item = itemService.completingMasterDataItem(item, masterDataItem);
    // Reuse the logic to process SivaItem update
    Mono<Boolean> sivaItemUpdateMono = processSivaItemUpdate(item, saveParam, completeItemData,
      ModuleProductUtil.fromItemSkuToProductSku(item.getItemSku()));

    // Handle eligibleDataSourcesFroUpsert
    Mono<Boolean> sivaProductUpdateMono = Mono.just(true); // Default to true if SivaProduct doesn't need updating
    if (eligibleDataSourcesFroUpsert.contains(SIVA_PRODUCT)) {
      Item finalItem = item;
      sivaProductUpdateMono = Boolean.TRUE.equals(
        Mono.fromSupplier(() -> directUpdateSivaProductByItem(finalItem, saveParam, completeItemData))
          .block()) ? Mono.just(true) : Mono.just(false);

    }
    // Zip the two monos. Both must complete successfully for the overall result to be true.
    return sivaItemUpdateMono.zipWith(sivaProductUpdateMono,
      (itemResult, productResult) -> itemResult && productResult);
  }

  private Mono<Boolean> processSivaItemUpdate(Item item, SaveParam saveParam, CompleteItemData completeItemData, String productSku) {
    Product product = productService.getExistingProduct(productSku, completeItemData.getAllProducts());
    SivaItem sivaItem = getExistingSivaItem(item.getItemSku(), product, item, completeItemData);

    if (Objects.nonNull(sivaItem)) {
      Boolean result = saveProcessedService.saveSivaItem(sivaItem, saveParam).block();
      return Boolean.TRUE.equals(result)
        ? Mono.just(true)
        : Mono.just(false);
    }

    cleanUpSaveParam(saveParam);
    return Mono.just(sivaItemConstructor.joinSivaItem(product, List.of(item), saveParam,
      completeItemData)).onErrorReturn(false);
  }

  private CompleteItemData fetchCompleteItemData(Item item, CompleteItemData completeItemData) {
    if (Objects.nonNull(completeItemData)) {
      return completeItemData;
    }
    CompleteItemData completeData = Optional.ofNullable(item)
      .map(Item::getProductSku)
      .map(this::costuctCompleteItemData)
      .orElseGet(CompleteItemData::new);
    if (Objects.nonNull(item)) {
      Item existingItem = itemService.getExistingItemWithObject(item, completeData.getAllItems());
      completeData.getAllItems().remove(existingItem);
      completeData.getAllItems().add(item);
      completeData.setFetchStockFromInventoryModule(item.getNewData());
    }
    fetchAllDataUsingAllItems(completeData);
    return completeData;
  }

  private SivaItem getExistingSivaItem(String itemSku, Product product, Item item, CompleteItemData completeItemData) {
    return Optional.ofNullable(sivaItemService.getExistingSivaItem(itemSku, completeItemData.getAllSivaItems()))
      .map(svItem -> {
        svItem.setTimestamp(item.getTimestamp());
        return sivaItemConstructor.constructSivaItemItemRelated(svItem, product, item, completeItemData);
      })
      .orElse(null);
  }

  private void cleanUpSaveParam(SaveParam saveParam) {
    Optional.ofNullable(saveParam).map(SaveParam::getDbParam)
      .ifPresent(dbParam -> dbParam.setSourceClassName(null));
  }


  public boolean directUpdateItemByPickupPoint(PickupPoint pickupPoint, SaveParam sourceSaveParam,
      CompleteItemData completeItemData) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam, Topics.GROUP_ID_SIVA_ITEM);
    String itemSku = pickupPoint.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    Item itemForSivaItem = itemService.getExistingItem(itemSku, completeItemData.getAllItems());
    Product product = productService.getExistingProduct(productSku, completeItemData.getAllProducts());

    SivaItem sivaItem =
        Optional.ofNullable(sivaItemService.getExistingSivaItem(itemSku, completeItemData.getAllSivaItems()))
            .map(svItem -> {
              svItem.setTimestamp(pickupPoint.getTimestamp());
              return sivaItemConstructor.constructSivaItemItemRelated(svItem, product, itemForSivaItem,
                  completeItemData);
            }).orElse(null);
    if (Objects.nonNull(sivaItem)) {
      return Boolean.TRUE.equals(saveProcessedService.saveSivaItem(sivaItem, saveParam).block());
    } else {
      Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      return sivaItemConstructor.joinSivaItem(product, MainUtil.toList(itemForSivaItem), saveParam, completeItemData);
    }
  }

  private boolean directUpdateSivaProductByItem(Item item, SaveParam sourceSaveParam,
    CompleteItemData completeItemData) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam, Topics.GROUP_ID_SIVA_PRODUCT);
    String itemSku = item.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);
    Product product = productService.getExistingProduct(productSku, completeItemData.getAllProducts());
    SivaProduct sivaProduct =
        Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku, completeItemData.getAllSivaProduct()))
            .map(svProduct -> {
              svProduct.setTimestamp(item.getTimestamp());
              return sivaProductConstructor.constructSivaProductItemsRelated(svProduct, product, completeItemData);
            }).orElse(null);

    if (Objects.nonNull(sivaProduct)) {
      return handleExistingSivaProduct(sivaProduct, saveParam, productSku, Topics.ITEM);
    } else
      return handleMissingSivaProduct(completeItemData, product, saveParam, Topics.ITEM);
  }

  private boolean handleMissingSivaProduct(CompleteItemData completeItemData, Product product,
    SaveParam saveParam, String topic) {
    Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      return sivaProductConstructor.joinSivaProductUsingCombinedUpsertFlow(product, saveParam,
        completeItemData, true, topic);
  }

  private boolean handleExistingSivaProduct(SivaProduct sivaProduct, SaveParam saveParam,
    String productSku, String topic) {
    if (sivaProductCombinedUpsertEnabled) {
      // Use the new combined upsert method
      return Boolean.TRUE.equals(
        sivaProductService.publishCombinedSivaProductUpsertModel(saveParam, productSku, topic,
          sivaProduct).thenReturn(true).onErrorReturn(false).block());
    }
    // Fall back to the original save method
    return Boolean.TRUE.equals(
      saveProcessedService.saveSivaProduct(sivaProduct, saveParam).map(Boolean.TRUE::equals)
        .defaultIfEmpty(false).block());
  }

  public boolean directUpdateSivaProductByPickupPoint(PickupPoint pickupPoint, SaveParam sourceSaveParam, CompleteItemData completeItemData) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    String itemSku = pickupPoint.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    Product product = productService.getExistingProduct(productSku, completeItemData.getAllProducts());

    SivaProduct sivaProduct =
        Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku, completeItemData.getAllSivaProduct()))
            .map(svProduct -> {
              svProduct.setTimestamp(pickupPoint.getTimestamp());
              return sivaProductConstructor.constructSivaProductItemsRelated(svProduct, product, completeItemData);
            }).orElse(null);

    if (Objects.nonNull(sivaProduct)) {
      return handleExistingSivaProduct(sivaProduct, saveParam, productSku, Topics.PICKUP_POINT);
    } else {
      return handleMissingSivaProduct(completeItemData, product, saveParam, Topics.PICKUP_POINT);
    }
  }

  private CompleteItemData costuctCompleteItemData(String productSku) {
    CompleteItemData.CompleteItemDataBuilder completeItemDataBuilder = CompleteItemData.builder();

    Product allProduct = productService.findByProductSku(productSku);
    completeItemDataBuilder.allProducts(Optional.ofNullable(allProduct).map(List::of).orElseGet(ArrayList::new));

    List<Item> allItems = itemService.findAllByProductSku(productSku);
    completeItemDataBuilder.allItems(Optional.ofNullable(allItems).orElseGet(ArrayList::new));

    List<PickupPoint> allPickupPoints = pickupPointService.findAllByProductSku(productSku);
    
    List<PickupPointInventory> allPickupPointInventory = syncPickupPointStockFromInventory(allPickupPoints);
    
    completeItemDataBuilder.allPickupPoints(Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new));
    
    if (skipStockFetchFromInventoryModule && CollectionUtils.isNotEmpty(allPickupPointInventory)) {
      completeItemDataBuilder.allPickupPointInventory(allPickupPointInventory);
    }

    if (fetchFlashSaleProducts) {
      List<FlashsaleProduct> allFlashSaleProducts = flashSaleProductService.findAllByProductSku(productSku);
      completeItemDataBuilder.allFlashSaleProducts(Optional.ofNullable(allFlashSaleProducts).orElseGet(ArrayList::new));
    }

    List<CheapestPriceDay> allCheapestPriceDay =
        cheapestPriceDayService.findAllByProductSku(productSku);
    completeItemDataBuilder.allCheapestPriceDays(
        Optional.ofNullable(allCheapestPriceDay).orElseGet(ArrayList::new));

    List<SivaProduct> allSivaProducts = sivaProductService.findAllByIds(Set.of(productSku));
    completeItemDataBuilder.allSivaProduct(allSivaProducts);

    List<CustomProductReview> allCustomProductReviews = customProductReviewService.findAllByIds(Set.of(productSku));
    completeItemDataBuilder.allCustomProductReviews(allCustomProductReviews);

    if (Optional.ofNullable(allProduct).map(Product::getProductCode).isPresent()) {
      List<MasterDataProduct> allMasterDataProduct = masterDataProductService.findAllByIds(Set.of(allProduct.getProductCode()));
      completeItemDataBuilder.allMasterDataProduct(allMasterDataProduct);
    }

    if (Optional.ofNullable(allProduct).map(Product::getMerchantCode).isPresent()) {
      List<CustomMerchant> allCustomMerchants = customMerchantService.findAllByIds(Set.of(allProduct.getMerchantCode()));
      completeItemDataBuilder.allCustomMerchants(allCustomMerchants);
    }

    Set<String> allCategoryCodes = getAllCategoryCodes(allProduct);
    if (CollectionUtils.isNotEmpty(allCategoryCodes)) {
      List<CustomPcbCategory> allCustomPcbCategory = customPcbCategoryService.findAllByCategoryCode(allCategoryCodes);
      completeItemDataBuilder.allCustomPcbCategory(allCustomPcbCategory);
    }

    return completeItemDataBuilder.build();
  }

  private List<PickupPointInventory> syncPickupPointStockFromInventory(List<PickupPoint> allPickupPoints) {
    if (!processPickupPointInventory || CollectionUtils.isEmpty(allPickupPoints)) {
      return new ArrayList<>();
    }
    try {
      Set<String> allPickupPointIds =
        allPickupPoints.stream().map(PickupPoint::getId).filter(Objects::nonNull)
          .collect(Collectors.toSet());
      if (allPickupPointIds.isEmpty()) {
        return new ArrayList<>();
      }
      List<PickupPointInventory> allInventories =
        pickupPointInventoryRepository.findAllById(allPickupPointIds);

      if (CollectionUtils.isEmpty(allInventories)) {
        log.debug("[STOCK_SYNC] No inventory records found for {} item SKUs",
          allPickupPointIds.size());
        return new ArrayList<>();
      }
      Map<String, PickupPointInventory> inventoryMap =
        allInventories.stream().filter(Objects::nonNull).collect(
          Collectors.toMap(PickupPointInventory::getId, Function.identity(),
            (existing, replacement) -> existing));
      allPickupPoints.forEach(pp -> updateInventoryRawPickupPoint(pp, inventoryMap));
      return allInventories;

    } catch (Exception e) {
      log.error(
        "[STOCK_SYNC] Error syncing pickup point stock from inventory - continuing with existing data",
        e);
      return new ArrayList<>();
    }
  }

  private static void updateInventoryRawPickupPoint(PickupPoint pp,
    Map<String, PickupPointInventory> inventoryMap) {
    String inventoryId =
      ModuleProductUtil.toPickupPointId(pp.getItemSku(), pp.getPickupPointCode());
    PickupPointInventory inventory = inventoryMap.get(inventoryId);

    if (Objects.nonNull(inventory)) {
      boolean stockChanged = pp.isInStock() != inventory.isInStock();
      boolean warehouseChanged = pp.isWarehouse() != inventory.getSyncStock();
      if (stockChanged || warehouseChanged) {
        pp.setInStock(inventory.isInStock());
        pp.setWarehouse(inventory.getSyncStock());
      }
    }
  }

  private void fetchAllDataUsingAllItems(CompleteItemData completeItemData) {
    Set<String> allItemSkus = getAllItemSkus(completeItemData.getAllItems());
    if (CollectionUtils.isNotEmpty(allItemSkus)) {
      List<AdjustmentProductQuota> allAdjustmentProductQuota =
          adjustmentProductQuotaService.findAllByItemSkuIn(allItemSkus);
      completeItemData.setAllAdjustmentProductQuotas(allAdjustmentProductQuota);

      List<AdjustmentProduct> allAdjustmentProduct = adjustmentProductService.findAllByItemSkuIn(allItemSkus);
      completeItemData.setAllAdjustmentProducts(allAdjustmentProduct);

      List<CampaignProduct> allCampaignProduct =
          campaignProductService.findAllByProductSku(allItemSkus);
      completeItemData.setAllCampaignProducts(
          Optional.ofNullable(allCampaignProduct).orElseGet(ArrayList::new));

      fetchStockInfoFromInventoryModule(completeItemData, allItemSkus);

      List<SivaItem> allSivaItems = sivaItemService.findAllByIds(allItemSkus);
      completeItemData.setAllSivaItems(allSivaItems);
    }
    // OPTIMIZATION: Only fetch inventory if not already populated (avoids duplicate DB call)
    if (CollectionUtils.isNotEmpty(completeItemData.getAllPickupPoints()) 
        && skipStockFetchFromInventoryModule 
        && CollectionUtils.isEmpty(completeItemData.getAllPickupPointInventory())) {
      List<PickupPointInventory> allPickupPointInventory = 
          pickupPointInventoryService.batchGetPickupPointInventories(completeItemData.getAllPickupPoints());
      completeItemData.setAllPickupPointInventory(allPickupPointInventory);
    }

    Set<String> allItemCodes = getAllItemCodes(completeItemData.getAllItems());
    if (CollectionUtils.isNotEmpty(allItemCodes)) {
      List<MasterDataItem> allMasterDataItem = masterDataItemService.findAllByIds(allItemCodes);
      completeItemData.setAllMasterDataItem(allMasterDataItem);
    }
  }

  private void fetchStockInfoFromInventoryModule(CompleteItemData completeItemData, Set<String> allItemSkus) {
    List<CustomInventoryInfo> allCustomInventoryInfo =
      fetchDataFromInventoryModule(completeItemData) ? Collections.emptyList() :
        customInventoryService.findAllCustomInventoryInfoByProductSku(allItemSkus);
    completeItemData.setAllCustomInventoryInfos(allCustomInventoryInfo);

    List<CustomInventoryPickupPointInfo> allCustomInventoryPickupPointInfo =
      fetchDataFromInventoryModule(completeItemData) ? Collections.emptyList() :
        customInventoryService.findAllCustomInventoryPickupPointInfoByItemSkus(allItemSkus);
    completeItemData.setAllCustomInventoryPickupPointInfos(
      Optional.ofNullable(allCustomInventoryPickupPointInfo).orElseGet(ArrayList::new));
  }

  private boolean fetchDataFromInventoryModule(CompleteItemData completeItemData) {
    return skipStockFetchFromInventoryModule && !completeItemData.isFetchStockFromInventoryModule();
  }

  private void fetchMerchantDiscountPriceByPickupPoint(CompleteItemData completeItemData, PickupPoint pickupPoint) {
    merchantDiscountPriceService.findById(pickupPoint.toId()).ifPresent(
        merchantDiscountPrice -> completeItemData.setAllMerchantDiscountPrice(List.of(merchantDiscountPrice)));
  }

  private void fetchCustomBusinessPartnerPickupPointByPickupPoint(CompleteItemData completeItemData,
      PickupPoint pickupPoint) {
    customBusinessPartnerPickupPointService.findById(pickupPoint.getPickupPointCode()).ifPresent(
        customBusinessPartnerPickupPoint -> completeItemData.setAllCustomBusinessPartnerPickupPoint(
            List.of(customBusinessPartnerPickupPoint)));
  }

  private Set<String> getAllItemSkus(List<Item> allItems) {
    return Optional.ofNullable(allItems).orElseGet(ArrayList::new).stream().map(Item::getItemSku)
        .filter(Objects::nonNull).collect(Collectors.toSet());
  }

  private Set<String> getAllItemCodes(List<Item> allItems) {
    return Optional.ofNullable(allItems).orElseGet(ArrayList::new).stream().map(Item::getItemCode)
        .filter(Objects::nonNull).collect(Collectors.toSet());
  }

  private Set<String> getAllCategoryCodes(Product allProduct) {
    Set<String> allCategoryCodes = new HashSet<>();

    Optional.ofNullable(allProduct)
        .map(Product::getSalesCatalogs).orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(Product.SalesCatalog::getListOfCategories)
        .filter(Objects::nonNull)
        .flatMap(List::stream)
        .map(Product.Category::getCategoryCode)
        .forEach(allCategoryCodes::add);

    Optional.ofNullable(allProduct)
        .map(Product::getMasterCatalog)
        .map(Product.MasterCatalog::getCategory)
        .map(Product.Category::getCategoryCode)
        .ifPresent(allCategoryCodes::add);

    return allCategoryCodes;
  }

  public void publishCombinedRawProductUpsertModel(SaveParam saveParam,
    String productSku, String topic, Product product) {
    RawProductCombinedUpsertEventModel rawProductCombinedUpsertEventModel =
      new RawProductCombinedUpsertEventModel();
    rawProductCombinedUpsertEventModel.setEventTrigger(topic);
    rawProductCombinedUpsertEventModel.setProduct(product);
    rawProductCombinedUpsertEventModel.setSaveParam(saveParam);
    rawProductCombinedUpsertEventModel.setId(productSku);
    LoggerUtil.info(String.format("Publishing Topic: %s after listening to topic: %s for product: %s",
      Topics.RAW_PRODUCT_COMBINED_UPSERT, topic, productSku));
    publisherService.publishRawProductUpsertCombinedEvent(rawProductCombinedUpsertEventModel)
        .thenReturn(true).onErrorReturn(false).block();
  }

  private Pair<Item, CompleteItemData> prepareCompleteItemDataAndUpdateItem(Item item) {
    // Prepare complete item data
    CompleteItemData completeItemData = Optional.ofNullable(item)
      .map(Item::getProductSku)
      .map(this::costuctCompleteItemData)
      .orElseGet(CompleteItemData::new);

    // Fetch and update item list
    Item existingItem = itemService.getExistingItemWithObject(item, completeItemData.getAllItems());
    completeItemData.getAllItems().remove(existingItem);
    completeItemData.getAllItems().add(item);
    completeItemData.setFetchStockFromInventoryModule(
      Optional.ofNullable(item).map(Item::getNewData).orElse(false));
    fetchAllDataUsingAllItems(completeItemData);

    // Update item ID and code
    Optional.ofNullable(item).ifPresent(data -> data.setId(data.toId()));
    item = itemService.toUpdatedItemCode(item, existingItem);

    // Fetch and complete master data item
    MasterDataItem masterDataItem = masterDataItemService.getExistingMasterDataItem(
      item.getItemCode(), completeItemData.getAllMasterDataItem());
    item = itemService.completingMasterDataItem(item, masterDataItem);

    // Update tags
    item = itemService.toUpdatedTag(item, existingItem);

    return Pair.of(item, completeItemData);
  }

  public void savePickupPointOnInventoryUpdate(PickupPoint pickupPoint,
    StockUpdateSearchEvent stockUpdateSearchEvent, SaveParam saveParam) {
    CompleteItemData completeItemData = getCompleteItemData(pickupPoint);

    try {
      if (stockUpdateSearchEvent.isDataUpdated()) {
        pickupPoint.setInStock(!stockUpdateSearchEvent.isOutOfStock());
        pickupPoint.setWarehouse(stockUpdateSearchEvent.isSyncStock());
        completeItemData.setStockUpdateSearchEvent(stockUpdateSearchEvent);
      }
    }catch (Exception e) {
      log.error("[DEBUG] Failed to save PickupPointInventory with ID: {}, eventTimestamp: {}", 
        pickupPoint.toId(), stockUpdateSearchEvent.getTimestamp(), e);
      throw e;
    }
    Optional.ofNullable(completeItemData.getAllPickupPointInventory()).orElseGet(ArrayList::new).stream()
      .filter(ppi -> ppi.getId().equals(pickupPoint.getId())).forEach(ppi -> {
        ppi.setInStock(!stockUpdateSearchEvent.isOutOfStock());
        ppi.setSyncStock(stockUpdateSearchEvent.isSyncStock());
      });

    pickupPointService.setMandatory(pickupPoint,
      completeItemData.getAllCustomInventoryPickupPointInfos(),
      completeItemData.getAllCustomInventoryInfos(),
      completeItemData.getAllCustomBusinessPartnerPickupPoint(),
      completeItemData.getAllMerchantDiscountPrice(), completeItemData.getAllAdjustmentProducts(),
      completeItemData.getAllAdjustmentProductQuotas(), completeItemData.getAllPickupPointInventory());
    
    updateSivaProductAndItemCollectionsForL5Update(pickupPoint, saveParam, completeItemData);
  }

  private static PickupPointInventory getPickupPointInventory(
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    PickupPointInventory inventory = PickupPointInventory.builder()
      .pickupPointCode(level2InventoryQuantityChangedEvent.getPickupPointCode())
      .itemSku(level2InventoryQuantityChangedEvent.getWarehouseItemSku()).inStock(
        level2InventoryQuantityChangedEvent.getSyncStock() ?
          level2InventoryQuantityChangedEvent.getAvailableStock() > 0 :
          CollectionUtils.isNotEmpty(level2InventoryQuantityChangedEvent.getWarehouseInfos()))
      .syncStock(level2InventoryQuantityChangedEvent.getSyncStock())
      .updatedTimestamp(level2InventoryQuantityChangedEvent.getTimestamp())
      .timestamp(level2InventoryQuantityChangedEvent.getTimestamp())
      .eventTimestamp(level2InventoryQuantityChangedEvent.getTimestamp()).build();
    String id = inventory.toId();
    inventory.setId(id);
    return inventory;
  }

  private static PickupPointInventory getPickupPointInventoryFromStockEvent(StockUpdateSearchEvent stockUpdateSearchEvent) {
    PickupPointInventory inventory =
      PickupPointInventory.builder().pickupPointCode(stockUpdateSearchEvent.getPickupPointCode())
        .itemSku(stockUpdateSearchEvent.getWebItemSku()).inStock(!stockUpdateSearchEvent.isOutOfStock())
        .syncStock(stockUpdateSearchEvent.isSyncStock())
        .updatedTimestamp(stockUpdateSearchEvent.getTimestamp())
        .createdTimestamp(stockUpdateSearchEvent.getTimestamp())
        .timestamp(stockUpdateSearchEvent.getTimestamp())
        .eventTimestamp(stockUpdateSearchEvent.getTimestamp()).build();
    inventory.setId(inventory.toId());
    return inventory;
  }

  private static PickupPointInventory getPickupPointInventoryWithStockData(PickupPoint pickupPoint,
    long timestamp, CompleteItemData completeItemData) {
    if (Objects.nonNull(completeItemData) && Objects.nonNull(
      completeItemData.getStockUpdateSearchEvent())) {
      // Use stock event data for accurate inventory information
      StockUpdateSearchEvent stockEvent = completeItemData.getStockUpdateSearchEvent();
      boolean correctInStock = !stockEvent.isOutOfStock();
      boolean correctSyncStock = stockEvent.isSyncStock();

      PickupPointInventory inventory =
        PickupPointInventory.builder().pickupPointCode(pickupPoint.getPickupPointCode())
          .itemSku(pickupPoint.getItemSku()).inStock(correctInStock).syncStock(correctSyncStock)
          .updatedTimestamp(timestamp).createdTimestamp(timestamp).timestamp(timestamp)
          .eventTimestamp(stockEvent.getTimestamp()).build();
      inventory.setId(inventory.toId());
      return inventory;
    } else {
      // Fallback: create new inventory from pickup point data
      return null;
    }
  }

  public void savePickupPointOnInventoryStockRepublish(PickupPoint pickupPoint,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent, SaveParam saveParam) {
    CompleteItemData completeItemData = getCompleteItemData(pickupPoint);
    // Update pickup point data from inventory stock republish event
    if (level2InventoryQuantityChangedEvent.isDataUpdated()) {
      ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint,
        level2InventoryQuantityChangedEvent);
      completeItemData.setLevel2InventoryQuantityChangedEvent(level2InventoryQuantityChangedEvent);
    }
    // instead of fetching inventory data from inventory module again, use
    // level2InventoryQuantityChangedEvent when calculating stock for cheapest PP
    pickupPointService.setMandatory(pickupPoint,
      completeItemData.getAllCustomInventoryPickupPointInfos(),
      completeItemData.getAllCustomInventoryInfos(),
      completeItemData.getAllCustomBusinessPartnerPickupPoint(),
      completeItemData.getAllMerchantDiscountPrice(), completeItemData.getAllAdjustmentProducts(),
      completeItemData.getAllAdjustmentProductQuotas(), completeItemData.getAllPickupPointInventory());
    updateSivaProductAndItemCollectionsForL5Update(pickupPoint, saveParam, completeItemData);
  }

  public void updateInventoryDataOnRepublishEvent(
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent, SaveParam saveParam) {
    PickupPointInventory inventory = getPickupPointInventory(level2InventoryQuantityChangedEvent);
    PickupPointInventory result = pickupPointInventoryService.save(inventory, saveParam).block();
    // set data updated to track if stock event was eligible for processing (for stale event
    // there is no need to update the PP collection with incoming stock event)
    level2InventoryQuantityChangedEvent.setDataUpdated(
      Objects.nonNull(result) && (result.getUpdatedTimestamp()
        >= level2InventoryQuantityChangedEvent.getTimestamp()));
  }

  public void performInventoryDataSaveOnStockUpdateEvent(
    StockUpdateSearchEvent stockUpdateSearchEvent, SaveParam saveParam) {
    PickupPointInventory inventory = getPickupPointInventoryFromStockEvent(stockUpdateSearchEvent);
    PickupPointInventory result =
      pickupPointInventoryService.save(inventory, saveParam).doOnSuccess(success -> {
      }).doOnError(error -> log.error("[DEBUG] Failed to save PickupPointInventory with ID: {}",
        inventory.toId(), error)).block();
    // set data updated to track if stock event was eligible for processing (for stale event
    // there is no need to update the PP collection with incoming stock event)
    stockUpdateSearchEvent.setDataUpdated(Objects.nonNull(result)
      && result.getUpdatedTimestamp() >= stockUpdateSearchEvent.getTimestamp());
  }
}
