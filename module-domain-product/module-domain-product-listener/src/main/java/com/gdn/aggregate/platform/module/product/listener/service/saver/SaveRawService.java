package com.gdn.aggregate.platform.module.product.listener.service.saver;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateQueue;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.UpdateQueueProperties;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.service.helper.SchedulerService;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.updater.MainUpdater;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CampaignProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CheapestPriceDayService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.FlashsaleProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MerchantDiscountPriceService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.UpdateQueueService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleScheduleService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@Component("ProductSaveRawService")
public class SaveRawService {

  @Autowired
  private UpdateQueueService updateQueueService;

  @Autowired
  private MerchantDiscountPriceService merchantDiscountPriceService;

  @Autowired
  private AdjustmentProductService adjustmentProductService;

  @Autowired
  private CampaignProductService campaignProductService;

  @Autowired
  private CheapestPriceDayService cheapestPriceDayService;

  @Autowired
  private FlashsaleProductService flashsaleProductService;

  @Autowired
  private SivaFlashsaleScheduleService sivaFlashsaleScheduleService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private MasterDataItemService masterDataItemService;

  @Autowired
  private MasterDataProductService masterDataProductService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private ProductService productService;

  @Autowired
  private AdjustmentProductQuotaService adjustmentProductQuotaService;

  @Autowired
  private SchedulerService schedulerService;

  @Autowired
  private SaveSemiService saveSemiService;

  @Autowired
  private MainUpdater mainUpdater;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private UpdateQueueProperties updateQueueProperties;

  @Autowired
  private PublisherService publisherService;

  /*Raw*/

  public Mono<Boolean> saveMerchantDiscountPriceChange(MerchantDiscountPrice merchantDiscountPrice, SaveParam saveParam) {
    if (Objects.isNull(merchantDiscountPrice)) {
      return MainUtil.failedResult();
    } else {
      merchantDiscountPriceService.setMandatory(merchantDiscountPrice, saveParam);
      return merchantDiscountPriceService.save(merchantDiscountPrice, saveParam)
          .flatMap(subVal -> chainSavePickupPoint(pickupPointService.getExistingPickupPoint(ModuleProductUtil.toPickupPointId(merchantDiscountPrice.getItemSku(), merchantDiscountPrice.getPickupPointCode())), saveParam));
    }
  }

  public Mono<Boolean> chainSaveAdjustmentProduct(String itemSku, String pickupPointCode, SaveParam saveParam) {
    List<AdjustmentProduct> adjustmentProducts = adjustmentProductService.getNotEndedAdjustmentProductsByL5(itemSku,pickupPointCode);
    if (CollectionUtils.isEmpty(adjustmentProducts)) {
      return MainUtil.failedResult();
    } else {
      return saveAdjustmentProducts(adjustmentProducts,ParamUtil.toSaveParamChainSave(saveParam));
    }
  }

  public Mono<Boolean> saveAdjustmentProducts(List<AdjustmentProduct> adjustmentProducts, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(adjustmentProducts)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(adjustmentProducts)
          .flatMap(adjustmentProduct -> saveAdjustmentProduct(adjustmentProduct,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveAdjustmentProduct(AdjustmentProduct adjustmentProduct, SaveParam saveParam) {
    if (Objects.isNull(adjustmentProduct)) {
      return MainUtil.failedResult();
    } else {
      adjustmentProductService.setMandatory(adjustmentProduct, saveParam);
      return adjustmentProductService.save(adjustmentProduct, saveParam)
          .flatMap(subVal -> chainSavePickupPoint(pickupPointService.getExistingPickupPoint(ModuleProductUtil.toPickupPointId(adjustmentProduct.getItemSku(), adjustmentProduct.getPickupPointCode())), saveParam));
    }
  }

  public Mono<Boolean> saveAdjustmentProductQuota(AdjustmentProductQuota adjustmentProductQuota, SaveParam saveParam) {
    if (Objects.isNull(adjustmentProductQuota)) {
      return MainUtil.failedResult();
    } else {
      adjustmentProductQuotaService.setMandatory(adjustmentProductQuota, saveParam);
      return adjustmentProductQuotaService.save(adjustmentProductQuota, saveParam)
          .flatMap(subVal -> {
            if (ProductType.DIGITAL.equals(adjustmentProductQuota.getPickupPointCode())) {
              return mainUpdater.directUpdateSivaProductByAdjustmentProductQuota(adjustmentProductQuota, saveParam);
            } else {
              return chainSaveAdjustmentProduct(adjustmentProductQuota.getItemSku(), adjustmentProductQuota.getPickupPointCode(), saveParam);
            }
          });
    }
  }

  public Mono<Boolean> saveMasterData(MasterData masterData, SaveParam saveParam, boolean newMasterDataFlowEnabled) {
    if (Objects.isNull(masterData)) {
      return MainUtil.failedResult();
    } else {
      masterDataService.setMandatory(masterData, saveParam);
      Mono<Boolean> saveMasterDataCommand = masterDataService.save(masterData, saveParam);

      MasterDataProduct masterDataProduct = masterDataProductService.toMasterDataProduct(masterData);
      List<MasterDataItem> masterDataItems = masterDataItemService.toMasterDataItems(masterData);
      List<Product> products = productService.getProductsAfterChangeMasterDataProduct(masterDataProduct);
      List<Item> items = itemService.getItemsAfterChangeMasterDataItems(masterDataItems);

      SaveParam saveParamRawOnly = ParamUtil.toSaveParamCustomSaveProcessed(saveParam, false);
      Optional.ofNullable(saveParamRawOnly).map(SaveParam::getEventParam).ifPresent(val -> val.setIgnoreTimestamp(true));
      Mono<Boolean> saveMasterDataProductAndItemsCommand = Mono.zip(saveMasterDataProduct(masterDataProduct, saveParamRawOnly), saveMasterDataItems(masterDataItems, saveParamRawOnly),
          (saveMasterDataProduct, saveMasterDataItems) -> true);
      Mono<Boolean> saveProductsAndItemsCommand = Mono.zip(saveProducts(products, saveParamRawOnly, newMasterDataFlowEnabled), saveItems(items, saveParamRawOnly,
              newMasterDataFlowEnabled),
          (saveProducts, saveItems) -> true);

      SaveParam saveParamWithProcessed = ParamUtil.toSaveParamCustomSaveProcessed(saveParam, true);
      Optional.ofNullable(saveParamWithProcessed).map(SaveParam::getEventParam).ifPresent(val -> val.setIgnoreTimestamp(true));
      Mono<Boolean> directUpdateCommand = mainUpdater.directUpdateSivaBothByMasterData(masterData, products, items, saveParamWithProcessed,newMasterDataFlowEnabled);

      return saveMasterDataCommand
          .flatMap(subVal -> saveMasterDataProductAndItemsCommand)
          .flatMap(subVal -> saveProductsAndItemsCommand)
          .flatMap(subVal -> directUpdateCommand);
    }
  }

  private Mono<Boolean> saveMasterDataProduct(MasterDataProduct masterDataProduct, SaveParam saveParam) {
    if (Objects.isNull(masterDataProduct)) {
      return MainUtil.failedResult();
    } else {
      masterDataProductService.setMandatory(masterDataProduct);
      return masterDataProductService.save(masterDataProduct, saveParam);
    }
  }

  private Mono<Boolean> saveMasterDataItems(List<MasterDataItem> masterDataItems, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(masterDataItems)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(masterDataItems)
          .flatMap(masterDataItem -> saveMasterDataItem(masterDataItem,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  private Mono<Boolean> saveMasterDataItem(MasterDataItem masterDataItem, SaveParam saveParam) {
    if (Objects.isNull(masterDataItem)) {
      return MainUtil.failedResult();
    } else {
      masterDataItemService.setMandatory(masterDataItem);
      return masterDataItemService.save(masterDataItem, saveParam);
    }
  }

  public Mono<Boolean> saveProducts(List<Product> products, SaveParam saveParam,
      boolean newMasterDataFlowEnabled) {
    if (CollectionUtils.isEmpty(products)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(products)
          .flatMap(product -> saveProduct(product,saveParam, newMasterDataFlowEnabled))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveProduct(Product product, SaveParam saveParam, boolean newMasterDataFlowEnabled) {
    if (Objects.isNull(product)) {
      return MainUtil.failedResult();
    } else {
      productService.setMandatory(product, saveParam);
      if(newMasterDataFlowEnabled) {
        log.info("New master data update queue enabled for product {}",product.getProductCode());
        RawProductCombinedUpsertEventModel rawProductCombinedUpsertEventModel =
            ModuleProductUtil.getRawProductCombinedUpsertEventModel(product, saveParam);
        return publisherService.publishRawProductUpsertCombinedEvent(rawProductCombinedUpsertEventModel);
      }
      else {
        return productService.save(product, saveParam).flatMap(val -> {
          if (ParamUtil.isSaveProcessed(saveParam)) {
            return mainUpdater.directUpdateSivaBothByProduct(product, saveParam);
          } else {
            return MainUtil.successResult();
          }
        });
      }
    }
  }

  public Mono<Boolean> chainSaveItem(Item item, SaveParam saveParam) {
    if (Objects.isNull(item)) {
      return MainUtil.failedResult();
    } else {
      return saveItem(item, ParamUtil.toSaveParamChainSave(saveParam), false);
    }
  }

  public Mono<Boolean> saveItems(List<Item> items, SaveParam saveParam, boolean newMasterDataFlowEnabled) {
    if (CollectionUtils.isEmpty(items)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(items)
          .flatMap(item -> saveItem(item,saveParam,newMasterDataFlowEnabled))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveItem(Item item, SaveParam saveParam, boolean newMasterDataFlowEnabled) {
    if (Objects.isNull(item)) {
      return MainUtil.failedResult();
    } else {
      itemService.setMandatory(item, saveParam);
      if (newMasterDataFlowEnabled) {
        log.info("New master data update queue enabled for item {}", item.getItemSku());
        RawItemCombinedUpsertEventModel rawItemCombinedUpsertEventModel =
            ModuleProductUtil.toRawItemCombinedUpsertEventModel(item, false, true);
        return publisherService.publishNewRawItemUpsertCombinedEvent(rawItemCombinedUpsertEventModel);
      } else {
        return itemService.save(item, saveParam).flatMap(val -> {
          if (ParamUtil.isSaveProcessed(saveParam)) {
            return mainUpdater.directUpdateSivaBothByItem(item, saveParam);
          } else {
            return MainUtil.successResult();
          }
        });
      }
    }
  }

  public Mono<Boolean> chainSavePickupPoint(PickupPoint pickupPoint, SaveParam saveParam) {
    if (Objects.isNull(pickupPoint)) {
      return MainUtil.failedResult();
    } else {
      return savePickupPoint(pickupPoint, ParamUtil.toSaveParamChainSave(saveParam));
    }
  }

  public Mono<Boolean> savePickupPoint(PickupPoint pickupPoint, SaveParam saveParam) {
    if (Objects.isNull(pickupPoint)) {
      return MainUtil.failedResult();
    } else {
      pickupPointService.setMandatory(pickupPoint, saveParam);
      Mono<Boolean> saveCommand = pickupPointService.save(pickupPoint, saveParam);
      Mono<Boolean> saveProcessedCommand;
      if (ModuleProductUtil.isAvailableUpdateQueuePickupPoint(pickupPoint, updateQueueProperties.isEnable(), saveParam)) {
        saveProcessedCommand = chainSaveUpdateQueue(pickupPoint, saveParam);
      } else if (ModuleProductUtil.isAvailableDirectUpdateByPickupPoint(pickupPoint, pickupPointProperties.getAvailableCampaignTypes(), pickupPointProperties.getRestrictedMerchantCodes())) {
        saveProcessedCommand = mainUpdater.directUpdateSivaBothByPickupPoint(pickupPoint, saveParam);
      } else {
        saveProcessedCommand = MainUtil.successResult();
      }
      return saveCommand.flatMap(val -> saveProcessedCommand);
    }
  }

  public Mono<Boolean> chainSaveUpdateQueue(PickupPoint pickupPoint, SaveParam saveParam) {
    if (Objects.isNull(pickupPoint)) {
      return MainUtil.failedResult();
    } else {
      UpdateQueue updateQueueL3 = UpdateQueue.builder()
          .timestamp(pickupPoint.getTimestamp())
          .id(pickupPoint.getProductSku())
          .level(3)
          .build();
      UpdateQueue updateQueueL4 = UpdateQueue.builder()
          .timestamp(pickupPoint.getTimestamp())
          .id(pickupPoint.getItemSku())
          .level(4)
          .build();
      return Mono.zip(saveUpdateQueue(updateQueueL3, ParamUtil.toSaveParamChainSave(saveParam)), saveUpdateQueue(updateQueueL4, ParamUtil.toSaveParamChainSave(saveParam)), (saveUpdateQueueL3, saveUpdateQueueL4) -> true);
    }
  }

  public Mono<Boolean> saveUpdateQueue(UpdateQueue updateQueue, SaveParam saveParam) {
    if (Objects.isNull(updateQueue)) {
      return MainUtil.failedResult();
    } else {
      updateQueueService.setMandatory(updateQueue, saveParam);
      return updateQueueService.save(updateQueue, saveParam);
    }
  }

  public Mono<Boolean> saveCampaignProducts(List<CampaignProduct> campaignProducts, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(campaignProducts)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(campaignProducts)
          .flatMap(campaignProduct -> saveCampaignProduct(campaignProduct,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveCampaignProduct(CampaignProduct campaignProduct, SaveParam saveParam) {
    if (Objects.isNull(campaignProduct)) {
      return MainUtil.failedResult();
    } else {
      campaignProductService.setMandatory(campaignProduct, saveParam);
      Mono<Boolean> saveCommand = campaignProductService.save(campaignProduct, saveParam);
      boolean exclusive = flashsaleProductService.isFlashsaleProductExclusive(campaignProduct);
      SivaFlashsaleSchedule existingFlashsaleSchedule = sivaFlashsaleScheduleService
        .getExistingSivaFlashsaleSchedule(
          ModuleProductUtil.toSivaFlashsaleScheduleId(
            campaignProduct.getPromotionStartTime(),
            campaignProduct.getPromotionEndTime()
          )
        );
      FlashsaleProduct flashsaleProduct = ModuleProductUtil.toFlashsaleProduct(campaignProduct, existingFlashsaleSchedule, exclusive, saveParam);
      if (Objects.nonNull(flashsaleProduct)) {
        return saveCommand.flatMap(val -> chainSaveFlashsaleProduct(flashsaleProduct, saveParam));
      } else {
        Item item = itemService.getExistingItem(ModuleProductUtil.toCampaignItemSku(campaignProduct));
        return saveCommand.flatMap(val -> chainSaveItem(item, saveParam));
      }
    }
  }

  public Mono<Boolean> chainSaveFlashsaleProduct(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    if (Objects.isNull(flashsaleProduct)) {
      return MainUtil.failedResult();
    } else {
      return saveFlashsaleProduct(flashsaleProduct, ParamUtil.toSaveParamChainSave(saveParam));
    }
  }

  public Mono<Boolean> saveFlashsaleProduct(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    if (Objects.isNull(flashsaleProduct)) {
      return MainUtil.failedResult();
    } else {
      flashsaleProductService.setMandatory(flashsaleProduct, saveParam);
      return flashsaleProductService.save(flashsaleProduct, saveParam)
          .flatMap(subVal -> chainSaveSivaProductByFlashsaleProduct(flashsaleProduct, saveParam))
          .flatMap(subVal -> saveSemiService.chainSaveSivaFlashsaleSchedule(flashsaleProduct, saveParam))
          .flatMap(subVal -> saveSemiService.chainSaveSivaFlashsaleGroup(flashsaleProduct, saveParam))
          .flatMap(subVal -> schedulerService.scheduleDeactivateSivaFlashsaleSchedule(flashsaleProduct,saveParam.getTraceId()))
          .flatMap(subVal -> schedulerService.scheduleDeactivateSivaFlashsaleGroup(flashsaleProduct,saveParam.getTraceId()));
    }
  }

  public Mono<Boolean> chainSaveSivaProductByFlashsaleProduct(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    if (ProductType.DIGITAL.equals(ModuleProductUtil.getProductTypeFromFlashsaleProduct(flashsaleProduct))) {
      return mainUpdater.directUpdateSivaProductByFlashsaleProduct(flashsaleProduct,saveParam);
    } else {
      return chainSavePickupPoint(pickupPointService.getExistingPickupPoint(ModuleProductUtil.toPickupPointId(flashsaleProduct.getItemSku(), flashsaleProduct.getPickupPointCode())), saveParam);
    }
  }

  public Mono<Boolean> saveCampaignProductDays(List<CheapestPriceDay> cheapestPriceDays, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(cheapestPriceDays)) {
      return MainUtil.failedResult();
    } else {
      return Flux.fromIterable(cheapestPriceDays)
          .flatMap(cheapestPriceDay -> saveCampaignProductDay(cheapestPriceDay,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveCampaignProductDay(CheapestPriceDay cheapestPriceDay, SaveParam saveParam) {
    if (Objects.isNull(cheapestPriceDay)) {
      return MainUtil.failedResult();
    } else {
      cheapestPriceDayService.setMandatory(cheapestPriceDay, saveParam);
      return cheapestPriceDayService.save(cheapestPriceDay, saveParam)
          .flatMap(val -> chainSaveItem(itemService.getExistingItem(cheapestPriceDay.getItemSku()), saveParam));
    }
  }

}
