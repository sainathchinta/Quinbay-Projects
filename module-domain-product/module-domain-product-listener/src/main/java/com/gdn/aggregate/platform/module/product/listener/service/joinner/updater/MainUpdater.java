package com.gdn.aggregate.platform.module.product.listener.service.joinner.updater;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.PartialClasses;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductEnded;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.ProductReviewChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StoreClosingMerchantChangeEvent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;

@Component("ProductMainUpdater")
public class MainUpdater {

  @Autowired
  private SivaProductUpdater sivaProductUpdater;
  @Autowired
  private SivaItemUpdater sivaItemUpdater;
  @Autowired
  private SivaFlashsaleScheduleUpdater sivaFlashsaleScheduleUpdater;
  @Autowired
  private SivaFlashsaleGroupUpdater sivaFlashsaleGroupUpdater;
  @Autowired
  private SivaCampaignProductUpdater sivaCampaignProductUpdater;

  public Mono<Boolean> directUpdateSivaBothByProductReviewChangeEvent(ProductReviewChangeEvent productReviewChangeEvent, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.PRODUCT_REVIEW_CHANGE_EVENT))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByProductReviewChangeEvent(productReviewChangeEvent,svParam), sivaProductUpdater.directUpdateByProductReviewChangeEvent(productReviewChangeEvent,svParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByStoreClosingMerchantChangeEvent(StoreClosingMerchantChangeEvent storeClosingMerchantChangeEvent, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.STORE_CLOSING_CHANGE_EVENT))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByStoreClosingMerchantChangeEvent(storeClosingMerchantChangeEvent,svParam), sivaProductUpdater.directUpdateByStoreClosingMerchantChangeEvent(storeClosingMerchantChangeEvent,svParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByMasterData(MasterData masterData, List<Product> products, List<Item> items, SaveParam saveParam,
      boolean newMasterDataFlowEnabled) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByMasterData(masterData,products,items,svParam), sivaProductUpdater.directUpdateByMasterData(masterData,products,svParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByProduct(Product product, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.PRODUCT))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByProduct(product,svParam), sivaProductUpdater.directUpdateByProduct(product,svParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByItem(Item item, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByItem(item,svParam), sivaProductUpdater.directUpdateByItem(item,svParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByPickupPoint(PickupPoint pickupPoint, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByPickupPoint(pickupPoint,svParam), sivaProductUpdater.directUpdateByPickupPoint(pickupPoint,svParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByTag(List<Item> items, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.TAGS))
        .flatMap(svParam -> Mono.zip(sivaItemUpdater.directUpdateByTag(items,svParam), sivaProductUpdater.directUpdateByTag(items,saveParam), MainUtil::reduce))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaBothByFbbItem(SivaItem sivaItem, SaveParam saveParam) {
    return sivaItemUpdater.directUpdateByFBBItem(sivaItem,saveParam)
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> queueUpdateSivaBothByItem(Item item, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> {
          if (ParamUtil.getLevel(saveParam)==3) {
            return sivaProductUpdater.directUpdateByItem(item,svParam);
          } else {
            return sivaItemUpdater.directUpdateByItem(item,svParam);
          }
        })
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaProductByFlashsaleProduct(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> sivaProductUpdater.directUpdateByFlashsaleProduct(flashsaleProduct,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaProductByAdjustmentProductQuota(AdjustmentProductQuota adjustmentProductQuota, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> sivaProductUpdater.directUpdateByAdjustmentProductQuota(adjustmentProductQuota,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaCampaignProductByCampaignProductLive(CampaignProductLive campaignProductLive, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> sivaCampaignProductUpdater.directUpdateByCampaignProductLive(campaignProductLive,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaCampaignProductByCampaignProductEnded(CampaignProductEnded campaignProductEnded, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.END_SIVA_CAMPAIGN_PRODUCT))
        .flatMap(svParam -> sivaCampaignProductUpdater.directUpdateByCampaignProductEnded(campaignProductEnded,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaCampaignProductByCampaignTeaserLive(CampaignTeaserLive campaignTeaserLive, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> sivaCampaignProductUpdater.directUpdateByCampaignTeaserLive(campaignTeaserLive,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaFlashsaleScheduleByFlashsaleSchedule(FlashsaleSchedule flashsaleSchedule, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> sivaFlashsaleScheduleUpdater.directUpdateByFlashsaleSchedule(flashsaleSchedule,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaFlashsaleScheduleClean(IdTimestamp idTimestamp, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.CLEAN_SIVA_FLASHSALE_SCHEDULE))
        .flatMap(svParam -> sivaFlashsaleScheduleUpdater.directUpdateClean(idTimestamp,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaFlashsaleScheduleDeactivate(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.DEACTIVATE_SIVA_FLASHSALE_SCHEDULE))
        .flatMap(svParam -> sivaFlashsaleScheduleUpdater.directUpdateDeactivate(flashsaleProduct,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaFlashsaleGroupByFlashsaleGroup(FlashsaleGroup flashsaleGroup, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,null))
        .flatMap(svParam -> sivaFlashsaleGroupUpdater.directUpdateByFlashsaleGroup(flashsaleGroup,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaFlashsaleGroupClean(IdTimestamp idTimestamp, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.CLEAN_SIVA_FLASHSALE_GROUP))
        .flatMap(svParam -> sivaFlashsaleGroupUpdater.directUpdateClean(idTimestamp,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> directUpdateSivaFlashsaleGroupDeactivate(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamDirectUpdate(saveParam,PartialClasses.DEACTIVATE_SIVA_FLASHSALE_GROUP))
        .flatMap(svParam -> sivaFlashsaleGroupUpdater.directUpdateDeactivate(flashsaleProduct,svParam))
        .onErrorResume(MainUtil::errorResult);
  }

}
