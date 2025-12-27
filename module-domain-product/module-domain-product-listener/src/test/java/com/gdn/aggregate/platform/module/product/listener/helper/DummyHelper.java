package com.gdn.aggregate.platform.module.product.listener.helper;

import com.gdn.aggregate.modules.agp.engagement.common.util.AGPEngagementCommonUtilApplication;
import com.gdn.aggregate.modules.agp.engagement.common.util.helper.BaseIntegrationTest;
import com.gdn.aggregate.modules.agp.engagement.common.util.helper.JsonParser;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.DeleteExpiredEvent;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteAllRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.ModuleDomainProductListener;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductTag;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductSaved;
import com.gdn.aggregate.platform.module.product.listener.model.raw.BusinessPartnerProfileChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductEnded;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductPublished;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductRemoved;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.DenpasarSearchLogisticOptionChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.DenpasarShippingLogisticOptionChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.raw.InventoryInfoChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.InventoryStatusChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.ProductReviewChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StoreClosingMerchantChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.TradeInChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateQueue;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@SpringBootTest(
    webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
    classes = {
        AGPEngagementCommonUtilApplication.class,
        ModuleDomainProductListener.class
    }
)
public class DummyHelper extends BaseIntegrationTest {

  protected static final List<String> INDICES = MainUtil.toList(Collections.SIVA_PRODUCT,Collections.SIVA_ITEM,Collections.SIVA_CAMPAIGN_PRODUCT,Collections.SIVA_FLASHSALE_GROUP,Collections.SIVA_FLASHSALE_SCHEDULE);
  protected static final float SLEEP_TIME_1P = 3.0f;

  protected IdTimestamp idTimestampUpdateQueue;
  protected IdTimestamp idTimestampProductLevel;
  protected IdTimestamp idTimestampItemLevel;
  protected IdTimestamp idTimestampSivaFlashsaleSchedule;
  protected IdTimestamp idTimestampSivaFlashsaleGroup;
  protected ProductReviewChangeEvent productReviewChangeEvent;
  protected BusinessPartnerProfileChangeEvent businessPartnerProfileChangeEvent;
  protected TradeInChangeEvent tradeInChangeEvent;
  protected StoreClosingMerchantChangeEvent storeClosingMerchantChangeEvent;
  protected InventoryInfoChange inventoryInfoChangeEvent;
  protected InventoryStatusChange inventoryStatusChangeEvent;

  protected CustomBusinessPartnerPickupPoint businessPartnerPickupPoint;
  protected CustomInventoryPickupPointInfo inventoryPickupPointInfo;
  protected CustomInventoryInfo inventoryInfo;
  protected CustomInventoryInfo emptyInventoryInfo;
  protected CustomMerchant merchant;
  protected CustomPcbCategory pcbCategory;
  protected CustomPcbCategory otherPcbCategory;
  protected CustomProductReview productReview;

  protected UpdateQueue updateQueueL3;
  protected UpdateQueue updateQueueL4;
  protected AdjustmentProduct adjustmentProduct;
  protected AdjustmentProductQuota adjustmentProductQuota;
  protected AdjustmentProductSaved adjustmentProductSaved;
  protected CampaignProductLive campaignProductLive;
  protected CampaignProductLive campaignProductLiveSub;
  protected CampaignProductLive campaignProductLiveSpecial;
  protected CampaignProductEnded campaignProductEnded;
  protected CampaignProductEnded campaignProductEndedSub;
  protected CampaignProductEnded campaignProductEndedSpecial;
  protected CampaignProductRemoved campaignProductRemoved;
  protected CampaignTeaserLive campaignTeaserLive;
  protected CampaignProductPublished campaignProductPublished;
  protected CampaignProductPublished campaignProductPublishedNewest;
  protected CampaignProductPublished campaignProductPublishedMultiple;
  protected CampaignProductPublished campaignProductPublishedSub;
  protected CampaignProductPublished campaignProductPublishedSpecial;
  protected CampaignProduct campaignProduct;
  protected CampaignProduct campaignProductNewest;
  protected CheapestPriceDay cheapestPriceDay;
  protected CheapestPriceDay cheapestPriceDayNewest;
  protected List<CampaignProductPublished.ProductSkuEventModel> skuList;
  protected CampaignProductPublished.ProductSkuEventModel sku;
  protected Integer cheapestPriceDays;
  protected CheapestPrice cheapestPrice;
  protected CheapestPrice cheapestPriceNewest;
  protected FlashsaleProduct flashsaleProduct;
  protected FlashsaleProduct otherFlashsaleProduct;
  protected FlashsaleGroup flashsaleGroup;
  protected FlashsaleSchedule flashsaleSchedule;
  protected Item item;
  protected Item cheapestItem;
  protected Item otherCheapestItem;
  protected PickupPoint pickupPoint;
  protected PickupPoint cheapestPickupPoint;
  protected PickupPoint otherCheapestPickupPoint;
  protected DenpasarShippingLogisticOptionChange denpasarShippingLogisticOptionChange;
  protected DenpasarSearchLogisticOptionChange denpasarSearchLogisticOptionChange;
  protected MasterData masterData;
  protected MasterData masterDataWithReviewPending;
  protected MasterDataItem masterDataItem;
  protected MasterDataProduct masterDataProduct;
  protected MerchantDiscountPrice merchantDiscountPrice;
  protected MerchantDiscountPrice merchantDiscountPriceMultiple;
  protected Product product;
  protected SivaProductCombinedUpsertEventModel sivaProductCombinedUpsertEventModel;
  protected SivaProduct digitalFlashsale;
  protected DeleteExpiredEvent sivaCampaignProductDeleteAllExpired;
  protected DeleteExpiredEvent sivaFlashsaleScheduleDeleteAllExpired;
  protected DeleteExpiredEvent sivaFlashsaleGroupDeleteAllExpired;

  protected SivaFlashsaleSchedule sivaFlashsaleSchedule;
  protected SivaFlashsaleSchedule otherSivaFlashsaleSchedule;
  protected SivaFlashsaleSchedule invalidSivaFlashsaleSchedule;
  protected SivaFlashsaleGroup sivaFlashsaleGroup;
  protected SivaFlashsaleGroup invalidSivaFlashsaleGroup;
  protected SivaCampaignProduct sivaCampaignProduct;
  protected SivaCampaignProduct otherSivaCampaignProduct;

  protected SivaProduct sivaProduct;
  protected SivaProduct invalidSivaProduct;
  protected SivaItem sivaItem;
  protected SivaItem invalidSivaItem;
  protected PickupPointUpsertCombinedEventModel pickupPointUpsertCombinedEventModel;

  protected void cleanDataDummyBeforeTest() {
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.MERCHANT)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.PRODUCT_REVIEW)
        .mongo(true)
        .build());

    deleteAll(DeleteAllRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ITEM)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.MERCHANT_DISCOUNT_PRICE)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.MASTER_DATA)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.PICKUP_POINT)
        .mongo(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.PRODUCT)
        .mongo(true)
        .build());

    deleteAll(DeleteAllRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .mongo(true)
        .elasticsearch(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.SIVA_ITEM)
        .mongo(true)
        .elasticsearch(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.SIVA_CAMPAIGN_PRODUCT)
        .mongo(true)
        .elasticsearch(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .mongo(true)
        .elasticsearch(true)
        .build());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .mongo(true)
        .elasticsearch(true)
        .build());
  }

  protected void setDataDummyForTest() {
    idTimestampUpdateQueue = JsonParser.toObject(IdTimestamp.class, "IdTimestampUpdateQueue.json");
    idTimestampProductLevel = JsonParser.toObject(IdTimestamp.class, "IdTimestampProductLevel.json");
    idTimestampItemLevel = JsonParser.toObject(IdTimestamp.class, "IdTimestampItemLevel.json");
    idTimestampSivaFlashsaleSchedule = JsonParser.toObject(IdTimestamp.class, "IdTimestampSivaFlashsaleSchedule.json");
    idTimestampSivaFlashsaleGroup = JsonParser.toObject(IdTimestamp.class, "IdTimestampSivaFlashsaleGroup.json");
    productReviewChangeEvent = JsonParser.toObject(ProductReviewChangeEvent.class, "ProductReviewChangeEvent.json");
    businessPartnerProfileChangeEvent = JsonParser.toObject(BusinessPartnerProfileChangeEvent.class, "BusinessPartnerProfileChangeEvent.json");
    tradeInChangeEvent = JsonParser.toObject(TradeInChangeEvent.class, "TradeInChangeEvent.json");
    storeClosingMerchantChangeEvent = JsonParser.toObject(StoreClosingMerchantChangeEvent.class, "StoreClosingMerchantChangeEvent.json");
    inventoryInfoChangeEvent = JsonParser.toObject(InventoryInfoChange.class, "InventoryInfoChange.json");
    inventoryStatusChangeEvent = JsonParser.toObject(InventoryStatusChange.class, "InventoryStatusChange.json");

    businessPartnerPickupPoint = JsonParser.toObject(CustomBusinessPartnerPickupPoint.class, "CustomBusinessPartnerPickupPoint.json");
    businessPartnerPickupPoint.setId(businessPartnerPickupPoint.toId());
    merchant = JsonParser.toObject(CustomMerchant.class, "CustomMerchant.json");
    merchant.setId(merchant.toId());
    inventoryPickupPointInfo = JsonParser.toObject(CustomInventoryPickupPointInfo.class, "InventoryPickupPointInfo.json");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryInfo = JsonParser.toObject(CustomInventoryInfo.class, "InventoryInfo.json");
    emptyInventoryInfo = JsonParser.toObject(CustomInventoryInfo.class, "EmptyInventoryInfo.json");
    pcbCategory = JsonParser.toObject(CustomPcbCategory.class, "PcbCategory.json");
    otherPcbCategory = JsonParser.toObject(CustomPcbCategory.class, "OtherPcbCategory.json");
    productReview = JsonParser.toObject(CustomProductReview.class, "CustomProductReview.json");
    productReview.setId(productReview.toId());

    updateQueueL3 = JsonParser.toObject(UpdateQueue.class, "UpdateQueueL3.json");
    updateQueueL3.setId(updateQueueL3.toId());
    updateQueueL4 = JsonParser.toObject(UpdateQueue.class, "UpdateQueueL4.json");
    updateQueueL4.setId(updateQueueL4.toId());
    adjustmentProduct = JsonParser.toObject(AdjustmentProduct.class, "AdjustmentProduct.json");
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProductQuota = JsonParser.toObject(AdjustmentProductQuota.class, "AdjustmentProductQuota.json");
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
    adjustmentProductSaved = JsonParser.toObject(AdjustmentProductSaved.class, "AdjustmentProductSaved.json");
    adjustmentProductSaved.setId(adjustmentProductSaved.toId());
    campaignProductLive = JsonParser.toObject(CampaignProductLive.class, "CampaignProductLive.json");
    campaignProductLiveSub = JsonParser.toObject(CampaignProductLive.class, "CampaignProductLiveSub.json");
    campaignProductLiveSpecial = JsonParser.toObject(CampaignProductLive.class, "CampaignProductLiveSpecial.json");
    campaignProductEnded = JsonParser.toObject(CampaignProductEnded.class, "CampaignProductEnded.json");
    campaignProductEndedSub = JsonParser.toObject(CampaignProductEnded.class, "CampaignProductEndedSub.json");
    campaignProductEndedSpecial = JsonParser.toObject(CampaignProductEnded.class, "CampaignProductEndedSpecial.json");
    campaignProductRemoved = JsonParser.toObject(CampaignProductRemoved.class, "CampaignProductRemoved.json");
    campaignTeaserLive = JsonParser.toObject(CampaignTeaserLive.class, "CampaignTeaserLive.json");
    campaignProductPublished = JsonParser.toObject(CampaignProductPublished.class, "CampaignProductPublished.json");
    campaignProductPublished.setId(campaignProductPublished.toId());
    campaignProductPublishedNewest = JsonParser.toObject(CampaignProductPublished.class, "CampaignProductPublishedNewest.json");
    campaignProductPublishedNewest.setId(campaignProductPublishedNewest.toId());
    skuList = campaignProductPublished.getSkuList();
    campaignProductPublishedMultiple = JsonParser.toObject(CampaignProductPublished.class, "CampaignProductPublishedMultiple.json");
    campaignProductPublishedMultiple.setId(campaignProductPublishedMultiple.toId());
    campaignProductPublishedSub = JsonParser.toObject(CampaignProductPublished.class, "CampaignProductPublishedSub.json");
    campaignProductPublishedSub.setId(campaignProductPublishedSub.toId());
    campaignProductPublishedSpecial = JsonParser.toObject(CampaignProductPublished.class, "CampaignProductPublishedSpecial.json");
    campaignProductPublishedSpecial.setId(campaignProductPublishedSpecial.toId());
    campaignProduct = JsonParser.toObject(CampaignProduct.class, "CampaignProduct.json");
    campaignProduct.setId(campaignProduct.toId());
    campaignProductNewest = JsonParser.toObject(CampaignProduct.class, "CampaignProductNewest.json");
    campaignProductNewest.setId(campaignProductNewest.toId());
    sku = campaignProduct.getSku();
    cheapestPrice = JsonParser.toObject(CheapestPrice.class, "CheapestPrice.json");
    cheapestPriceNewest = JsonParser.toObject(CheapestPrice.class, "CheapestPriceNewest.json");
    cheapestPriceDays = cheapestPrice.getSkuCheapestPriceDetails().stream()
        .filter(Objects::nonNull)
        .map(CheapestPrice.SkuCheapestPriceDetailEventModel::getCheapestPriceDays)
        .findFirst()
        .orElse(null);
    cheapestPriceDay = JsonParser.toObject(CheapestPriceDay.class, "CheapestPriceDay.json");
    cheapestPriceDay.setId(campaignProduct.toId());
    cheapestPriceDayNewest = JsonParser.toObject(CheapestPriceDay.class, "CheapestPriceDayNewest.json");
    cheapestPriceDayNewest.setId(campaignProductNewest.toId());
    flashsaleProduct = JsonParser.toObject(FlashsaleProduct.class, "FlashsaleProduct.json");
    flashsaleProduct.setId(flashsaleProduct.toId());
    otherFlashsaleProduct = JsonParser.toObject(FlashsaleProduct.class, "FlashsaleProductNewest.json");
    otherFlashsaleProduct.setId(otherFlashsaleProduct.toId());
    flashsaleGroup = JsonParser.toObject(FlashsaleGroup.class, "FlashsaleGroup.json");
    flashsaleGroup.setGroupId(flashsaleGroup.toId());
    flashsaleSchedule = JsonParser.toObject(FlashsaleSchedule.class, "FlashsaleSchedule.json");
    flashsaleSchedule.setId(flashsaleSchedule.toId());
    item = JsonParser.toObject(Item.class, "Item.json");
    item.setId(item.toId());
    cheapestItem = JsonParser.toObject(Item.class, "CheapestItem.json");
    cheapestItem.setId(cheapestItem.toId());
    otherCheapestItem = JsonParser.toObject(Item.class, "OtherCheapestItem.json");
    otherCheapestItem.setId(otherCheapestItem.toId());
    denpasarShippingLogisticOptionChange = JsonParser.toObject(DenpasarShippingLogisticOptionChange.class, "DenpasarShippingLogisticOptionChange.json");
    denpasarShippingLogisticOptionChange.setId(denpasarShippingLogisticOptionChange.toId());
    denpasarSearchLogisticOptionChange = JsonParser.toObject(DenpasarSearchLogisticOptionChange.class, "DenpasarSearchLogisticOptionChange.json");
    denpasarSearchLogisticOptionChange.setId(denpasarSearchLogisticOptionChange.toId());
    masterData = JsonParser.toObject(MasterData.class, "MasterData.json");
    masterData.setId(masterData.toId());
    masterDataWithReviewPending = JsonParser.toObject(MasterData.class, "MasterDataWithReviewPending.json");
    masterDataWithReviewPending.setId(masterData.toId());
    masterDataItem = JsonParser.toObject(MasterDataItem.class, "MasterDataItem.json");
    masterDataItem.setId(masterDataItem.toId());
    masterDataProduct = JsonParser.toObject(MasterDataProduct.class, "MasterDataProduct.json");
    masterDataProduct.setId(masterDataProduct.toId());
    merchantDiscountPrice = JsonParser.toObject(MerchantDiscountPrice.class, "MerchantDiscountPrice.json");
    merchantDiscountPrice.setId(merchantDiscountPrice.toId());
    merchantDiscountPriceMultiple = JsonParser.toObject(MerchantDiscountPrice.class, "MerchantDiscountPriceMultiple.json");
    merchantDiscountPriceMultiple.setId(merchantDiscountPriceMultiple.toId());
    pickupPoint = JsonParser.toObject(PickupPoint.class, "PickupPoint.json");
    pickupPoint.setId(pickupPoint.toId());
    cheapestPickupPoint = JsonParser.toObject(PickupPoint.class, "CheapestPickupPoint.json");
    cheapestPickupPoint.setId(cheapestPickupPoint.toId());
    otherCheapestPickupPoint = JsonParser.toObject(PickupPoint.class, "OtherCheapestPickupPoint.json");
    otherCheapestPickupPoint.setId(otherCheapestPickupPoint.toId());
    product = JsonParser.toObject(Product.class, "Product.json");
    sivaProductCombinedUpsertEventModel =
      JsonParser.toObject(SivaProductCombinedUpsertEventModel.class,
        "SivaProductCombinedUpsertEventModel.json");
    product.setId(product.toId());
    digitalFlashsale = JsonParser.toObject(SivaProduct.class,"DigitalFlashsale.json");
    digitalFlashsale.setId(digitalFlashsale.toId());
    sivaCampaignProductDeleteAllExpired = JsonParser.toObject(DeleteExpiredEvent.class, "SivaCampaignProductDeleteAllExpired.json");
    sivaFlashsaleScheduleDeleteAllExpired = JsonParser.toObject(DeleteExpiredEvent.class, "SivaFlashsaleScheduleDeleteAllExpired.json");
    sivaFlashsaleGroupDeleteAllExpired = JsonParser.toObject(DeleteExpiredEvent.class, "SivaFlashsaleGroupDeleteAllExpired.json");

    sivaFlashsaleSchedule = JsonParser.toObject(SivaFlashsaleSchedule.class, "SivaFlashsaleSchedule.json");
    sivaFlashsaleSchedule.setId(sivaFlashsaleSchedule.toId());
    otherSivaFlashsaleSchedule = JsonParser.toObject(SivaFlashsaleSchedule.class, "OtherSivaFlashsaleSchedule.json");
    otherSivaFlashsaleSchedule.setId(otherSivaFlashsaleSchedule.toId());
    invalidSivaFlashsaleSchedule = JsonParser.toObject(SivaFlashsaleSchedule.class, "SivaFlashsaleSchedule.json");
    invalidSivaFlashsaleSchedule.setId(invalidSivaFlashsaleSchedule.toId());
    invalidSivaFlashsaleSchedule.setTimestamp(0L);
    sivaFlashsaleGroup = JsonParser.toObject(SivaFlashsaleGroup.class, "SivaFlashsaleGroup.json");
    sivaFlashsaleGroup.setId(sivaFlashsaleGroup.toId());
    invalidSivaFlashsaleGroup = JsonParser.toObject(SivaFlashsaleGroup.class, "SivaFlashsaleGroup.json");
    invalidSivaFlashsaleGroup.setId(invalidSivaFlashsaleGroup.toId());
    invalidSivaFlashsaleGroup.setTimestamp(0L);
    sivaCampaignProduct = JsonParser.toObject(SivaCampaignProduct.class, "SivaCampaignProduct.json");
    sivaCampaignProduct.setId(sivaCampaignProduct.toId());
    otherSivaCampaignProduct = JsonParser.toObject(SivaCampaignProduct.class, "OtherSivaCampaignProduct.json");
    otherSivaCampaignProduct.setId(otherSivaCampaignProduct.toId());
    otherSivaCampaignProduct.setTimestamp(0L);

    sivaProduct = JsonParser.toObject(SivaProduct.class, "SivaProduct.json");
    sivaProduct.setId(sivaProduct.toId());
    invalidSivaProduct = JsonParser.toObject(SivaProduct.class, "SivaProduct.json");
    invalidSivaProduct.setId(invalidSivaProduct.toId());
    invalidSivaProduct.setTimestamp(0L);
    sivaItem = JsonParser.toObject(SivaItem.class, "SivaItem.json");
    sivaItem.setId(sivaItem.toId());
    invalidSivaItem = JsonParser.toObject(SivaItem.class, "SivaItem.json");
    invalidSivaItem.setId(invalidSivaItem.toId());
    invalidSivaItem.setTimestamp(0L);
    pickupPointUpsertCombinedEventModel =
        JsonParser.toObject(PickupPointUpsertCombinedEventModel.class,
            "PickupPointUpsertCombinedEventModel.json");
  }

  protected void fullSaveDataDummy() {
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(pcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT_REVIEW)
        .domain(productReview)
        .clazz(CustomProductReview.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    save(SaveRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .domain(updateQueueL3)
        .clazz(UpdateQueue.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .domain(updateQueueL4)
        .clazz(UpdateQueue.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MERCHANT_DISCOUNT_PRICE)
        .domain(merchantDiscountPrice)
        .clazz(MerchantDiscountPrice.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PICKUP_POINT)
        .domain(pickupPoint)
        .clazz(PickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PICKUP_POINT)
        .domain(pickupPoint)
        .clazz(PickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_CAMPAIGN_PRODUCT)
        .domain(sivaCampaignProduct)
        .clazz(SivaCampaignProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
  }

  protected void createCampaign(String itemSku, String pickupPointCode, String merchantCode, String pickupPointType, String campaignCode, Integer sessionId, long start, long end, boolean active, boolean exclusive, boolean timeBased, long discount, Integer priority) {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setItemSku(itemSku);
    inventoryPickupPointInfo.setPickupPointCode(pickupPointCode);
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    if (InventoryType.TYPE_ONLINE_WAREHOUSE.equals(pickupPointType)) {
      inventoryPickupPointInfo.setWarehouseInfos(MainUtil.toList(
          new CustomInventoryPickupPointInfo.WarehouseInfo("warehouseCode1","warehouseName1","warehouseLocation1",5,5),
          new CustomInventoryPickupPointInfo.WarehouseInfo("warehouseCode2","warehouseName2","warehouseLocation2",9,15)
      ));
    } else {
      inventoryPickupPointInfo.setWarehouseInfos(null);
    }
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(itemSku);
    inventoryInfo.setId(itemSku);
    inventoryInfo.setType(pickupPointType);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    if (Objects.nonNull(priority)) {
      adjustmentProduct.setItemSku(itemSku);
      adjustmentProduct.setPickupPointCode(pickupPointCode);
      adjustmentProduct.setCampaignCode(campaignCode);
      adjustmentProduct.setSessionId(sessionId);
      adjustmentProduct.setPriority(priority);
      adjustmentProduct.setPromoType(ModuleProductUtil.getCampaignType(priority));
      adjustmentProduct.setStartDate(start);
      adjustmentProduct.setEndDate(end);
      adjustmentProduct.setActivated(active);
      adjustmentProduct.setValue(discount);
      adjustmentProduct = ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct);
      save(SaveRequest.builder()
          .index(Collections.ADJUSTMENT_PRODUCT)
          .domain(adjustmentProduct)
          .clazz(AdjustmentProduct.class)
          .mongo(true)
          .elasticsearch(false)
          .build());
      adjustmentProductQuota.setItemSku(itemSku);
      adjustmentProductQuota.setPickupPointCode(pickupPointCode);
      adjustmentProductQuota.setCampaignCode(campaignCode);
      adjustmentProductQuota.setId(adjustmentProduct.toId());
      if (active) {
        adjustmentProductQuota.setQuota(10);
        adjustmentProductQuota.setUsedQuota(5);
        adjustmentProductQuota.setUsedQuotaPerTransaction(1);
      } else {
        adjustmentProductQuota.setQuota(0);
        adjustmentProductQuota.setUsedQuota(0);
        adjustmentProductQuota.setUsedQuotaPerTransaction(0);
      }
      save(SaveRequest.builder()
          .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
          .domain(adjustmentProductQuota)
          .clazz(AdjustmentProductQuota.class)
          .mongo(true)
          .elasticsearch(false)
          .build());
      campaignProduct.setPromotionStartTime(adjustmentProduct.getStartDate());
      campaignProduct.setPromotionEndTime(adjustmentProduct.getEndDate());
      campaignProduct.getSku().setItemSku(itemSku);
      campaignProduct.getSku().setPickupPointCode(pickupPointCode);
      campaignProduct.setPriority(priority);
      campaignProduct.setCampaignType(ModuleProductUtil.getCampaignType(priority));
      campaignProduct.setCampaignCode(campaignCode);
      campaignProduct.setCampaignCodeSessionId(ModuleProductUtil.toCampaignCodeSessionId(campaignCode, sessionId));
      campaignProduct.setId(campaignProduct.toId());
      campaignProduct.setActive(active);
      campaignProduct.setExclusive(exclusive);
      save(SaveRequest.builder()
          .index(Collections.CAMPAIGN_PRODUCT)
          .domain(campaignProduct)
          .clazz(CampaignProduct.class)
          .mongo(true)
          .elasticsearch(false)
          .build());
    }
    if (CampaignPriority.FLASH_SALE.equals(priority)) {
      flashsaleProduct = JsonParser.toObject(FlashsaleProduct.class, "FlashsaleProduct.json");
      flashsaleProduct.setProductSku(ModuleProductUtil.fromItemSkuToProductSku(itemSku));
      flashsaleProduct.setItemSku(itemSku);
      flashsaleProduct.setPickupPointCode(pickupPointCode);
      flashsaleProduct.setCampaignCode(campaignCode);
      flashsaleProduct.setSessionId(sessionId);
      flashsaleProduct.getSchedule().setStart(adjustmentProduct.getStartDate());
      flashsaleProduct.getSchedule().setEnd(adjustmentProduct.getEndDate());
      flashsaleProduct.getSchedule().setTimeBased(timeBased);
      flashsaleProduct.setActive(active);
      flashsaleProduct.setExclusive(exclusive);
      flashsaleProduct.setId(ModuleProductUtil.toFlashsaleProductId(flashsaleProduct.getProductSku(),flashsaleProduct.getItemSku(),flashsaleProduct.getCampaignCode(),flashsaleProduct.getSchedule().isTimeBased()));
      save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    } else {
      flashsaleProduct = null;
    }
    businessPartnerPickupPoint.setCode(pickupPointCode);
    businessPartnerPickupPoint.setId(pickupPointCode);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setItemSku(itemSku);
    pickupPoint.setProductSku(ModuleProductUtil.fromItemSkuToProductSku(itemSku));
    pickupPoint.setPickupPointCode(pickupPointCode);
    pickupPoint.setMerchantCode(merchantCode);
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> {
      val.setChannel(Channel.DEFAULT);
      val.setCampaignCode(campaignCode);
      val.setPromoCampaign(Objects.nonNull(priority));
      val.setPriority(priority);
      val.setOfferPrice(10_000D);
      val.setListPrice(20_000D);
      val.setAdjustment(discount);
      val.setFinalOfferPrice(val.getOfferPrice()-val.getAdjustment());
    });
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setChannel(Channel.DEFAULT);
      val.setBuyable(true);
      val.getItemBuyableSchedules().setBuyable(true);
      val.setDiscoverable(true);
      val.getItemDiscoverableSchedules().setDiscoverable(true);
    });
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.PICKUP_POINT)
        .domain(pickupPoint)
        .clazz(PickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.setItemSku(itemSku);
    item.setProductSku(ModuleProductUtil.fromItemSkuToProductSku(itemSku));
    item.setMerchantCode(merchantCode);
    item.setId(item.toId());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    product.setProductSku(ModuleProductUtil.fromItemSkuToProductSku(itemSku));
    product.setMerchantCode(merchantCode);
    product.setId(product.toId());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    sivaCampaignProduct.setCampaignCode(campaignCode);
    if (CampaignPriority.FLASH_SALE.equals(priority)) {
      sivaCampaignProduct.setActivateSession(123);
    } else {
      sivaCampaignProduct.setActivateSession(null);
    }
    sivaCampaignProduct.setId(sivaCampaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.SIVA_CAMPAIGN_PRODUCT)
        .domain(sivaCampaignProduct)
        .clazz(SivaCampaignProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaProduct.setProductSku(ModuleProductUtil.fromItemSkuToProductSku(itemSku));
    sivaProduct.setMerchantCode(merchantCode);
    sivaProduct.setId(sivaProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaItem.setItemSku(itemSku);
    sivaItem.setMerchantCode(merchantCode);
    sivaItem.setId(sivaItem.toId());
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
  }

  protected void verify1P(String itemSku, String pickupPointCode,
      boolean l5buyable, boolean l5discoverable, boolean l5buyableCnc, boolean l5discoverableCnc, String l5type, int l5score,
      boolean l4hasCncActivated, boolean l4hasBuyableCnc, boolean l4hasDiscoverableCnc, boolean l4hasCncTag, String defaultL5IdSivaItem, int defaultL5ScoreSivaItem,String defaultOfflineL5IdSivaItem, int defaultOfflineL5ScoreSivaItem,
      boolean l3hasCncActivated, boolean l3hasBuyableCnc, boolean l3hasDiscoverableCnc, boolean l3hasCncTag, String defaultL5IdSivaProduct, int defaultL5ScoreSivaProduct, String defaultOfflineL5IdSivaProduct, int defaultOfflineL5ScoreSivaProduct) {
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    item.setItemSku(itemSku);
    item.setId(item.toId());
    publishAndRefreshMultiple(Topics.ITEM,item.toId(),item,SLEEP_TIME_1P,INDICES);
    pickupPoint.setItemSku(itemSku);
    pickupPoint.setPickupPointCode(pickupPointCode);
    pickupPoint.setId(pickupPoint.toId());
    Set<PickupPoint.ItemViewConfig> itemViewConfigs = new HashSet<PickupPoint.ItemViewConfig>();
    itemViewConfigs.add(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(l5buyable)
        .discoverable(l5discoverable)
        .build());
    itemViewConfigs.add(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.CNC)
        .buyable(l5buyableCnc)
        .discoverable(l5discoverableCnc)
        .build());
    pickupPoint.setItemViewConfigs(itemViewConfigs);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,SLEEP_TIME_1P, INDICES);



    /*Pickup Point Verification*/
    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(l5buyable,pickupPointMongoResult.isBuyable());
    assertEquals(l5discoverable,pickupPointMongoResult.isDiscoverable());
    assertEquals(l5buyableCnc,pickupPointMongoResult.isBuyableCnc());
    assertEquals(l5discoverableCnc,pickupPointMongoResult.isDiscoverableCnc());
    assertEquals(l5type,pickupPointMongoResult.getPurchasedType());
    assertEquals(l5score,pickupPointMongoResult.getPurchasedTypeScore());
    /*End of Pickup Point Verification*/



    /*Siva Item Verification*/
    PickupPoint defaultL5SivaItem = getFromMongo(Collections.PICKUP_POINT,defaultL5IdSivaItem,PickupPoint.class);
    assertNotNull(defaultL5SivaItem);
    assertEquals(defaultL5ScoreSivaItem,defaultL5SivaItem.getPurchasedTypeScore());

    PickupPoint defaultOfflineL5SivaItem = getFromMongo(Collections.PICKUP_POINT,defaultOfflineL5IdSivaItem,PickupPoint.class);
    assertNotNull(defaultOfflineL5SivaItem);
    assertEquals(defaultOfflineL5ScoreSivaItem,defaultOfflineL5SivaItem.getPurchasedTypeScore());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,itemSku,SivaItem.class);
    assertNotNull(sivaItemMongoResult);

    List<PickupPoint> allFromMongo = getAllFromMongo(Collections.PICKUP_POINT, PickupPoint.class);
    List<PickupPoint> pickupPoints =
      allFromMongo.stream().filter(p -> p.getItemSku().equals(itemSku)).toList();
    assertEquals(defaultL5SivaItem.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(defaultL5SivaItem.getPickupPointCode(),sivaItemMongoResult.getPrice().getCheapestPickupPointCode());
    assertEquals(defaultL5SivaItem.getPurchasedType(),sivaItemMongoResult.getPrice().getPurchasedType());
    assertEquals(defaultL5ScoreSivaItem,sivaItemMongoResult.getPrice().getPurchasedTypeScore());
    assertEquals(defaultL5SivaItem.getPurchasedType(),sivaItemMongoResult.getPurchasedType());
    assertEquals(defaultL5ScoreSivaItem,sivaItemMongoResult.getPurchasedTypeScore());

    assertEquals(defaultOfflineL5SivaItem.getItemSku(),sivaItemMongoResult.getOfflinePrice().getCheapestItemSku());
    assertEquals(defaultOfflineL5SivaItem.getPickupPointCode(),sivaItemMongoResult.getOfflinePrice().getCheapestPickupPointCode());
    assertEquals(defaultOfflineL5SivaItem.getPurchasedType(),sivaItemMongoResult.getOfflinePrice().getPurchasedType());
    assertEquals(defaultOfflineL5ScoreSivaItem,sivaItemMongoResult.getOfflinePrice().getPurchasedTypeScore());
    assertEquals(defaultOfflineL5SivaItem.getPurchasedType(),sivaItemMongoResult.getOfflinePurchasedType());
    assertEquals(defaultOfflineL5ScoreSivaItem,sivaItemMongoResult.getOfflinePurchasedTypeScore());

    assertEquals(defaultL5SivaItem.isBuyable(),sivaItemMongoResult.getBuyable().isValue());
    assertEquals(defaultL5SivaItem.isDiscoverable(),sivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(defaultL5SivaItem.isBuyableCnc(),sivaItemMongoResult.getBuyableCnc().isValue());
    assertEquals(defaultL5SivaItem.isDiscoverableCnc(),sivaItemMongoResult.getDiscoverableCnc().isValue());
    assertEquals(l4hasCncActivated,sivaItemMongoResult.isHasCncActivated());
    assertEquals(pickupPoints.stream().anyMatch(PickupPoint::isBuyableCnc),
      sivaItemMongoResult.isHasBuyableCnc());
    assertEquals(pickupPoints.stream().anyMatch(PickupPoint::isDiscoverableCnc), sivaItemMongoResult.isHasDiscoverableCnc());
    assertEquals(l4hasCncTag,sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    /*End of Siva Item Verification*/



    /*Siva Product Verification*/
    PickupPoint defaultL5SivaProduct = getFromMongo(Collections.PICKUP_POINT,defaultL5IdSivaProduct,PickupPoint.class);
    assertNotNull(defaultL5SivaProduct);
    assertEquals(defaultL5ScoreSivaProduct,defaultL5SivaProduct.getPurchasedTypeScore());

    PickupPoint defaultOfflineL5SivaProduct = getFromMongo(Collections.PICKUP_POINT,defaultOfflineL5IdSivaProduct,PickupPoint.class);
    assertNotNull(defaultOfflineL5SivaProduct);
    assertEquals(defaultOfflineL5ScoreSivaProduct,defaultOfflineL5SivaProduct.getPurchasedTypeScore());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,productSku,SivaProduct.class);
    assertNotNull(sivaProductMongoResult);

    assertEquals(defaultL5SivaProduct.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(defaultL5SivaProduct.getPickupPointCode(),sivaProductMongoResult.getPrice().getCheapestPickupPointCode());
    assertEquals(defaultL5SivaProduct.getPurchasedType(),sivaProductMongoResult.getPrice().getPurchasedType());
    assertEquals(defaultL5ScoreSivaProduct,sivaProductMongoResult.getPrice().getPurchasedTypeScore());
    assertEquals(defaultL5SivaProduct.getPurchasedType(),sivaProductMongoResult.getPurchasedType());
    assertEquals(defaultL5ScoreSivaProduct,sivaProductMongoResult.getPurchasedTypeScore());

    assertEquals(defaultOfflineL5SivaProduct.getItemSku(),sivaProductMongoResult.getOfflinePrice().getCheapestItemSku());
    assertEquals(defaultOfflineL5SivaProduct.getPickupPointCode(),sivaProductMongoResult.getOfflinePrice().getCheapestPickupPointCode());
    assertEquals(defaultOfflineL5SivaProduct.getPurchasedType(),sivaProductMongoResult.getOfflinePrice().getPurchasedType());
    assertEquals(defaultOfflineL5ScoreSivaProduct,sivaProductMongoResult.getOfflinePrice().getPurchasedTypeScore());
    assertEquals(defaultOfflineL5SivaProduct.getPurchasedType(),sivaProductMongoResult.getOfflinePurchasedType());
    assertEquals(defaultOfflineL5ScoreSivaProduct,sivaProductMongoResult.getOfflinePurchasedTypeScore());

    assertEquals(defaultL5SivaProduct.isBuyable(),sivaProductMongoResult.getBuyable().isValue());
    assertEquals(defaultL5SivaProduct.isDiscoverable(),sivaProductMongoResult.getDiscoverable().isValue());
    assertEquals(defaultL5SivaProduct.isBuyableCnc(),sivaProductMongoResult.getBuyableCnc().isValue());
    assertEquals(defaultL5SivaProduct.isDiscoverableCnc(),sivaProductMongoResult.getDiscoverableCnc().isValue());
    assertEquals(l3hasCncActivated,sivaProductMongoResult.isHasCncActivated());
    assertEquals(pickupPoints.stream()
      .filter(p -> p.getProductSku().equals(sivaProductMongoResult.getProductSku()))
      .anyMatch(PickupPoint::isBuyableCnc),sivaProductMongoResult.isHasBuyableCnc());
    assertEquals(pickupPoints.stream()
      .filter(p -> p.getProductSku().equals(sivaProductMongoResult.getProductSku()))
      .anyMatch(PickupPoint::isDiscoverableCnc), sivaProductMongoResult.isHasDiscoverableCnc());
    assertEquals(l3hasCncTag,sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    /*End of Siva Product Verification*/
  }

}
