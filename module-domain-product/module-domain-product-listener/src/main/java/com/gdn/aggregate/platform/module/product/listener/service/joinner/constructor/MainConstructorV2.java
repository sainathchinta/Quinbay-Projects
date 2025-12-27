package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.util.StockEventType;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointInventoryService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.model.other.CalculatedItem;
import com.gdn.aggregate.platform.module.product.listener.model.other.SivaProductMetaData;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Fbb;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Flashsale;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Measurement;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.model.util.CompleteItemData;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.service.helper.DataEncryptionService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomInventoryServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CampaignProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CheapestPriceDayServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.FlashSaleProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component
public class MainConstructorV2 {
  private static final String SOURCE_META_DATA = "PROMO";

  private static final double FORCE_REPUBLISH_PRICE = -100_000.0;

  @Autowired
  private DataEncryptionService dataEncryptionService;

  @Autowired
  private AdjustmentProductQuotaServiceV2 adjustmentProductQuotaService;

  @Autowired
  private CustomInventoryServiceV2 customInventoryService;

  @Autowired
  private PickupPointServiceV2 pickupPointService;

  @Autowired
  private FlashSaleProductServiceV2 flashProductSaleService;

  @Autowired
  private CampaignProductServiceV2 campaignProductService;

  @Autowired
  private CheapestPriceDayServiceV2 cheapestPriceDayService;

  @Autowired
  private AdjustmentProductServiceV2 adjustmentProductService;

  @Autowired
  private MasterDataItemServiceV2 masterDataItemService;

  @Autowired
  private MasterDataProductServiceV2 masterDataProductService;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private PickupPointInventoryService pickupPointInventory;

  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  @Value("${filter.distribution.pickup.points.enabled}")
  private boolean filterDistributionPickupPoints;


  public Map<String, CampaignInfo> toCampaignInfos(Product product, List<Item> items, Boolean fbbActivated,
      boolean sortByCheapest, CompleteItemData completeItemData) {
    Set<String> itemSkus = MainUtil.fromListToSet(ModuleProductUtil.toItemSkus(items));
    Set<String> campaignCodes = getCampaignCodesByMultipleL4(itemSkus, completeItemData.getAllAdjustmentProducts(), completeItemData.getAllFlashSaleProducts());

    List<PickupPoint> pickupPoints =
        toCampaignInfoPickupPointsItemLevel(itemSkus, campaignCodes, fbbActivated, sortByCheapest, completeItemData.getAllPickupPoints());
    List<CalculatedItem> calculatedItems = toCampaignInfoCalculatedItems(pickupPoints, completeItemData);

    return Optional.ofNullable(calculatedItems).orElseGet(ArrayList::new).stream()
        .filter(val -> ModuleProductUtil.campaignInfoKeyValid(val.getCampaignCode(), val.getSessionId())).collect(
            Collectors.groupingBy(
                calculatedItem -> ModuleProductUtil.toCampaignInfoKey(calculatedItem.getCampaignCode(),
                    calculatedItem.getSessionId()))).entrySet().stream()
        .map(entry -> toCampaignInfo(product, items, pickupPoints, entry, completeItemData)).collect(Collectors.toMap(
            campaignInfo -> ModuleProductUtil.toCampaignInfoKey(campaignInfo.getCampaignCode(),
                campaignInfo.getSessionId()), campaignInfo -> campaignInfo));
  }

  private List<PickupPoint> toCampaignInfoPickupPointsItemLevel(Set<String> itemSkus, Set<String> campaignCodes,
      Boolean fbbActivated, boolean sortByCheapest, List<PickupPoint> allPickupPoints) {
    // Filter out distribution pickup points if there are non-distribution alternatives
    List<PickupPoint> filteredPickupPoints =
      ModuleProductUtil.filterDistributionPickupPoints(allPickupPoints,
        filterDistributionPickupPoints);

    return Optional.ofNullable(itemSkus).orElseGet(HashSet::new).stream().map(
        itemSku -> toCampaignInfoPickupPointsCampaignLevel(itemSku, campaignCodes, fbbActivated,
          sortByCheapest, filteredPickupPoints)).filter(CollectionUtils::isNotEmpty)
      .flatMap(Collection::stream).collect(Collectors.toList());
  }

  private List<PickupPoint> toCampaignInfoPickupPointsCampaignLevel(String itemSku, Set<String> campaignCodes,
      Boolean fbbActivated, boolean sortByCheapest, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(campaignCodes).orElseGet(HashSet::new).stream().map(
            campaignCode -> pickupPointService.getMinimumPickupPoints(itemSku, campaignCode, fbbActivated, sortByCheapest, allPickupPoints))
        .flatMap(Collection::stream).filter(Objects::nonNull).collect(Collectors.toList());
  }

  private CampaignInfo toCampaignInfo(Product product, List<Item> items, List<PickupPoint> pickupPoints,
      Map.Entry<String, List<CalculatedItem>> entry, CompleteItemData completeItemData) {
    Stock cheapestStock;
    CalculatedItem cheapestCalculatedItem;
    Long currentTime = ModuleProductUtil.getCurrentTimestamp();
    Long startTime = ModuleProductUtil.toCampaignStart(entry.getValue());
    Long endTime = ModuleProductUtil.toCampaignEnd(entry.getValue());
    String campaignCode = ModuleProductUtil.toCampaignCampaignCode(entry.getValue());
    Integer sessionId = ModuleProductUtil.toCampaignSessionId(entry.getValue());
    String productSku = ModuleProductUtil.toProductSku(product);
    List<String> itemSkus = ModuleProductUtil.toCampaignItemSkus(entry.getValue());
    List<String> pickupPointCodes = ModuleProductUtil.toCampaignPickupPointCodes(entry.getValue());

    SivaProduct.FinalPrice tempPrice =
        ModuleProductUtil.toCampaignInfoFinalPrice(entry.getValue(), campaignCode, false);
    SivaProduct.FinalPrice tempOfflinePrice =
        ModuleProductUtil.toCampaignInfoFinalPrice(entry.getValue(), campaignCode, true);
    SivaProduct.FinalPrice price = MainUtil.getOrDefault(tempPrice, tempOfflinePrice);
    SivaProduct.FinalPrice offlinePrice = MainUtil.getOrDefault(tempOfflinePrice, tempPrice);

    /*Online*/
    String cheapestItemSku = ModuleProductUtil.getCheapestItemSku(price, itemSkus);
    Item cheapestItem = ModuleProductUtil.getItem(items, cheapestItemSku);
    String cheapestPickupPointCode = ModuleProductUtil.getCheapestPickupPointCode(price, pickupPointCodes);
    PickupPoint cheapestPickupPoint = ModuleProductUtil.getPickupPoint(pickupPoints,
        ModuleProductUtil.toPickupPointId(cheapestItemSku, cheapestPickupPointCode));
    CampaignProduct campaignProduct = campaignProductService.getExistingCampaignProduct(
        ModuleProductUtil.toCampaignProductId(campaignCode, cheapestItemSku, cheapestPickupPointCode),
        completeItemData.getAllCampaignProducts());
    List<CampaignProduct> campaignProducts =
        campaignProductService.getCampaignProductsByL5(cheapestItemSku, cheapestPickupPointCode, completeItemData.getAllCampaignProducts());
    SivaProduct.PriceTeaser priceTeaser = toPriceTeaser(campaignProducts, startTime, true);
    SivaProduct.PriceTeaser currentPriceTeaser = toPriceTeaser(campaignProducts, currentTime, false);
    if (skipStockFetchFromInventoryModule) {
      PickupPoint inventoryPickupPoint =
        findValidPickupPointForInventory(cheapestPickupPoint, completeItemData.getAllPickupPoints(),
          itemSkus, completeItemData);
      cheapestStock = toStock(inventoryPickupPoint, completeItemData);
      cheapestCalculatedItem = Optional.ofNullable(inventoryPickupPoint)
        .map(pp -> ModuleProductUtil.toCheapestCalculatedItem(entry.getValue(), pp))
        .orElse(null);
    } else {
      cheapestCalculatedItem = Optional.ofNullable(cheapestPickupPoint)
        .map(pp -> ModuleProductUtil.toCheapestCalculatedItem(entry.getValue(), pp))
        .orElse(null);
      cheapestStock = toStock(cheapestPickupPoint, completeItemData);
    }

    /*Offline*/
    String offlineCheapestItemSku = ModuleProductUtil.getCheapestItemSku(offlinePrice, itemSkus);
    Item offlineCheapestItem = ModuleProductUtil.getItem(items, offlineCheapestItemSku);
    String offlineCheapestPickupPointCode =
        ModuleProductUtil.getCheapestPickupPointCode(offlinePrice, pickupPointCodes);
    PickupPoint offlineCheapestPickupPoint = ModuleProductUtil.getPickupPoint(pickupPoints,
        ModuleProductUtil.toPickupPointId(offlineCheapestItemSku, offlineCheapestPickupPointCode));
    CalculatedItem offlineCheapestCalculatedItem = Optional.ofNullable(offlineCheapestPickupPoint)
        .map(pp -> ModuleProductUtil.toCheapestCalculatedItem(entry.getValue(), pp))
        .orElse(null);

    // Filter distribution pickup points for nearest pickup points calculation
    List<PickupPoint> pickupPointsForNearest =
      ModuleProductUtil.filterDistributionPickupPoints(completeItemData.getAllPickupPoints(),
        filterDistributionPickupPoints);
    
    CampaignInfo campaignInfo = CampaignInfo.builder().campaignCode(campaignCode)
        .campaignName(ModuleProductUtil.toCampaignName(campaignProduct))
        .tagLabel(ModuleProductUtil.toCampaignTagLabel(campaignProduct)).sessionId(sessionId)
        .buyable(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem, Channel.DEFAULT, true))
        .discoverable(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem, Channel.DEFAULT, false))
        .buyableCnc(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem, Channel.CNC, true))
        .discoverableCnc(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem, Channel.CNC, false))
        .purchasedType(ModuleProductUtil.toPurchasedType(cheapestCalculatedItem))
        .purchasedTypeScore(ModuleProductUtil.toPurchasedTypeScore(cheapestCalculatedItem))
        .offlinePurchasedType(ModuleProductUtil.toPurchasedType(offlineCheapestCalculatedItem))
        .offlinePurchasedTypeScore(ModuleProductUtil.toPurchasedTypeScore(offlineCheapestCalculatedItem)).price(price)
        .offlinePrice(offlinePrice).priceTeaser(priceTeaser).currentPriceTeaser(currentPriceTeaser).itemSkus(itemSkus)
        .productSku(productSku).cheapestItemSku(cheapestItemSku).offlineCheapestItemSku(offlineCheapestItemSku)
        .pickupPointCode(Optional.ofNullable(cheapestPickupPoint).map(ModuleProductUtil::getPickupPointCode).orElse(null))
        .externalPickupPointCode(Optional.ofNullable(cheapestPickupPoint).map(ModuleProductUtil::getExternalPickupPointCode).orElse(null))
        .offlinePickupPointCode(Optional.ofNullable(offlineCheapestPickupPoint).map(ModuleProductUtil::getPickupPointCode).orElse(null))
        .offlineExternalPickupPointCode(Optional.ofNullable(offlineCheapestPickupPoint).map(ModuleProductUtil::getExternalPickupPointCode).orElse(null))
        .nearestPickupPoints(
            Optional.ofNullable(cheapestPickupPoint).map(pp -> pickupPointService.getNearestPickupPoints(price, pp, pickupPointsForNearest)).orElse(null))
      // cheapest stock is evaluated only on Stock update event and stock republish event
        .inventory(cheapestStock).fbbActivated(Optional.ofNullable(cheapestPickupPoint).map(ModuleProductUtil::isPickupPointFbbActivated).orElse(false))
        .image(ModuleProductUtil.toCampaignImage(cheapestItem)).startTime(startTime).endTime(endTime)
        .activated(ModuleProductUtil.isCampaignActivated(entry.getValue())).build();

    Flashsale flashsale =
        toFlashsaleFromFlashsaleProduct(productSku, cheapestItemSku, campaignCode, startTime, endTime, false,
            currentTime, completeItemData.getAllFlashSaleProducts());
    Flashsale subFlashsale =
        toFlashsaleFromFlashsaleProduct(productSku, cheapestItemSku, campaignCode, startTime, endTime, true,
            currentTime, completeItemData.getAllFlashSaleProducts());
    if (Objects.isNull(flashsale) && Objects.isNull(subFlashsale)) {
      flashsale = toFlashsaleFromCampaignInfo(campaignInfo, false, currentTime);
      if (Objects.isNull(flashsale)) {
        subFlashsale = toFlashsaleFromCampaignInfo(campaignInfo, true, currentTime);
      }
    }
    campaignInfo.setFlashsale(flashsale);
    campaignInfo.setSubFlashsale(subFlashsale);
    Flashsale choosenFs = MainUtil.getOrDefault(flashsale, subFlashsale);
    String campaignCodeFs = ModuleProductUtil.toCampaignCampaignCode(campaignCode, choosenFs);

    campaignInfo.setFlashsaleItemSkus(
        ModuleProductUtil.toCampaignFlashsaleItemSkus(MainUtil.toList(cheapestItemSku), choosenFs));

    // Filter distribution pickup points for top performer calculation
    List<PickupPoint> filteredPickupPointsForTopPerformer =
      ModuleProductUtil.filterDistributionPickupPoints(completeItemData.getAllPickupPoints(),
        filterDistributionPickupPoints);
    List<PickupPoint> topPerformerPickupPoints =
      pickupPointService.getLeastPickupPointsByItemSkus(itemSkus, campaignCodeFs,
        filteredPickupPointsForTopPerformer);
    List<Quota> topPerformerAdjustments =
        adjustmentProductQuotaService.toQuotaAdjustments(topPerformerPickupPoints, campaignCodeFs,
            completeItemData.getAllAdjustmentProductQuotas());
    Quota topPerformerAdjustment = ModuleProductUtil.findMaxAdjustment(topPerformerAdjustments);
    Stock topPerformerStock =
      getTopPerformingStockAdjustment(completeItemData, topPerformerPickupPoints, topPerformerAdjustment);

    List<PickupPoint> cheapestPickupPoints = MainUtil.toList(cheapestPickupPoint);
    List<Quota> cheapestAdjustments =
        adjustmentProductQuotaService.toQuotaAdjustments(cheapestPickupPoints, campaignCodeFs,
            completeItemData.getAllAdjustmentProductQuotas());
    Quota cheapestAdjustment = ModuleProductUtil.findAdjustmentByL5(cheapestAdjustments, cheapestPickupPoint);

    campaignInfo.setFlashsaleInventory(
        ModuleProductUtil.toSafeFlashsaleInventory(choosenFs, topPerformerAdjustment, cheapestAdjustment,
            topPerformerStock, campaignInfo.getInventory()));

    campaignInfo.setAdjustments(
        ModuleProductUtil.toSafeAdjustments(campaignInfo.getFlashsaleInventory(), topPerformerAdjustments,
            cheapestPickupPoint, MainUtil.toList(cheapestAdjustment)));

    String metaData = toMetaData(campaignInfo);
    campaignInfo.setProductUrl(ModuleProductUtil.toProductUrl(product, campaignInfo, metaData));
    campaignInfo.setItemUrl(ModuleProductUtil.toItemUrl(product, cheapestItem, campaignInfo, metaData));

    return campaignInfo;
  }

  private Stock getTopPerformingStockAdjustment(CompleteItemData completeItemData,
    List<PickupPoint> topPerformerPickupPoints, Quota topPerformerAdjustment) {
    if (Objects.isNull(topPerformerAdjustment)) {
      return null;
    }
    Stock topPerformerStock;
    if (skipStockFetchFromInventoryModule) {
      PickupPoint topPerformerAdjustmentPickupPoint = Optional.ofNullable(topPerformerPickupPoints)
        .orElseGet(ArrayList::new).stream()
        .filter(pickupPoint -> Objects.nonNull(pickupPoint) && Objects.nonNull(pickupPoint.getItemSku()))
        .filter(pickupPoint -> pickupPoint.getItemSku().equals(topPerformerAdjustment.getItemSku()))
        .filter(pickupPoint -> Objects.nonNull(pickupPoint.getPickupPointCode()))
        .filter(pickupPoint -> pickupPoint.getPickupPointCode()
          .equals(topPerformerAdjustment.getPickupPointCode())).findFirst().orElse(null);
      topPerformerStock = toStockByQuotaAdjustment(topPerformerAdjustment, completeItemData,
        topPerformerAdjustmentPickupPoint);
    } else {
      topPerformerStock = toStockByQuotaAdjustment(topPerformerAdjustment, completeItemData, null);
    }
    return topPerformerStock;
  }

  public Stock toStock(PickupPoint pickupPoint, CompleteItemData completeItemData) {
    if (Objects.isNull(pickupPoint)) {
      return null;
    }
    String itemSku = ModuleProductUtil.getItemSku(pickupPoint);
    String pickupPointCode = ModuleProductUtil.getPickupPointCode(pickupPoint);
    return resolveStock(itemSku, pickupPointCode, completeItemData, pickupPoint);
  }

  public Stock toStockByQuotaAdjustment(Quota quota, CompleteItemData completeItemData,
    PickupPoint topPerformerAdjustmentPickupPoint) {
    if (Objects.isNull(quota)) {
      return null;
    }
    String itemSku = ModuleProductUtil.getItemSkuFromQuota(quota);
    String pickupPointCode = ModuleProductUtil.getPickupPointCodeFromQuota(quota);
    return resolveStock(itemSku, pickupPointCode, completeItemData, topPerformerAdjustmentPickupPoint);
  }

  private Stock resolveStock(String itemSku, String pickupPointCode,
    CompleteItemData completeItemData, PickupPoint pickupPoint) {
    if (skipStockFetchFromInventoryModule) {
      Stock stock = fetchStockFromNewUpdateEventFlow(itemSku, pickupPointCode, completeItemData);
      if (Optional.ofNullable(stock).map(Stock::isExists).isPresent()) {
        return stock;
      }
    }
    return customInventoryService.getStockByL5(itemSku, pickupPointCode,
      completeItemData.getAllCustomInventoryPickupPointInfos(),
      completeItemData.getAllCustomInventoryInfos(), pickupPoint);
  }

  private Stock fetchStockFromNewUpdateEventFlow(String itemSku, String pickupPointCode,
    CompleteItemData completeItemData) {
    PickupPointInventory existingPickupPointInventory =
      pickupPointInventory.getExistingPickupPointInventory(
        ModuleProductUtil.toPickupPointId(itemSku, pickupPointCode),
        completeItemData.getAllPickupPointInventory());
    
    // If we have existing inventory, use it directly for accurate stock calculation
    if (Objects.nonNull(existingPickupPointInventory)) {
      return ModuleProductUtil.getCheapestStock(itemSku, pickupPointCode, completeItemData,
        existingPickupPointInventory);
    }
    PickupPoint pickupPoint =
      Optional.ofNullable(completeItemData.getAllPickupPoints()).orElseGet(ArrayList::new).stream()
        .filter(pp -> Objects.nonNull(pp) && 
                      Objects.equals(itemSku, pp.getItemSku()) && 
                      Objects.equals(pickupPointCode, pp.getPickupPointCode()))
        .findFirst().orElse(null);
    
    return switch (StockEventType.from(completeItemData)) {
      case STOCK_UPDATE_SEARCH ->
        ModuleProductUtil.toUpdatedStockFromStockUpdateSearchEvent(itemSku, pickupPointCode,
          completeItemData.getStockUpdateSearchEvent(), pickupPoint);
      case STOCK_REPUBLISH ->
        ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(itemSku, pickupPointCode,
          completeItemData.getLevel2InventoryQuantityChangedEvent(), pickupPoint);
      case INVENTORY_MODULE_FETCH ->
        // when new Data is True fetch from Inventory Module
        customInventoryService.getStockByL5(itemSku, pickupPointCode,
          completeItemData.getAllCustomInventoryPickupPointInfos(),
          completeItemData.getAllCustomInventoryInfos(), pickupPoint);
      default -> {
        // Fallback when no inventory exists
        yield ModuleProductUtil.getCheapestStock(itemSku, pickupPointCode, completeItemData, null);
      }
    };
  }

  private String toMetaData(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo).map(CampaignInfo::getPrice).map(SivaProduct.FinalPrice::getOfferValue).map(
            offerValue -> SivaProductMetaData.builder().salePrice(offerValue)
                .isInStock(ModuleProductUtil.isCampaignInStock(campaignInfo)).source(SOURCE_META_DATA).build())
        .map(dataEncryptionService::encrypt).map(
            encryptedMetaData -> Optional.ofNullable(campaignInfo.getPickupPointCode())
                .map(val -> String.format("&metaData=%s", encryptedMetaData))
                .orElseGet(() -> String.format("?metaData=%s", encryptedMetaData)))
        .orElseGet(() -> toFallbackMetaData(campaignInfo));
  }

  private String toFallbackMetaData(CampaignInfo campaignInfo) {
    SivaProductMetaData metaData =
        SivaProductMetaData.builder().salePrice(FORCE_REPUBLISH_PRICE).isInStock(Boolean.FALSE).source(SOURCE_META_DATA)
            .build();

    return Optional.ofNullable(dataEncryptionService.encrypt(metaData)).map(
            encryptedMetaData -> Optional.ofNullable(campaignInfo.getPickupPointCode())
                .map(val -> String.format("&metaData=%s", encryptedMetaData))
                .orElseGet(() -> String.format("?metaData=%s", encryptedMetaData)))
        .orElseGet(() -> MainUtil.DEFAULT_STRING);
  }

  public Flashsale toFlashsaleFromFlashsaleProduct(String productSku, String itemSku, String campaignCode,
      Long startTime, Long endTime, boolean timeBased, Long currentTime, List<FlashsaleProduct> allFlashsaleProduct) {
    return Optional.ofNullable(
            flashProductSaleService.getNearestActiveFlashsaleProductByProductSkuAndTimeBased(productSku, itemSku,
                campaignCode, timeBased, startTime, endTime, currentTime, allFlashsaleProduct))
        .map(ModuleProductUtil::toFlashsaleFromFlashsaleProduct).orElse(null);
  }

  private Flashsale toFlashsaleFromCampaignInfo(CampaignInfo campaignInfo, boolean timeBased, Long currentTime) {
    return Optional.ofNullable(campaignInfo).filter(
            cpgInfo -> ModuleProductUtil.toCampaignInfoPriority(cpgInfo).intValue()
                == CampaignPriority.FLASH_SALE.intValue()).filter(cpgInfo -> Objects.nonNull(cpgInfo.getStartTime()))
        .filter(cpgInfo -> Objects.nonNull(cpgInfo.getEndTime()))
        .filter(cpgInfo -> ModuleProductUtil.isItNotEnded(cpgInfo.getEndTime(), currentTime))
        .map(cpgInfo -> ModuleProductUtil.toFlashsaleFromCampaignInfo(cpgInfo, timeBased)).orElse(null);
  }

  public SivaProduct.PriceTeaser toPriceTeaser(List<CampaignProduct> campaignProducts, Long campaignStartTime, boolean forNextPriceTeaser) {
    return Optional.ofNullable(campaignProducts)
        .map(vals -> campaignProductService.getUpcomingCampaignPriceSchedule(vals, ModuleProductUtil.toBaseCurrentDate(campaignStartTime,forNextPriceTeaser)))
        .map(val -> SivaProduct.PriceTeaser.builder()
            .start(val.getStart())
            .end(val.getEnd())
            .campaignPrice(val.getCampaignPrice())
            .campaignTag(val.getCampaignTag())
            .campaignCode(val.getCampaignCode())
            .build())
        .orElse(null);
  }

  private List<CalculatedItem> toCampaignInfoCalculatedItems(List<PickupPoint> pickupPoints, CompleteItemData completeItemData) {
    return Optional.ofNullable(pickupPoints).map(
        val -> calculateItems(val, completeItemData)).orElseGet(ArrayList::new).stream()
      .filter(Objects::nonNull).collect(Collectors.toList());
  }

  public List<CalculatedItem> calculateItems(List<PickupPoint> pickupPoints, CompleteItemData completeItemData) {
    return Optional.ofNullable(
      doCalculateItems(pickupPoints, false, completeItemData)).filter(CollectionUtils::isNotEmpty).orElseGet(
      () -> doCalculateItems(pickupPoints, true, completeItemData));
  }

  private List<CalculatedItem> doCalculateItems(List<PickupPoint> pickupPoints, boolean ignoreFilter, CompleteItemData completeItemData) {
    return Optional.ofNullable(pickupPoints)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> doCalculateItem(val, ignoreFilter, completeItemData))
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream)
        .collect(Collectors.toList());
  }

  private List<CalculatedItem> doCalculateItem(PickupPoint pickupPoint, boolean ignoreFilter, CompleteItemData completeItemData) {
    return Optional.ofNullable(pickupPoint)
      .filter(val -> ModuleProductUtil.canCalculateItem(val, ignoreFilter))
      .map(val -> toCalculatedItems(val, completeItemData))
      .orElseGet(ArrayList::new);
  }

  public List<CalculatedItem> toCalculatedItems(PickupPoint pickupPoint, CompleteItemData completeItemData) {
    List<AdjustmentProduct> adjustmentProducts = getNotEndedAdjustmentProductsByL5(pickupPoint, completeItemData.getAllAdjustmentProducts(), completeItemData.getAllFlashSaleProducts());
    List<AdjustmentProduct> campaignAdjustments = ModuleProductUtil.toCampaignAdjustments(adjustmentProducts);
    List<AdjustmentProduct> nonCampaignAdjustments = ModuleProductUtil.toNonCampaignAdjustments(adjustmentProducts);

    List<CalculatedItem> calculatedItems = campaignAdjustments
        .stream()
        .filter(Objects::nonNull)
        .map(campaignAdjustment -> ModuleProductUtil.combineCampaignAndNonCampaignAdjustment(
          campaignAdjustment, nonCampaignAdjustments))
        .map(val -> toCalculatedItem(val, pickupPoint, false, completeItemData))
        .sorted(Comparator.comparing(CalculatedItem::isCurrentlyActive, Comparator.reverseOrder()))
        .collect(Collectors.toList());
    
    Optional.ofNullable(nonCampaignAdjustments)
        .orElseGet(ArrayList::new)
        .stream()
        .map(val -> toCalculatedItem(val, pickupPoint, false, completeItemData))
        .forEach(calculatedItems::add);
    
    calculatedItems.add(toCalculatedItem(null, pickupPoint, true, completeItemData));
    return calculatedItems;
  }

  private List<AdjustmentProduct> getNotEndedAdjustmentProductsByL5(PickupPoint pickupPoint, List<AdjustmentProduct> allAdjustmentProducts, List<FlashsaleProduct> allFlashSaleProducts) {
    if (Objects.isNull(pickupPoint) || Objects.isNull(pickupPoint.getItemSku()) || Objects.isNull(pickupPoint.getPickupPointCode())) {
      return new ArrayList<>();
    }
    List<AdjustmentProduct> xpromoAdjustmentProducts = adjustmentProductService.getNotEndedAdjustmentProductsByL5(ModuleProductUtil.getItemSku(pickupPoint),ModuleProductUtil.getPickupPointCode(pickupPoint), allAdjustmentProducts);
    List<AdjustmentProduct> denpasarAdjustmentProducts = flashProductSaleService.getNotEndedAdjustmentProductsByL5(ModuleProductUtil.getItemSku(pickupPoint),ModuleProductUtil.getPickupPointCode(pickupPoint), allFlashSaleProducts).stream()
        .filter(Objects::nonNull)
        .filter(val -> !ModuleProductUtil.isAdjustmentExists(xpromoAdjustmentProducts,ModuleProductUtil.toCampaignProductId(val.getCampaignCode(),val.getItemSku(),val.getPickupPointCode())))
        .collect(Collectors.toList());
    return MainUtil.combineList(xpromoAdjustmentProducts,denpasarAdjustmentProducts);
  }

  private CalculatedItem toCalculatedItem(AdjustmentProduct adjustmentProduct, PickupPoint pickupPoint, boolean defaultCalculatedItem, CompleteItemData completeItemData) {
    CalculatedItem calculatedItem = toBaseCalculatedItem(pickupPoint, completeItemData);

    if (defaultCalculatedItem) {
      calculatedItem.setCampaignCode(Default.CAMPAIGN_CODE);
    }
    if (Objects.nonNull(adjustmentProduct)) {
      Double finalOffered = ModuleProductUtil.getFinalOffered(adjustmentProduct, ModuleProductUtil.toOffered(calculatedItem));
      Optional.ofNullable(calculatedItem)
          .map(CalculatedItem::getPrice)
          .ifPresent(val -> val.setFinalOffered(finalOffered));
      calculatedItem.setCheapestPriceDays(getCheapestPriceDays(adjustmentProduct.getCampaignCode(),
          adjustmentProduct.getItemSku(), adjustmentProduct.getPickupPointCode(), completeItemData.getAllCheapestPriceDays()));
      calculatedItem.setCurrentlyActive(ModuleProductUtil.isAdjustmentCurrentlyActive(adjustmentProduct));
      calculatedItem.setCampaignCode(adjustmentProduct.getCampaignCode());
      calculatedItem.setSessionId(adjustmentProduct.getSessionId());
      calculatedItem.setAdjustmentName(adjustmentProduct.getAdjustmentName());
      calculatedItem.setStart(adjustmentProduct.getStartDate());
      calculatedItem.setEnd(adjustmentProduct.getEndDate());
      calculatedItem.setValue(ModuleProductUtil.getAdjustmentValue(adjustmentProduct));
      calculatedItem.setActivated(adjustmentProduct.isActivated());
      calculatedItem.setFlashsaleStock(ModuleProductUtil.toFlashsaleStockMultiple(
          adjustmentProductQuotaService.toQuotaAdjustments(adjustmentProduct.getItemSku(),
              adjustmentProduct.getPickupPointCode(), ModuleProductUtil.toAdjCampaignCode(adjustmentProduct),
              completeItemData.getAllAdjustmentProductQuotas()),
          calculatedItem.getStock()));
      calculatedItem.setFlashsale(CampaignType.FLASH_SALE.equals(adjustmentProduct.getPromoType()));
      calculatedItem.setAvailable(ModuleProductUtil.isAvailable(calculatedItem));
      calculatedItem.setPriority(adjustmentProduct.getPriority());
    }

    return calculatedItem;
  }

  private Integer getCheapestPriceDays(String campaignCode, String itemSku, String pickupPointCode, List<CheapestPriceDay> allCheapestPriceDays) {
    return Optional.ofNullable(ModuleProductUtil.toCheapestPriceDayId(campaignCode,itemSku,pickupPointCode))
        .map(id -> cheapestPriceDayService.getExistingCheapestPriceDay(id, allCheapestPriceDays))
        .map(CheapestPriceDay::getDays)
        .orElse(null);
  }

  private CalculatedItem toBaseCalculatedItem(PickupPoint pickupPoint, CompleteItemData completeItemData) {
    return CalculatedItem.builder()
        .itemSku(pickupPoint.getItemSku())
        .pickupPointCode(pickupPoint.getPickupPointCode())
        .externalPickupPointCode(pickupPoint.getExternalPickupPointCode())
        .price(ModuleProductUtil.toPriceDetail(pickupPoint))
        .stock(toStock(pickupPoint, completeItemData))
        .start(0L)
        .end(Long.MAX_VALUE)
        .activated(true)
        .purchasedType(pickupPoint.getPurchasedType())
        .purchasedTypeScore(pickupPoint.getPurchasedTypeScore())
        .cncActivated(pickupPoint.isCncActive())
        .fbbActivated(pickupPoint.isFbbActivated())
        .markForDelete(pickupPoint.isMarkForDelete())
        .build();
  }

  private Set<String> getCampaignCodesByMultipleL4(Set<String> itemSkus,  List<AdjustmentProduct> allAdjustmentProducts, List<FlashsaleProduct> allFlashSaleProducts) {
    Set<String> xpromoCampaignCodes = MainUtil.toCleanSet(adjustmentProductService.getNotEndedCampaignCodesByMultipleL4(itemSkus, allAdjustmentProducts));
    Set<String> denpasarCampaignCodes = MainUtil.toCleanSet(flashProductSaleService.getNotEndedCampaignCodesByMultipleL4(itemSkus, allFlashSaleProducts));
    Set<String> campaignCodes = MainUtil.combineSet(xpromoCampaignCodes,denpasarCampaignCodes);
    campaignCodes.add(null);
    return campaignCodes;
  }

  public Fbb toFbb(Product product, List<Item> items, Fbb currentFbb, boolean sortByCheapest, CompleteItemData completeItemData) {
    Fbb fbbOrigin = calculateFbb(product,items,true,sortByCheapest, completeItemData);
    Fbb fbbAlternative = calculateFbb(product,items,false,sortByCheapest, completeItemData);
    return ModuleProductUtil.chooseFbb(currentFbb,fbbOrigin,fbbAlternative);
  }

  public Fbb toNonFbb(Product product, List<Item> items, boolean sortByCheapest, CompleteItemData completeItemData) {
    return calculateFbb(product,items,false,sortByCheapest, completeItemData);
  }

  private Fbb calculateFbb(Product product, List<Item> items, Boolean fbbActivated, boolean sortByCheapest, CompleteItemData completeItemData) {
    return Optional.ofNullable(items).map(vals -> toCampaignInfos(product, vals, fbbActivated, sortByCheapest,
            completeItemData))
        .map(cpgInfos -> cpgInfos.get(ModuleProductUtil.toDefaultCampaignInfoKey(cpgInfos, pickupPointProperties.isSortByInStock())))
        .map(cpgInfo -> ModuleProductUtil.toFbb(cpgInfo, fbbActivated)).orElse(null);
  }

  public Measurement toMeasurement(Item item, Product product, CompleteItemData completeItemData) {
    MasterDataItem masterDataItem = Optional.ofNullable(ModuleProductUtil.getMasterDataItemFromItem(item))
        .filter(ModuleProductUtil::isMasterDataItemHasMeasurement).orElseGet(
            () -> masterDataItemService.getExistingMasterDataItem(ModuleProductUtil.toItemCode(item),
                completeItemData.getAllMasterDataItem()));
    MasterDataProduct masterDataProduct =
        Optional.ofNullable(ModuleProductUtil.getMasterDataProductFromProduct(product))
            .filter(ModuleProductUtil::isMasterDataProductHasMeasurement).orElseGet(
                () -> masterDataProductService.getExistingMasterDataProduct(ModuleProductUtil.getProductCodeFromItem(item),
                    completeItemData.getAllMasterDataProduct()));
    return ModuleProductUtil.toMeasurement(masterDataItem, masterDataProduct);
  }


  private PickupPoint findValidPickupPointForInventory(PickupPoint cheapestPickupPoint, 
      List<PickupPoint> allPickupPoints, List<String> itemSkus, CompleteItemData completeItemData) {

    // Filter out distribution pickup points if there are non-distribution alternatives
    List<PickupPoint> filteredPickupPoints =
      ModuleProductUtil.filterDistributionPickupPoints(allPickupPoints,
        filterDistributionPickupPoints);

    List<String> safeItemSkus = Optional.ofNullable(itemSkus).orElseGet(ArrayList::new);
    List<PickupPoint> relevantPickupPoints =
      Optional.ofNullable(filteredPickupPoints).orElseGet(ArrayList::new).stream()
        .filter(Objects::nonNull)
        .filter(pp -> Objects.nonNull(pp.getItemSku()))
        .filter(pp -> safeItemSkus.contains(pp.getItemSku())).toList();

    Map<String, PickupPointInventory> inventoryMap =
      Optional.ofNullable(completeItemData.getAllPickupPointInventory()).orElseGet(ArrayList::new)
        .stream().filter(Objects::nonNull)
        .filter(inv -> Objects.nonNull(inv.getId()))
        .collect(Collectors.toMap(PickupPointInventory::getId, inv -> inv, (a, b) -> a));

    List<PickupPoint> inStockPickupPoints = relevantPickupPoints.stream()
        .filter(pp -> Objects.nonNull(pp.getItemSku()) && Objects.nonNull(pp.getPickupPointCode()))
        .filter(pp -> {
          String inventoryId = ModuleProductUtil.toPickupPointId(pp.getItemSku(), pp.getPickupPointCode());
          PickupPointInventory inventory = inventoryMap.get(inventoryId);
          return Objects.nonNull(inventory) && inventory.isInStock();
        }).collect(Collectors.toList());
    
    // If we have in-stock pickup points, select the cheapest one
    if (CollectionUtils.isNotEmpty(inStockPickupPoints)) {
      return inStockPickupPoints.stream()
          .sorted(pickupPointService.getMinPriceComparatorFinalPriceAsc())
          .findFirst()
          .orElse(cheapestPickupPoint);
    }

    return Optional.ofNullable(cheapestPickupPoint).orElseGet(
      () -> relevantPickupPoints.stream().findFirst().orElse(null));
  }
}
