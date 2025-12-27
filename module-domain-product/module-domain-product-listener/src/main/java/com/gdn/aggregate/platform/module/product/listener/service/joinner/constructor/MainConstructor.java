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

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.*;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Fbb;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Measurement;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomInventoryServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MasterDataProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.model.other.SivaProductMetaData;
import com.gdn.aggregate.platform.module.product.listener.model.other.CalculatedItem;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Flashsale;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.service.helper.DataEncryptionService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CampaignProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CheapestPriceDayService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomInventoryService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.FlashsaleProductService;

@Component("ProductMainConstructor")
public class MainConstructor {

  private static final String SOURCE_META_DATA = "PROMO";

  private static final double FORCE_REPUBLISH_PRICE = -100_000.0;

  @Autowired
  private AdjustmentProductService adjustmentProductService;

  @Autowired
  private AdjustmentProductQuotaService adjustmentProductQuotaService;

  @Autowired
  private MasterDataItemService masterDataItemService;

  @Autowired
  private MasterDataProductService masterDataProductService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductService productService;

  @Autowired
  private FlashsaleProductService flashsaleProductService;

  @Autowired
  private CustomInventoryService customInventoryService;

  @Autowired
  private CampaignProductService campaignProductService;

  @Autowired
  private CheapestPriceDayService cheapestPriceDayService;

  @Autowired
  private DataEncryptionService dataEncryptionService;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private CustomInventoryServiceV2 customInventoryServiceV2;

  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  /*Processed Converter*/
  private String toMetaData(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPrice)
        .map(SivaProduct.FinalPrice::getOfferValue)
        .map(offerValue -> SivaProductMetaData.builder()
            .salePrice(offerValue)
            .isInStock(ModuleProductUtil.isCampaignInStock(campaignInfo))
            .source(SOURCE_META_DATA)
            .build())
        .map(dataEncryptionService::encrypt)
        .map(encryptedMetaData ->
            Optional.ofNullable(campaignInfo.getPickupPointCode())
                .map(val -> String.format("&metaData=%s", encryptedMetaData))
                .orElseGet(() -> String.format("?metaData=%s", encryptedMetaData)))
        .orElseGet(() -> toFallbackMetaData(campaignInfo));
  }

  private String toFallbackMetaData(CampaignInfo campaignInfo) {
    SivaProductMetaData metaData = SivaProductMetaData.builder()
        .salePrice(FORCE_REPUBLISH_PRICE)
        .isInStock(Boolean.FALSE)
        .source(SOURCE_META_DATA)
        .build();

    return Optional.ofNullable(dataEncryptionService.encrypt(metaData))
        .map(encryptedMetaData ->
            Optional.ofNullable(campaignInfo.getPickupPointCode())
                .map(val -> String.format("&metaData=%s", encryptedMetaData))
                .orElseGet(() -> String.format("?metaData=%s", encryptedMetaData)))
        .orElseGet(() -> MainUtil.DEFAULT_STRING);
  }
  /*End of Processed Converter*/

  /*Items Converter*/
  public List<CalculatedItem> calculateItems(List<PickupPoint> pickupPoints) {
    return Optional.ofNullable(doCalculateItems(pickupPoints,false))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(() -> doCalculateItems(pickupPoints,true));
  }

  private List<CalculatedItem> doCalculateItems(List<PickupPoint> pickupPoints, boolean ignoreFilter) {
    return Optional.ofNullable(pickupPoints)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> doCalculateItem(val,ignoreFilter))
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream)
        .collect(Collectors.toList());
  }

  private List<CalculatedItem> doCalculateItem(PickupPoint pickupPoint, boolean ignoreFilter) {
    return Optional.ofNullable(pickupPoint)
        .filter(val -> ModuleProductUtil.canCalculateItem(val,ignoreFilter))
        .map(this::toCalculatedItems)
        .orElseGet(ArrayList::new);
  }

  private List<AdjustmentProduct> getNotEndedAdjustmentProductsByL5(PickupPoint pickupPoint) {
    List<AdjustmentProduct> xpromoAdjustmentProducts = adjustmentProductService.getNotEndedAdjustmentProductsByL5(ModuleProductUtil.getItemSku(pickupPoint),ModuleProductUtil.getPickupPointCode(pickupPoint));
    List<AdjustmentProduct> denpasarAdjustmentProducts = flashsaleProductService.getNotEndedAdjustmentProductsByL5(ModuleProductUtil.getItemSku(pickupPoint),ModuleProductUtil.getPickupPointCode(pickupPoint)).stream()
        .filter(Objects::nonNull)
        .filter(val -> !ModuleProductUtil.isAdjustmentExists(xpromoAdjustmentProducts,ModuleProductUtil.toCampaignProductId(val.getCampaignCode(),val.getItemSku(),val.getPickupPointCode())))
        .collect(Collectors.toList());
    return MainUtil.combineList(xpromoAdjustmentProducts,denpasarAdjustmentProducts);
  }

  public List<CalculatedItem> toCalculatedItems(PickupPoint pickupPoint) {
    List<AdjustmentProduct> adjustmentProducts = getNotEndedAdjustmentProductsByL5(pickupPoint);
    List<AdjustmentProduct> campaignAdjustments = ModuleProductUtil.toCampaignAdjustments(adjustmentProducts);
    List<AdjustmentProduct> nonCampaignAdjustments = ModuleProductUtil.toNonCampaignAdjustments(adjustmentProducts);

    List<CalculatedItem> calculatedItems = campaignAdjustments
        .stream()
        .filter(Objects::nonNull)
        .map(campaignAdjustment -> ModuleProductUtil.combineCampaignAndNonCampaignAdjustment(campaignAdjustment,nonCampaignAdjustments))
        .map(val -> toCalculatedItem(val,pickupPoint,false))
        .sorted(Comparator.comparing(CalculatedItem::isCurrentlyActive,Comparator.reverseOrder()))
        .collect(Collectors.toList());
    Optional.ofNullable(nonCampaignAdjustments)
        .orElseGet(ArrayList::new)
        .stream()
        .map(val -> toCalculatedItem(val,pickupPoint,false))
        .forEach(calculatedItems::add);
    calculatedItems.add(toCalculatedItem(null,pickupPoint,true));
    return calculatedItems;
  }

  private CalculatedItem toBaseCalculatedItem(PickupPoint pickupPoint) {
    return CalculatedItem.builder()
        .itemSku(pickupPoint.getItemSku())
        .pickupPointCode(pickupPoint.getPickupPointCode())
        .externalPickupPointCode(pickupPoint.getExternalPickupPointCode())
        .price(ModuleProductUtil.toPriceDetail(pickupPoint))
        .stock(toStock(pickupPoint))
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

  private CalculatedItem toCalculatedItem(AdjustmentProduct adjustmentProduct, PickupPoint pickupPoint, boolean defaultCalculatedItem) {
    CalculatedItem calculatedItem = toBaseCalculatedItem(pickupPoint);

    if (defaultCalculatedItem) {
      calculatedItem.setCampaignCode(Default.CAMPAIGN_CODE);
    }
    if (Objects.nonNull(adjustmentProduct)) {
      Double finalOffered = ModuleProductUtil.getFinalOffered(adjustmentProduct, ModuleProductUtil.toOffered(calculatedItem));
      Optional.ofNullable(calculatedItem)
          .map(CalculatedItem::getPrice)
          .ifPresent(val -> val.setFinalOffered(finalOffered));
      calculatedItem.setCheapestPriceDays(getCheapestPriceDays(adjustmentProduct.getCampaignCode(),adjustmentProduct.getItemSku(),adjustmentProduct.getPickupPointCode()));
      calculatedItem.setCurrentlyActive(ModuleProductUtil.isAdjustmentCurrentlyActive(adjustmentProduct));
      calculatedItem.setCampaignCode(adjustmentProduct.getCampaignCode());
      calculatedItem.setSessionId(adjustmentProduct.getSessionId());
      calculatedItem.setAdjustmentName(adjustmentProduct.getAdjustmentName());
      calculatedItem.setStart(adjustmentProduct.getStartDate());
      calculatedItem.setEnd(adjustmentProduct.getEndDate());
      calculatedItem.setValue(ModuleProductUtil.getAdjustmentValue(adjustmentProduct));
      calculatedItem.setActivated(adjustmentProduct.isActivated());
      calculatedItem.setFlashsaleStock(ModuleProductUtil.toFlashsaleStockMultiple(adjustmentProductQuotaService.toQuotaAdjustments(adjustmentProduct.getItemSku(),adjustmentProduct.getPickupPointCode(),ModuleProductUtil.toAdjCampaignCode(adjustmentProduct)),calculatedItem.getStock()));
      calculatedItem.setFlashsale(CampaignType.FLASH_SALE.equals(adjustmentProduct.getPromoType()));
      calculatedItem.setAvailable(ModuleProductUtil.isAvailable(calculatedItem));
      calculatedItem.setPriority(adjustmentProduct.getPriority());
    }

    return calculatedItem;
  }

  private Integer getCheapestPriceDays(String campaignCode, String itemSku, String pickupPointCode) {
    return Optional.ofNullable(ModuleProductUtil.toCheapestPriceDayId(campaignCode,itemSku,pickupPointCode))
        .map(cheapestPriceDayService::getExistingCheapestPriceDay)
        .map(CheapestPriceDay::getDays)
        .orElse(null);
  }
  /*End of Items Converter*/

  /*FBB Converter*/
  public Fbb toFbb(Product product, List<Item> items, Fbb currentFbb, boolean sortByCheapest) {
    Fbb fbbOrigin = calculateFbb(product,items,true,sortByCheapest);
    Fbb fbbAlternative = calculateFbb(product,items,false,sortByCheapest);
    return ModuleProductUtil.chooseFbb(currentFbb,fbbOrigin,fbbAlternative);
  }

  public Fbb toNonFbb(Product product, List<Item> items, boolean sortByCheapest) {
    return calculateFbb(product,items,false,sortByCheapest);
  }

  private Fbb calculateFbb(Product product, List<Item> items, Boolean fbbActivated, boolean sortByCheapest) {
    return Optional.ofNullable(items)
        .map(vals -> toCampaignInfos(product,vals,fbbActivated,sortByCheapest))
        .map(cpgInfos -> cpgInfos.get(ModuleProductUtil.toDefaultCampaignInfoKey(cpgInfos, pickupPointProperties.isSortByInStock())))
        .map(cpgInfo -> ModuleProductUtil.toFbb(cpgInfo,fbbActivated))
        .orElse(null);
  }
  /*End of FBB Converter*/

  /*Price Converter*/
  private Set<String> getCampaignCodesByMultipleL4(Set<String> itemSkus) {
    Set<String> xpromoCampaignCodes = MainUtil.toCleanSet(adjustmentProductService.getNotEndedCampaignCodesByMultipleL4(itemSkus));
    Set<String> denpasarCampaignCodes = MainUtil.toCleanSet(flashsaleProductService.getNotEndedCampaignCodesByMultipleL4(itemSkus));
    Set<String> campaignCodes = MainUtil.combineSet(xpromoCampaignCodes,denpasarCampaignCodes);
    campaignCodes.add(null);
    return campaignCodes;
  }

  public Map<String,CampaignInfo> toCampaignInfos(Product product, List<Item> items, Boolean fbbActivated, boolean sortByCheapest) {
    Set<String> itemSkus = MainUtil.fromListToSet(ModuleProductUtil.toItemSkus(items));
    Set<String> campaignCodes = getCampaignCodesByMultipleL4(itemSkus);

    List<PickupPoint> pickupPoints = toCampaignInfoPickupPointsItemLevel(itemSkus, campaignCodes,fbbActivated,sortByCheapest);
    List<CalculatedItem> calculatedItems = toCampaignInfoCalculatedItems(pickupPoints);

    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(val -> ModuleProductUtil.campaignInfoKeyValid(val.getCampaignCode(),val.getSessionId()))
        .collect(Collectors.groupingBy(calculatedItem -> ModuleProductUtil.toCampaignInfoKey(calculatedItem.getCampaignCode(),calculatedItem.getSessionId())))
        .entrySet()
        .stream()
        .map(entry -> toCampaignInfo(product,items,pickupPoints,entry))
        .collect(Collectors.toMap(campaignInfo -> ModuleProductUtil.toCampaignInfoKey(campaignInfo.getCampaignCode(),campaignInfo.getSessionId()), campaignInfo -> campaignInfo));
  }

  private List<PickupPoint> toCampaignInfoPickupPointsItemLevel(Set<String> itemSkus, Set<String> campaignCodes, Boolean fbbActivated, boolean sortByCheapest) {
    return Optional.ofNullable(itemSkus)
        .orElseGet(HashSet::new)
        .stream()
        .map(itemSku -> toCampaignInfoPickupPointsCampaignLevel(itemSku,campaignCodes,fbbActivated,sortByCheapest))
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream)
        .collect(Collectors.toList());
  }

  private List<PickupPoint> toCampaignInfoPickupPointsCampaignLevel(String itemSku, Set<String> campaignCodes, Boolean fbbActivated, boolean sortByCheapest) {
    return Optional.ofNullable(campaignCodes)
        .orElseGet(HashSet::new)
        .stream()
        .map(campaignCode -> pickupPointService.getMinimumPickupPoints(itemSku,campaignCode,fbbActivated,sortByCheapest))
        .flatMap(Collection::stream)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private List<CalculatedItem> toCampaignInfoCalculatedItems(List<PickupPoint> pickupPoints) {
    return Optional.ofNullable(pickupPoints)
        .map(this::calculateItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private CampaignInfo toCampaignInfo(Product product, List<Item> items, List<PickupPoint> pickupPoints, Map.Entry<String,List<CalculatedItem>> entry) {
    Long currentTime = ModuleProductUtil.getCurrentTimestamp();
    Long startTime = ModuleProductUtil.toCampaignStart(entry.getValue());
    Long endTime = ModuleProductUtil.toCampaignEnd(entry.getValue());
    String campaignCode = ModuleProductUtil.toCampaignCampaignCode(entry.getValue());
    Integer sessionId = ModuleProductUtil.toCampaignSessionId(entry.getValue());
    String productSku = ModuleProductUtil.toProductSku(product);
    List<String> itemSkus = ModuleProductUtil.toCampaignItemSkus(entry.getValue());
    List<String> pickupPointCodes = ModuleProductUtil.toCampaignPickupPointCodes(entry.getValue());

    SivaProduct.FinalPrice tempPrice = ModuleProductUtil.toCampaignInfoFinalPrice(entry.getValue(),campaignCode,false);
    SivaProduct.FinalPrice tempOfflinePrice = ModuleProductUtil.toCampaignInfoFinalPrice(entry.getValue(),campaignCode,true);
    SivaProduct.FinalPrice price = MainUtil.getOrDefault(tempPrice,tempOfflinePrice);
    SivaProduct.FinalPrice offlinePrice = MainUtil.getOrDefault(tempOfflinePrice,tempPrice);

    /*Online*/
    String cheapestItemSku = ModuleProductUtil.getCheapestItemSku(price,itemSkus);
    Item cheapestItem = ModuleProductUtil.getItem(items,cheapestItemSku);
    String cheapestPickupPointCode = ModuleProductUtil.getCheapestPickupPointCode(price,pickupPointCodes);
    PickupPoint cheapestPickupPoint = ModuleProductUtil.getPickupPoint(pickupPoints, ModuleProductUtil.toPickupPointId(cheapestItemSku,cheapestPickupPointCode));
    CalculatedItem cheapestCalculatedItem = ModuleProductUtil.toCheapestCalculatedItem(entry.getValue(),cheapestPickupPoint);
    CampaignProduct campaignProduct = campaignProductService.getExistingCampaignProduct(ModuleProductUtil.toCampaignProductId(campaignCode,cheapestItemSku,cheapestPickupPointCode));
    List<CampaignProduct> campaignProducts = campaignProductService.getCampaignProductsByL5(cheapestItemSku,cheapestPickupPointCode);
    SivaProduct.PriceTeaser priceTeaser = toPriceTeaser(campaignProducts,startTime,true);
    SivaProduct.PriceTeaser currentPriceTeaser = toPriceTeaser(campaignProducts,currentTime,false);
    Stock cheapestStock = toStock(cheapestPickupPoint);

    /*Offline*/
    String offlineCheapestItemSku = ModuleProductUtil.getCheapestItemSku(offlinePrice,itemSkus);
    Item offlineCheapestItem = ModuleProductUtil.getItem(items,offlineCheapestItemSku);
    String offlineCheapestPickupPointCode = ModuleProductUtil.getCheapestPickupPointCode(offlinePrice,pickupPointCodes);
    PickupPoint offlineCheapestPickupPoint = ModuleProductUtil.getPickupPoint(pickupPoints, ModuleProductUtil.toPickupPointId(offlineCheapestItemSku,offlineCheapestPickupPointCode));
    CalculatedItem offlineCheapestCalculatedItem = ModuleProductUtil.toCheapestCalculatedItem(entry.getValue(),offlineCheapestPickupPoint);
    //CampaignProduct offlineCampaignProduct = campaignProductService.getExistingCampaignProduct(ModuleProductUtil.toCampaignProductId(campaignCode,offlineCheapestItemSku,offlineCheapestPickupPointCode));
    //List<CampaignProduct> offlineCampaignProducts = campaignProductService.getCampaignProductsByL5(offlineCheapestItemSku,offlineCheapestPickupPointCode);
    //SivaProduct.PriceTeaser offlinePriceTeaser = toPriceTeaser(offlineCampaignProducts,startTime,true);
    //SivaProduct.PriceTeaser offlineCurrentPriceTeaser = toPriceTeaser(offlineCampaignProducts,currentTime,false);
    //Stock offlineCheapestStock = toStock(offlineCheapestPickupPoint);

    CampaignInfo campaignInfo = CampaignInfo.builder()
        .campaignCode(campaignCode)
        .campaignName(ModuleProductUtil.toCampaignName(campaignProduct))
        .tagLabel(ModuleProductUtil.toCampaignTagLabel(campaignProduct))
        .sessionId(sessionId)
        .buyable(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem,Channel.DEFAULT,true))
        .discoverable(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem,Channel.DEFAULT,false))
        .buyableCnc(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem,Channel.CNC,true))
        .discoverableCnc(ModuleProductUtil.toViewSchedule(cheapestCalculatedItem,Channel.CNC,false))
        .purchasedType(ModuleProductUtil.toPurchasedType(cheapestCalculatedItem))
        .purchasedTypeScore(ModuleProductUtil.toPurchasedTypeScore(cheapestCalculatedItem))
        .offlinePurchasedType(ModuleProductUtil.toPurchasedType(offlineCheapestCalculatedItem))
        .offlinePurchasedTypeScore(ModuleProductUtil.toPurchasedTypeScore(offlineCheapestCalculatedItem))
        .price(price)
        .offlinePrice(offlinePrice)
        .priceTeaser(priceTeaser)
        .currentPriceTeaser(currentPriceTeaser)
        .itemSkus(itemSkus)
        .productSku(productSku)
        .cheapestItemSku(cheapestItemSku)
        .offlineCheapestItemSku(offlineCheapestItemSku)
        .pickupPointCode(ModuleProductUtil.getPickupPointCode(cheapestPickupPoint))
        .externalPickupPointCode(ModuleProductUtil.getExternalPickupPointCode(cheapestPickupPoint))
        .offlinePickupPointCode(ModuleProductUtil.getPickupPointCode(offlineCheapestPickupPoint))
        .offlineExternalPickupPointCode(ModuleProductUtil.getExternalPickupPointCode(offlineCheapestPickupPoint))
        .nearestPickupPoints(pickupPointService.getNearestPickupPoints(price,cheapestPickupPoint))
        .inventory(cheapestStock)
        .fbbActivated(ModuleProductUtil.isPickupPointFbbActivated(cheapestPickupPoint))
        .image(ModuleProductUtil.toCampaignImage(cheapestItem))
        .startTime(startTime)
        .endTime(endTime)
        .activated(ModuleProductUtil.isCampaignActivated(entry.getValue()))
        .build();

    Flashsale flashsale = toFlashsaleFromFlashsaleProduct(productSku,cheapestItemSku,campaignCode,startTime,endTime,false,currentTime);
    Flashsale subFlashsale = toFlashsaleFromFlashsaleProduct(productSku,cheapestItemSku,campaignCode,startTime,endTime,true,currentTime);
    if (Objects.isNull(flashsale) && Objects.isNull(subFlashsale)) {
      flashsale = toFlashsaleFromCampaignInfo(campaignInfo,false,currentTime);
      if (Objects.isNull(flashsale)) {
        subFlashsale = toFlashsaleFromCampaignInfo(campaignInfo,true,currentTime);
      }
    }
    campaignInfo.setFlashsale(flashsale);
    campaignInfo.setSubFlashsale(subFlashsale);
    Flashsale choosenFs = MainUtil.getOrDefault(flashsale,subFlashsale);
    String campaignCodeFs = ModuleProductUtil.toCampaignCampaignCode(campaignCode,choosenFs);

    campaignInfo.setFlashsaleItemSkus(ModuleProductUtil.toCampaignFlashsaleItemSkus(MainUtil.toList(cheapestItemSku),choosenFs));

    List<PickupPoint> topPerformerPickupPoints = pickupPointService.getLeastPickupPointsByItemSkus(itemSkus,campaignCodeFs);
    List<Quota> topPerformerAdjustments = adjustmentProductQuotaService.toQuotaAdjustments(topPerformerPickupPoints,campaignCodeFs);
    Quota topPerformerAdjustment = ModuleProductUtil.findMaxAdjustment(topPerformerAdjustments);
    Stock topPerformerStock = getTopPerformingStockFromAdjustment(topPerformerPickupPoints, topPerformerAdjustment);

    List<PickupPoint> cheapestPickupPoints = MainUtil.toList(cheapestPickupPoint);
    List<Quota> cheapestAdjustments = adjustmentProductQuotaService.toQuotaAdjustments(cheapestPickupPoints,campaignCodeFs);
    Quota cheapestAdjustment = ModuleProductUtil.findAdjustmentByL5(cheapestAdjustments,cheapestPickupPoint);

    campaignInfo.setFlashsaleInventory(ModuleProductUtil.toSafeFlashsaleInventory(
      choosenFs,
      topPerformerAdjustment,
      cheapestAdjustment,
      topPerformerStock,
      campaignInfo.getInventory()));

    campaignInfo.setAdjustments(ModuleProductUtil.toSafeAdjustments(
      campaignInfo.getFlashsaleInventory(),
      topPerformerAdjustments,
      cheapestPickupPoint,
      MainUtil.toList(cheapestAdjustment)
    ));

    String metaData = toMetaData(campaignInfo);
    campaignInfo.setProductUrl(ModuleProductUtil.toProductUrl(product,campaignInfo,metaData));
    campaignInfo.setItemUrl(ModuleProductUtil.toItemUrl(product,cheapestItem,campaignInfo,metaData));

    return campaignInfo;
  }

  private Stock getTopPerformingStockFromAdjustment(List<PickupPoint> topPerformerPickupPoints, Quota topPerformerAdjustment) {
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

      topPerformerStock =
        toStockByQuotaAdjustment(topPerformerAdjustment, topPerformerAdjustmentPickupPoint);
    } else {
      topPerformerStock = toStockByQuotaAdjustment(topPerformerAdjustment, null);
    }
    return topPerformerStock;
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
  /*End of Price Converter*/

  /*Flashsale Converter*/
  public Flashsale toFlashsaleFromFlashsaleProduct(String productSku, String itemSku, String campaignCode, Long startTime, Long endTime, boolean timeBased, Long currentTime) {
    return Optional.ofNullable(flashsaleProductService.getNearestActiveFlashsaleProductByProductSkuAndTimeBased(productSku,itemSku,campaignCode,timeBased,startTime,endTime,currentTime))
        .map(ModuleProductUtil::toFlashsaleFromFlashsaleProduct)
        .orElse(null);
  }

  private Flashsale toFlashsaleFromCampaignInfo(CampaignInfo campaignInfo, boolean timeBased, Long currentTime) {
    return Optional.ofNullable(campaignInfo)
        .filter(cpgInfo -> ModuleProductUtil.toCampaignInfoPriority(cpgInfo).intValue()==CampaignPriority.FLASH_SALE.intValue()).filter(cpgInfo -> Objects.nonNull(cpgInfo.getStartTime()))
        .filter(cpgInfo -> Objects.nonNull(cpgInfo.getEndTime()))
        .filter(cpgInfo -> ModuleProductUtil.isItNotEnded(cpgInfo.getEndTime(),currentTime))
        .map(cpgInfo -> ModuleProductUtil.toFlashsaleFromCampaignInfo(cpgInfo,timeBased))
        .orElse(null);
  }

  public Stock toStock(PickupPoint pickupPoint) {
    if (Objects.isNull(pickupPoint)) {
      return null;
    }
    return getStock(ModuleProductUtil.getItemSku(pickupPoint),
      ModuleProductUtil.getPickupPointCode(pickupPoint), pickupPoint);
  }

  public Stock toStockByQuotaAdjustment(Quota quota, PickupPoint topPerformerAdjustmentPickupPoint) {
    if (Objects.isNull(quota)) {
      return null;
    }
    return getStock(ModuleProductUtil.getItemSkuFromQuota(quota),
      ModuleProductUtil.getPickupPointCodeFromQuota(quota), topPerformerAdjustmentPickupPoint);
  }

  private Stock getStock(String itemSku, String pickupPointCode, PickupPoint pickupPoint) {
    if (skipStockFetchFromInventoryModule) {
      return customInventoryServiceV2.getStockByL5(itemSku, pickupPointCode, null, null,
        pickupPoint);
    }
    return customInventoryService.getStockByL5(itemSku, pickupPointCode, pickupPoint);
  }
  /*End of Flashsale Converter*/

  /*Measurement Converter*/
  public Measurement toMeasurement(Item item, Product product) {
    MasterDataItem masterDataItem = Optional.ofNullable(ModuleProductUtil.getMasterDataItemFromItem(item))
        .filter(ModuleProductUtil::isMasterDataItemHasMeasurement)
        .orElseGet(() -> masterDataItemService.getExistingMasterDataItem(ModuleProductUtil.toItemCode(item)));
    MasterDataProduct masterDataProduct = Optional.ofNullable(ModuleProductUtil.getMasterDataProductFromProduct(product))
        .filter(ModuleProductUtil::isMasterDataProductHasMeasurement)
        .orElseGet(() -> masterDataProductService.getExistingMasterDataProduct(ModuleProductUtil.getProductCodeFromItem(item)));
    return ModuleProductUtil.toMeasurement(masterDataItem,masterDataProduct);
  }
  /*End of Measurement Converter*/

}
