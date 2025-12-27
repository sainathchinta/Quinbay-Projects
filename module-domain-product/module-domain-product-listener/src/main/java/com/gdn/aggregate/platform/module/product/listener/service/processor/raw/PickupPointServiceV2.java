package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.cookie.Cookie;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.PickupPointLocation;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.PickupPointRepository;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomBusinessPartnerPickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomInventoryServiceV2;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import com.gdn.aggregate.platform.module.product.listener.util.PickupPointFilterUtil;
import com.gdn.aggregate.platform.module.product.listener.util.PickupPointSortUtil;
import reactor.core.publisher.Mono;

@Component("ProductPickupPointServiceV2")
public class PickupPointServiceV2 {
  private static final String SAVE_COMMAND = "savePickupPoint";

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Autowired
  private DBService dbService;

  @Autowired
  private MerchantDiscountPriceServiceV2 merchantDiscountPriceService;

  @Autowired
  private AdjustmentProductServiceV2 adjustmentProductService;

  @Autowired
  private AdjustmentProductQuotaServiceV2 adjustmentProductQuotaService;

  @Autowired
  private CustomInventoryServiceV2 customInventoryService;

  @Autowired
  private CustomBusinessPartnerPickupPointServiceV2 customBusinessPartnerPickupPointService;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private PickupPointInventoryService pickupPointInventoryService;

  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  public List<PickupPoint> findAllByProductSku(String productSku) {
    return pickupPointRepository.findAllByProductSku(productSku);
  }

  public Optional<PickupPoint> getExistingPickupPoint(String id) {
    return pickupPointRepository.findById(id);
  }

  public Mono<Boolean> save(PickupPoint pickupPoint, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.PICKUP_POINT)
            .domain(pickupPoint)
            .clazz(PickupPoint.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(pickupPoint,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(pickupPoint,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public List<Item> getItemsByProductSku(String productSku, List<Item> allItems,
    List<PickupPoint> allPickupPoints, StockUpdateSearchEvent stockUpdateSearchEvent,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent, int maxSize) {
    List<Item> result = new ArrayList<>();
    setStockInfoFromInventoryEvent(allPickupPoints, stockUpdateSearchEvent,
      level2InventoryQuantityChangedEvent);
    Map<String, Item> itemSkuToItemMap = toItemSkuAndItemMap(allItems);
    Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new).stream().filter(
            pickupPoint -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getProductSku()))
        .sorted(getPickupPointComparatorForMinItemSkuByProductSku())
        .findFirst()
        .map(PickupPoint::getItemSku)
        .map(itemSkuToItemMap::get)
        .filter(item -> Objects.isNull(ModuleProductUtil.getItem(result, ModuleProductUtil.toItemSku(item))))
        .ifPresent(result::add);

    Optional.ofNullable(allItems).orElseGet(ArrayList::new).stream()
        .filter(item -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(item.getProductSku()))
        .sorted(getItemComparatorForArchivedASCAndMarkForDeleteASC()).limit(maxSize)
        .filter(item -> Objects.isNull(ModuleProductUtil.getItem(result, ModuleProductUtil.toItemSku(item))))
        .forEach(result::add);

    Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new).stream().filter(
            pickupPoint -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getProductSku()))
        .sorted(getPickupPointComparatorForMaxItemSkuByProductSku())
        .findFirst().map(PickupPoint::getItemSku)
        .map(itemSkuToItemMap::get)
        .filter(item -> Objects.isNull(ModuleProductUtil.getItem(result, ModuleProductUtil.toItemSku(item))))
        .ifPresent(result::add);

    return result;
  }

  private void setStockInfoFromInventoryEvent(List<PickupPoint> allPickupPoints,
    StockUpdateSearchEvent stockUpdateSearchEvent,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    if (!pickupPointProperties.isUpdateStockFromInventoryEvent()) {
      return;
    }

    Map<String, PickupPoint> pickupPointMap = allPickupPoints.stream().collect(
      Collectors.toMap(ModuleProductUtil::getPickupPointId, Function.identity(),
        (pp1, pp2) -> pp1));

    revaluateStockStatus(stockUpdateSearchEvent, pickupPointMap);
    revaluateStockStatus(level2InventoryQuantityChangedEvent, pickupPointMap);
  }

  private void revaluateStockStatus(Object event, Map<String, PickupPoint> pickupPointMap) {
    // re-evaluate stock status based on event type
    if (event instanceof StockUpdateSearchEvent stockEvent) {
      String pickupPointId = ModuleProductUtil.toPickupPointId(stockEvent.getWebItemSku(),
        stockEvent.getPickupPointCode());
      Optional.ofNullable(pickupPointMap.get(pickupPointId))
        .ifPresent(pp -> pp.setInStock(!stockEvent.isOutOfStock()));

    } else if (event instanceof Level2InventoryQuantityChangedEvent level2Event) {
      String pickupPointId = ModuleProductUtil.toPickupPointId(level2Event.getLevel2Id(),
        level2Event.getPickupPointCode());
      Optional.ofNullable(pickupPointMap.get(pickupPointId)).ifPresent(
        pp -> ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pp, level2Event));
    }
  }



  public List<PickupPointLocation> getNearestPickupPoints(SivaProduct.FinalPrice price, PickupPoint cheapestPickupPoint, List<PickupPoint> allPickupPoints) {
    int size = pickupPointProperties.getNearestSize();
    int limit = size + 1;

    //findAllByItemSkuAndPrice_CampaignCodeAndPrice_FinalOfferPriceAndWarehouseAndPickupPointCodeNotAndMarkForDeleteFalseAndInStockTrueAndPurchasedTypeNot
    return Optional.of(Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(price).map(SivaProduct.FinalPrice::getCheapestItemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
        .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint, ModuleProductUtil.getRealCampaignCodeFinalPrice(price)))
        .filter(pickupPoint -> PickupPointFilterUtil.isFinalOfferPricePresent(pickupPoint, MainUtil.toNotNullDouble(price.getOfferValue())))
        .filter(pickupPoint -> ModuleProductUtil.getPickupPointWarehouseFlag(cheapestPickupPoint) == pickupPoint.isWarehouse())
        .filter(pickupPoint -> !ModuleProductUtil.getPickupPointCode(cheapestPickupPoint).equals(pickupPoint.getPickupPointCode()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .filter(PickupPoint::isInStock)
        .filter(pickupPoint -> !PurchasedType.NOT_AVAILABLE.equals(pickupPoint.getPurchasedType()))
        .limit(limit)
        .collect(Collectors.toList()))
        .map(nearestPickupPoints -> MainUtil.combineList(MainUtil.toList(cheapestPickupPoint),nearestPickupPoints))
        .filter(nearestPickupPoints -> nearestPickupPoints.size()<=size)
        .map(nearestPickupPoints -> nearestPickupPoints.stream()
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::toPickupPointLocation)
            .filter(Objects::nonNull)
            .sorted(Comparator.comparing(ppl -> MainUtil.toNotNullString(ppl.getCityName())))
            .collect(Collectors.toList()))
        .filter(CollectionUtils::isNotEmpty)
        .orElse(null);
  }

  public List<PickupPoint> getLeastPickupPointsByItemSkus(List<String> itemSkus, String campaignCode, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(itemSkus)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(itemSku -> getLeastPickupPointByItemSku(itemSku, campaignCode, allPickupPoints))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public PickupPoint getLeastPickupPointByItemSku(String itemSku, String campaignCode, List<PickupPoint> allPickupPoints) {
    if (!pickupPointProperties.isOnePartyActivated()) {
      return getLeastPickupPointByItemSku1PDisabled(itemSku, campaignCode, allPickupPoints);
    } else {
      return getLeastPickupPointByItemSku1PEnabled(itemSku,campaignCode, allPickupPoints);
    }
  }

  public List<PickupPoint> getMinimumPickupPoints(String itemSku, String campaignCode, Boolean fbbActivated, boolean sortByCheapest, List<PickupPoint> allPickupPoints) {
    PickupPoint onlineAvailability = getMinimumPickupPoint(itemSku,campaignCode,MainUtil.toSet(PurchasedType.ONLINE_CNC,PurchasedType.ONLINE),fbbActivated,sortByCheapest,allPickupPoints);
    PickupPoint offlineAvailability = getMinimumPickupPoint(itemSku,campaignCode,MainUtil.toSet(PurchasedType.CNC,PurchasedType.NOT_AVAILABLE),fbbActivated,sortByCheapest,allPickupPoints);

    return MainUtil.toList(onlineAvailability,offlineAvailability);
  }

  public boolean hasCncActivatedByItemSku(String itemSku, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .anyMatch(PickupPoint::isCncActive);
  }

  public boolean hasBuyableCncByItemSku(String itemSku, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .anyMatch(PickupPoint::isBuyableCnc);
  }

  public boolean hasDiscoverableCncByItemSku(String itemSku, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .anyMatch(PickupPoint::isDiscoverableCnc);
  }

  public boolean hasCncActivatedByProductSku(String productSku, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getProductSku()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .anyMatch(PickupPoint::isCncActive);
  }

  public boolean hasBuyableCncByProductSku(String productSku, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getProductSku()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .anyMatch(PickupPoint::isBuyableCnc);
  }

  public boolean hasDiscoverableCncByProductSku(String productSku, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new)
        .stream()
        .filter(pickupPoint -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getProductSku()))
        .filter(Predicate.not(PickupPoint::isMarkForDelete))
        .anyMatch(PickupPoint::isDiscoverableCnc);
  }

  private PickupPoint getMinimumPickupPoint(String itemSku, String campaignCode, Set<String> purchasedTypes, Boolean fbbActivated, boolean sortByCheapest, List<PickupPoint> allPickupPoints) {
    if (!pickupPointProperties.isOnePartyActivated()) {
      return getMinimumPickupPoint1PDisabled(itemSku,campaignCode,purchasedTypes,fbbActivated,sortByCheapest,allPickupPoints);
    } else {
      return getMinimumPickupPoint1PEnabled(itemSku,campaignCode,purchasedTypes,fbbActivated,sortByCheapest, allPickupPoints);
    }
  }

  public void setMandatory(PickupPoint pickupPoint,
      List<CustomInventoryPickupPointInfo> allCustomInventoryPickupPointInfos,
      List<CustomInventoryInfo> allCustomInventoryInfo,
      List<CustomBusinessPartnerPickupPoint> allCustomBusinessPartnerPickupPoint,
      List<MerchantDiscountPrice> allMerchantDiscountPrice, List<AdjustmentProduct> allAdjustmentProduct,
      List<AdjustmentProductQuota> allAdjustmentProductQuotas,
      List<PickupPointInventory> allPickupPointInventory) {
    Optional.ofNullable(pickupPoint).ifPresent(val -> {
      val.setId(val.toId());
      toUpdatedStock(pickupPoint, allCustomInventoryPickupPointInfos, allCustomInventoryInfo, allPickupPointInventory);
      toUpdatedBusinessPartner(val, allCustomBusinessPartnerPickupPoint);
      val.setPrice(getPrice(val, allMerchantDiscountPrice, allAdjustmentProduct));
      toUpdatedQuota(val, allAdjustmentProductQuotas);
      ModuleProductUtil.toUpdatedFlags(val, pickupPointProperties.isOnePartyActivated());
    });
  }

  private void toUpdatedStock(PickupPoint pickupPoint,
    List<CustomInventoryPickupPointInfo> allCustomInventoryPickupPointInfos,
    List<CustomInventoryInfo> allCustomInventoryInfo,
    List<PickupPointInventory> allPickupPointInventory) {
    boolean skipFetchFromInventoryModule =
      skipStockFetchFromInventoryModule && CollectionUtils.isEmpty(
        allCustomInventoryPickupPointInfos);
    if (skipFetchFromInventoryModule) {
      Optional.ofNullable(pickupPoint).map(
        val -> pickupPointInventoryService.getExistingPickupPointInventory(
          ModuleProductUtil.toPickupPointId(val.getItemSku(), val.getPickupPointCode()),
          allPickupPointInventory)).ifPresent(pickupPointInventory -> {
        pickupPoint.setInStock(pickupPointInventory.isInStock());
        pickupPoint.setWarehouse(pickupPointInventory.getSyncStock());
      });
    } else {
      Optional.ofNullable(pickupPoint).map(
        val -> customInventoryService.getStockByL5(val.getItemSku(), val.getPickupPointCode(),
          allCustomInventoryPickupPointInfos, allCustomInventoryInfo, pickupPoint)).ifPresent(stock -> {
        pickupPoint.setWarehouse(stock.isWarehouse());
        pickupPoint.setInStock(ModuleProductUtil.isPickupPointInStock(stock));
      });
    }
  }

  private Set<PickupPoint.Price> getPrice(PickupPoint pickupPoint, List<MerchantDiscountPrice> allMerchantDiscountPrice, List<AdjustmentProduct> allAdjustmentProduct) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .peek(price -> {
          if (pickupPointProperties.isAlwaysUpdateMerchantDiscount()) {
            updateMerchantPromoDiscountAndOfferPRiceFromEvent(pickupPoint, allMerchantDiscountPrice,
              price);
          }
          else {
            // OLD FLOW: only update if merchant discount exists
            Optional.ofNullable(pickupPoint).map(PickupPoint::toId).map(
                id -> merchantDiscountPriceService.getDiscountPrice(id, allMerchantDiscountPrice))
              .ifPresent(merchantPromoDiscountPrice -> {
                price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
                price.setOfferPrice(ModuleProductUtil.getOfferPrice(price));
              });
          }
          
          price.setFinalOfferPrice(price.getOfferPrice());
          price.setAdjustment(0D);
          price.setCampaignCode(null);
          price.setPromoCampaign(false);
          price.setPriority(null);
          Optional.ofNullable(pickupPoint)
              .map(val -> adjustmentProductService.getActiveAdjustmentProductByL5(val.getItemSku(), val.getPickupPointCode(), allAdjustmentProduct))
              .ifPresent(adjustmentProduct -> {
                price.setFinalOfferPrice(price.getOfferPrice() - adjustmentProduct.getValue());
                price.setAdjustment(adjustmentProduct.getValue());
                price.setCampaignCode(ModuleProductUtil.toRealCampaignCode(adjustmentProduct.getCampaignCode()));
                price.setPromoCampaign(adjustmentProduct.isPromoCampaign());
                price.setPriority(adjustmentProduct.getPriority());
              });
        })
        .collect(Collectors.toSet());
  }

  private void updateMerchantPromoDiscountAndOfferPRiceFromEvent(PickupPoint pickupPoint,
    List<MerchantDiscountPrice> allMerchantDiscountPrice, PickupPoint.Price price) {
    // NEW FLOW: Always update with fresh data from MerchantDiscountPrice event
    String pickupPointId = Optional.ofNullable(pickupPoint).map(PickupPoint::toId).orElse(null);
    PickupPoint.Price freshMerchantPrice = Optional.ofNullable(pickupPointId)
      .map(id -> merchantDiscountPriceService.getFullPrice(id, allMerchantDiscountPrice))
      .orElse(null);

    PickupPoint.DiscountPrice merchantPromoDiscountPrice =
      Optional.ofNullable(freshMerchantPrice).map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .orElse(null);
    price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);

    if (Objects.nonNull(freshMerchantPrice) && Objects.isNull(merchantPromoDiscountPrice)) {
      // promo is expired so use offer Price without merchant promo
      price.setOfferPrice(freshMerchantPrice.getOfferPrice());
    } else {
      price.setOfferPrice(ModuleProductUtil.getOfferPrice(price));
    }
  }

  private void toUpdatedBusinessPartner(PickupPoint pickupPoint, List<CustomBusinessPartnerPickupPoint> allCustomBusinessPartnerPickupPoint) {
    Optional.ofNullable(pickupPoint)
        .map(val -> customBusinessPartnerPickupPointService.getExistingCustomBusinessPartnerPickupPoint(val.getPickupPointCode(), allCustomBusinessPartnerPickupPoint))
        .ifPresent(val -> {
          pickupPoint.setFbbActivated(MainUtil.toNotNullBoolean(val.getFbbActivated()));
          pickupPoint.setLongitude(ModuleProductUtil.getLongitude(val.getGeolocation()));
          pickupPoint.setLatitude(ModuleProductUtil.getLatitude(val.getGeolocation()));
          pickupPoint.setCityName(val.getCityName());
        });
  }

  private void toUpdatedQuota(PickupPoint pickupPoint, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    Optional.ofNullable(pickupPoint)
        .map(val -> adjustmentProductQuotaService.toQuotaAdjustments(val.getItemSku(),val.getPickupPointCode(),ModuleProductUtil.getPickupPointCampaignCode(val), allAdjustmentProductQuotas))
        .map(ModuleProductUtil::findMaxAdjustment)
        .ifPresent(quota -> {
          pickupPoint.setQuotaPercentage(ModuleProductUtil.toPercentage(quota.getQuota(),quota.getRemaining()));
          pickupPoint.setInQuota(ModuleProductUtil.isPickupPointInQuota(quota));
        });

  }

  private PickupPoint getMinimumPickupPoint1PDisabled(String itemSku, String campaignCode, Set<String> purchasedTypes, Boolean fbbActivated, boolean sortByCheapest, List<PickupPoint> allPickupPoint) {
    if (Objects.isNull(fbbActivated)) {
      //findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc
      return Optional.ofNullable(allPickupPoint).orElseGet(ArrayList::new)
          .stream()
          .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
          .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint,  Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY)))
          .filter(pickupPoint -> Optional.ofNullable(purchasedTypes).orElseGet(HashSet::new).contains(pickupPoint.getPurchasedType()))
          .sorted(getMinPriceComparatorFinalPriceAsc())
          .findFirst()
          .orElse(null);

    } else {
      if (sortByCheapest) {
        //findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc
        return Optional.ofNullable(allPickupPoint).orElseGet(ArrayList::new)
            .stream()
            .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
            .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint,  Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY)))
            .filter(pickupPoint -> Objects.equals(fbbActivated, pickupPoint.isFbbActivated()))
            .sorted(getMinPriceComparatorFinalPriceAsc())
            .findFirst()
            .orElse(null);
      } else {
        //findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceDescWarehouseDescIdAsc
        return Optional.ofNullable(allPickupPoint).orElseGet(ArrayList::new)
            .stream()
            .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
            .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint,  Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY)))
            .filter(pickupPoint -> Objects.equals(fbbActivated, pickupPoint.isFbbActivated()))
            .sorted(getMinPriceComparatorFinalPriceDesc())
            .findFirst()
            .orElse(null);
      }
    }
  }

  public Comparator<PickupPoint> getMinPriceComparatorFinalPriceAsc() {
    Comparator<PickupPoint> baseComparator = Comparator.comparing(PickupPoint::isMarkForDelete)
      .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed());

    // Add purchased type score comparison only if new sorting is enabled
    if (pickupPointProperties.isNewPickupPointSortingEnabled()) {
      baseComparator = baseComparator.thenComparing(
        Comparator.comparing(PickupPoint::getPurchasedTypeScore).reversed());
    }
    return baseComparator.thenComparing(Comparator.comparing(PickupPoint::isBuyable).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::isDiscoverable).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::isCncActive).reversed()).thenComparing(
        Comparator.comparingInt((PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(PickupPointSortUtil::getMinFinalOfferPrice)
      .thenComparing(Comparator.comparing(PickupPoint::isWarehouse).reversed())
      .thenComparing(PickupPoint::getId);
  }

  private Comparator<PickupPoint> getMinPriceComparatorFinalPriceDesc() {
    Comparator<PickupPoint> baseComparator = Comparator.comparing(PickupPoint::isMarkForDelete)
        .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed());
    if (pickupPointProperties.isNewPickupPointSortingEnabled()) {
      baseComparator = baseComparator.thenComparing(
        Comparator.comparing(PickupPoint::getPurchasedTypeScore).reversed());
    }
    return baseComparator
        .thenComparing(Comparator.comparing(PickupPoint::isBuyable).reversed())
        .thenComparing(Comparator.comparing(PickupPoint::isDiscoverable).reversed())
        .thenComparing(Comparator.comparing(PickupPoint::isCncActive).reversed())
        .thenComparing(Comparator.comparingInt((PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp, pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed())
        .thenComparing(Comparator.comparingInt(pp -> PickupPointSortUtil.getMinPriority(pp, pickupPointProperties.isPrioritizeLowestPriceEnabled())))
        .thenComparing(Comparator.comparing(PickupPointSortUtil::getMaxFinalOfferPrice).reversed())
        .thenComparing(Comparator.comparing(PickupPoint::isWarehouse).reversed()).thenComparing(PickupPoint::getId);
  }

  private PickupPoint getMinimumPickupPoint1PEnabled(String itemSku, String campaignCode, Set<String> purchasedTypes, Boolean fbbActivated, boolean sortByCheapest, List<PickupPoint> allPickupPoint) {
    if (Objects.isNull(fbbActivated)) {
      //findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc
      return Optional.ofNullable(allPickupPoint).orElseGet(ArrayList::new)
          .stream()
          .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
          .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint,  Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY)))
          .filter(pickupPoint -> Optional.ofNullable(purchasedTypes).orElseGet(HashSet::new).contains(pickupPoint.getPurchasedType()))
          .sorted(getMinimumPickupPoint1PEnabledComparatorPriceAsc()
          )
          .findFirst()
          .orElse(null);
     } else {
      if (sortByCheapest) {
       //findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc
        return Optional.ofNullable(allPickupPoint).orElseGet(ArrayList::new)
            .stream()
            .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
            .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint,  Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY)))
            .sorted(getMinimumPickupPoint1PEnabledComparatorPriceAsc())
            .findFirst()
            .orElse(null);
      } else {
        //findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceDescWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes,fbbActivated);
        return Optional.ofNullable(allPickupPoint).orElseGet(ArrayList::new)
            .stream()
            .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
            .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint,  Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY)))
            .sorted(getMinimumPickupPoint1PEnabledComparatorPriceDesc())
            .findFirst()
            .orElse(null);
      }
    }
  }

  private Comparator<PickupPoint> getMinimumPickupPoint1PEnabledComparatorPriceAsc() {
    return Comparator.comparing(PickupPoint::isMarkForDelete)
      .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::getPurchasedTypeScore).reversed())
      .thenComparing(Comparator.comparingInt(
        (PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(PickupPointSortUtil::getMinFinalOfferPrice)
      .thenComparing(Comparator.comparing(PickupPoint::isWarehouse).reversed())
      .thenComparing(PickupPoint::getId);
  }

  private Comparator<PickupPoint> getMinimumPickupPoint1PEnabledComparatorPriceDesc() {
    return Comparator.comparing(PickupPoint::isMarkForDelete)
      .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::getPurchasedTypeScore).reversed())
      .thenComparing(Comparator.comparingInt(
        (PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(PickupPointSortUtil::getMinFinalOfferPrice)
      .thenComparing(Comparator.comparing(PickupPoint::isWarehouse).reversed())
      .thenComparing(PickupPoint::getId);
  }

  private static Map<String, Item> toItemSkuAndItemMap(List<Item> allItems) {
    return Optional.ofNullable(allItems).orElseGet(ArrayList::new).stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (v1, v2) -> v2));
  }

  private static Comparator<Item> getItemComparatorForArchivedASCAndMarkForDeleteASC() {
    return Comparator.comparing(Item::isArchived).thenComparing(Item::isMarkForDelete);
  }

  private Comparator<PickupPoint> getPickupPointComparatorForMinItemSkuByProductSku() {
    Comparator<PickupPoint> pickupPointComparator =
      Comparator.comparing(PickupPoint::isMarkForDelete);
    pickupPointComparator = addInstockComparator(pickupPointComparator);
    return pickupPointComparator.thenComparing(Comparator.comparingInt(
        (PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(PickupPointSortUtil::getMinFinalOfferPrice);
  }

  private Comparator<PickupPoint> getPickupPointComparatorForMaxItemSkuByProductSku() {
    Comparator<PickupPoint> pickupPointComparator =
      Comparator.comparing(PickupPoint::isMarkForDelete);
    pickupPointComparator = addInstockComparator(pickupPointComparator);
    return pickupPointComparator.thenComparing(Comparator.comparingInt(
        (PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(Comparator.comparing(PickupPointSortUtil::getMaxFinalOfferPrice).reversed());
  }

  private Comparator<PickupPoint> addInstockComparator(Comparator<PickupPoint> pickupPointComparator) {
    if (pickupPointProperties.isSortByInStock()) {
      return pickupPointComparator
          .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed())
          .thenComparing(Comparator.comparing(PickupPoint::isBuyable).reversed())
          .thenComparing(Comparator.comparing(PickupPoint::isDiscoverable).reversed())
          .thenComparing(Comparator.comparing(PickupPoint::isBuyableCnc).reversed())
          .thenComparing(Comparator.comparing(PickupPoint::isDiscoverableCnc).reversed())
          .thenComparing(Comparator.comparing(PickupPoint::isCncActive).reversed());
    }
    return pickupPointComparator;
  }

  private PickupPoint getLeastPickupPointByItemSku1PDisabled(String itemSku, String campaignCode,
      List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new).stream()
        .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
        .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint, campaignCode))
        .sorted(getLeastPickupPointByItemSku1PDisabledComparator()).findFirst().orElse(null);
  }

  private PickupPoint getLeastPickupPointByItemSku1PEnabled(String itemSku, String campaignCode, List<PickupPoint> allPickupPoints) {
    return Optional.ofNullable(allPickupPoints).orElseGet(ArrayList::new).stream()
        .filter(pickupPoint -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(pickupPoint.getItemSku()))
        .filter(pickupPoint -> PickupPointFilterUtil.isCampaignCodePresent(pickupPoint, campaignCode))
        .sorted(getLeastPickupPointByItemSku1PEnabledComparator()).findFirst().orElse(null);

  }

  private Comparator<PickupPoint> getLeastPickupPointByItemSku1PDisabledComparator() {
    Comparator<PickupPoint> baseComparator = Comparator.comparing(PickupPoint::isMarkForDelete)
      .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed());
    // Add purchased type score
    if (pickupPointProperties.isNewPickupPointSortingEnabled()) {
      baseComparator = baseComparator.thenComparing(
        Comparator.comparing(PickupPoint::getPurchasedTypeScore).reversed());
    }
    return baseComparator.thenComparing(Comparator.comparing(PickupPoint::isBuyable).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::isDiscoverable).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::isCncActive).reversed()).thenComparing(
        Comparator.comparingInt((PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(PickupPointSortUtil::getMinFinalOfferPrice)
      .thenComparing(Comparator.comparing(PickupPoint::isWarehouse).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::isInQuota).reversed())
      .thenComparing(PickupPoint::getQuotaPercentage).thenComparing(PickupPoint::getId);
  }

  private Comparator<PickupPoint> getLeastPickupPointByItemSku1PEnabledComparator() {
    //findFirstByItemSkuAndPrice_CampaignCodeOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescInQuotaDescQuotaPercentageAscIdAsc
    Comparator<PickupPoint> baseComparator = Comparator.comparing(PickupPoint::isMarkForDelete)
      .thenComparing(Comparator.comparing(PickupPoint::isInStock).reversed());
    // Add purchased type score
    if (pickupPointProperties.isNewPickupPointSortingEnabled()) {
      baseComparator = baseComparator.thenComparing(
        Comparator.comparing(PickupPoint::getPurchasedTypeScore).reversed());
    }
    return baseComparator.thenComparing(Comparator.comparingInt(
        (PickupPoint pp) -> PickupPointSortUtil.getMaxPromoCampaign(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled())).reversed()).thenComparingInt(
        pp -> PickupPointSortUtil.getMinPriority(pp,
          pickupPointProperties.isPrioritizeLowestPriceEnabled()))
      .thenComparing(PickupPointSortUtil::getMinFinalOfferPrice)
      .thenComparing(Comparator.comparing(PickupPoint::isWarehouse).reversed())
      .thenComparing(Comparator.comparing(PickupPoint::isInQuota).reversed())
      .thenComparing(PickupPoint::getQuotaPercentage).thenComparing(PickupPoint::getId);
  }


}
