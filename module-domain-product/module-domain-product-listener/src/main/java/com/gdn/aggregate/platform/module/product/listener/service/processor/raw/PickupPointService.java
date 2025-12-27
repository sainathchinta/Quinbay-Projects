package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.PickupPointLocation;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.PickupPointRepository;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomBusinessPartnerPickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomInventoryService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Component("ProductPickupPointService")
public class PickupPointService {

  private static final String SAVE_COMMAND = "savePickupPoint";

  @Autowired
  private DBService dbService;

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Autowired
  private CustomBusinessPartnerPickupPointService customBusinessPartnerPickupPointService;

  @Autowired
  private CustomInventoryService customInventoryService;

  @Autowired
  private MerchantDiscountPriceService merchantDiscountPriceService;

  @Autowired
  private AdjustmentProductService adjustmentProductService;

  @Autowired
  private AdjustmentProductQuotaService adjustmentProductQuotaService;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private PickupPointInventoryService pickupPointInventoryService;

  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  /*Save*/
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

  public void setMandatory(PickupPoint pickupPoint, SaveParam saveParam) {
    Optional.ofNullable(pickupPoint)
        .ifPresent(val -> {
          val.setId(val.toId());
          val = toUpdatedStock(pickupPoint);
          val = toUpdatedBusinessPartner(val);
          val.setPrice(getPrice(val));
          val = toUpdatedQuota(val);
          val = ModuleProductUtil.toUpdatedFlags(val,pickupPointProperties.isOnePartyActivated());
        });
  }

  private PickupPoint toUpdatedStock(PickupPoint pickupPoint) {
    if (Objects.isNull(pickupPoint)) {
      return null;
    }
    if (skipStockFetchFromInventoryModule) {
      //Use product_pickup_point_inventory instead of inventory_inventory_pickup_point_infos
      PickupPointInventory inventory = pickupPointInventoryService.getExistingPickupPointInventory(
        ModuleProductUtil.toPickupPointId(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode()));
      
      Optional.ofNullable(inventory).ifPresent(inv -> {
        pickupPoint.setInStock(inv.isInStock());
        pickupPoint.setWarehouse(inv.getSyncStock());
      });
    } else {
      Optional.of(pickupPoint).map(
        val -> customInventoryService.getStockByL5(val.getItemSku(), val.getPickupPointCode(),
          pickupPoint)).ifPresent(stock -> {
        pickupPoint.setWarehouse(stock.isWarehouse());
        pickupPoint.setInStock(ModuleProductUtil.isPickupPointInStock(stock));
      });
    }
    return pickupPoint;
  }

  private PickupPoint toUpdatedQuota(PickupPoint pickupPoint) {
    Optional.ofNullable(pickupPoint)
        .map(val -> adjustmentProductQuotaService.toQuotaAdjustments(val.getItemSku(),val.getPickupPointCode(),ModuleProductUtil.getPickupPointCampaignCode(val)))
        .map(ModuleProductUtil::findMaxAdjustment)
        .ifPresent(quota -> {
          pickupPoint.setQuotaPercentage(ModuleProductUtil.toPercentage(quota.getQuota(),quota.getRemaining()));
          pickupPoint.setInQuota(ModuleProductUtil.isPickupPointInQuota(quota));
        });

    return pickupPoint;
  }

  private PickupPoint toUpdatedBusinessPartner(PickupPoint pickupPoint) {
    Optional.ofNullable(pickupPoint)
        .map(val -> customBusinessPartnerPickupPointService.getExistingCustomBusinessPartnerPickupPoint(val.getPickupPointCode()))
        .ifPresent(val -> {
          pickupPoint.setFbbActivated(MainUtil.toNotNullBoolean(val.getFbbActivated()));
          pickupPoint.setLongitude(ModuleProductUtil.getLongitude(val.getGeolocation()));
          pickupPoint.setLatitude(ModuleProductUtil.getLatitude(val.getGeolocation()));
          pickupPoint.setCityName(val.getCityName());
        });

    return pickupPoint;
  }

  private Set<PickupPoint.Price> getPrice(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .peek(price -> {
          if (pickupPointProperties.isAlwaysUpdateMerchantDiscount()) {
            updateMerchantPromoPriceAndOfferPriceFromEvent(pickupPoint, price);
          }
          else {
            // OLD FLOW: only update if merchant discount exists
            Optional.ofNullable(pickupPoint).map(PickupPoint::toId)
              .map(merchantDiscountPriceService::getDiscountPrice)
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
              .map(val -> adjustmentProductService.getActiveAdjustmentProductByL5(val.getItemSku(),val.getPickupPointCode()))
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

  private void updateMerchantPromoPriceAndOfferPriceFromEvent(PickupPoint pickupPoint, PickupPoint.Price price) {
    // Always update with fresh data from MerchantDiscountPrice event
    String pickupPointId =
      Optional.ofNullable(pickupPoint).map(PickupPoint::toId).orElse(null);
    PickupPoint.Price freshMerchantPrice =
      Optional.ofNullable(pickupPointId).map(merchantDiscountPriceService::getFullPrice)
        .orElse(null);

    PickupPoint.DiscountPrice merchantPromoDiscountPrice =
      Optional.ofNullable(freshMerchantPrice)
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice).orElse(null);
    price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
    if (Objects.nonNull(freshMerchantPrice) && Objects.isNull(merchantPromoDiscountPrice)) {
      price.setOfferPrice(freshMerchantPrice.getOfferPrice());
    } else {
      price.setOfferPrice(ModuleProductUtil.getOfferPrice(price));
    }
  }
  /*End of Save*/

  /*Getters*/
  public PickupPoint getExistingPickupPoint(String id) {
    return Optional.ofNullable(id)
        .flatMap(pickupPointRepository::findById)
        .orElse(null);
  }

  public boolean hasCncActivatedByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(pickupPointRepository::existsByProductSkuAndMarkForDeleteFalseAndCncActiveTrue)
        .orElseGet(() -> false);
  }

  public boolean hasCncActivatedByItemSku(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::existsByItemSkuAndMarkForDeleteFalseAndCncActiveTrue)
        .orElseGet(() -> false);
  }

  public boolean hasBuyableCncByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(pickupPointRepository::existsByProductSkuAndMarkForDeleteFalseAndBuyableCncTrue)
        .orElseGet(() -> false);
  }

  public boolean hasBuyableCncByItemSku(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::existsByItemSkuAndMarkForDeleteFalseAndBuyableCncTrue)
        .orElseGet(() -> false);
  }

  public boolean hasDiscoverableCncByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(pickupPointRepository::existsByProductSkuAndMarkForDeleteFalseAndDiscoverableCncTrueAndBuyableCncTrue)
        .orElseGet(() -> false);
  }

  public boolean hasBuyableByItemSku(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::existsByItemSkuAndMarkForDeleteFalseAndBuyableTrue)
        .orElseGet(() -> false);
  }

  public boolean hasDiscoverableByItemSku(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::existsByItemSkuAndMarkForDeleteFalseAndDiscoverableTrue)
        .orElseGet(() -> false);
  }

  public boolean hasBuyableByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(pickupPointRepository::existsByProductSkuAndMarkForDeleteFalseAndBuyableTrue)
        .orElseGet(() -> false);
  }

  public boolean hasDiscoverableByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(pickupPointRepository::existsByProductSkuAndMarkForDeleteFalseAndDiscoverableTrue)
        .orElseGet(() -> false);
  }

  public boolean hasDiscoverableCncByItemSku(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::existsByItemSkuAndMarkForDeleteFalseAndDiscoverableCncTrueAndBuyableCncTrue)
        .orElseGet(() -> false);
  }

  public String getMinItemSkuByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(this::getMinPickupPointByProductSku)
        .map(PickupPoint::getItemSku)
        .orElse(null);
  }

  public PickupPoint getMinPickupPointByProductSku(String productSku) {
    return pickupPointProperties.isSortByInStock() ?
        Optional.ofNullable(productSku).map(
                pickupPointRepository::findFirstByProductSkuOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescBuyableCncDescDiscoverableCncDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAsc)
            .orElse(null) :
        Optional.ofNullable(productSku).map(
                pickupPointRepository::findFirstByProductSkuOrderByMarkForDeleteAscPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAsc)
            .orElse(null);
  }

  public String getMaxItemSkuByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(this::getMaxPickupPointByProductSku)
        .map(PickupPoint::getItemSku)
        .orElse(null);
  }

  public PickupPoint getMaxPickupPointByProductSku(String productSku) {
    return pickupPointProperties.isSortByInStock() ?
        Optional.ofNullable(productSku).map(
                pickupPointRepository::findFirstByProductSkuOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescBuyableCncDescDiscoverableCncDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceDesc)
            .orElse(null) :
        Optional.ofNullable(productSku).map(
                pickupPointRepository::findFirstByProductSkuOrderByMarkForDeleteAscPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceDesc)
            .orElse(null);
  }

  public PickupPoint getAnyPickupPointByItemSku(String itemSku) {
    if (!pickupPointProperties.isOnePartyActivated()) {
      return getAnyPickupPointByItemSku1PDisabled(itemSku);
    } else {
      return getAnyPickupPointByItemSku1PEnabled(itemSku);
    }
  }

  private PickupPoint getAnyPickupPointByItemSku1PDisabled(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::findFirstByItemSkuOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_FinalOfferPriceAscWarehouseDescIdAsc)
        .orElse(null);
  }

  private PickupPoint getAnyPickupPointByItemSku1PEnabled(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(pickupPointRepository::findFirstByItemSkuOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_FinalOfferPriceAscWarehouseDescIdAsc)
        .orElse(null);
  }

  public List<PickupPoint> getMinimumPickupPoints(String itemSku, String campaignCode, Boolean fbbActivated, boolean sortByCheapest) {
    PickupPoint onlineAvailability = getMinimumPickupPoint(itemSku,campaignCode,MainUtil.toSet(PurchasedType.ONLINE_CNC,PurchasedType.ONLINE),fbbActivated,sortByCheapest);
    PickupPoint offlineAvailability = getMinimumPickupPoint(itemSku,campaignCode,MainUtil.toSet(PurchasedType.CNC,PurchasedType.NOT_AVAILABLE),fbbActivated,sortByCheapest);

    return MainUtil.toList(onlineAvailability,offlineAvailability);
  }

  private PickupPoint getMinimumPickupPoint(String itemSku, String campaignCode, Set<String> purchasedTypes, Boolean fbbActivated, boolean sortByCheapest) {
    if (!pickupPointProperties.isOnePartyActivated()) {
      return getMinimumPickupPoint1PDisabled(itemSku,campaignCode,purchasedTypes,fbbActivated,sortByCheapest);
    } else {
      return getMinimumPickupPoint1PEnabled(itemSku,campaignCode,purchasedTypes,fbbActivated,sortByCheapest);
    }
  }

  private PickupPoint getMinimumPickupPoint1PDisabled(String itemSku, String campaignCode, Set<String> purchasedTypes, Boolean fbbActivated, boolean sortByCheapest) {
    if (Objects.isNull(fbbActivated)) {
      return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes);
    } else {
      if (sortByCheapest) {
        return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes,fbbActivated);
      } else {
        return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceDescWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes,fbbActivated);
      }
    }
  }

  private PickupPoint getMinimumPickupPoint1PEnabled(String itemSku, String campaignCode, Set<String> purchasedTypes, Boolean fbbActivated, boolean sortByCheapest) {
    if (Objects.isNull(fbbActivated)) {
      return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes);
    } else {
      if (sortByCheapest) {
        return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes,fbbActivated);
      } else {
        return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeAndPurchasedTypeInAndFbbActivatedOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceDescWarehouseDescIdAsc(itemSku,campaignCode,purchasedTypes,fbbActivated);
      }
    }
  }

  public List<PickupPoint> getLeastPickupPointsByItemSkus(List<String> itemSkus, String campaignCode) {
    return Optional.ofNullable(itemSkus)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(itemSku -> getLeastPickupPointByItemSku(itemSku,campaignCode))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public PickupPoint getLeastPickupPointByItemSku(String itemSku, String campaignCode) {
    if (!pickupPointProperties.isOnePartyActivated()) {
      return getLeastPickupPointByItemSku1PDisabled(itemSku,campaignCode);
    } else {
      return getLeastPickupPointByItemSku1PEnabled(itemSku,campaignCode);
    }
  }

  private PickupPoint getLeastPickupPointByItemSku1PDisabled(String itemSku, String campaignCode) {
    return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeOrderByMarkForDeleteAscInStockDescBuyableDescDiscoverableDescCncActiveDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescInQuotaDescQuotaPercentageAscIdAsc(itemSku,campaignCode);
  }

  private PickupPoint getLeastPickupPointByItemSku1PEnabled(String itemSku, String campaignCode) {
    return pickupPointRepository.findFirstByItemSkuAndPrice_CampaignCodeOrderByMarkForDeleteAscInStockDescPurchasedTypeScoreDescPrice_PromoCampaignDescPrice_PriorityAscPrice_FinalOfferPriceAscWarehouseDescInQuotaDescQuotaPercentageAscIdAsc(itemSku,campaignCode);
  }

  public List<PickupPointLocation> getNearestPickupPoints(SivaProduct.FinalPrice price, PickupPoint cheapestPickupPoint) {
    int size = pickupPointProperties.getNearestSize();
    int limit = size + 1;
    return Optional.ofNullable(ModuleProductUtil.toNearestPickupPointPageable(limit))
        .map(pageable -> pickupPointRepository.findAllByItemSkuAndPrice_CampaignCodeAndPrice_FinalOfferPriceAndWarehouseAndPickupPointCodeNotAndMarkForDeleteFalseAndInStockTrueAndPurchasedTypeNot(price.getCheapestItemSku(),ModuleProductUtil.getRealCampaignCodeFinalPrice(price),MainUtil.toNotNullDouble(price.getOfferValue()),ModuleProductUtil.getPickupPointWarehouseFlag(cheapestPickupPoint),ModuleProductUtil.getPickupPointCode(cheapestPickupPoint),PurchasedType.NOT_AVAILABLE,pageable))
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
  /*End of Getters*/

}
