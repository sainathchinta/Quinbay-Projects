package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.PricePriority;
import com.gdn.aggregate.platform.module.product.listener.model.other.CalculatedItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.util.CompleteItemData;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.PriceProperties;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomMerchantServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomPcbCategoryServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomProductReviewServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveProcessedService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component("ProductSivaItemConstructorV2")
public class SivaItemConstructorV2 {

  @Autowired
  private SaveProcessedService saveProcessedService;

  @Autowired
  private SivaItemServiceV2 sivaItemService;

  @Autowired
  private ItemServiceV2 itemService;

  @Autowired
  private PickupPointServiceV2 productPickupPointService;

  @Autowired
  private CustomPcbCategoryServiceV2 customPcbCategoryService;

  @Autowired
  private CustomMerchantServiceV2 customMerchantService;

  @Autowired
  private CustomProductReviewServiceV2 customProductReviewService;

  @Autowired
  private MainConstructorV2 mainConstructor;

  @Autowired
  private PriceProperties priceProperties;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Value("${module.domain.product.max-item-size:20}")
  private int maxItemSize = 20;

  @Value("${back-fill.siva.docs.for.missing-fields}")
  private boolean backFillSivaDocsForMissingFields;

  public SivaItem constructSivaItemItemRelated(SivaItem result, Product product, Item item, CompleteItemData completeItemData) {
    String productSku = Optional.ofNullable(result)
        .map(SivaItem::getProductSku)
        .orElseGet(() -> ModuleProductUtil.fromItemSkuToProductSku(ModuleProductUtil.toItemSku(item)));
    String itemSku = Optional.ofNullable(result)
        .map(SivaItem::getItemSku)
        .orElseGet(() -> ModuleProductUtil.toItemSku(item));

    result.setItemCode(ModuleProductUtil.toItemCode(item));
    result.setOff2OnChannelActiveItem(ModuleProductUtil.toItemOff2OnChannelActive(item));
    result.setBuyable(ModuleProductUtil.getDefaultViewSchedule());
    result.setDiscoverable(ModuleProductUtil.getDefaultViewSchedule());
    result.setSync(ModuleProductUtil.toItemSync(item));
    result.setSubscribable(ModuleProductUtil.toItemSubscribable(item));
    result.setItemName(ModuleProductUtil.toItemName(product,item));
    result.setItemUrlName(ModuleProductUtil.toItemUrlName(product,item));
    result.setItemSku(ModuleProductUtil.toItemSku(item));
    result.setArchived(ModuleProductUtil.toItemArchived(item));
    result.setMarkForDelete(ModuleProductUtil.toItemMarkForDelete(item,product));
    result.setOmniChannelSku(item.getOmniChannelSku());
    /*will be removed in SP17*/
    result.setMasterDataItemAttributeValues(ModuleProductUtil.toMasterDataItemAttributeValues(item));
    /*End of will be removed in SP17*/

    List<Item> itemsForProductCampaignInfos =
      productPickupPointService.getItemsByProductSku(productSku, completeItemData.getAllItems(),
        completeItemData.getAllPickupPoints(), completeItemData.getStockUpdateSearchEvent(),
        completeItemData.getLevel2InventoryQuantityChangedEvent(), maxItemSize);
    Map<String, CampaignInfo> productCampaignInfos = mainConstructor.toCampaignInfos(product,itemsForProductCampaignInfos,null,true,
        completeItemData);
    CampaignInfo productCampaignInfo = productCampaignInfos.get(ModuleProductUtil.toDefaultCampaignInfoKey(productCampaignInfos, pickupPointProperties.isSortByInStock()));

    List<Item> itemsForItemCampaignInfos = MainUtil.toList(item);
    Map<String, CampaignInfo> itemCampaignInfos = mainConstructor.toCampaignInfos(product,itemsForItemCampaignInfos,null,true,
        completeItemData);
    CampaignInfo itemCampaignInfo = itemCampaignInfos.get(ModuleProductUtil.toDefaultCampaignInfoKey(itemCampaignInfos, pickupPointProperties.isSortByInStock()));

    boolean hasCncActivated = productPickupPointService.hasCncActivatedByItemSku(itemSku, completeItemData.getAllPickupPoints());
    boolean hasBuyableCnc = productPickupPointService.hasBuyableCncByItemSku(itemSku, completeItemData.getAllPickupPoints());
    boolean hasDiscoverableCnc = productPickupPointService.hasDiscoverableCncByItemSku(itemSku, completeItemData.getAllPickupPoints());

    if (Objects.nonNull(productCampaignInfo)) {
      result.setProductImage(productCampaignInfo.getImage());
    }
    if (Objects.nonNull(itemCampaignInfo)) {
      SivaProduct.FinalPrice offlinePrice = ModuleProductUtil.toFinalPrice(itemCampaignInfo,true);
      SivaProduct.FinalPrice price = MainUtil.getOrDefault(ModuleProductUtil.toFinalPrice(itemCampaignInfo,false),offlinePrice);
      CalculatedItem.Price itemPrice = ModuleProductUtil.toFinalItemPrice(price,offlinePrice);

      /*will be removed in SP17*/
      result.setOfflineItemPrice(offlinePrice);
      result.setItemPrice(itemPrice);
      result.setItemPriceTeaser(ModuleProductUtil.toCurrentPriceTeaser(itemCampaignInfo));
      /*End of will be removed in SP17*/

      result.setMeasurement(mainConstructor.toMeasurement(ModuleProductUtil.getItem(itemsForItemCampaignInfos, ModuleProductUtil.getCheapestItemSkuFinalPrice(price)), product,
          completeItemData));
      result.setCampaignInfos(itemCampaignInfos);
      result.setBuyable(itemCampaignInfo.getBuyable());
      result.setDiscoverable(itemCampaignInfo.getDiscoverable());
      result.setBuyableCnc(itemCampaignInfo.getBuyableCnc());
      result.setDiscoverableCnc(itemCampaignInfo.getDiscoverableCnc());
      result.setPurchasedType(itemCampaignInfo.getPurchasedType());
      result.setPurchasedTypeScore(itemCampaignInfo.getPurchasedTypeScore());
      result.setOfflinePurchasedType(itemCampaignInfo.getOfflinePurchasedType());
      result.setOfflinePurchasedTypeScore(itemCampaignInfo.getOfflinePurchasedTypeScore());
      result.setOfflinePrice(offlinePrice);
      result.setPrice(price);
      result.setPriceTeaser(ModuleProductUtil.toCurrentPriceTeaser(itemCampaignInfo));
      result.setItemImage(itemCampaignInfo.getImage());
      result.setAdjustments(itemCampaignInfo.getAdjustments());
      result.setInventory(itemCampaignInfo.getInventory());

      result.setFlashsale(ModuleProductUtil.toCampaignInfoFlashsale(itemCampaignInfo,itemCampaignInfos));
      result.setSubFlashsale(ModuleProductUtil.toCampaignInfoSubFlashsale(itemCampaignInfo,itemCampaignInfos));
      result.setFlashsaleItemSku(ModuleProductUtil.toCampaignInfoFlashsaleItemSku(itemCampaignInfo,itemCampaignInfos));
      result.setFlashsaleInventory(ModuleProductUtil.toCampaignInfoFlashsaleInventory(itemCampaignInfo,itemCampaignInfos));

      result.setCampaignCodes(ModuleProductUtil.toCampaignInfosCampaignCodes(itemCampaignInfos,true));
      result.setCampaignInfoCodes(ModuleProductUtil.toCampaignInfosCampaignCodes(itemCampaignInfos,false));
      result.setPickupPointCode(itemCampaignInfo.getPickupPointCode());
      result.setExternalPickupPointCode(itemCampaignInfo.getExternalPickupPointCode());
      result.setOfflinePickupPointCode(itemCampaignInfo.getOfflinePickupPointCode());
      result.setOfflineExternalPickupPointCode(itemCampaignInfo.getOfflineExternalPickupPointCode());
      result.setNearestPickupPoints(itemCampaignInfo.getNearestPickupPoints());
      result.setProductUrl(itemCampaignInfo.getProductUrl());
      result.setItemUrl(itemCampaignInfo.getItemUrl());
      result.setFbbActivated(itemCampaignInfo.isFbbActivated());
      result.setFbb(mainConstructor.toFbb(product,itemsForItemCampaignInfos,result.getFbb(),PricePriority.LOWEST.equals(priceProperties.getFbbPriority()),
          completeItemData));
      result.setNonFbb(mainConstructor.toNonFbb(product,itemsForItemCampaignInfos,PricePriority.LOWEST.equals(priceProperties.getNonFbbPriority()),
          completeItemData));
    }
    result.setTags(ModuleProductUtil.toTags(itemsForItemCampaignInfos,result.isFbbActivated(),hasCncActivated,result.getPurchasedType(),result.getPurchasedTypeScore(), pickupPointProperties.isOnePartyActivated()));
    result.setHasCncActivated(hasCncActivated);
    result.setOnlineOnly(!hasCncActivated);
    result.setHasBuyableCnc(hasBuyableCnc);
    result.setHasDiscoverableCnc(hasDiscoverableCnc);
    if (backFillSivaDocsForMissingFields) {
      ModuleProductUtil.setMandatoryDataInSivaItem(result, item, completeItemData);
    }
    return result;
  }

  public boolean joinSivaItem(Product product, List<Item> items, SaveParam saveParam, CompleteItemData completeItemData) {
    boolean result = Boolean.TRUE.equals(
        saveProcessedService.saveSivaItems(toSivaItems(product, items, saveParam, completeItemData), saveParam).block());
    return result;
  }

  private List<SivaItem> toSivaItems(Product product, List<Item> items, SaveParam saveParam, CompleteItemData completeItemData) {
    return Optional.ofNullable(items)
        .filter(vals -> !CollectionUtils.isEmpty(vals))
        .map(vals -> vals.stream()
            .map(item -> toSivaItem(product, item, saveParam, completeItemData))
            .filter(Objects::nonNull)
            .collect(Collectors.toList()))
        .orElse(null);
  }

  private SivaItem toSivaItem(Product product, Item item, SaveParam saveParam, CompleteItemData completeItemData) {
    return Optional.ofNullable(initSivaItem(product,item, ParamUtil.getTimestamp(saveParam), completeItemData.getAllSivaItems()))
        .map(result -> {
          result = constructSivaItemProductRelated(result,product,item, completeItemData);
          result = constructSivaItemItemRelated(result,product, item, completeItemData);
          result.setId(result.toId());

          return result;
        })
        .orElse(null);
  }

  private SivaItem initSivaItem(Product product, Item item, long timestamp, List<SivaItem> allSivaItem) {
    return Optional.ofNullable(item)
        .filter(itm -> !ModuleProductUtil.isProductAndItemsExists(product, MainUtil.toList(itm)))
        .map(Item::getItemSku)
        .map(itemSku -> sivaItemService.getExistingSivaItem(itemSku, allSivaItem))
        .map(sivaItem -> {
          sivaItem.setTimestamp(timestamp);
          return sivaItem;
        })
        .orElseGet(() -> initDefaultSivaItem(item,timestamp));
  }

  public SivaItem constructSivaItemProductRelated(SivaItem result, Product product, Item item, CompleteItemData completeItemData) {
    //product
    result.setProductCode(ModuleProductUtil.toProductCode(product));
    result.setProductType(ModuleProductUtil.getProductTypeFromProduct(product));
    result.setMerchantCode(ModuleProductUtil.toProductMerchantCode(product));
    result.setOff2OnChannelActiveProduct(ModuleProductUtil.toProductOff2OnChannelActive(product));
    result.setReview(customProductReviewService.getExistingReviewByProduct(product, completeItemData.getAllCustomProductReviews()));
    result.setMerchant(customMerchantService.getExistingCustomMerchantByProduct(product, completeItemData.getAllCustomMerchants()));
    result.setProductName(ModuleProductUtil.toProductName(product));
    result.setProductUrlName(ModuleProductUtil.toProductUrlName(product));
    result.setProductSku(ModuleProductUtil.toProductSku(product));
    result.setBrand(ModuleProductUtil.toBrand(product));
    result.setBrandLogoUrl(ModuleProductUtil.toBrandLogoUrl(product));
    result.setMasterCategories(customPcbCategoryService.getBreadCrumbMasterCategoryCodes(product, completeItemData.getAllCustomPcbCategory()));
    result.setSalesCategories(customPcbCategoryService.getBreadCrumbSalesCategoryCodes(product, completeItemData.getAllCustomPcbCategory()));

    /*will be removed in SP17*/
    result.setDescription(ModuleProductUtil.toDescription(product));
    result.setBreadCrumb(customPcbCategoryService.getBreadCrumbSales(product, completeItemData.getAllCustomPcbCategory()));
    result.setLevelCategory(customPcbCategoryService.getCurrentLevelSalesCategory(product, completeItemData.getAllCustomPcbCategory()));
    result.setTopLevelCategory(customPcbCategoryService.getTopLevelSalesCategory(product, completeItemData.getAllCustomPcbCategory()));
    /*End of will be removed in SP17*/

    result.setMarkForDelete(ModuleProductUtil.toItemMarkForDelete(item,product));
    if (backFillSivaDocsForMissingFields) {
      ModuleProductUtil.setMandatoryDataInSivaItem(result, item, completeItemData);
    }
    return result;
  }

  private SivaItem initDefaultSivaItem(Item item, long timestamp) {
    return Optional.ofNullable(item)
        .map(val -> SivaItem.builder()
            .timestamp(timestamp)
            .build())
        .orElse(null);
  }
}
