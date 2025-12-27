package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import io.micrometer.tracing.Tracer;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.PricePriority;
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
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveProcessedService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import reactor.core.publisher.Mono;

@Component("ProductSivaProductConstructorV2")
public class SivaProductConstructorV2 {

  public static final String SIVA_PRODUCT_PROCESSED_SUCCESSFULLY =
    "SivaProduct processed successfully for productSku: %s";
  public static final String SIVA_PRODUCT_PROCESSING_RETURNED_FALSE_FOR_PRODUCT_SKU =
    "SivaProduct processing returned false for productSku: %s";
  public static final String ERROR_PROCESSING_SIVA_PRODUCT_FOR_PRODUCT_SKU =
    "Error processing SivaProduct for productSku: %s";
  public static final String PROCESSING_SIVA_PRODUCT_FOR_PRODUCT_SKU =
    "Processing SivaProduct for productSku: %s, topic: %s";

  @Autowired
  private SaveProcessedService saveProcessedService;

  @Autowired
  private SivaProductServiceV2 sivaProductService;

  @Autowired
  private PickupPointServiceV2 pickupPointService;

  @Autowired
  private ItemServiceV2 itemService;

  @Autowired
  private MainConstructorV2 mainConstructor;

  @Autowired
  private CustomPcbCategoryServiceV2 customPcbCategoryService;

  @Autowired
  private CustomMerchantServiceV2 customMerchantService;

  @Autowired
  private CustomProductReviewServiceV2 customProductReviewService;

  @Autowired
  private PriceProperties priceProperties;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private Tracer tracer;

  @Value("${module.domain.product.max-item-size:20}")
  private int maxItemSize = 20;


  private SivaProduct toSivaProduct(Product product, List<Item> items, SaveParam saveParam, CompleteItemData completeItemData) {
    return Optional.ofNullable(initSivaProduct(product,items, ParamUtil.getTimestamp(saveParam), completeItemData))
        .map(result -> {
          result = constructSivaProductProductRelated(result,product, completeItemData);
          result = constructSivaProductItemsRelated(result,product, completeItemData);
          result.setId(result.toId());

          return result;
        })
        .orElse(null);
  }

  private SivaProduct initSivaProduct(Product product, List<Item> items, long timestamp, CompleteItemData completeItemData) {
    return Optional.ofNullable(product)
        .filter(prd -> !ModuleProductUtil.isProductAndItemsExists(prd,items))
        .map(Product::getProductSku)
        .map(id -> sivaProductService.getExistingSivaProduct(id, completeItemData.getAllSivaProduct()))
        .map(sivaProduct -> {
          sivaProduct.setTimestamp(timestamp);
          return sivaProduct;
        })
        .orElseGet(() -> initDefaultSivaProduct(product,timestamp));
  }

  private SivaProduct initDefaultSivaProduct(Product product, long timestamp) {
    return Optional.ofNullable(product)
        .map(val -> SivaProduct.builder()
            .timestamp(timestamp)
            .build())
        .orElse(null);
  }

  public SivaProduct constructSivaProductItemsRelated(SivaProduct result, Product product, CompleteItemData completeItemData) {
    String productSku = Optional.ofNullable(result)
        .map(SivaProduct::getProductSku)
        .orElseGet(() -> ModuleProductUtil.toProductSku(product));
    result.setItemSkus(itemService.getItemSkusByProductSku(productSku, completeItemData.getAllItems()));
    result.setBuyable(ModuleProductUtil.getDefaultViewSchedule());
    result.setDiscoverable(ModuleProductUtil.getDefaultViewSchedule());
    result.setArchived(itemService.isAllItemsArchivedTrue(productSku, completeItemData.getAllItems()));
    result.setMarkForDelete(ModuleProductUtil.toProductMarkForDelete(product,itemService.isAllItemsMarkForDeleteTrue(productSku, completeItemData.getAllItems())));

    List<Item> itemsForProductCampaignInfos =
      pickupPointService.getItemsByProductSku(productSku, completeItemData.getAllItems(),
        completeItemData.getAllPickupPoints(), completeItemData.getStockUpdateSearchEvent(),
        completeItemData.getLevel2InventoryQuantityChangedEvent(), maxItemSize);
    Map<String, CampaignInfo> productCampaignInfos = mainConstructor.toCampaignInfos(product,itemsForProductCampaignInfos,null,true,
        completeItemData);
    CampaignInfo campaignInfo = productCampaignInfos.get(ModuleProductUtil.toDefaultCampaignInfoKey(productCampaignInfos, pickupPointProperties.isSortByInStock()));

    boolean hasCncActivated = pickupPointService.hasCncActivatedByProductSku(productSku, completeItemData.getAllPickupPoints());
    boolean hasBuyableCnc = pickupPointService.hasBuyableCncByProductSku(productSku, completeItemData.getAllPickupPoints());
    boolean hasDiscoverableCnc = pickupPointService.hasDiscoverableCncByProductSku(productSku, completeItemData.getAllPickupPoints());

    if (Objects.nonNull(campaignInfo)) {
      SivaProduct.FinalPrice offlinePrice = ModuleProductUtil.toFinalPrice(campaignInfo,true);
      SivaProduct.FinalPrice price = MainUtil.getOrDefault(ModuleProductUtil.toFinalPrice(campaignInfo,false),offlinePrice);

      result.setMeasurement(mainConstructor.toMeasurement(ModuleProductUtil.getItem(itemsForProductCampaignInfos, ModuleProductUtil.getCheapestItemSkuFinalPrice(price)),product,
          completeItemData));
      result.setCampaignInfos(productCampaignInfos);
      result.setBuyable(campaignInfo.getBuyable());
      result.setDiscoverable(campaignInfo.getDiscoverable());
      result.setBuyableCnc(campaignInfo.getBuyableCnc());
      result.setDiscoverableCnc(campaignInfo.getDiscoverableCnc());
      result.setPurchasedType(campaignInfo.getPurchasedType());
      result.setPurchasedTypeScore(campaignInfo.getPurchasedTypeScore());
      result.setOfflinePurchasedType(campaignInfo.getOfflinePurchasedType());
      result.setOfflinePurchasedTypeScore(campaignInfo.getOfflinePurchasedTypeScore());
      result.setOfflinePrice(offlinePrice);
      result.setPrice(price);
      result.setPriceTeaser(ModuleProductUtil.toCurrentPriceTeaser(campaignInfo));
      result.setImage(campaignInfo.getImage());
      result.setAdjustments(campaignInfo.getAdjustments());
      result.setInventory(campaignInfo.getInventory());

      result.setFlashsale(ModuleProductUtil.toCampaignInfoFlashsale(campaignInfo,productCampaignInfos));
      result.setSubFlashsale(ModuleProductUtil.toCampaignInfoSubFlashsale(campaignInfo,productCampaignInfos));
      result.setFlashsaleItemSkus(ModuleProductUtil.toCampaignInfoFlashsaleItemSkus(campaignInfo,productCampaignInfos));
      result.setFlashsaleInventory(ModuleProductUtil.toCampaignInfoFlashsaleInventory(campaignInfo,productCampaignInfos));

      result.setCampaignCodes(ModuleProductUtil.toCampaignInfosCampaignCodes(productCampaignInfos,true));
      result.setCampaignInfoCodes(ModuleProductUtil.toCampaignInfosCampaignCodes(productCampaignInfos,false));
      result.setPickupPointCode(campaignInfo.getPickupPointCode());
      result.setExternalPickupPointCode(campaignInfo.getExternalPickupPointCode());
      result.setOfflinePickupPointCode(campaignInfo.getOfflinePickupPointCode());
      result.setOfflineExternalPickupPointCode(campaignInfo.getOfflineExternalPickupPointCode());
      result.setNearestPickupPoints(campaignInfo.getNearestPickupPoints());
      result.setUrl(campaignInfo.getItemUrl());
      result.setFbbActivated(campaignInfo.isFbbActivated());
      result.setFbb(mainConstructor.toFbb(product,itemsForProductCampaignInfos,result.getFbb(),PricePriority.LOWEST.equals(priceProperties.getFbbPriority()),
          completeItemData));
      result.setNonFbb(mainConstructor.toNonFbb(product,itemsForProductCampaignInfos,PricePriority.LOWEST.equals(priceProperties.getNonFbbPriority()),
          completeItemData));
    }
    result.setTags(ModuleProductUtil.toTags(itemsForProductCampaignInfos,result.isFbbActivated(),hasCncActivated,result.getPurchasedType(),result.getPurchasedTypeScore(),pickupPointProperties.isOnePartyActivated()));
    result.setHasCncActivated(hasCncActivated);
    result.setOnlineOnly(!hasCncActivated);
    result.setHasBuyableCnc(hasBuyableCnc);
    result.setHasDiscoverableCnc(hasDiscoverableCnc);

    return result;
  }

  public SivaProduct constructSivaProductProductRelated(SivaProduct result, Product product, CompleteItemData completeItemData) {
    //product
    result.setProductCode(ModuleProductUtil.toProductCode(product));
    result.setProductType(ModuleProductUtil.getProductTypeFromProduct(product));
    result.setMerchantCode(ModuleProductUtil.toProductMerchantCode(product));
    result.setSync(ModuleProductUtil.toProductSync(product));
    result.setReview(customProductReviewService.getExistingReviewByProduct(product, completeItemData.getAllCustomProductReviews()));
    result.setMerchant(customMerchantService.getExistingCustomMerchantByProduct(product, completeItemData.getAllCustomMerchants()));
    result.setName(ModuleProductUtil.toProductName(product));
    result.setUrlName(ModuleProductUtil.toProductUrlName(product));
    result.setProductSku(ModuleProductUtil.toProductSku(product));
    result.setBrand(ModuleProductUtil.toBrand(product));
    result.setBrandLogoUrl(ModuleProductUtil.toBrandLogoUrl(product));
    result.setMasterCategories(customPcbCategoryService.getBreadCrumbMasterCategoryCodes(product, completeItemData.getAllCustomPcbCategory()));
    result.setSalesCategories(customPcbCategoryService.getBreadCrumbSalesCategoryCodes(product, completeItemData.getAllCustomPcbCategory()));
    result.setMarkForDelete(ModuleProductUtil.toProductMarkForDelete(product,itemService.isAllItemsMarkForDeleteTrue(ModuleProductUtil.toProductSku(product), completeItemData.getAllItems())));

    return result;
  }

  public boolean joinSivaProductUsingCombinedUpsertFlow(Product product, SaveParam saveParam,
    CompleteItemData completeItemData, boolean sivaProductCombinedUpsertEnabled, String topic) {
    boolean result = false;
    String productSku = ModuleProductUtil.toProductSku(product);
    if (StringUtils.isNotBlank(productSku)) {
      List<Item> itemList = getItemListForProduct(productSku, completeItemData);
        SivaProduct sivaProduct = toSivaProduct(product, itemList, saveParam, completeItemData);
        if (Objects.nonNull(sivaProduct)) {
          result =
            processSivaProduct(sivaProduct, saveParam, productSku, sivaProductCombinedUpsertEnabled, topic);
        }
      }
    return result;
  }

  private List<Item> getItemListForProduct(String productSku, CompleteItemData completeItemData) {
    return pickupPointService.getItemsByProductSku(productSku, completeItemData.getAllItems(),
      completeItemData.getAllPickupPoints(), completeItemData.getStockUpdateSearchEvent(),
      completeItemData.getLevel2InventoryQuantityChangedEvent(), maxItemSize);
  }

  private boolean processSivaProduct(SivaProduct sivaProduct, SaveParam saveParam,
    String productSku, boolean sivaProductCombinedUpsertEnabled, String topic) {
    String command = String.format(PROCESSING_SIVA_PRODUCT_FOR_PRODUCT_SKU, productSku, topic);
    LoggerUtil.info(command);

    try {
      boolean result = processSivaProductDelete(sivaProduct, saveParam, productSku, sivaProductCombinedUpsertEnabled, topic);

      logSivaProductProcessingResult(result, productSku, saveParam.getTraceId());
      return result;
    } catch (Exception e) {
      TraceHelper.recordError(tracer, e);
      LoggerUtil.error(e, String.format(ERROR_PROCESSING_SIVA_PRODUCT_FOR_PRODUCT_SKU, productSku), saveParam.getTraceId());
      return false;
    }
  }

  private boolean processSivaProductDelete(SivaProduct sivaProduct, SaveParam saveParam,
    String productSku, boolean sivaProductCombinedUpsertEnabled, String topic) {
    if (sivaProductCombinedUpsertEnabled) {
      sivaProductService.publishCombinedSivaProductUpsertModel(saveParam, productSku, topic,
        sivaProduct).subscribe();
    } else {
      return Boolean.TRUE.equals(saveProcessedService.saveSivaProduct(sivaProduct, saveParam).block());
    }
    return false;
  }

  private void logSivaProductProcessingResult(boolean result, String productSku, String traceId) {
    if (result) {
      LoggerUtil.success(String.format(SIVA_PRODUCT_PROCESSED_SUCCESSFULLY, productSku), traceId);
    } else {
      LoggerUtil.warn(String.format(SIVA_PRODUCT_PROCESSING_RETURNED_FALSE_FOR_PRODUCT_SKU, productSku));
    }
  }



}
