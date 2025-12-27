package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.PricePriority;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.PriceProperties;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveProcessedService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomMerchantService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomPcbCategoryService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomProductReviewService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@Component("ProductSivaProductConstructor")
public class SivaProductConstructor {

  @Autowired
  private CustomPcbCategoryService customPcbCategoryService;

  @Autowired
  private CustomProductReviewService customProductReviewService;

  @Autowired
  private CustomMerchantService customMerchantService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductService productService;

  @Autowired
  private SivaProductService sivaProductService;

  @Autowired
  private SaveProcessedService saveProcessedService;

  @Autowired
  private MainConstructor mainConstructor;

  @Autowired
  private PriceProperties priceProperties;

  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Autowired
  private SivaProductServiceV2 sivaProductServiceV2;

  public Mono<Boolean> joinSivaProduct(Product product, SaveParam saveParam) {
    return saveProcessedService.saveSivaProduct(toSivaProduct(product,itemService.getItemsByProductSku(ModuleProductUtil.toProductSku(product)),saveParam),saveParam);
  }

  private SivaProduct initDefaultSivaProduct(Product product, long timestamp) {
    return Optional.ofNullable(product)
        .map(val -> SivaProduct.builder()
            .timestamp(timestamp)
            .build())
        .orElse(null);
  }

  private SivaProduct initSivaProduct(Product product, List<Item> items, long timestamp) {
    return Optional.ofNullable(product)
        .filter(prd -> !ModuleProductUtil.isProductAndItemsExists(prd,items))
        .map(Product::getProductSku)
        .map(sivaProductService::getExistingSivaProduct)
        .map(sivaProduct -> {
          sivaProduct.setTimestamp(timestamp);
          return sivaProduct;
        })
        .orElseGet(() -> initDefaultSivaProduct(product,timestamp));
  }

  private SivaProduct toSivaProduct(Product product, List<Item> items, SaveParam saveParam) {
    return Optional.ofNullable(initSivaProduct(product,items,ParamUtil.getTimestamp(saveParam)))
        .map(result -> {
          result = constructSivaProductProductRelated(result,product);
          result = constructSivaProductItemsRelated(result,product);
          result.setId(result.toId());

          return result;
        })
        .orElse(null);
  }

  public SivaProduct constructSivaProductProductRelated(SivaProduct result, Product product) {
    //product
    result.setProductCode(ModuleProductUtil.toProductCode(product));
    result.setProductType(ModuleProductUtil.getProductTypeFromProduct(product));
    result.setMerchantCode(ModuleProductUtil.toProductMerchantCode(product));
    result.setSync(ModuleProductUtil.toProductSync(product));
    result.setReview(customProductReviewService.getExistingReviewByProduct(product));
    result.setMerchant(customMerchantService.getExistingCustomMerchantByProduct(product));
    result.setName(ModuleProductUtil.toProductName(product));
    result.setUrlName(ModuleProductUtil.toProductUrlName(product));
    result.setProductSku(ModuleProductUtil.toProductSku(product));
    result.setBrand(ModuleProductUtil.toBrand(product));
    result.setBrandLogoUrl(ModuleProductUtil.toBrandLogoUrl(product));
    result.setMasterCategories(customPcbCategoryService.getBreadCrumbMasterCategoryCodes(product));
    result.setSalesCategories(customPcbCategoryService.getBreadCrumbSalesCategoryCodes(product));
    result.setMarkForDelete(ModuleProductUtil.toProductMarkForDelete(product,itemService.isAllItemsMarkForDeleteTrue(ModuleProductUtil.toProductSku(product))));

    return result;
  }

  public SivaProduct constructSivaProductItemsRelated(SivaProduct result, Product product) {
    String productSku = Optional.ofNullable(result)
        .map(SivaProduct::getProductSku)
        .orElseGet(() -> ModuleProductUtil.toProductSku(product));

    result.setItemSkus(itemService.getItemSkusByProductSku(productSku));
    result.setBuyable(ModuleProductUtil.getDefaultViewSchedule());
    result.setDiscoverable(ModuleProductUtil.getDefaultViewSchedule());
    result.setArchived(itemService.isAllItemsArchivedTrue(productSku));
    result.setMarkForDelete(ModuleProductUtil.toProductMarkForDelete(product,itemService.isAllItemsMarkForDeleteTrue(productSku)));

    List<Item> itemsForProductCampaignInfos = itemService.getItemsByProductSku(productSku);
    Map<String, CampaignInfo> productCampaignInfos = mainConstructor.toCampaignInfos(product,itemsForProductCampaignInfos,null,true);
    CampaignInfo campaignInfo = productCampaignInfos.get(ModuleProductUtil.toDefaultCampaignInfoKey(productCampaignInfos, pickupPointProperties.isSortByInStock()));

    boolean hasCncActivated = pickupPointService.hasCncActivatedByProductSku(productSku);
    boolean hasBuyableCnc = pickupPointService.hasBuyableCncByProductSku(productSku);
    boolean hasDiscoverableCnc = pickupPointService.hasDiscoverableCncByProductSku(productSku);
    boolean hasBuyable = pickupPointService.hasBuyableByProductSku(productSku);
    boolean hasDiscoverable = pickupPointService.hasDiscoverableByProductSku(productSku);

    if (Objects.nonNull(campaignInfo)) {
      SivaProduct.FinalPrice offlinePrice = ModuleProductUtil.toFinalPrice(campaignInfo,true);
      SivaProduct.FinalPrice price = MainUtil.getOrDefault(ModuleProductUtil.toFinalPrice(campaignInfo,false),offlinePrice);

      result.setMeasurement(mainConstructor.toMeasurement(ModuleProductUtil.getItem(itemsForProductCampaignInfos, ModuleProductUtil.getCheapestItemSkuFinalPrice(price)),product));
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
      result.setFbb(mainConstructor.toFbb(product,itemsForProductCampaignInfos,result.getFbb(),PricePriority.LOWEST.equals(priceProperties.getFbbPriority())));
      result.setNonFbb(mainConstructor.toNonFbb(product,itemsForProductCampaignInfos,PricePriority.LOWEST.equals(priceProperties.getNonFbbPriority())));
    }
    result.setTags(ModuleProductUtil.toTags(itemsForProductCampaignInfos,result.isFbbActivated(),hasCncActivated,result.getPurchasedType(),result.getPurchasedTypeScore(),pickupPointProperties.isOnePartyActivated()));
    result.setHasCncActivated(hasCncActivated);
    result.setOnlineOnly(!hasCncActivated);
    result.setHasBuyableCnc(hasBuyableCnc);
    result.setHasDiscoverableCnc(hasDiscoverableCnc);
    Optional.of(result).map(SivaProduct::getBuyable).ifPresent(viewSchedule -> viewSchedule.setValue(hasBuyable));
    Optional.of(result).map(SivaProduct::getDiscoverable)
        .ifPresent(viewSchedule -> viewSchedule.setValue(hasDiscoverable));
    return result;
  }

  public Mono<Boolean> joinSivaProductUsingCombinedUpsertFlow(Product product,
    SaveParam saveParam) {
    String productSku = ModuleProductUtil.toProductSku(product);
    Mono<Boolean> result = Mono.just(false);
    if (StringUtils.isNotBlank(productSku)) {
      List<Item> itemList = itemService.getItemsByProductSku(productSku);
        SivaProduct sivaProduct = toSivaProduct(product, itemList, saveParam);
        if (Objects.nonNull(sivaProduct)) {
          result = sivaProductServiceV2.publishCombinedSivaProductUpsertModel(saveParam, productSku,
            Topics.PRODUCT, sivaProduct);
      }
    }
    return result;
  }
}
