package com.gdn.aggregate.platform.module.product.listener.service.joinner.updater;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.raw.*;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Flashsale;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.properties.PickupPointProperties;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.MainConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaProductConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaProductConstructorV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveProcessedService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomMerchantService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.custom.CustomProductReviewService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Component("ProductSivaProductUpdater")
public class SivaProductUpdater {
  /*Raw*/
  @Autowired
  private ProductService productService;
  @Autowired
  private ItemService itemService;
  @Autowired
  private PickupPointService pickupPointService;

  /*Custom*/
  @Autowired
  private CustomMerchantService customMerchantService;
  @Autowired
  private CustomProductReviewService customProductReviewService;

  /*Raw*/
  @Autowired
  private AdjustmentProductQuotaService adjustmentProductQuotaService;

  /*Processed*/
  @Autowired
  private SivaProductService sivaProductService;
  @Autowired
  private SaveProcessedService saveProcessedService;

  /*Helper*/
  @Autowired
  private MainConstructor mainConstructor;
  @Autowired
  private SivaProductConstructor sivaProductConstructor;

  /*Properties*/
  @Autowired
  private PickupPointProperties pickupPointProperties;

  @Value("${module.domain.product.siva.product.combined.upsert}")
  private boolean sivaProductCombinedUpsertEnabled;

  @Autowired
  private SivaProductServiceV2 sivaProductServiceV2;

  @Autowired
  private SivaProductConstructorV2 sivaProductConstructorV2;

  @Autowired
  private PublisherService publisherService;

  @Value("${new.master.data.flow.enabled}")
  private boolean newMasterDataFlowEnabled;


  /*DirectUpdate*/
  public Mono<Boolean> directUpdateByProductReviewChangeEvent(ProductReviewChangeEvent productReviewChangeEvent, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    CustomProductReview productReview = customProductReviewService.getExistingCustomProductReview(productReviewChangeEvent.getId());
    String productSku = ModuleProductUtil.toReviewId(productReview);
    Review review = ModuleProductUtil.toReview(productReview);

    SivaProduct sivaProduct = Optional.ofNullable(productSku)
        .map(sivaProductService::getExistingSivaProduct)
        .map(svProduct -> {
          svProduct.setTimestamp(productReview.getTimestamp());
          svProduct.setReview(review);
          return svProduct;
        })
        .orElse(null);

    return saveProcessedService.saveSivaProduct(sivaProduct,saveParam);
  }

  public Mono<Boolean> directUpdateByStoreClosingMerchantChangeEvent(StoreClosingMerchantChangeEvent storeClosingMerchantChangeEvent, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    CustomMerchant merchant = customMerchantService.getExistingCustomMerchant(storeClosingMerchantChangeEvent.getId());
    List<String> productSkus = productService.getProductSkusByMerchantCode(merchant.getId());

    List<SivaProduct> sivaProducts = productSkus.stream()
        .map(sivaProductService::getExistingSivaProduct)
        .filter(Objects::nonNull)
        .map(svProduct -> {
          svProduct.setTimestamp(storeClosingMerchantChangeEvent.getTimestamp());
          svProduct.setMerchant(merchant);
          return svProduct;
        })
        .collect(Collectors.toList());

    return saveProcessedService.saveSivaProducts(sivaProducts,saveParam);
  }

  public Mono<Boolean> directUpdateByMasterData(MasterData masterData, List<Product> products, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);

    List<SivaProduct> sivaProducts = products.stream()
        .filter(Objects::nonNull)
        .map(prd -> {
          String productSku = prd.getProductSku();

          return Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku))
              .map(svProduct -> {
                svProduct.setTimestamp(masterData.getTimestamp());
                svProduct = sivaProductConstructor.constructSivaProductProductRelated(svProduct,prd);
                svProduct = sivaProductConstructor.constructSivaProductItemsRelated(svProduct,prd);
                return svProduct;
              })
              .orElse(null);
        })
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
    if (newMasterDataFlowEnabled) {
      return Flux.fromIterable(sivaProducts).doOnNext(
          sivaProduct -> log.info("Processing SivaProduct with productSku: {} for topic: {}",
              sivaProduct.getProductSku(), Topics.SIVA_PRODUCT_COMBINED_UPSERT)).flatMap(
          sivaProduct -> sivaProductServiceV2.publishCombinedSivaProductUpsertModel(saveParam,
              sivaProduct.getProductSku(), Topics.SIVA_PRODUCT_COMBINED_UPSERT, sivaProduct)).all(result -> result);
    } else {
      return saveProcessedService.saveSivaProducts(sivaProducts, saveParam);
    }
  }

  public Mono<Boolean> directUpdateByProduct(Product product, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    String productSku = product.getProductSku();

    SivaProduct sivaProduct = Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku))
        .map(svProduct -> {
          svProduct.setTimestamp(product.getTimestamp());
          return sivaProductConstructor.constructSivaProductProductRelated(svProduct,product);
         })
        .orElse(null);

    if (Objects.nonNull(sivaProduct)) {
      return processExistingSivaProduct(saveParam, productSku, sivaProduct);
    } else {
      return processForMissingSivaProduct(product, saveParam);
    }
  }

  private Mono<Boolean> processForMissingSivaProduct(Product product, SaveParam saveParam) {
    Optional.of(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
    if(sivaProductCombinedUpsertEnabled){
      return sivaProductConstructor.joinSivaProductUsingCombinedUpsertFlow(product, saveParam);
    }
    return sivaProductConstructor.joinSivaProduct(product, saveParam);
  }

  private Mono<Boolean> processExistingSivaProduct(SaveParam saveParam, String productSku,
    SivaProduct sivaProduct) {
    if(sivaProductCombinedUpsertEnabled) {
      return sivaProductServiceV2.publishCombinedSivaProductUpsertModel(saveParam, productSku,
       Topics.PRODUCT, sivaProduct);
    }
    return saveProcessedService.saveSivaProduct(sivaProduct, saveParam);
  }

  public Mono<Boolean> directUpdateByItem(Item item, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    String itemSku = item.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    Product product = productService.getExistingProduct(productSku);

    SivaProduct sivaProduct = Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku))
        .map(svProduct -> {
          svProduct.setTimestamp(item.getTimestamp());
          return sivaProductConstructor.constructSivaProductItemsRelated(svProduct,product);
        })
        .orElse(null);

    if (Objects.nonNull(sivaProduct)) {
      return saveProcessedService.saveSivaProduct(sivaProduct,saveParam);
    } else {
      Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      return sivaProductConstructor.joinSivaProduct(product,saveParam);
    }
  }

  public Mono<Boolean> directUpdateByPickupPoint(PickupPoint pickupPoint, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    String itemSku = pickupPoint.getItemSku();
    String productSku = ModuleProductUtil.fromItemSkuToProductSku(itemSku);

    Product product = productService.getExistingProduct(productSku);

    SivaProduct sivaProduct = Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku))
        .map(svProduct -> {
          svProduct.setTimestamp(pickupPoint.getTimestamp());
          return sivaProductConstructor.constructSivaProductItemsRelated(svProduct,product);
        })
        .orElse(null);

    if (Objects.nonNull(sivaProduct)) {
      return saveProcessedService.saveSivaProduct(sivaProduct,saveParam);
    } else {
      Optional.ofNullable(saveParam).map(SaveParam::getDbParam).ifPresent(val -> val.setSourceClassName(null));
      return sivaProductConstructor.joinSivaProduct(product,saveParam);
    }
  }

  public Mono<Boolean> directUpdateByTag(List<Item> items, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    long timestamp = ModuleProductUtil.toFirstTimestamp(items);

    SivaProduct sivaProduct = Optional.ofNullable(ModuleProductUtil.toFirstProductSku(items))
        .map(sivaProductService::getExistingSivaProduct)
        .map(svProduct -> {
          svProduct.setTimestamp(timestamp);
          svProduct.setTags(ModuleProductUtil.toTags(items,svProduct.isFbbActivated(),svProduct.isHasCncActivated(),svProduct.getPurchasedType(),svProduct.getPurchasedTypeScore(),pickupPointProperties.isOnePartyActivated()));
          return svProduct;
        })
        .orElse(null);

    return saveProcessedService.saveSivaProduct(sivaProduct,saveParam);
  }

  public Mono<Boolean> directUpdateByFlashsaleProduct(FlashsaleProduct flashsaleProduct, SaveParam sourceSaveParam) {
    SaveParam saveParam = ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT);
    String productSku = flashsaleProduct.getProductSku();
    Long startTime = ModuleProductUtil.getFlashsaleProductStart(flashsaleProduct);
    Long endTime = ModuleProductUtil.getFlashsaleProductEnd(flashsaleProduct);
    Long currentTime = ModuleProductUtil.getCurrentTimestamp();

    SivaProduct sivaProduct = Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku))
        .map(svProduct -> {
          svProduct.setTimestamp(flashsaleProduct.getTimestamp());
          svProduct.setFlashsale(mainConstructor.toFlashsaleFromFlashsaleProduct(productSku,null,null,startTime,endTime,false,currentTime));
          if (Objects.isNull(svProduct.getFlashsale())) {
            svProduct.setSubFlashsale(mainConstructor.toFlashsaleFromFlashsaleProduct(productSku, null, null, startTime, endTime, true, currentTime));
          } else {
            svProduct.setSubFlashsale(null);
          }
          svProduct.setProductType(ModuleProductUtil.getFlashsaleType(MainUtil.getOrDefault(svProduct.getFlashsale(),svProduct.getSubFlashsale())));
          return svProduct;
        })
        .orElse(null);

    return saveProcessedService.saveSivaProduct(sivaProduct,saveParam);
  }

  public Mono<Boolean> directUpdateByAdjustmentProductQuota(AdjustmentProductQuota adjustmentProductQuota, SaveParam sourceSaveParam) {
    return Mono.fromCallable(() -> ParamUtil.toSaveParamSavingProcessed(sourceSaveParam,Topics.GROUP_ID_SIVA_PRODUCT))
        .flatMap(saveParam -> {
          String productSku = adjustmentProductQuota.getItemSku();
          String itemSku = adjustmentProductQuota.getItemSku();
          String pickupPointCode = adjustmentProductQuota.getPickupPointCode();
          String campaignCode = adjustmentProductQuota.getCampaignCode();
          SivaProduct sivaProduct = Optional.ofNullable(sivaProductService.getExistingSivaProduct(productSku))
              .map(svProduct -> {
                svProduct.setTimestamp(adjustmentProductQuota.getTimestamp());
                Flashsale flashsale = MainUtil.getOrDefault(svProduct.getFlashsale(),svProduct.getSubFlashsale());
                List<Quota> adjustments = adjustmentProductQuotaService.toQuotaAdjustments(itemSku,pickupPointCode,campaignCode);
                Quota maxAdjustment = ModuleProductUtil.findMaxAdjustment(adjustments);
                Stock inventory = ModuleProductUtil.fromQuotaToStock(maxAdjustment);
                svProduct.setFlashsaleInventory(ModuleProductUtil.toFlashsaleInventory(flashsale,adjustments,inventory));
                return svProduct;
              })
              .orElse(null);
          return saveProcessedService.saveSivaProduct(sivaProduct,saveParam);
        });
  }

}
