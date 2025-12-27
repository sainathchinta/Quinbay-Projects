package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.FlashsaleProductRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
public class FlashsaleProductService {

  private static final String SAVE_COMMAND = "saveFlashsaleProduct";

  @Autowired
  private DBService dbService;

  @Autowired
  private FlashsaleProductRepository flashsaleProductRepository;

  @Autowired
  private TimeService timeService;

  /*Save*/
  public Mono<Boolean> save(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.FLASHSALE_PRODUCT)
            .domain(flashsaleProduct)
            .clazz(FlashsaleProduct.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .flatMap(saveResult -> delete(flashsaleProduct))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(flashsaleProduct,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(flashsaleProduct,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public Mono<Boolean> delete(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getProductSku)
        .filter(flashsaleProductRepository::existsByProductSkuAndItemSkuNotNull)
        .map(flashsaleProductRepository::findFirstByProductSkuAndItemSkuNull)
        .map(FlashsaleProduct::toId)
        .map(id -> DeleteRequest.builder()
            .index(Collections.FLASHSALE_PRODUCT)
            .id(id)
            .mongo(true)
            .elasticsearch(false)
            .build())
        .map(deleteRequest -> dbService.delete(deleteRequest))
        .orElseGet(MainUtil::successResult);
  }

  public void setMandatory(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    Optional.ofNullable(flashsaleProduct)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setProductType(ModuleProductUtil.getProductTypeFromFlashsaleProduct(val));
          val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.getFlashsaleProductEnd(val),MainUtil.toFriendlyClassName(val)));
        });
  }
  /*End of Save*/

  /*Getters*/
  public FlashsaleProduct getNearestActiveFlashsaleProductByProductSkuAndTimeBased(String productSku, String itemSku, String campaignCode, boolean timeBased, Long start, Long end, Long currentTime) {
    return Optional.ofNullable(productSku)
        .map(prdSku -> {
          try(Stream<FlashsaleProduct> flashsaleProductStream = flashsaleProductRepository.streamAllByProductSkuAndSchedule_TimeBasedAndSchedule_EndAfter(prdSku,timeBased,currentTime)) {
            return flashsaleProductStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(item -> MainUtil.toNotNullString(item.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new)
        .stream()
        .filter(val -> ModuleProductUtil.filterNearestActiveFlashsaleProductByCampaignCode(val, ModuleProductUtil.toRealCampaignCode(campaignCode)))
        .filter(val -> ModuleProductUtil.filterNearestActiveFlashsaleProductByItemSku(val,itemSku))
        .min(Comparator
            .<FlashsaleProduct,Boolean>comparing(ModuleProductUtil::isFlashsaleProductActive, Comparator.reverseOrder())
            .thenComparing(val -> MainUtil.isLongEqual(ModuleProductUtil.getFlashsaleProductStart(val),start), Comparator.reverseOrder())
            .thenComparing(val -> MainUtil.isLongEqual(ModuleProductUtil.getFlashsaleProductEnd(val),end), Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::getFlashsaleProductStart)
            .thenComparing(ModuleProductUtil::getFlashsaleProductEnd)
            .thenComparing(ModuleProductUtil::getFlashsaleExclusive, Comparator.reverseOrder()))
        .orElse(null);
  }

  public boolean isFlashsaleProductExclusive(CampaignProduct campaignProduct) {
    return Optional.ofNullable(getNearestActiveFlashsaleProductByProductSkuAndTimeBased(ModuleProductUtil.toCampaignProductSku(campaignProduct), ModuleProductUtil.toCampaignItemSku(campaignProduct), ModuleProductUtil.toCampaignCampaignCode(campaignProduct), ModuleProductUtil.toCampaignTimeBased(campaignProduct), ModuleProductUtil.toCampaignPromotionStartTime(campaignProduct), ModuleProductUtil.toCampaignPromotionEndTime(campaignProduct), ModuleProductUtil.getCurrentTimestamp()))
        .map(FlashsaleProduct::isExclusive)
        .orElseGet(() -> false);
  }

  private Set<String> getNotEndedCampaignCodesByL4(String itemSku, Long currentDate) {
    return Optional.ofNullable(itemSku)
        .map(l4Id -> {
          try(Stream<FlashsaleProduct> flashsaleProductStream = flashsaleProductRepository.getNotEndedCampaignCodesByItemSku(itemSku,currentDate)) {
            return flashsaleProductStream
                .filter(Objects::nonNull)
                .map(FlashsaleProduct::getCampaignCode)
                .collect(Collectors.toSet());
          }
        })
        .orElseGet(HashSet::new);
  }

  public Set<String> getNotEndedCampaignCodesByMultipleL4(Set<String> itemSkus) {
    Long currentDate = MainUtil.getCurrentTimestamp();
    return Optional.ofNullable(itemSkus)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(itemSku -> getNotEndedCampaignCodesByL4(itemSku,currentDate))
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream)
        .collect(Collectors.toSet());
  }

  private Set<String> getNotEndedCampaignCodesByL5(String itemSku, String pickupPointCode, Long currentDate) {
    return Optional.ofNullable(ModuleProductUtil.toPickupPointId(itemSku,pickupPointCode))
        .map(l5Id -> {
          try(Stream<FlashsaleProduct> flashsaleProductStream = flashsaleProductRepository.getNotEndedCampaignCodesByItemSkuAndPickupPointCode(itemSku,pickupPointCode,currentDate)) {
            return flashsaleProductStream
                .filter(Objects::nonNull)
                .map(FlashsaleProduct::getCampaignCode)
                .collect(Collectors.toSet());
          }
        })
        .orElseGet(HashSet::new);
  }

  public List<AdjustmentProduct> getNotEndedAdjustmentProductsByL5(String itemSku, String pickupPointCode) {
    Long currentDate = MainUtil.getCurrentTimestamp();
    return Optional.ofNullable(ModuleProductUtil.toPickupPointId(itemSku,pickupPointCode))
        .map(l5Id -> getNotEndedCampaignCodesByL5(itemSku,pickupPointCode,currentDate)
            .stream()
            .map(cpgCode -> flashsaleProductRepository.findFirstByItemSkuAndCampaignCodeAndSchedule_EndAfterOrderBySchedule_StartAsc(itemSku,cpgCode,currentDate))
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::fromFlashsaleProductToAdjustmentProduct)
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::toCleanAdjustmentProduct)
            .collect(Collectors.toList()))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(ArrayList::new);
  }
  /*End of Getters*/

}
