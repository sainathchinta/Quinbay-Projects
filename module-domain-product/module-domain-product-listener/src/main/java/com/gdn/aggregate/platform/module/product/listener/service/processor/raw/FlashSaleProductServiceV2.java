package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.FlashsaleProductRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component
public class FlashSaleProductServiceV2 {

  @Autowired
  private FlashsaleProductRepository flashsaleProductRepository;

  public List<FlashsaleProduct> findAllByProductSku(String productSku) {
    return flashsaleProductRepository.findAllByProductSku(productSku);
  }

  public FlashsaleProduct getNearestActiveFlashsaleProductByProductSkuAndTimeBased(String productSku, String itemSku, String campaignCode, boolean timeBased, Long start, Long end, Long currentTime, List<FlashsaleProduct> allFlashSaleProducts) {
    return
        Optional.ofNullable(allFlashSaleProducts).orElseGet(ArrayList::new)
            .stream()
            .filter(flashsaleProduct -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(flashsaleProduct.getProductSku()))
            .filter(flashsaleProduct -> timeBased == Optional.ofNullable(flashsaleProduct.getSchedule()).orElseGet(
                SivaFlashsaleSchedule::new).isTimeBased())
            .filter(flashsaleProduct -> currentTime < Optional.ofNullable(flashsaleProduct.getSchedule()).orElseGet(
                SivaFlashsaleSchedule::new).getEnd())
            .filter(Objects::nonNull)
            .sorted(Comparator.comparing(item -> MainUtil.toNotNullString(item.toId())))
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

  public List<AdjustmentProduct> getNotEndedAdjustmentProductsByL5(String itemSku, String pickupPointCode, List<FlashsaleProduct> allFlashSaleProducts) {
    Long currentDate = MainUtil.getCurrentTimestamp();
    return Optional.ofNullable(ModuleProductUtil.toPickupPointId(itemSku,pickupPointCode))
        .map(l5Id -> getNotEndedCampaignCodesByL5(itemSku,pickupPointCode,currentDate, allFlashSaleProducts)
            .stream()
            .map(cpgCode -> findFirstByItemSkuAndCampaignCodeAndSchedule_EndAfterOrderBySchedule_StartAsc(itemSku,cpgCode,currentDate,allFlashSaleProducts))
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::fromFlashsaleProductToAdjustmentProduct)
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::toCleanAdjustmentProduct)
            .collect(Collectors.toList()))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(ArrayList::new);
  }

  public Set<String> getNotEndedCampaignCodesByMultipleL4(Set<String> itemSkus,List<FlashsaleProduct> allFlashSaleProducts) {
    Long currentDate = MainUtil.getCurrentTimestamp();
    return Optional.ofNullable(itemSkus)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(itemSku -> getNotEndedCampaignCodesByL4(itemSku,currentDate,allFlashSaleProducts))
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream)
        .collect(Collectors.toSet());
  }

  private Set<String> getNotEndedCampaignCodesByL4(String itemSku, Long currentDate, List<FlashsaleProduct> allFlashSaleProducts) {
    return Optional.ofNullable(allFlashSaleProducts).orElseGet(ArrayList::new)
        .stream()
        .filter(flashsaleProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(flashsaleProduct.getItemSku()))
        .filter(flashsaleProduct -> Optional.ofNullable(flashsaleProduct.getSchedule()).map(SivaFlashsaleSchedule::getEnd).orElse(0L) > currentDate)
        .filter(Objects::nonNull)
        .map(FlashsaleProduct::getCampaignCode)
        .collect(Collectors.toSet());
  }

  private FlashsaleProduct findFirstByItemSkuAndCampaignCodeAndSchedule_EndAfterOrderBySchedule_StartAsc(String itemSku, String campaignCode, Long currentDate, List<FlashsaleProduct> allFlashSaleProducts) {
    return Optional.ofNullable(allFlashSaleProducts).orElseGet(ArrayList::new)
        .stream()
        .filter(flashsaleProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(flashsaleProduct.getItemSku()))
        .filter(flashsaleProduct -> Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY).equals(flashsaleProduct.getCampaignCode()))
        .filter(flashsaleProduct -> Optional.ofNullable(flashsaleProduct.getSchedule()).map(SivaFlashsaleSchedule::getEnd).orElse(0L) < currentDate)
        .sorted(Comparator.comparing(flashsaleProduct -> Optional.ofNullable(flashsaleProduct.getSchedule()).map(SivaFlashsaleSchedule::getStart).orElse(Long.MIN_VALUE)))
        .findFirst()
        .orElse(null);
  }

  private Set<String> getNotEndedCampaignCodesByL5(String itemSku, String pickupPointCode, Long currentDate, List<FlashsaleProduct> allFlashSaleProducts) {
   return Optional.ofNullable(allFlashSaleProducts).orElseGet(ArrayList::new)
       .stream()
       .filter(flashsaleProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(flashsaleProduct.getItemSku()))
       .filter(flashsaleProduct -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY).equals(flashsaleProduct.getPickupPointCode()))
       .filter(flashsaleProduct -> Optional.ofNullable(flashsaleProduct.getSchedule()).map(SivaFlashsaleSchedule::getEnd).orElse(0L) < currentDate)
       .filter(Objects::nonNull)
       .map(FlashsaleProduct::getCampaignCode)
       .collect(Collectors.toSet());
  }

}
