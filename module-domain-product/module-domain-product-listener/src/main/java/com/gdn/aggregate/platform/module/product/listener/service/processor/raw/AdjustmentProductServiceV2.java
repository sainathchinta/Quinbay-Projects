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

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.AdjustmentProductRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import reactor.core.publisher.Mono;

@Component
public class AdjustmentProductServiceV2 {
  private static final String SAVE_COMMAND = "saveAdjustmentProduct";

  @Autowired
  private AdjustmentProductRepository adjustmentProductRepository;

  @Autowired
  private AdjustmentProductQuotaServiceV2 adjustmentProductQuotaService;

  @Autowired
  private TimeService timeService;

  @Autowired
  private DBService dbService;

  public List<AdjustmentProduct> findAllByItemSkuIn(Set<String> itemSkus) {
    return adjustmentProductRepository.findByItemSkuIn(itemSkus);
  }

  public Optional<AdjustmentProduct> findById(String id) {
    return adjustmentProductRepository.findById(id);
  }

  public Mono<Boolean> save(AdjustmentProduct adjustmentProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.ADJUSTMENT_PRODUCT)
            .domain(adjustmentProduct)
            .clazz(AdjustmentProduct.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(adjustmentProduct,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(adjustmentProduct,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public List<AdjustmentProduct> getNotEndedAdjustmentProductsByL5(String itemSku, String pickupPointCode, List<AdjustmentProduct> allAdjustmentProducts) {
    Long currentDate = ModuleProductUtil.getCurrentTimestamp();
    return Optional.ofNullable(ModuleProductUtil.toPickupPointId(itemSku,pickupPointCode))
        .map(l5Id -> getNotEndedCampaignCodesByL5(itemSku,pickupPointCode,currentDate,allAdjustmentProducts)
            .stream()
            .map(cpgCode -> findFirstByItemSkuAndPickupPointCodeAndCampaignCodeAndEndDateAfterOrderByPriorityAscValueDesc(itemSku,pickupPointCode,cpgCode,currentDate, allAdjustmentProducts))
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::toCleanAdjustmentProduct)
            .collect(Collectors.toList()))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(ArrayList::new);
  }

  public Set<String> getNotEndedCampaignCodesByMultipleL4(Set<String> itemSkus, List<AdjustmentProduct> allAdjustmentProducts) {
    Long currentDate = MainUtil.getCurrentTimestamp();
    return Optional.ofNullable(itemSkus)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(itemSku -> getNotEndedCampaignCodesByL4(itemSku, currentDate, allAdjustmentProducts))
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream)
        .collect(Collectors.toSet());
  }

  public AdjustmentProduct getActiveAdjustmentProductByL5(String itemSku, String pickupPointCode, List<AdjustmentProduct> allAdjustmentProduct) {
    Long currentDate = ModuleProductUtil.getCurrentTimestamp();
    return Optional.ofNullable(getMaximumActiveAdjustmentProductByL5(itemSku,pickupPointCode,true, currentDate, allAdjustmentProduct))
        .map(campaignAdjustment -> ModuleProductUtil.combineCampaignAndNonCampaignAdjustment(campaignAdjustment,getActiveNonCampaignAdjustmentProductsByL5(itemSku,pickupPointCode,false, currentDate, allAdjustmentProduct)))
        .orElseGet(() -> getMaximumActiveAdjustmentProductByL5(itemSku,pickupPointCode,false, currentDate, allAdjustmentProduct));
  }

  public void setMandatory(AdjustmentProduct adjustmentProduct, SaveParam saveParam, List<AdjustmentProduct> allAdjustmentProduct,  List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    Optional.ofNullable(adjustmentProduct)
        .ifPresent(val -> {
          val = ModuleProductUtil.generateAdjMandatoryData(val);
          setValueForSaving(val,saveParam, allAdjustmentProduct, allAdjustmentProductQuotas);
          val.setEndDate(getEndDateForSaving(val, allAdjustmentProduct));
          val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.toAdjProductEndDate(val),MainUtil.toFriendlyClassName(val)));
        });
  }

  private Long getEndDateForSaving(AdjustmentProduct adjustmentProduct, List<AdjustmentProduct> allAdjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .filter(val -> ModuleProductUtil.isItSameTime(val.getStartDate(),val.getEndDate()))
        .map(val -> getExistingAdjustmentProductWithObject(val, allAdjustmentProduct))
        .map(AdjustmentProduct::getEndDate)
        .orElseGet(() -> ModuleProductUtil.toAdjProductEndDate(adjustmentProduct));
  }

  private void setValueForSaving(AdjustmentProduct newData, SaveParam saveParam, List<AdjustmentProduct> allAdjustmentProduct,  List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    Optional.ofNullable(newData)
        .map(adjustmentProduct -> getExistingAdjustmentProductWithObject(adjustmentProduct, allAdjustmentProduct))
        .ifPresent(existingData -> {
          Long initValue = MainUtil.getOrDefault(existingData.getInitValue(),newData.getValue());
          Long value = existingData.getValue();
          Set<String> initBudgetOwners = MainUtil.getOrDefault(existingData.getInitBudgetOwners(),newData.getBudgetOwners());
          Set<String> budgetOwners = existingData.getBudgetOwners();
          if (ModuleProductUtil.isNewBudgetOwnersMoreComplete(existingData,newData, ParamUtil.isChain(saveParam))) {
            initValue = newData.getValue();
            value = newData.getValue();
            initBudgetOwners = newData.getBudgetOwners();
            budgetOwners = newData.getBudgetOwners();
          } else {
            String quotaStatus = ModuleProductUtil.toAdjQuotaStatus(adjustmentProductQuotaService.toQuotaAdjustments(newData.getItemSku(),newData.getPickupPointCode(),newData.getCampaignCode(), allAdjustmentProductQuotas));
            if (StockStatus.AVAILABLE.equals(quotaStatus)) {
              value = newData.getValue();
              budgetOwners = newData.getBudgetOwners();
            } else {
              value = MainUtil.getOrDefault(existingData.getInitValue(),existingData.getValue());
              budgetOwners = MainUtil.getOrDefault(existingData.getInitBudgetOwners(),existingData.getBudgetOwners());
            }
          }
          newData.setInitValue(initValue);
          newData.setValue(value);
          newData.setInitBudgetOwners(initBudgetOwners);
          newData.setBudgetOwners(budgetOwners);
        });
    Optional.ofNullable(newData)
        .filter(val -> Objects.isNull(val.getInitValue()))
        .ifPresent(val -> val.setInitValue(val.getValue()));
    Optional.ofNullable(newData)
        .filter(val -> Objects.isNull(val.getInitBudgetOwners()))
        .ifPresent(val -> val.setInitBudgetOwners(val.getBudgetOwners()));
  }

  private AdjustmentProduct getExistingAdjustmentProductWithObject(AdjustmentProduct adjustmentProduct, List<AdjustmentProduct> allAdjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(AdjustmentProduct::toId)
        .map(id -> getExistingAdjustmentProduct(id, allAdjustmentProduct))
        .orElse(null);
  }

  private AdjustmentProduct getExistingAdjustmentProduct(String id, List<AdjustmentProduct> allAdjustmentProduct) {
    return Optional.ofNullable(allAdjustmentProduct).orElseGet(ArrayList::new)
        .stream()
        .filter(adjustmentProduct -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getId()))
        .findFirst()
        .orElse(null);
  }

  private AdjustmentProduct getMaximumActiveAdjustmentProductByL5(String itemSku, String pickupPointCode, boolean promoCampaign, Long currentDate, List<AdjustmentProduct> allAdjustmentProduct) {
    return Optional.ofNullable(itemSku).map(
            itmSku -> getActiveNonCampaignAdjustmentProductsByL5(itmSku, pickupPointCode, promoCampaign, currentDate,
                allAdjustmentProduct)).flatMap(adjustmentProducts -> adjustmentProducts.stream().findFirst())
        .map(ModuleProductUtil::toCleanAdjustmentProduct)
        .orElse(null);
  }

  private List<AdjustmentProduct> getActiveNonCampaignAdjustmentProductsByL5(String itemSku, String pickupPointCode, boolean promoCampaign, Long currentDate, List<AdjustmentProduct> allAdjustmentProduct) {
   //streamAllByItemSkuAndPickupPointCodeAndActivatedTrueAndPromoCampaignAndStartDateBeforeAndEndDateAfterOrderByPriorityAscValueDesc
    return Optional.ofNullable(allAdjustmentProduct).orElseGet(ArrayList::new)
        .stream()
        .filter(adjustmentProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getItemSku()))
        .filter(adjustmentProduct -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getPickupPointCode()))
        .filter(adjustmentProduct -> Boolean.TRUE.equals(adjustmentProduct.isActivated()))
        .filter(adjustmentProduct -> promoCampaign == adjustmentProduct.isPromoCampaign())
        .filter(adjustmentProduct -> currentDate > Optional.ofNullable(adjustmentProduct.getStartDate()).orElse(Long.MAX_VALUE))
        .filter(adjustmentProduct -> currentDate < Optional.ofNullable(adjustmentProduct.getEndDate()).orElse(Long.MIN_VALUE))
        .sorted(Comparator.comparing(AdjustmentProduct::isActivated).thenComparing(Comparator.comparing(AdjustmentProduct::getValue).reversed()))
        .map(ModuleProductUtil::toCleanAdjustmentProduct)
        .sorted(Comparator.comparing(adjPrd -> MainUtil.toNotNullString(adjPrd.toId())))
        .collect(Collectors.toList());
  }

  private Set<String> getNotEndedCampaignCodesByL4(String itemSku, Long currentDate, List<AdjustmentProduct> allAdjustmentProducts) {
    return  Optional.ofNullable(allAdjustmentProducts).orElseGet(ArrayList::new)
        .stream()
        .filter(adjustmentProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getItemSku()))
        .filter(adjustmentProduct -> Optional.ofNullable(adjustmentProduct.getEndDate()).orElse(Long.MIN_VALUE) > currentDate)
        .filter(Objects::nonNull)
        .map(AdjustmentProduct::getCampaignCode)
        .collect(Collectors.toSet());
  }

  private AdjustmentProduct findFirstByItemSkuAndPickupPointCodeAndCampaignCodeAndEndDateAfterOrderByPriorityAscValueDesc(String itemSku, String pickupPointCode, String campaignCode, Long currentDate, List<AdjustmentProduct> allAdjustmentProducts) {
    return Optional.ofNullable(allAdjustmentProducts).orElseGet(ArrayList::new)
        .stream()
        .filter(adjustmentProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getItemSku()))
        .filter(adjustmentProduct -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getPickupPointCode()))
        .filter(adjustmentProduct -> Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getCampaignCode()))
        .filter(adjustmentProduct -> Optional.ofNullable(adjustmentProduct.getEndDate()).orElse(Long.MIN_VALUE) > currentDate)
        .sorted(Comparator.comparing(AdjustmentProduct::getPriority).thenComparing(Comparator.comparing(AdjustmentProduct::getValue).reversed()))
        .findFirst()
        .orElse(null);
  }

  private Set<String> getNotEndedCampaignCodesByL5(String itemSku, String pickupPointCode, Long currentDate, List<AdjustmentProduct> allAdjustmentProducts) {
   return Optional.ofNullable(allAdjustmentProducts).orElseGet(ArrayList::new)
       .stream()
       .filter(adjustmentProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getItemSku()))
       .filter(adjustmentProduct -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY).equals(adjustmentProduct.getPickupPointCode()))
       .filter(adjustmentProduct -> Optional.ofNullable(adjustmentProduct.getEndDate()).orElse(Long.MIN_VALUE) > currentDate)
       .filter(Objects::nonNull)
       .map(AdjustmentProduct::getCampaignCode)
       .collect(Collectors.toSet());
  }
}
