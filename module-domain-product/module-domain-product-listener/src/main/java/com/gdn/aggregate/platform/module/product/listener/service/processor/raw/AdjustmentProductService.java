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
import java.util.stream.Stream;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.AdjustmentProductRepository;
import reactor.core.publisher.Mono;

@Component
public class AdjustmentProductService {

  private static final String SAVE_COMMAND = "saveAdjustmentProduct";

  @Autowired
  private DBService dbService;

  @Autowired
  private AdjustmentProductRepository adjustmentProductRepository;

  @Autowired
  private TimeService timeService;

  @Autowired
  private AdjustmentProductQuotaService adjustmentProductQuotaService;

  /*Save*/
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

  public void setMandatory(AdjustmentProduct adjustmentProduct, SaveParam saveParam) {
    Optional.ofNullable(adjustmentProduct)
        .ifPresent(val -> {
          val = ModuleProductUtil.generateAdjMandatoryData(val);
          setValueForSaving(val,saveParam);
          val.setEndDate(getEndDateForSaving(val));
          val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.toAdjProductEndDate(val),MainUtil.toFriendlyClassName(val)));
        });
  }

  public void setValueForSaving(AdjustmentProduct newData, SaveParam saveParam) {
    Optional.ofNullable(newData)
        .map(this::getExistingAdjustmentProductWithObject)
        .ifPresent(existingData -> {
          Long initValue = MainUtil.getOrDefault(existingData.getInitValue(),newData.getValue());
          Long value = existingData.getValue();
          Set<String> initBudgetOwners = MainUtil.getOrDefault(existingData.getInitBudgetOwners(),newData.getBudgetOwners());
          Set<String> budgetOwners = existingData.getBudgetOwners();
          if (ModuleProductUtil.isNewBudgetOwnersMoreComplete(existingData,newData,ParamUtil.isChain(saveParam))) {
            initValue = newData.getValue();
            value = newData.getValue();
            initBudgetOwners = newData.getBudgetOwners();
            budgetOwners = newData.getBudgetOwners();
          } else {
            String quotaStatus = ModuleProductUtil.toAdjQuotaStatus(adjustmentProductQuotaService.toQuotaAdjustments(newData.getItemSku(),newData.getPickupPointCode(),newData.getCampaignCode()));
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

  public Long getEndDateForSaving(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .filter(val -> ModuleProductUtil.isItSameTime(val.getStartDate(),val.getEndDate()))
        .map(this::getExistingAdjustmentProductWithObject)
        .map(AdjustmentProduct::getEndDate)
        .orElseGet(() -> ModuleProductUtil.toAdjProductEndDate(adjustmentProduct));
  }
  /*End of Save*/

  /*Getters*/
  public AdjustmentProduct getExistingAdjustmentProduct(String id) {
    return Optional.ofNullable(id)
        .flatMap(adjustmentProductRepository::findById)
        .orElse(null);
  }

  public AdjustmentProduct getExistingAdjustmentProductWithObject(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(AdjustmentProduct::toId)
        .map(this::getExistingAdjustmentProduct)
        .orElse(null);
  }

  private Set<String> getNotEndedCampaignCodesByL4(String itemSku, Long currentDate) {
    return Optional.ofNullable(itemSku)
        .map(l4Id -> {
          try(Stream<AdjustmentProduct> adjustmentProductStream = adjustmentProductRepository.getNotEndedCampaignCodesByItemSku(itemSku,currentDate)) {
            return adjustmentProductStream
                .filter(Objects::nonNull)
                .map(AdjustmentProduct::getCampaignCode)
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
          try(Stream<AdjustmentProduct> adjustmentProductStream = adjustmentProductRepository.getNotEndedCampaignCodesByItemSkuAndPickupPointCode(itemSku,pickupPointCode,currentDate)) {
            return adjustmentProductStream
                .filter(Objects::nonNull)
                .map(AdjustmentProduct::getCampaignCode)
                .collect(Collectors.toSet());
          }
        })
        .orElseGet(HashSet::new);
  }

  public List<AdjustmentProduct> getNotEndedAdjustmentProductsByL5(String itemSku, String pickupPointCode) {
    Long currentDate = ModuleProductUtil.getCurrentTimestamp();
    return Optional.ofNullable(ModuleProductUtil.toPickupPointId(itemSku,pickupPointCode))
        .map(l5Id -> getNotEndedCampaignCodesByL5(itemSku,pickupPointCode,currentDate)
            .stream()
            .map(cpgCode -> adjustmentProductRepository.findFirstByItemSkuAndPickupPointCodeAndCampaignCodeAndEndDateAfterOrderByPriorityAscValueDesc(itemSku,pickupPointCode,cpgCode,currentDate))
            .filter(Objects::nonNull)
            .map(ModuleProductUtil::toCleanAdjustmentProduct)
            .collect(Collectors.toList()))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(ArrayList::new);
  }

  private AdjustmentProduct getMaximumActiveAdjustmentProductByL5(String itemSku, String pickupPointCode, boolean promoCampaign, Long currentDate) {
    return Optional.ofNullable(itemSku)
        .map(itmSku -> adjustmentProductRepository.findFirstByItemSkuAndPickupPointCodeAndActivatedTrueAndPromoCampaignAndStartDateBeforeAndEndDateAfterOrderByPriorityAscValueDesc(itmSku,pickupPointCode,promoCampaign,currentDate,currentDate))
        .map(ModuleProductUtil::toCleanAdjustmentProduct)
        .orElse(null);
  }

  private List<AdjustmentProduct> getActiveNonCampaignAdjustmentProductsByL5(String itemSku, String pickupPointCode, boolean promoCampaign, Long currentDate) {
    return Optional.ofNullable(itemSku)
        .map(itmSku -> {
          try(Stream<AdjustmentProduct> adjustmentProductStream = adjustmentProductRepository.streamAllByItemSkuAndPickupPointCodeAndActivatedTrueAndPromoCampaignAndStartDateBeforeAndEndDateAfterOrderByPriorityAscValueDesc(itmSku,pickupPointCode,promoCampaign,currentDate,currentDate)) {
            return adjustmentProductStream
                .filter(Objects::nonNull)
                .map(ModuleProductUtil::toCleanAdjustmentProduct)
                .sorted(Comparator.comparing(adjPrd -> MainUtil.toNotNullString(adjPrd.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public AdjustmentProduct getActiveAdjustmentProductByL5(String itemSku, String pickupPointCode) {
    Long currentDate = ModuleProductUtil.getCurrentTimestamp();
    return Optional.ofNullable(getMaximumActiveAdjustmentProductByL5(itemSku,pickupPointCode,true,currentDate))
        .map(campaignAdjustment -> ModuleProductUtil.combineCampaignAndNonCampaignAdjustment(campaignAdjustment,getActiveNonCampaignAdjustmentProductsByL5(itemSku,pickupPointCode,false,currentDate)))
        .orElseGet(() -> getMaximumActiveAdjustmentProductByL5(itemSku,pickupPointCode,false,currentDate));
  }
  /*End of Getters*/

}
