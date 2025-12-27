package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.validation.Validation;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.CampaignProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.CheapestPriceDayRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class CheapestPriceDayService {

  private static final String SAVE_COMMAND = "saveCheapestPriceDay";

  @Autowired
  private DBService dbService;

  @Autowired
  private CheapestPriceDayRepository cheapestPriceDayRepository;

  @Autowired
  private CampaignProductRepository campaignProductRepository;

  public Mono<Boolean> save(CheapestPriceDay cheapestPriceDay, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.CHEAPEST_PRICE_DAY)
            .domain(cheapestPriceDay)
            .clazz(CheapestPriceDay.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(cheapestPriceDay,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(cheapestPriceDay,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(CheapestPriceDay cheapestPriceDay, SaveParam saveParam) {
    Optional.ofNullable(cheapestPriceDay)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setExpiryTime(getExpiryTimeForSaving(val));
        });
  }

  private Long getExpiryTimeForSaving(CheapestPriceDay cheapestPriceDay) {
    return Optional.ofNullable(cheapestPriceDay)
        .map(Validation::toId)
        .flatMap(campaignProductRepository::findById)
        .map(CampaignProduct::getExpiryTime)
        .orElse(null);
  }

  public CheapestPriceDay getExistingCheapestPriceDay(String id) {
    return Optional.ofNullable(id)
        .flatMap(cheapestPriceDayRepository::findById)
        .orElse(null);
  }

  public List<CheapestPriceDay> fromCheapestPriceToCheapestPriceDays(CheapestPrice cheapestPrice) {
    return Optional.ofNullable(cheapestPrice)
        .map(CheapestPrice::getSkuCheapestPriceDetails)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> CheapestPriceDay.builder()
            .timestamp(cheapestPrice.getTimestamp())
            .id(ModuleProductUtil.toCheapestPriceDayId(cheapestPrice.getCampaignCode(),val.getItemSku(),val.getPickupPointCode()))
            .campaignCode(cheapestPrice.getCampaignCode())
            .productSku(ModuleProductUtil.fromItemSkuToProductSku(val.getItemSku()))
            .itemSku(val.getItemSku())
            .pickupPointCode(val.getPickupPointCode())
            .days(val.getCheapestPriceDays())
            .build())
        .collect(Collectors.toList());
  }

}
