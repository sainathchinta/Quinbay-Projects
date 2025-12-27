package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MerchantDiscountPriceRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;

@Component
public class MerchantDiscountPriceService {

  private static final String SAVE_COMMAND = "saveMerchantDiscountPrice";

  @Autowired
  private DBService dbService;

  @Autowired
  private MerchantDiscountPriceRepository merchantDiscountPriceRepository;

  @Autowired
  private TimeService timeService;

  /*Save*/
  public Mono<Boolean> save(MerchantDiscountPrice merchantDiscountPrice, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.MERCHANT_DISCOUNT_PRICE)
            .domain(merchantDiscountPrice)
            .clazz(MerchantDiscountPrice.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(merchantDiscountPrice,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(merchantDiscountPrice,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(MerchantDiscountPrice merchantDiscountPrice, SaveParam saveParam) {
    Optional.ofNullable(merchantDiscountPrice)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.getMerchantDiscountPriceEnd(val),MainUtil.toFriendlyClassName(val)));
        });
  }
  /*End of Save*/

  /*Getters*/
  public MerchantDiscountPrice getExistingMerchantDiscountPrice(String id) {
    return Optional.ofNullable(id)
        .flatMap(merchantDiscountPriceRepository::findById)
        .orElse(null);
  }

  public PickupPoint.DiscountPrice getDiscountPrice(String id) {
    return Optional.ofNullable(id)
        .map(this::getExistingMerchantDiscountPrice)
        .filter(val -> !val.isMarkForDelete())
        .map(MerchantDiscountPrice::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .findFirst()
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .orElse(null);
  }

  public PickupPoint.Price getFullPrice(String id) {
    return Optional.ofNullable(id).map(this::getExistingMerchantDiscountPrice)
      .filter(val -> !val.isMarkForDelete()).map(MerchantDiscountPrice::getPrice)
      .orElseGet(HashSet::new).stream().filter(Objects::nonNull).findFirst().orElse(null);
  }
  /*End of Getters*/

}
