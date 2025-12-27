package com.gdn.aggregate.platform.module.product.listener.service.processor.semi;

import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.PartialSaveMultipleRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.PartialSaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.sub.DataPair;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.repository.semi.SivaCampaignProductRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PartialUpdateService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
public class SivaCampaignProductService {

  public static final String SAVE_COMMAND = "saveSivaCampaignProduct";

  public static final String SAVE_MULTIPLE_COMMAND = "saveMultipleSivaCampaignProduct";

  @Autowired
  private DBService dbService;

  @Autowired
  private PartialUpdateService partialUpdateService;

  @Autowired
  private SivaCampaignProductRepository sivaCampaignProductRepository;

  @Autowired
  private SchedulerHelper schedulerHelper;

  @Autowired
  private TimeService timeService;

  /*Save*/
  public Mono<Boolean> upsert(SivaCampaignProduct sivaCampaignProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> DataPair.builder()
            .partial(partialUpdateService.toPartialData(sivaCampaignProduct, saveParam))
            .full(sivaCampaignProduct)
            .build())
        .map(domain -> PartialSaveRequest.builder()
            .index(Collections.SIVA_CAMPAIGN_PRODUCT)
            .domain(domain)
            .partialClazz(MainUtil.getPartialClazz(domain))
            .fullClazz(MainUtil.getFullClazz(domain))
            .mongo(true)
            .elasticsearch(true)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.partialSave(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaCampaignProduct, SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaCampaignProduct, t, SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult)
        .subscribeOn(schedulerHelper.of("SivaCampaignProductSave"));
  }

  public Mono<Boolean> upsertMultiple(List<SivaCampaignProduct> sivaCampaignProducts, SaveParam saveParam) {
    return Flux.fromIterable(sivaCampaignProducts)
        .filter(Objects::nonNull)
        .map(sivaCampaignProduct -> DataPair.builder()
            .partial(partialUpdateService.toPartialData(sivaCampaignProduct, saveParam))
            .full(sivaCampaignProduct)
            .build())
        .collectList()
        .map(domains -> PartialSaveMultipleRequest.builder()
            .index(Collections.SIVA_CAMPAIGN_PRODUCT)
            .domains(domains)
            .partialClazz(MainUtil.getPartialClazzFromList(domains))
            .fullClazz(MainUtil.getFullClazzFromList((domains)))
            .mongo(true)
            .elasticsearch(true)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveMultipleRequest -> dbService.partialSaveMultiple(saveMultipleRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaCampaignProducts, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaCampaignProducts, t, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult)
        .subscribeOn(schedulerHelper.of("SivaCampaignProductSave"));
  }

  public void setMandatory(SivaCampaignProduct sivaCampaignProduct, SaveParam saveParam) {
    Optional.ofNullable(sivaCampaignProduct)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.toSivaCampaignProductEndDate(val),MainUtil.toFriendlyClassName(val)));
        });
  }

  public void setSaveParam(SaveParam saveParam) {
    Optional.ofNullable(saveParam)
        .map(SaveParam::getDbParam)
        .ifPresent(val -> {
          val.setCollection(Collections.SIVA_CAMPAIGN_PRODUCT);
        });
  }
  /*End of Save*/

  /*Getters*/
  public List<SivaCampaignProduct> getExistingSivaCampaignProducts(List<String> ids) {
    return Optional.ofNullable(ids)
        .filter(CollectionUtils::isNotEmpty)
        .map(sivaCampaignProductRepository::findAllByIdIn)
        .orElse(null);
  }

  public List<SivaCampaignProduct> getActiveSivaCampaignProductsByCampaignCodes(List<String> campaignCodes) {
    return Optional.ofNullable(campaignCodes)
        .map(cpgCodes -> {
          try(Stream<SivaCampaignProduct> campaignProductStream = sivaCampaignProductRepository.streamAllByCampaignCodeInAndActiveTrue(cpgCodes)) {
            return campaignProductStream
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }
  /*End of Getters*/

}
