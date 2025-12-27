package com.gdn.aggregate.platform.module.product.listener.service.processor.processed;

import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.PartialSaveMultipleRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.PartialSaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.sub.DataPair;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaItemRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PartialUpdateService;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
public class SivaItemService {

  private static final String SAVE_COMMAND = "saveSivaItem";

  private static final String SAVE_MUTIPLE_COMMAND = "saveMultipleSivaItem";

  private static final String DELETE_COMMAND = "deleteSivaItem";

  @Autowired
  private DBService dbService;

  @Autowired
  private PartialUpdateService partialUpdateService;

  @Autowired
  private SivaItemRepository sivaItemRepository;

  @Autowired
  private SchedulerHelper schedulerHelper;

  @Autowired
  private PublisherService publisherService;

  /*End of Save*/
  public Mono<Boolean> upsert(SivaItem sivaItem, SaveParam saveParam) {
    return Mono.fromCallable(() -> DataPair.builder()
            .partial(partialUpdateService.toPartialData(sivaItem, saveParam))
            .full(sivaItem)
            .build())
        .map(domain -> PartialSaveRequest.builder()
            .index(Collections.SIVA_ITEM)
            .domain(domain)
            .partialClazz(MainUtil.getPartialClazz(domain))
            .fullClazz(MainUtil.getFullClazz(domain))
            .mongo(true)
            .elasticsearch(true)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.partialSave(saveRequest))
        .flatMap(val -> publisherService.publishIdTimestampSivaItem(sivaItem, saveParam))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaItem, SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaItem, t, SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult)
        .subscribeOn(schedulerHelper.of("SivaItemSave"));
  }

  public Mono<Boolean> upsertMultiple(List<SivaItem> sivaItems, SaveParam saveParam) {
    return Flux.fromIterable(sivaItems)
        .filter(Objects::nonNull)
        .map(sivaItem -> DataPair.builder()
            .partial(partialUpdateService.toPartialData(sivaItem, saveParam))
            .full(sivaItem)
            .build())
        .collectList()
        .map(domains -> PartialSaveMultipleRequest.builder()
            .index(Collections.SIVA_ITEM)
            .domains(domains)
            .partialClazz(MainUtil.getPartialClazzFromList(domains))
            .fullClazz(MainUtil.getFullClazzFromList((domains)))
            .mongo(true)
            .elasticsearch(true)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveMultipleRequest -> dbService.partialSaveMultiple(saveMultipleRequest))
        .flatMap(val -> publisherService.publishIdTimestampSivaItems(sivaItems, saveParam))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaItems, SAVE_MUTIPLE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaItems, t, SAVE_MUTIPLE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult)
        .subscribeOn(schedulerHelper.of("SivaItemSave"));
  }

  public Mono<Boolean> delete(SivaItem sivaItem, SaveParam saveParam) {
    return Mono.fromCallable(() -> DeleteRequest.builder()
            .index(Collections.SIVA_ITEM)
            .id(sivaItem.toId())
            .mongo(true)
            .elasticsearch(true)
            .build())
        .flatMap(dbService::delete)
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaItem,DELETE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaItem,t,DELETE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void performItemDeletionByProductSku(String productSku, SaveParam saveParam) {
    try {
      sivaItemRepository.deleteByProductSku(productSku);
      LoggerUtil.sendSuccessExecuteLog(productSku, DELETE_COMMAND + "ByProductSku-MongoDB", saveParam.getTraceId());
      
    } catch (Exception e) {
      LoggerUtil.sendErrorExecuteLog(productSku, e, DELETE_COMMAND + "ByProductSku-MongoDB", saveParam.getTraceId());
    }
  }

  public Mono<Boolean> deleteByProductSkuFromElasticsearch(String productSku, SaveParam saveParam) {
    // Get only the IDs of SivaItems that need to be deleted from ES
    List<String> sivaItemIds =
      sivaItemRepository.findIdsByProductSku(productSku).stream().map(SivaItem::getId)
        .collect(Collectors.toList());

    if (sivaItemIds.isEmpty()) {
      LoggerUtil.sendSuccessExecuteLog(productSku, DELETE_COMMAND + "ByProductSku-ES-NoItems",
        saveParam.getTraceId());
      return Mono.just(true);
    }

    // Delete from Elasticsearch using individual DeleteRequests
    return Flux.fromIterable(sivaItemIds).flatMap(sivaItemId -> {
      DeleteRequest deleteRequest =
        DeleteRequest.builder().index(Collections.SIVA_ITEM).id(sivaItemId)
          .mongo(false) // Only delete from ES
          .elasticsearch(true).build();
      return dbService.delete(deleteRequest).doOnError(
        t -> LoggerUtil.sendErrorExecuteLog(sivaItemId, t,
          DELETE_COMMAND + "ByProductSku-ES-Individual", saveParam.getTraceId()));
    }).reduce(MainUtil::reduce).doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(productSku,
      DELETE_COMMAND + "ByProductSku-ES-Completed", saveParam.getTraceId())).doOnError(
      t -> LoggerUtil.sendErrorExecuteLog(productSku, t, DELETE_COMMAND + "ByProductSku-ES",
        saveParam.getTraceId())).onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(SivaItem sivaItem, SaveParam saveParam) {
    Optional.ofNullable(sivaItem)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setOnL2(ModuleProductUtil.isSivaItemOnL2(val));
          val.setMinVersion(getMinVersionForSaving(val));
        });
  }

  public Map<String,Long> getMinVersionForSaving(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(SivaItem::getMinVersion)
        .orElseGet(HashMap::new);
  }

  public void setSaveParam(SaveParam saveParam) {
    Optional.ofNullable(saveParam)
        .map(SaveParam::getDbParam)
        .ifPresent(val -> {
          val.setCollection(Collections.SIVA_ITEM);
        });
  }
  /*End of Save*/

  /*Getters*/
  public SivaItem getExistingSivaItem(String id) {
    return Optional.ofNullable(id)
        .flatMap(sivaItemRepository::findById)
        .orElse(null);
  }

  public SivaItem toL2SivaItem(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .filter(ModuleProductUtil::isSivaItemOnL2)
        .orElseGet(() -> getL2SivaItem(sivaItem));
  }

  private SivaItem getL2SivaItem(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(val -> sivaItemRepository.findFirstByItemCodeAndOnL2OrderByIdAsc(val.getItemCode(),true))
        .orElse(null);
  }

  public SivaItem toL4SivaItem(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .filter(ModuleProductUtil::isSivaItemOnL4)
        .orElseGet(() -> getL4SivaItem(sivaItem));
  }

  private SivaItem getL4SivaItem(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(val -> sivaItemRepository.findFirstByItemCodeAndOnL2OrderByIdAsc(val.getItemCode(),false))
        .orElse(null);
  }

  public List<SivaItem> getSivaItemsByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(prdSku -> {
          try(Stream<SivaItem> sivaItemStream = sivaItemRepository.streamAllByProductSku(prdSku)) {
            return sivaItemStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(sivaItem -> MainUtil.toNotNullString(sivaItem.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }
  /*End of Getters*/

}
