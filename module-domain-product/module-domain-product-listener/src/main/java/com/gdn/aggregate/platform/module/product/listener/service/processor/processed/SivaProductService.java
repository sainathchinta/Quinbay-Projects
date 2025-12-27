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
import com.gdn.aggregate.platform.module.product.listener.constants.ProductType;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaProductRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PartialUpdateService;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@Component
public class SivaProductService {

    private static final String SAVE_COMMAND = "saveSivaProduct";

    private static final String SAVE_MULTIPLE_COMMAND = "saveMultipleSivaProduct";

    private static final String DELETE_COMMAND = "deleteSivaProduct";

    @Autowired
    private DBService dbService;

    @Autowired
    private PartialUpdateService partialUpdateService;

    @Autowired
    private SivaProductRepository sivaProductRepository;

    @Autowired
    private SchedulerHelper schedulerHelper;

    @Autowired
    private PublisherService publisherService;

    /*Save*/
    public Mono<Boolean> upsert(SivaProduct sivaProduct, SaveParam saveParam) {
        return Mono.fromCallable(() -> DataPair.builder()
                .partial(partialUpdateService.toPartialData(sivaProduct, saveParam))
                .full(sivaProduct)
                .build())
            .map(domain -> PartialSaveRequest.builder()
                .index(Collections.SIVA_PRODUCT)
                .domain(domain)
                .partialClazz(MainUtil.getPartialClazz(domain))
                .fullClazz(MainUtil.getFullClazz(domain))
                .mongo(true)
                .elasticsearch(true)
                .republish(ParamUtil.isRepublish(saveParam))
                .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
                .build())
            .flatMap(saveRequest -> dbService.partialSave(saveRequest))
            .flatMap(val -> publisherService.publishIdTimestampSivaProduct(sivaProduct, saveParam))
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaProduct, SAVE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaProduct, t, SAVE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("SivaProductDelete"));
    }

    public Mono<Boolean> upsertMultiple(List<SivaProduct> sivaProducts, SaveParam saveParam) {
        return Flux.fromIterable(sivaProducts)
            .filter(Objects::nonNull)
            .map(sivaProduct -> DataPair.builder()
                .partial(partialUpdateService.toPartialData(sivaProduct, saveParam))
                .full(sivaProduct)
                .build())
            .collectList()
            .map(domains -> PartialSaveMultipleRequest.builder()
                .index(Collections.SIVA_PRODUCT)
                .domains(domains)
                .partialClazz(MainUtil.getPartialClazzFromList(domains))
                .fullClazz(MainUtil.getFullClazzFromList((domains)))
                .mongo(true)
                .elasticsearch(true)
                .republish(ParamUtil.isRepublish(saveParam))
                .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
                .build())
            .flatMap(saveMultipleRequest -> dbService.partialSaveMultiple(saveMultipleRequest))
            .flatMap(val -> publisherService.publishIdTimestampSivaProducts(sivaProducts, saveParam))
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaProducts, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaProducts, t, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("SivaProductDelete"));
    }

    public Mono<Boolean> delete(SivaProduct sivaProduct, SaveParam saveParam) {
        return Mono.fromCallable(() -> DeleteRequest.builder()
                .index(Collections.SIVA_PRODUCT)
                .id(sivaProduct.toId())
                .mongo(true)
                .elasticsearch(true)
                .build())
            .flatMap(dbService::delete)
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaProduct, DELETE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaProduct, t, DELETE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult);
    }

    public Mono<Boolean> deleteById(String productSku, SaveParam saveParam) {
        return Mono.fromCallable(() -> DeleteRequest.builder()
                .index(Collections.SIVA_PRODUCT)
                .id(productSku)
                .mongo(true)
                .elasticsearch(true)
                .build())
            .flatMap(dbService::delete)
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(productSku, DELETE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(productSku, t, DELETE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult);
    }

    public Mono<Boolean> deleteProductByProductSkuFromElasticsearch(String productSku,
      SaveParam saveParam) {
        return Mono.fromCallable(() -> {
              DeleteRequest deleteRequest =
                DeleteRequest.builder().index(Collections.SIVA_PRODUCT).id(productSku)
                  .mongo(false) // Only delete from ES
                  .elasticsearch(true).build();
              return deleteRequest;
          }).flatMap(dbService::delete).doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(productSku,
            DELETE_COMMAND + "ByProductSku-ES-Completed", saveParam.getTraceId())).doOnError(
            t -> LoggerUtil.sendErrorExecuteLog(productSku, t,
              DELETE_COMMAND + "ByProductSku-ES-Individual", saveParam.getTraceId()))
          .onErrorResume(MainUtil::errorResult);
    }

    public void setMandatory(SivaProduct sivaProduct, SaveParam saveParam) {
        Optional.ofNullable(sivaProduct)
            .ifPresent(val -> {
                val.setId(val.toId());
                val.setPickupPointCode(getPickupPointCodeForSaving(val));
                val.setMinVersion(getMinVersionForSaving(val));
            });
    }

    public String getPickupPointCodeForSaving(SivaProduct sivaProduct) {
        return Optional.ofNullable(sivaProduct)
            .filter(val -> Objects.isNull(val.getPickupPointCode()))
            .filter(val -> ProductType.DIGITAL.equals(val.getProductType()))
            .map(val -> ProductType.DIGITAL)
            .orElseGet(() -> ModuleProductUtil.getSivaProductPickupPoint(sivaProduct));
    }

    public Map<String,Long> getMinVersionForSaving(SivaProduct sivaProduct) {
        return Optional.ofNullable(sivaProduct)
            .map(SivaProduct::getMinVersion)
            .orElseGet(HashMap::new);
    }

    public void setSaveParam(SaveParam saveParam) {
        Optional.ofNullable(saveParam)
            .map(SaveParam::getDbParam)
            .ifPresent(val -> {
                val.setCollection(Collections.SIVA_PRODUCT);
            });
    }
    /*End of Save*/

    /*Getters*/
    public SivaProduct getExistingSivaProduct(String id) {
        return Optional.ofNullable(id)
            .flatMap(sivaProductRepository::findById)
            .orElse(null);
    }

    public boolean doesScheduleHaveProducts(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
        return Optional.ofNullable(sivaFlashsaleSchedule)
            .map(SivaFlashsaleSchedule::toId)
            .map(scheduleId -> sivaProductRepository.existsByFlashsale_ScheduleIdAndFlashsale_ActiveTrue(scheduleId) || sivaProductRepository.existsBySubFlashsale_ScheduleIdAndSubFlashsale_ActiveTrue(scheduleId))
            .orElseGet(() -> false);
    }

    public boolean doesScheduleHaveProductsAndGroupId(SivaFlashsaleSchedule sivaFlashsaleSchedule, String groupId) {
        return Optional.ofNullable(sivaFlashsaleSchedule)
            .map(SivaFlashsaleSchedule::toId)
            .map(scheduleId -> sivaProductRepository.existsByFlashsale_ScheduleIdAndFlashsale_ActiveTrueAndFlashsale_GroupIds(scheduleId,groupId) || sivaProductRepository.existsBySubFlashsale_ScheduleIdAndSubFlashsale_ActiveTrueAndSubFlashsale_GroupIds(scheduleId,groupId))
            .orElseGet(() -> false);
    }
    /*End of Getters*/

}
