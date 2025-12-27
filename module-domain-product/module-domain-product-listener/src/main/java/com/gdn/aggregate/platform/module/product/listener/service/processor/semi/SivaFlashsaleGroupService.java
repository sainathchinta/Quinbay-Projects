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
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.repository.semi.SivaFlashsaleGroupRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PartialUpdateService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Component
public class SivaFlashsaleGroupService {

    public static final String SAVE_COMMAND = "saveSivaFlashsaleGroup";

    public static final String SAVE_MULTIPLE_COMMAND = "saveMultipleSivaFlashsaleGroup";

    @Autowired
    private DBService dbService;

    @Autowired
    private PartialUpdateService partialUpdateService;

    @Autowired
    private SivaFlashsaleGroupRepository sivaFlashsaleGroupRepository;

    @Autowired
    private SchedulerHelper schedulerHelper;

    @Autowired
    private TimeService timeService;

    /*Save*/
    public Mono<Boolean> upsert(SivaFlashsaleGroup sivaFlashsaleGroup, SaveParam saveParam) {
        return Mono.fromCallable(() -> DataPair.builder()
                .partial(partialUpdateService.toPartialData(sivaFlashsaleGroup, saveParam))
                .full(sivaFlashsaleGroup)
                .build())
            .map(domain -> PartialSaveRequest.builder()
                .index(Collections.SIVA_FLASHSALE_GROUP)
                .domain(domain)
                .partialClazz(MainUtil.getPartialClazz(domain))
                .fullClazz(MainUtil.getFullClazz(domain))
                .mongo(true)
                .elasticsearch(true)
                .republish(ParamUtil.isRepublish(saveParam))
                .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
                .build())
            .flatMap(saveRequest -> dbService.partialSave(saveRequest))
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaFlashsaleGroup, SAVE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaFlashsaleGroup, t, SAVE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("SivaFlashsaleGroupSave"));
    }

    public Mono<Boolean> upsertMultiple(List<SivaFlashsaleGroup> sivaFlashsaleGroups, SaveParam saveParam) {
        return Flux.fromIterable(sivaFlashsaleGroups)
            .filter(Objects::nonNull)
            .map(sivaFlashsaleGroup -> DataPair.builder()
                .partial(partialUpdateService.toPartialData(sivaFlashsaleGroup, saveParam))
                .full(sivaFlashsaleGroup)
                .build())
            .collectList()
            .map(domains -> PartialSaveMultipleRequest.builder()
                .index(Collections.SIVA_FLASHSALE_GROUP)
                .domains(domains)
                .partialClazz(MainUtil.getPartialClazzFromList(domains))
                .fullClazz(MainUtil.getFullClazzFromList((domains)))
                .mongo(true)
                .elasticsearch(true)
                .republish(ParamUtil.isRepublish(saveParam))
                .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
                .build())
            .flatMap(saveMultipleRequest -> dbService.partialSaveMultiple(saveMultipleRequest))
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaFlashsaleGroups, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaFlashsaleGroups, t, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("SivaFlashsaleGroupSave"));
    }

    public void setMandatory(SivaFlashsaleGroup sivaFlashsaleGroup, SaveParam saveParam) {
        Optional.ofNullable(sivaFlashsaleGroup)
            .ifPresent(val -> {
                val.setId(val.toId());
                val.setSchedules(ModuleProductUtil.toNotEndedFlashsaleGroupSchedules(val));
                val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.toSivaFlashsaleGroupEndDate(val),MainUtil.toFriendlyClassName(val)));
            });
    }

    public void setSaveParam(SaveParam saveParam) {
        Optional.ofNullable(saveParam)
            .map(SaveParam::getDbParam)
            .ifPresent(val -> {
                val.setCollection(Collections.SIVA_FLASHSALE_GROUP);
            });
    }
    /*End of Save*/

    /*Getters*/
    public SivaFlashsaleGroup getExistingSivaFlashsaleGroup(String id) {
        return Optional.ofNullable(id)
            .flatMap(sivaFlashsaleGroupRepository::findById)
            .orElse(null);
    }

    public List<SivaFlashsaleGroup> getExistingSivaFlashsaleGroups(Set<String> ids) {
        return Optional.ofNullable(ids)
            .filter(CollectionUtils::isNotEmpty)
            .map(sivaFlashsaleGroupRepository::findAllByIdIn)
            .orElse(null);
    }
    /*End of Getters*/

}
