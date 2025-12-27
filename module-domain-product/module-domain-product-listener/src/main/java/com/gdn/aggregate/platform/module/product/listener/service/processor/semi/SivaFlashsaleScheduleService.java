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
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.repository.semi.SivaFlashsaleScheduleRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PartialUpdateService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
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
public class SivaFlashsaleScheduleService {

    public static final String SAVE_COMMAND = "saveSivaFlashsaleSchedule";

    public static final String SAVE_MULTIPLE_COMMAND = "saveMultipleSivaFlashsaleSchedule";

    @Autowired
    private DBService dbService;

    @Autowired
    private PartialUpdateService partialUpdateService;

    @Autowired
    private SivaFlashsaleScheduleRepository sivaFlashsaleScheduleRepository;

    @Autowired
    private SchedulerHelper schedulerHelper;

    @Autowired
    private TimeService timeService;

    /*Save*/
    public Mono<Boolean> upsert(SivaFlashsaleSchedule sivaFlashsaleSchedule, SaveParam saveParam) {
        return Mono.fromCallable(() -> DataPair.builder()
                .partial(partialUpdateService.toPartialData(sivaFlashsaleSchedule, saveParam))
                .full(sivaFlashsaleSchedule)
                .build())
            .map(domain -> PartialSaveRequest.builder()
                .index(Collections.SIVA_FLASHSALE_SCHEDULE)
                .domain(domain)
                .partialClazz(MainUtil.getPartialClazz(domain))
                .fullClazz(MainUtil.getFullClazz(domain))
                .mongo(true)
                .elasticsearch(true)
                .republish(ParamUtil.isRepublish(saveParam))
                .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
                .build())
            .flatMap(saveRequest -> dbService.partialSave(saveRequest))
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaFlashsaleSchedule, SAVE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaFlashsaleSchedule, t, SAVE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("SivaFlashsaleScheduleSave"));
    }

    public Mono<Boolean> upsertMultiple(List<SivaFlashsaleSchedule> sivaFlashsaleSchedules, SaveParam saveParam) {
        return Flux.fromIterable(sivaFlashsaleSchedules)
            .filter(Objects::nonNull)
            .map(sivaFlashsaleSchedule -> DataPair.builder()
                .partial(partialUpdateService.toPartialData(sivaFlashsaleSchedule, saveParam))
                .full(sivaFlashsaleSchedule)
                .build())
            .collectList()
            .map(domains -> PartialSaveMultipleRequest.builder()
                .index(Collections.SIVA_FLASHSALE_SCHEDULE)
                .domains(domains)
                .partialClazz(MainUtil.getPartialClazzFromList(domains))
                .fullClazz(MainUtil.getFullClazzFromList((domains)))
                .mongo(true)
                .elasticsearch(true)
                .republish(ParamUtil.isRepublish(saveParam))
                .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
                .build())
            .flatMap(saveMultipleRequest -> dbService.partialSaveMultiple(saveMultipleRequest))
            .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(sivaFlashsaleSchedules, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
            .doOnError(t -> LoggerUtil.sendErrorExecuteLog(sivaFlashsaleSchedules, t, SAVE_MULTIPLE_COMMAND, saveParam.getTraceId()))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("SivaFlashsaleScheduleSave"));
    }

    public void setMandatory(SivaFlashsaleSchedule sivaFlashsaleSchedule, SaveParam saveParam) {
        Optional.ofNullable(sivaFlashsaleSchedule)
            .ifPresent(val -> {
                val.setId(val.toId());
                val.setExpiryTime(timeService.getExpiryTime(ModuleProductUtil.toSivaFlashsaleScheduleEndDate(val),MainUtil.toFriendlyClassName(val)));
            });
    }

    public void setSaveParam(SaveParam saveParam) {
        Optional.ofNullable(saveParam)
            .map(SaveParam::getDbParam)
            .ifPresent(val -> {
                val.setCollection(Collections.SIVA_FLASHSALE_SCHEDULE);
            });
    }
    /*End of Save*/

    /*Getters*/
    public SivaFlashsaleSchedule getExistingSivaFlashsaleSchedule(String id) {
        return Optional.ofNullable(id)
            .flatMap(sivaFlashsaleScheduleRepository::findById)
            .orElse(null);
    }

    public List<SivaFlashsaleSchedule> getNotEndedSivaFlashsaleSchedules(long timestamp) {
            return Optional.of(timestamp)
                .map(time -> {
                    try (Stream<SivaFlashsaleSchedule> sivaFlashsaleScheduleStream = sivaFlashsaleScheduleRepository.streamAllByEndAfter(timestamp)) {
                        return sivaFlashsaleScheduleStream.collect(Collectors.toList());
                    }
                })
                .orElseGet(ArrayList::new);
    }
    /*End of Getters*/

}
