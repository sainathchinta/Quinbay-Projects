package com.gdn.aggregate.platform.module.product.listener.service.helper;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.DeleteExpiredEvent;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DelayedJobRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.KafkaService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.DataUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;

@Component("ProductSchedulerService")
public class SchedulerService extends DataUtil {

  public static final String SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_SCHEDULE_COMMAND = "scheduleDeactivateSivaFlashsaleSchedule";

  public static final String SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_GROUP_COMMAND = "scheduleDeactivateSivaFlashsaleGroup";

  public static final String SCHEDULE_DELETE_SIVA_FLASHSALE_SCHEDULE_COMMAND = "scheduleDeleteSivaFlashsaleSchedule";

  public static final String SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND = "scheduleDeleteSivaFlashsaleGroup";

  public static final String SCHEDULE_DELETE_SIVA_CAMPAIGN_PRODUCT_COMMAND = "scheduleDeleteSivaCampaignProduct";

  private TimeService timeService;

  private KafkaService kafkaService;

  public SchedulerService(ObjectMapper objectMapper, TimeService timeService, KafkaService kafkaService) {
    super(objectMapper,timeService);
    this.timeService = timeService;
    this.kafkaService = kafkaService;
  }

  public Mono<Boolean> scheduleDeactivateSivaFlashsaleSchedule(FlashsaleProduct flashsaleProduct, String traceId) {
    return Mono.fromCallable(() -> toDeactivateSivaFlashsaleScheduleEvent(flashsaleProduct))
        .flatMap(event -> kafkaService.schedule(event,SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_SCHEDULE_COMMAND))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(flashsaleProduct,SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_SCHEDULE_COMMAND,traceId))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(flashsaleProduct,t,SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_SCHEDULE_COMMAND,traceId))
        .onErrorResume(MainUtil::errorResult);
  }

  private DelayedJobRequest toDeactivateSivaFlashsaleScheduleEvent(FlashsaleProduct flashsaleProduct) {
    List<Long> notifyTimes = ModuleProductUtil.toNotifyTimes(flashsaleProduct);

    FlashsaleProduct data = new FlashsaleProduct();
    BeanUtils.copyProperties(flashsaleProduct, data, "timestamp");
    data.setTimestamp(MainUtil.getLatestTimestamp(notifyTimes));

    return DelayedJobRequest.builder()
        .id(data.getId())
        .name(data.getId())
        .topic(Topics.DEACTIVATE_FLASHSALE_SCHEDULE)
        .payload(toJson(data))
        .notifyTimes(notifyTimes)
        .build();
  }

  public Mono<Boolean> scheduleDeactivateSivaFlashsaleGroup(FlashsaleProduct flashsaleProduct, String traceId) {
    return Mono.fromCallable(() -> toDeactivateSivaFlashsaleGroupEvent(flashsaleProduct))
        .flatMap(event -> kafkaService.schedule(event,SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_GROUP_COMMAND))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(flashsaleProduct,SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_GROUP_COMMAND,traceId))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(flashsaleProduct,t,SCHEDULE_DEACTIVATE_SIVA_FLASHSALE_GROUP_COMMAND,traceId))
        .onErrorResume(MainUtil::errorResult);
  }

  private DelayedJobRequest toDeactivateSivaFlashsaleGroupEvent(FlashsaleProduct flashsaleProduct) {
    List<Long> notifyTimes = ModuleProductUtil.toNotifyTimes(flashsaleProduct);

    FlashsaleProduct data = new FlashsaleProduct();
    BeanUtils.copyProperties(flashsaleProduct, data, "timestamp");
    data.setTimestamp(MainUtil.getLatestTimestamp(notifyTimes));

    return DelayedJobRequest.builder()
        .id(data.getId())
        .name(data.getId())
        .topic(Topics.DEACTIVATE_FLASHSALE_GROUP)
        .payload(toJson(data))
        .notifyTimes(notifyTimes)
        .build();
  }

  public Mono<Boolean> scheduleDeleteSivaFlashsaleSchedule(DeleteExpiredEvent event, String traceId) {
    return Mono.fromCallable(() -> toDeleteSivaFlashsaleScheduleEvent(event))
        .flatMap(data -> kafkaService.schedule(data,SCHEDULE_DELETE_SIVA_FLASHSALE_SCHEDULE_COMMAND))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(event,SCHEDULE_DELETE_SIVA_FLASHSALE_SCHEDULE_COMMAND,traceId))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(event,t,SCHEDULE_DELETE_SIVA_FLASHSALE_SCHEDULE_COMMAND,traceId))
        .onErrorResume(MainUtil::errorResult);
  }

  private DelayedJobRequest toDeleteSivaFlashsaleScheduleEvent(DeleteExpiredEvent event) {
    DeleteExpiredEvent data = DeleteExpiredEvent.builder()
        .collection(Collections.SIVA_FLASHSALE_SCHEDULE)
        .expiryTime(timeService.getScheduleTime(event.getExpiryTime(),SCHEDULE_DELETE_SIVA_FLASHSALE_SCHEDULE_COMMAND))
        .mongo(true)
        .elasticsearch(true)
        .build();

    return DelayedJobRequest.builder()
        .id(SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND)
        .name(Topics.SCHEDULE_NAME_DELETE_SIVA_FLASHSALE_SCHEDULE)
        .topic(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_SCHEDULE)
        .payload(toJson(data))
        .notifyTimes(MainUtil.toList(data.getExpiryTime()))
        .build();
  }

  public Mono<Boolean> scheduleDeleteSivaFlashsaleGroup(DeleteExpiredEvent event, String traceId) {
    return Mono.fromCallable(() -> toDeleteSivaFlashsaleGroupEvent(event))
        .flatMap(data -> kafkaService.schedule(data,SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(event,SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND,traceId))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(event,t,SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND,traceId))
        .onErrorResume(MainUtil::errorResult);
  }

  private DelayedJobRequest toDeleteSivaFlashsaleGroupEvent(DeleteExpiredEvent event) {
    DeleteExpiredEvent data = DeleteExpiredEvent.builder()
        .collection(Collections.SIVA_FLASHSALE_GROUP)
        .expiryTime(timeService.getScheduleTime(event.getExpiryTime(),SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND))
        .mongo(true)
        .elasticsearch(true)
        .build();

    return DelayedJobRequest.builder()
        .id(SCHEDULE_DELETE_SIVA_FLASHSALE_GROUP_COMMAND)
        .name(Topics.SCHEDULE_NAME_DELETE_SIVA_FLASHSALE_GROUP)
        .topic(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_GROUP)
        .payload(toJson(data))
        .notifyTimes(MainUtil.toList(data.getExpiryTime()))
        .build();
  }

  public Mono<Boolean> scheduleDeleteSivaCampaignProduct(DeleteExpiredEvent event, String traceId) {
    return Mono.fromCallable(() -> toDeleteSivaCampaignProductEvent(event))
        .flatMap(data -> kafkaService.schedule(data,SCHEDULE_DELETE_SIVA_CAMPAIGN_PRODUCT_COMMAND))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(event,SCHEDULE_DELETE_SIVA_CAMPAIGN_PRODUCT_COMMAND,traceId))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(event,t,SCHEDULE_DELETE_SIVA_CAMPAIGN_PRODUCT_COMMAND,traceId))
        .onErrorResume(MainUtil::errorResult);
  }

  private DelayedJobRequest toDeleteSivaCampaignProductEvent(DeleteExpiredEvent event) {
    DeleteExpiredEvent data = DeleteExpiredEvent.builder()
        .collection(Collections.SIVA_CAMPAIGN_PRODUCT)
        .expiryTime(timeService.getScheduleTime(event.getExpiryTime(),SCHEDULE_DELETE_SIVA_CAMPAIGN_PRODUCT_COMMAND))
        .mongo(true)
        .elasticsearch(true)
        .build();

    return DelayedJobRequest.builder()
        .id(SCHEDULE_DELETE_SIVA_CAMPAIGN_PRODUCT_COMMAND)
        .name(Topics.SCHEDULE_NAME_DELETE_SIVA_CAMPAIGN_PRODUCT)
        .topic(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_CAMPAIGN_PRODUCT)
        .payload(toJson(data))
        .notifyTimes(MainUtil.toList(data.getExpiryTime()))
        .build();
  }

}
