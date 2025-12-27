package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateQueue;
import com.gdn.aggregate.platform.module.product.listener.properties.UpdateQueueProperties;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.UpdateQueueRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component("ProductUpdateQueueService")
public class UpdateQueueService {

  public static final String SAVE_COMMAND = "saveUpdateQueue";

  @Autowired
  private DBService dbService;

  @Autowired
  private UpdateQueueRepository updateQueueRepository;

  @Autowired
  private TimeService timeService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private UpdateQueueProperties updateQueueProperties;

  public Mono<Boolean> save(UpdateQueue updateQueue, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.UPDATE_QUEUE)
            .domain(updateQueue)
            .clazz(UpdateQueue.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(updateQueue,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(updateQueue,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(UpdateQueue updateQueue, SaveParam saveParam) {
    Optional.ofNullable(updateQueue)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setExpiryTime(timeService.getExpiryTime(val.getTimestamp(),MainUtil.toFriendlyClassName(val)));
        });
  }

  public UpdateQueue getExistingUpdateQueue(String id) {
    return Optional.ofNullable(id)
        .flatMap(updateQueueRepository::findById)
        .orElse(null);
  }

  public UpdateQueue getExistingUpdateQueueWithObject(UpdateQueue updateQueue) {
    return Optional.ofNullable(updateQueue)
        .map(UpdateQueue::toId)
        .map(this::getExistingUpdateQueue)
        .orElse(null);
  }

  public List<UpdateQueue> getUpdateQueueByLevel(int level, long timestamp) {
    return updateQueueRepository.findAllByLevelAndTimestampBefore(level,timestamp,MainUtil.toSafePageable(null,updateQueueProperties.getSize()))
        .stream()
        .filter(Objects::nonNull)
        .peek(val -> updateQueueRepository.deleteById(val.getId()))
        .collect(Collectors.toList());
  }

  public Item getItemByUpdateQueue(UpdateQueue updateQueue, long timestamp) {
    return Optional.ofNullable(updateQueue)
        .map(val -> {
          if (val.getLevel()==3) {
            return itemService.getExistingItem(pickupPointService.getMinItemSkuByProductSku(val.getId()));
          } else {
            return itemService.getExistingItem(val.getId());
          }
        })
        .map(val -> {
          val.setTimestamp(timestamp);
          return val;
        })
        .orElse(null);
  }

}
