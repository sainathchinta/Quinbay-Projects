package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.Optional;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataRepository;
import reactor.core.publisher.Mono;

@Component
public class MasterDataService {

  private static final String SAVE_COMMAND = "saveMasterData";

  @Autowired
  private DBService dbService;

  @Autowired
  private MasterDataRepository masterDataRepository;

  /*Save*/
  public Mono<Boolean> save(MasterData masterData, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.MASTER_DATA)
            .domain(masterData)
            .clazz(MasterData.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(masterData,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(masterData,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(MasterData masterData, SaveParam saveParam) {
    Optional.ofNullable(masterData)
        .ifPresent(val -> {
          val.setId(val.toId());
        });
  }
  /*End of Save*/

  /*Getters*/
  public MasterData getExistingMasterData(String id) {
    return Optional.ofNullable(id)
        .flatMap(masterDataRepository::findById)
        .orElse(null);
  }
  /*End of Getters*/

}
