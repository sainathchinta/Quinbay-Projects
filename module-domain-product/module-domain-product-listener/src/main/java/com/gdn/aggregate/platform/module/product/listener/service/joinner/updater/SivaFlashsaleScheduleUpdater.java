package com.gdn.aggregate.platform.module.product.listener.service.joinner.updater;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaFlashsaleScheduleConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveSemiService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

@Component("ProductSivaFlashsaleScheduleUpdater")
public class SivaFlashsaleScheduleUpdater {

  /*Processed*/
  @Autowired
  private SaveSemiService saveSemiService;

  /*Helper*/
  @Autowired
  private SivaFlashsaleScheduleConstructor sivaFlashsaleScheduleConstructor;

  /*DirectUpdate*/
  public Mono<Boolean> directUpdateByFlashsaleSchedule(FlashsaleSchedule flashsaleSchedule, SaveParam saveParam) {
    return saveSemiService.saveSivaFlashsaleSchedule(sivaFlashsaleScheduleConstructor.toSivaFlashsaleSchedule(flashsaleSchedule),saveParam);
  }

  public Mono<Boolean> directUpdateClean(IdTimestamp idTimestamp, SaveParam saveParam) {
    return saveSemiService.saveSivaFlashsaleSchedules(sivaFlashsaleScheduleConstructor.toCleanSivaFlashsaleSchedules(idTimestamp.getTimestamp()),saveParam);
  }

  public Mono<Boolean> directUpdateDeactivate(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    return saveSemiService.saveSivaFlashsaleSchedule(sivaFlashsaleScheduleConstructor.toDeactivateSivaFlashsaleSchedule(flashsaleProduct),saveParam);
  }

}
