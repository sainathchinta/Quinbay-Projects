package com.gdn.aggregate.platform.module.product.listener.service.joinner.updater;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaFlashsaleGroupConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveSemiService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

@Component("ProductSivaFlashsaleGroupUpdater")
public class SivaFlashsaleGroupUpdater {

  /*Processed*/
  @Autowired
  private SaveSemiService saveSemiService;

  /*Helper*/
  @Autowired
  private SivaFlashsaleGroupConstructor sivaFlashsaleGroupConstructor;

  /*DirectUpdate*/
  public Mono<Boolean> directUpdateByFlashsaleGroup(FlashsaleGroup flashsaleGroup, SaveParam saveParam) {
    return saveSemiService.saveSivaFlashsaleGroup(sivaFlashsaleGroupConstructor.toSivaFlashsaleGroup(flashsaleGroup),saveParam);
  }

  public Mono<Boolean> directUpdateClean(IdTimestamp idTimestamp, SaveParam saveParam) {
    return saveSemiService.saveSivaFlashsaleGroup(sivaFlashsaleGroupConstructor.toCleanSivaFlashsaleGroup(idTimestamp),saveParam);
  }

  public Mono<Boolean> directUpdateDeactivate(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    return saveSemiService.saveSivaFlashsaleGroups(sivaFlashsaleGroupConstructor.toDeactivateSivaFlashsaleGroups(flashsaleProduct),saveParam);
  }

}
