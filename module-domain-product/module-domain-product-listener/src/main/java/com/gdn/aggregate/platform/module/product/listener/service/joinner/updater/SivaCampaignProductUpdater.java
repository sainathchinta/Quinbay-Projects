package com.gdn.aggregate.platform.module.product.listener.service.joinner.updater;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductEnded;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaCampaignProductConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveSemiService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

@Component("ProductSivaCampaignProductUpdater")
public class SivaCampaignProductUpdater {

  /*Processed*/
  @Autowired
  private SaveSemiService saveSemiService;

  /*Helper*/
  @Autowired
  private SivaCampaignProductConstructor sivaCampaignProductConstructor;

  /*DirectUpdate*/
  public Mono<Boolean> directUpdateByCampaignProductLive(CampaignProductLive campaignProductLive, SaveParam saveParam) {
    return saveSemiService.saveSivaCampaignProducts(sivaCampaignProductConstructor.toActiveSivaCampaignProducts(campaignProductLive),saveParam);
  }

  public Mono<Boolean> directUpdateByCampaignProductEnded(CampaignProductEnded campaignProductEnded, SaveParam saveParam) {
    return saveSemiService.saveSivaCampaignProducts(sivaCampaignProductConstructor.toEndedSivaCampaignProducts(campaignProductEnded),saveParam);
  }

  public Mono<Boolean> directUpdateByCampaignTeaserLive(CampaignTeaserLive campaignTeaserLive, SaveParam saveParam) {
    return saveSemiService.saveSivaCampaignProducts(sivaCampaignProductConstructor.toUpdatedTagLabelSivaCampaignProducts(campaignTeaserLive),saveParam);
  }

}
