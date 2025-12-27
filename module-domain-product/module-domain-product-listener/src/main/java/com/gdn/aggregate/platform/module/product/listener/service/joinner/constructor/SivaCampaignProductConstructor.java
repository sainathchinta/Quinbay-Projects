package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductEnded;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaCampaignProductService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component("ProductSivaCampaignProductConstructor")
public class SivaCampaignProductConstructor {

  @Autowired
  private SivaCampaignProductService sivaCampaignProductService;

  public List<SivaCampaignProduct> toActiveSivaCampaignProducts(CampaignProductLive campaignProductLive) {
    return Optional.ofNullable(campaignProductLive)
        .map(CampaignProductLive::getCampaigns)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .peek(val -> {
          val.setTimestamp(campaignProductLive.getTimestamp());
          val.setActive(true);
        })
        .collect(Collectors.toList());
  }

  public List<SivaCampaignProduct> toEndedSivaCampaignProducts(CampaignProductEnded campaignProductEnded) {
    return Optional.ofNullable(campaignProductEnded)
        .map(ModuleProductUtil::toCampaignCodeSessionIds)
        .map(sivaCampaignProductService::getExistingSivaCampaignProducts)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .peek(val -> {
          val.setTimestamp(campaignProductEnded.getTimestamp());
          val.setActive(false);
        })
        .collect(Collectors.toList());
  }

  public List<SivaCampaignProduct> toUpdatedTagLabelSivaCampaignProducts(CampaignTeaserLive campaignTeaserLive) {
    return Optional.ofNullable(campaignTeaserLive)
        .map(CampaignTeaserLive::getCampaignCode)
        .map(MainUtil::toList)
        .map(sivaCampaignProductService::getActiveSivaCampaignProductsByCampaignCodes)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .peek(val -> {
          val.setTimestamp(campaignTeaserLive.getTimestamp());
          val.setTagLabel(campaignTeaserLive.getTagLabel());
        })
        .collect(Collectors.toList());
  }

}
