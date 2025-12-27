package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductPublished;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.CAMPAIGN_PRODUCT)
public class CampaignProduct extends BaseData {

  private String campaignCode;

  private String campaignCodeSessionId;

  private String campaignName;

  private long promotionStartTime;

  private long promotionEndTime;

  private boolean active;

  private String tagLabel;

  private boolean exclusive;

  private boolean retainData;

  private String campaignType;

  private String flashSaleCampaignName;

  private int priority;

  private String backgroundImageUrl;

  private CampaignProductPublished.ProductSkuEventModel sku;

  @Override
  public List<String> toIds() {
    return ModuleProductUtil.toCampaignProductIds(this.campaignCode,ModuleProductUtil.toCampaignItemSku(this),ModuleProductUtil.toCampaignPickupPointCode(this));
  }

}
