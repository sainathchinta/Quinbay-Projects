package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class CampaignTeaserLive extends BaseData {

  private long publishTimestamp;

  private String campaignCode;

  private String campaignName;

  private long promotionStartTime;

  private long promotionEndTime;

  private long teaserStartDate;

  private String tagLabel;

  private boolean exclusive;

  private String campaignType;

  private String flashSaleCampaignName;

  private int priority;

  private String backgroundImageUrl;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.campaignCode);
    String publishTimestamp = MainUtil.toNotNullString(MainUtil.fromLongToString(this.publishTimestamp));
    return MainUtil.toList(id,publishTimestamp);
  }

}
