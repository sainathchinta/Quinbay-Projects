package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class CampaignProductEnded extends BaseData {

  private List<CampaignSession> campaignSessionList;

  private List<CampaignEnd> campaignEndModelList;

  private List<String> campaignCodeList;

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class CampaignSession {

    private String campaignCode;

    private Integer session;

  }

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class CampaignEnd {

    private String campaignCode;

    private Integer priority;

  }

}
