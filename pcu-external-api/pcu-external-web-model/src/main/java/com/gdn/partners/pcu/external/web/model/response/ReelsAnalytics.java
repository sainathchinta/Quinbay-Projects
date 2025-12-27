package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReelsAnalytics {
  private String reelId;
  private Long totalViews;
  private Long totalLikes;
  private Long totalShares;
}
