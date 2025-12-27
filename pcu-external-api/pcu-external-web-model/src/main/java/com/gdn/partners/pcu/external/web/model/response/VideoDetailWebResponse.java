package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
@Data
@AllArgsConstructor
@NoArgsConstructor
public class VideoDetailWebResponse {
  private String videoId;
  private String videoUrl;
  private String coverImagePath;
  private String caption;
  private String videoName;
  private int ownerType;
  private ReelsAnalytics reelsAnalytics;
  private List<String> productSkuList;
  private List<ReelProductDetailWebResponse> reelProductDetailWebResponses;
  private List<String> inActiveProductSkuList;
}
