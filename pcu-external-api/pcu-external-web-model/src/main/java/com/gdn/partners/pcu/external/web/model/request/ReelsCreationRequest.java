package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReelsCreationRequest {
  private String videoId;
  private String sourceUrl;
  private String coverImagePath;
  private String videoName;
  private String caption;
  private List<String> productSkuList = new ArrayList<>();
  private int ownerType;
  private String ownerId;
  private int videoDuration;
}
