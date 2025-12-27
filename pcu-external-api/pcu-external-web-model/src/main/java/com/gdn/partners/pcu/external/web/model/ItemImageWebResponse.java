package com.gdn.partners.pcu.external.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemImageWebResponse {
  private static final long serialVersionUID = -7391255382969622954L;
  private String id;
  private Boolean mainImages;
  private Integer sequence;
  private String locationPath;
  private boolean activeLocation;
  private boolean commonImage;
  private boolean markForDelete;

  public ItemImageWebResponse(String id, Boolean mainImages, Integer sequence, String locationPath,
      boolean activeLocation, boolean commonImage) {
    this.id = id;
    this.mainImages = mainImages;
    this.sequence = sequence;
    this.locationPath = locationPath;
    this.activeLocation = activeLocation;
    this.commonImage = commonImage;
  }
}
