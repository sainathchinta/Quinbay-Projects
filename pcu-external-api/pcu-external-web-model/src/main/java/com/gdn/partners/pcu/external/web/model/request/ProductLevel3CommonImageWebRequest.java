package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3CommonImageWebRequest {

  private static final long serialVersionUID = -4165214336872356970L;
  private String locationPath;
  private Boolean mainImages;
  private Boolean markForDelete;
  private String reviewType;
  private Boolean activeLocation;
  private Integer sequence;
}
