package com.gdn.partners.pcu.external.web.model.response;

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
public class ProductLevel3ImageWebResponse {
  private static final long serialVersionUID = -7391255382969622954L;
  private String id;
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  private boolean activeLocation;
  private boolean commonImage;
}
