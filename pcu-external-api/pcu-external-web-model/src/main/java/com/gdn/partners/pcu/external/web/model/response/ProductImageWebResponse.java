package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductImageWebResponse {
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  private boolean markForDelete;
  private boolean activeLocation;
  private boolean commonImage;

}
