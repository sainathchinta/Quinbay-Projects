package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageResponse {
  private Boolean mainImages;
  private Integer sequence;
  private String locationPath;
  private boolean markForDelete;
  private boolean activeLocation;
  private boolean commonImage;
}
