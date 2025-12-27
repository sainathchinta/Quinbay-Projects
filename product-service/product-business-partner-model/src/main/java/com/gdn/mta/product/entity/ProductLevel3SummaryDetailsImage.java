package com.gdn.mta.product.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryDetailsImage {
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  private boolean markForDelete;
  private String reviewType;
  private boolean activeLocation;
}
