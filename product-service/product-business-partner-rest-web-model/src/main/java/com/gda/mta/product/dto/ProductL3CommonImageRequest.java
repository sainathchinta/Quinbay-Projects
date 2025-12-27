package com.gda.mta.product.dto;

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
public class ProductL3CommonImageRequest extends BaseRequest{

  private static final long serialVersionUID = -4165214336872356970L;
  private String locationPath;
  private Boolean mainImage;
  private Boolean markForDelete;
  private String reviewType;
  private Integer sequence;
  private Boolean activeLocation;
}
