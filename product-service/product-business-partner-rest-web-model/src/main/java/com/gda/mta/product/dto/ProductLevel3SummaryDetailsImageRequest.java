package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryDetailsImageRequest implements Serializable {

  private static final long serialVersionUID = 1990684036416673379L;
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  private Boolean markForDelete;
  private String reviewType;
  private boolean resizeEventPublished;
}
