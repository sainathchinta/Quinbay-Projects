package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by parvej on 27/01/2020 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class CategoryConfigurationRequest {

  private String categoryCode;
  private String categoryName;
  private String reviewConfig;
}