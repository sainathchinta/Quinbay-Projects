package com.gdn.x.product.model.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductSummaryRequestVo {

  private String merchantCode;
  private String keyword;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private List<String> promoTypes;
  private String sortField;
  private String sortOrder;
  private Boolean inStock;
  private Boolean archived;
  private Boolean suspended;
  private Boolean b2cActivated;
  private Boolean b2bActivated;
  private Boolean bundleProduct;
  private String sizeChartCode;
  private String attributeCode;
  private Boolean fetchSizeChartDetails;
  private Boolean pureExternalUser;
}
