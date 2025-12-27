package com.gdn.mta.product.valueobject;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.pbp.commons.constants.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BasicProductDetail {

  private String productCode;
  private String productName;
  private String brand;
  private String productSku;
  private List<BasicItemDetail> productItems;
  private String distributionMappingStatus = Constants.NON_DISTRIBUTION;

}
