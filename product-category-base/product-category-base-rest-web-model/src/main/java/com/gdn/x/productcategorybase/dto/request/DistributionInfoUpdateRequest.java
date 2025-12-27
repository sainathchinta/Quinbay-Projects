package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class DistributionInfoUpdateRequest implements Serializable {

  private String sellerCode;
  private Map<String, String> distributionInfoRequest;
  private List<ProductItemUomInfoDTO> productItemUomInfoDTOS;
}