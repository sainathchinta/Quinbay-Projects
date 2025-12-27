package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductItemUomInfoDTO implements Serializable {

  private String skuCode;
  private DistributionItemInfoRequest distributionItemInfoRequest;
  private List<DimensionAndUomDTO> dimensionsAndUOMRequest;
}