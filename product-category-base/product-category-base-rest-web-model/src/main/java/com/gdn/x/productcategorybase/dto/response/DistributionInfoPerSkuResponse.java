package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionInfoPerSkuResponse extends BaseDTOResponse implements Serializable {
  private DistributionInfoResponse distributionInfoResponse;
  private String skuCode;
  private String itemName;
  private DistributionItemInfoResponse distributionItemInfoResponse;
  private List<DimensionsAndUomResponse> dimensionsAndUomResponse = new ArrayList<>();

  public DistributionInfoPerSkuResponse(DistributionInfoResponse distributionInfoResponse, String skuCode,
      DistributionItemInfoResponse distributionItemInfoResponse,
      List<DimensionsAndUomResponse> dimensionsAndUomResponse) {
    this.distributionInfoResponse = distributionInfoResponse;
    this.skuCode = skuCode;
    this.distributionItemInfoResponse = distributionItemInfoResponse;
    this.dimensionsAndUomResponse = dimensionsAndUomResponse;
  }
}
