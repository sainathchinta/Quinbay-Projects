package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionItemRequest {

  private String origin;
  private String omniChannelSku;
  private boolean expiry;
}
