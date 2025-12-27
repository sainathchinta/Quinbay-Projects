package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class NewlyAddedL5Response extends BaseResponse {

  private static final long serialVersionUID = 6057338557329809406L;
  private String itemSku;
  private String pickupPointCode;
  private String itemCode;
  private String itemName;
}
