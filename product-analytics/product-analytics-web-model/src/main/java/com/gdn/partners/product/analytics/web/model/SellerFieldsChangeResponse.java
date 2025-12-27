package com.gdn.partners.product.analytics.web.model;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
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
public class SellerFieldsChangeResponse extends BaseResponse {

  private String sellerCode;
  private String categoryCode;
  private boolean newData;
  private Map<String, ChangeFieldResponse> changeFieldResponseMap;
}
