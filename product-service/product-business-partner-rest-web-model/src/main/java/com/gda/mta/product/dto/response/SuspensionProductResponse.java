package com.gda.mta.product.dto.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
public class SuspensionProductResponse extends BaseResponse {

  private String productCode;
  private String productName;
  private String productSku;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String state;
  private String errorMessage;
  private Integer itemCount;
  private List<String> itemSku = new ArrayList<>();

  public SuspensionProductResponse(String productCode, String productName, String businessPartnerCode, String errorMessage){
    this.productCode = productCode;
    this.productName = productName;
    this.businessPartnerCode = businessPartnerCode;
    this.errorMessage = errorMessage;
  }
}
