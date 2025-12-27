package com.gda.mta.product.dto.response;

import java.util.Date;

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
public class ReviewProductResponse extends BaseResponse {

  private String productId;
  private String productCode;
  private String productName;
  private String brand;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String assignedTo;
  private Date submittedDate;
  private String state;
  private boolean isSourceDb;
}
