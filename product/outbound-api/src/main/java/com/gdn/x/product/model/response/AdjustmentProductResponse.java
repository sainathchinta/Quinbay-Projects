package com.gdn.x.product.model.response;

import java.util.Date;
import java.util.Set;

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
public class AdjustmentProductResponse extends BaseResponse {

  private String adjustmentName;
  private String description;
  private String productSku;
  private Date startDate;
  private Date endDate;
  private long value;
  private Set<String> budgetOwners;
  private boolean activated;
  private boolean exclusiveProduct;
  private String campaignCode;
  private String promoType;
  private String changelog;
  private int priority;
}
