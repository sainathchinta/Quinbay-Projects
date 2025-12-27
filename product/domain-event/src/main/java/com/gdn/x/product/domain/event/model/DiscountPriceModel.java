package com.gdn.x.product.domain.event.model;

import java.util.Date;
import com.gdn.x.product.model.entity.AdjustmentType;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DiscountPriceModel {
  private double discountPrice;
  private Date startDateTime;
  private Date endDateTime;
  private String adjustmentName;
  private AdjustmentType adjustmentType;
  private String campaignCode;
}
