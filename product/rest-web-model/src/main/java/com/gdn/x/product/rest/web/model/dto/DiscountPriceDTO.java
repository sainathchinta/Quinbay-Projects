package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.AdjustmentTypeEnum;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DiscountPriceDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private double discountPrice;
  private AdjustmentTypeEnum adjustmentType;
  private Date startDateTime;
  private Date endDateTime;
  private String adjustmentName;
  private String campaignCode;
  private int priority;
  private boolean exclusiveProduct;

  public DiscountPriceDTO(double discountPrice, AdjustmentTypeEnum adjustmentType,
      Date startDateTime, Date endDateTime, String adjustmentName) {
    super();
    this.discountPrice = discountPrice;
    this.adjustmentType = adjustmentType;
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
    this.adjustmentName = adjustmentName;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }
}
