package com.gdn.x.product.model.entity;

import jakarta.persistence.Enumerated;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class DiscountPrice implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  private double discountPrice;

  private Date startDateTime;

  private Date endDateTime;

  private String adjustmentName;

  private String campaignCode;

  private int priority;

  private boolean exclusiveProduct;

  @Enumerated
  private AdjustmentType adjustmentType;

  public DiscountPrice(double discountPrice, Date startDateTime, Date endDateTime,
      String adjustmentName, AdjustmentType adjustmentType) {
    super();
    this.discountPrice = Math.round(discountPrice);
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
    this.adjustmentName = adjustmentName;
    this.adjustmentType = adjustmentType;
  }

  public DiscountPrice(double discountPrice, Date startDateTime, Date endDateTime,
      String adjustmentName, AdjustmentType adjustmentType, int priority) {
    super();
    this.discountPrice = Math.round(discountPrice);
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
    this.adjustmentName = adjustmentName;
    this.adjustmentType = adjustmentType;
    this.priority = priority;
  }

  public DiscountPrice(double discountPrice, Date startDateTime, Date endDateTime,
      String adjustmentName, AdjustmentType adjustmentType, String campaignCode) {
    super();
    this.discountPrice = Math.round(discountPrice);
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
    this.adjustmentName = adjustmentName;
    this.adjustmentType = adjustmentType;
    this.campaignCode = campaignCode;
  }

}
