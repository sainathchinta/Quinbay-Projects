package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.AdjustmentTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DiscountPriceRequest implements Serializable {

  private static final long serialVersionUID = -4203110833121113827L;

  private double discountPrice;
  private AdjustmentTypeEnum adjustmentType;
  private Date startDateTime;
  private Date endDateTime;
  private String adjustmentName;

}
