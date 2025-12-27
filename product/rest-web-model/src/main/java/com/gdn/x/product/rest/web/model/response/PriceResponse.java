package com.gdn.x.product.rest.web.model.response;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PriceResponse {
  private String id;
  private String channelId;
  private String promotionName;
  private double price;
  private double salePrice;
  private double discountPercentage;
  private Double discountAmount;
  private Date discountStartDate;
  private Date discountEndDate;
}
