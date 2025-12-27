package com.gdn.x.product.domain.event.model;

import java.util.Date;
import java.util.List;

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
public class PriceModel {
  private String currency;
  private double offerPrice;
  private double listPrice;
  private List<DiscountPriceModel> listOfDiscountPrices;
  private DiscountPriceModel merchantPromoDiscountPrice;
  private String channel;
  private String lastUpdatedBy;
  private Date lastUpdatedDate;
}
