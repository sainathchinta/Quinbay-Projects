package com.gda.mta.product.dto;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProductItemHistoryForWholesale implements Serializable {

  private static final long serialVersionUID = -4069956773610962695L;

  private int quantity;
  private double wholesaleDiscount;
  private double afterDiscountSellingPrice;
}
