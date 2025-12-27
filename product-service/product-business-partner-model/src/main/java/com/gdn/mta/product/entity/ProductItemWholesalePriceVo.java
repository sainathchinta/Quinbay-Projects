package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductItemWholesalePriceVo implements Serializable {

  private static final long serialVersionUID = 6643712056536575874L;
  private int quantity;
  private double wholesaleDiscount;
}
