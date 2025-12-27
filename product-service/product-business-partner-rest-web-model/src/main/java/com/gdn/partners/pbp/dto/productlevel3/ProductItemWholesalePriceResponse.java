package com.gdn.partners.pbp.dto.productlevel3;

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
public class ProductItemWholesalePriceResponse implements Serializable {

  private static final long serialVersionUID = 2539159078634204517L;
  private int quantity;
  private double wholesaleDiscount;
}
