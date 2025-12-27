package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3StockRequest implements Serializable {

  private static final long serialVersionUID = -6496881437417699732L;
  private String gdnSku;
  private Integer deltaStock;
  private Integer minimumStock;

  public ProductLevel3StockRequest(String gdnSku, Integer deltaStock) {
    super();
    this.gdnSku = gdnSku;
    this.deltaStock = deltaStock;
  }
}
