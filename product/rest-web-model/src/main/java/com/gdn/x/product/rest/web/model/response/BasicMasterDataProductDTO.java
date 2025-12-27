package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicMasterDataProductDTO {

  private String description;
  private String brand;
  private double shippingWeight;

  public BasicMasterDataProductDTO(String brand, double shippingWeight) {
    this.brand = brand;
    this.shippingWeight = shippingWeight;
  }
}
