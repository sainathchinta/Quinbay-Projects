package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicMasterDataItemDTO {

  private double itemDeliveryWeight;
  private double itemHeight;
  private double itemLength;
  private double itemWeight;
  private double itemWidth;
  private int dangerousLevel;

  public BasicMasterDataItemDTO(double itemDeliveryWeight) {
    this.itemDeliveryWeight = itemDeliveryWeight;
  }

  public BasicMasterDataItemDTO(double itemDeliveryWeight, double itemHeight, double itemLength, double itemWeight,
      double itemWidth) {
    this.itemDeliveryWeight = itemDeliveryWeight;
    this.itemHeight = itemHeight;
    this.itemLength = itemLength;
    this.itemWeight = itemWeight;
    this.itemWidth = itemWidth;
  }
}
