package com.gda.mta.product.dto;

import com.gdn.mta.product.enums.ProductLevel3Status;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class QuickEditRequest {
  private String itemSku;
  private ProductLevel3PriceRequest price;
  private Integer deltaStock;
  private ProductLevel3Status status;
  private Boolean wholeSaleActivated;
  private Boolean off2OnActiveFlag;
  private String sellerSku;
  private String pickupPointCode;
  private Boolean useWarehouseStock;
  private Long version;
}
