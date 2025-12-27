package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class StoreCopyFailedProducts extends InternalBulkProcessFailedData {
  private String productCode;
  private String productName;
  private String productSku;
  private String itemSku;
  private String itemCode;
  private String copyProductName;
  private String sellerSku;
  private Double listPrice;
  private Double offerPrice;
  private Integer stock;
  private String shippingType;
  private Integer minimumStock;
  private String pickupPointCode;
  private Integer status;
  private String errorMessage;
}
