package com.gdn.mta.bulk.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkPriceUpdateRequestData {

  private String sellerCode;
  private String productSku;
  private String parentProductName;
  private String itemSku;
  private String pickupPointCode;
  private String productName;
  private String skuCode;
  private String sellerSku;
  private String listPrice;
  private String salesPrice;
  private Integer stock;
  private Integer status;
  private Integer delivery;
  private Integer cnc;
  private Integer instore;
  private Integer warehouseStock;
  private int excelRowNumber;
  private String id;
  private String campaignCode;
  private String campaignPrice;
}
