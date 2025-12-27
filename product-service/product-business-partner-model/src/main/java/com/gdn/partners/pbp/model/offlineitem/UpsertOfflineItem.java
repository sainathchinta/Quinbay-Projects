package com.gdn.partners.pbp.model.offlineitem;

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
public class UpsertOfflineItem implements Serializable {

  private static final long serialVersionUID = -4462341235626566267L;

  private String itemSku;
  private String pickupPointCode;
  private Integer stock;
  private Double listPrice;
  private Double offerPrice;
  private String fileName;
  private boolean cncActive;
  private boolean isBuyable;
  private boolean isDiscoverable;
  private boolean isNew;
  private String itemName;
  private String productSku;
  private String itemCode;
  private boolean fbbActivated;

  public UpsertOfflineItem(String itemSku, String pickupPointCode, Integer stock, Double listPrice, Double offerPrice,
      String fileName, boolean cncActive, boolean isBuyable, boolean isDiscoverable, boolean isNew, String itemName,
      String productSku) {
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
    this.stock = stock;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
    this.fileName = fileName;
    this.cncActive = cncActive;
    this.isBuyable = isBuyable;
    this.isDiscoverable = isDiscoverable;
    this.isNew = isNew;
    this.itemName = itemName;
    this.productSku = productSku;
  }
}
