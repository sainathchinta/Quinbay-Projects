package com.gdn.x.product.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItem {
  private String itemSku;
  private String itemName;
  private String sellerSku;
  private String upcCode;
  private List<OdooPickupPointModel> pickupPoints;
}
