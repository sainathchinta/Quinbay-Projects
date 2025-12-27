package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductPriceAndStockUpdateWebRequest {

  private String categoryCode;
  private String itemSku;
  private String skuCode;
  private int deltaStock;
  private int minimumStock;
  private String merchantSku;
  private Long version;
  private boolean synchronizeStock;
  private Boolean wholesalePriceActivated;
  List<PriceUpdateWebRequest> prices;
  List<ProductItemWholesalePriceWebRequest> productItemWholesalePriceRequests;
  private String pickupPointCode;
}
