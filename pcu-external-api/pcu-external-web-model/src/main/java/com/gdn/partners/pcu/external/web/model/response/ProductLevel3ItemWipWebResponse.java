package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ItemWipWebResponse {

  private String productLevel1ItemId;
  private String gdnSku;
  private String merchantSku;
  private String pickupPointCode;
  private Integer productType;
  private Double price;
  private Double salePrice;
  private Integer stock;
  private Integer minimumStock;
  private boolean display;
  private boolean buyable;
  private boolean needInstallation;
  private Boolean wholesalePriceActivated;
  private List<ProductItemWholesalePriceWebResponse> productItemWholesalePriceResponses;
}
