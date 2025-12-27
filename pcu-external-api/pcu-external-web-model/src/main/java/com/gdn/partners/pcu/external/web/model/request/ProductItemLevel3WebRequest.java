package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

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
public class ProductItemLevel3WebRequest {
  private String itemSku;
  private String skuCode;
  private String merchantSku;
  private String upcCode;
  private String itemName;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private Integer dangerousGoodsLevel;
  private Boolean lateFulfillment;
  private String pickupPointCode;
  private String pickupPointName;
  private Integer deltaStock;
  private Integer minimumStock;
  private Boolean synchronizeStock;
  private Boolean off2OnActiveFlag;
  private List<ProductLevel3PriceWebRequest> prices;
  private List<ProductLevel3ViewConfigWebRequest> viewConfigs;
  private List<ProductLevel3ImageWebRequest> images;
  private boolean contentChanged;
}
