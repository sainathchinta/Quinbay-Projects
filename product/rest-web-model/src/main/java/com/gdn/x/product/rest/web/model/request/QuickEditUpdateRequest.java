package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class QuickEditUpdateRequest {
  private String itemSku;
  private PriceDTO price;
  private String status;
  private Boolean wholeSaleActivated;
  private Boolean off2OnActiveFlag;
  private String sellerSku;
  private String pickupPointCode;
  private Long version;
}
