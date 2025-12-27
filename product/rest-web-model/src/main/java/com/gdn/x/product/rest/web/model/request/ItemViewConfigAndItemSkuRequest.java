package com.gdn.x.product.rest.web.model.request;

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
public class ItemViewConfigAndItemSkuRequest extends ItemViewConfigRequest {
  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String pickupPointCode;
}