package com.gdn.x.product.rest.web.model.request;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoCreatePickupPointRequest extends BaseRequest {
  private static final long serialVersionUID = -3724593212210064863L;

  private String webItemSku;
  private String merchantCode;
  private String pickupPointCode;
  private Set<String> ppCodeInRegion;
  private String warehouseItemSku;
  private String webProductSku;
  private boolean deliveryFlag;
}
