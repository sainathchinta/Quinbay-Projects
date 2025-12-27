package com.gdn.x.product.rest.web.model.request;

import java.util.List;

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
public class DeleteItemPickupPointRequest extends BaseRequest {
  private static final long serialVersionUID = -2302640588649283571L;

  private String productSku;
  private String pickupPointCode;
  private List<String> itemSkus;
  private String businessPartnerCode;
  private String defaultPickupPointCode;
  private boolean isDefaultPickupPointFbbActive;

  public DeleteItemPickupPointRequest(String productSku, String pickupPointCode, List<String> itemSkus,
      String businessPartnerCode) {
    this.productSku = productSku;
    this.pickupPointCode = pickupPointCode;
    this.itemSkus = itemSkus;
    this.businessPartnerCode = businessPartnerCode;
  }
}
