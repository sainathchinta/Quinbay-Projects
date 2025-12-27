package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemL5ListingResponse extends BaseResponse {
  private String productSku;
  private String itemSku;
  private String itemName;
  private String ppCode;
  private String ppCodeName;
  private String mainImageUrl;
  private Double offerPrice;
  private boolean cncActivated;
}
