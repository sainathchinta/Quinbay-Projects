package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteOfflineItemResponse extends BaseResponse {

  private static final long serialVersionUID = -6559848175228216162L;

  private String itemSku;
  private String pickupPointCode;
  private String productSku;
  private String itemName;
  private boolean cncUpdated;
  private boolean buyableUpdated;
  private boolean discoverableUpdated;
  private boolean success;
  private String errorMessage;

}
