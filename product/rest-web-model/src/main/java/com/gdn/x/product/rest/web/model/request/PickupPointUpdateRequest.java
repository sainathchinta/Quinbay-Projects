package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor

@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointUpdateRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;
  private String productSku;
  private boolean differentLocation;
  private Boolean fbbActivated;
  private List<PickupPointUpdateItemRequest> pickupPointUpdateItemRequestList;
}
