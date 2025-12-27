package com.gda.mta.product.dto.response;

import java.util.HashMap;
import java.util.Map;

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
public class ProductCreationFailureResponse extends BaseResponse {
  private Map<String, OmniChannelSkuResponse> existingSellerSkusAndProductDetailsMap = new HashMap<>();
}
