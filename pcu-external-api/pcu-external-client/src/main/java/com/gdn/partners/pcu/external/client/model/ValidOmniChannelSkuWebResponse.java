package com.gdn.partners.pcu.external.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gda.mta.product.dto.response.OmniChannelSkuResponse;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ValidOmniChannelSkuWebResponse extends BaseResponse {

  private Map<String, OmniChannelSkuResponse> existingOmniChannelSkusAndProductDetailsMap =
    new HashMap<>();
}
