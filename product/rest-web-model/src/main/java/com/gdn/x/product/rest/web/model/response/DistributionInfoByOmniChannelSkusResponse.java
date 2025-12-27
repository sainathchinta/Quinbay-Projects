package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionInfoByOmniChannelSkusResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;

  private List<OmniChannelSkuDetailResponse> omniChannelSkuDetails = new ArrayList<>();
}