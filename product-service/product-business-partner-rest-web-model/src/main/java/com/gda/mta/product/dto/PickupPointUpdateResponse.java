package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.ApiErrorCode;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointUpdateResponse extends BaseResponse {

  private static final long serialVersionUID = -8611389870882810163L;
  ApiErrorCode apiErrorCode;
  private Long l3version;

  public Long getL3version() {
    return l3version;
  }

  public void setL3version(Long l3version) {
    this.l3version = l3version;
  }

  private List<PickupPointResponse> responses = new ArrayList<>();

  public PickupPointUpdateResponse(ApiErrorCode apiErrorCode, List<PickupPointResponse> responses) {
    this.apiErrorCode = apiErrorCode;
    this.responses = responses;
  }
}
