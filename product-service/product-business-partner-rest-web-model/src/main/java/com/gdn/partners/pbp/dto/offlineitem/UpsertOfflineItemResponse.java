package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpsertOfflineItemResponse extends BaseResponse {

  private static final long serialVersionUID = 2293766558878036302L;

  private String merchantCode;
  private List<UpsertOfflineItemDetailResponse> successProducts;
  private List<UpsertOfflineItemFailedResponse> failedProducts;
}
