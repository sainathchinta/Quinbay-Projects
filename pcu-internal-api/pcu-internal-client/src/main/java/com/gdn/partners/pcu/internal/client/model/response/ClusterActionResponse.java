package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ClusterActionResponse extends BaseResponse {
  private static final long serialVersionUID = 5725825591445183744L;
  private List<ClusterItemErrorListResponse> clusterItemErrorListListResponse = new ArrayList<>();
}
