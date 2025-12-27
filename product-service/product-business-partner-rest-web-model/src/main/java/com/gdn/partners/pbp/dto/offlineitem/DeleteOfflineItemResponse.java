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
public class DeleteOfflineItemResponse extends BaseResponse {

  private static final long serialVersionUID = 9151340714774772440L;

  private String merchantCode;
  private List<DeleteOfflineItemDetailResponse> successProducts;
  private List<DeleteOfflineItemDetailResponse> failedProducts;

}
