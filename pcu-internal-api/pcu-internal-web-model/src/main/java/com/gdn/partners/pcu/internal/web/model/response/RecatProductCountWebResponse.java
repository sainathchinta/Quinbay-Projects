package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.core.web.dto.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecatProductCountWebResponse extends BaseResponse {
  private long totalCount;
  private long successCount;
  private long inProgressCount;
  private long failedCount;
  private String status;
}
