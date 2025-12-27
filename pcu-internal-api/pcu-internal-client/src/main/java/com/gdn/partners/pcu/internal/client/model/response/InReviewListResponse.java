package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Navya Naveli
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class InReviewListResponse extends BaseResponse {
  private ItemSkuDetailResponse masterItemSkuFirst;
  private ItemSkuDetailResponse masterItemSkuSecond;
  private long addedDate;
  private long assignedDate;
  private String assignedTo;
  private boolean reviewed;
  private String action;
}
