package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Objects;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class CompareAnchorResponse extends BaseResponse {
  private static final long serialVersionUID = -4873640586277232970L;
  private AnchorDetailResponse masterItemSkuFirst;
  private AnchorDetailResponse masterItemSkuSecond;
  private String assignedTo;
  private String action;
  @JsonIgnore
  private Boolean isReviewed;

  @JsonProperty("reviewed")
  public void setReviewed(Boolean reviewed) {
    this.isReviewed = reviewed;
  }

  @JsonProperty("isReviewed")
  public boolean getIsReviewed() {
    return Objects.nonNull(isReviewed) && isReviewed;
  }
}