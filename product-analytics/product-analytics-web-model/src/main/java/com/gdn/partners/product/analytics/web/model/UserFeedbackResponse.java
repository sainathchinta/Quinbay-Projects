package com.gdn.partners.product.analytics.web.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class UserFeedbackResponse extends BaseResponse {

  private String productCode;
  private OtherModelFeedbackResponse otherModelFeedback;
  private List<UserImageFeedbackResponse> userImageFeedback = new ArrayList<>();
}
