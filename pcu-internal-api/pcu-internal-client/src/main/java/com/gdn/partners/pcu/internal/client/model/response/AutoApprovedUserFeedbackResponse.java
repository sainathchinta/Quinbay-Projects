package com.gdn.partners.pcu.internal.client.model.response;


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
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class AutoApprovedUserFeedbackResponse extends BaseResponse {
  private String productCode;
  private OtherModelFeedbackResponse otherModelFeedback;
  private List<UserImageFeedbackResponse> userImageFeedback = new ArrayList<>();
}
