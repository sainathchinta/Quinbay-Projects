package com.gdn.partners.pcu.internal.client.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovedUserFeedbackRequest {
  private List<UserImageFeedbackRequest> userImageFeedback = new ArrayList<>();
  private OtherModelFeedbackRequest otherModelFeedbackRequest;
}
