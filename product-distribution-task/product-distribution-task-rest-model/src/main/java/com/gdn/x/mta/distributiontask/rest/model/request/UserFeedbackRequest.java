package com.gdn.x.mta.distributiontask.rest.model.request;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserFeedbackRequest {
  private List<UserImageFeedbackRequest> userFeedback = new ArrayList<>();
  private Set<String> otherModelFeedBack = new HashSet<>();
}
