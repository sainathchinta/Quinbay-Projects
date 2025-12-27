package com.gdn.mta.domain.event.modal;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserFeedbackImageQcResponse {
  private String locationPath;
  private List<String> userPrediction = new ArrayList<>();
}
