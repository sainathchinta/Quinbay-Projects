package com.gdn.partners.pcu.internal.web.model.request;

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
public class ImageFeedbackWebRequest {

  private String locationPath;
  private List<String> systemFeedback = new ArrayList<>();
  private List<String> userFeedback = new ArrayList<>();
  
}
