package com.gdn.partners.pcu.internal.web.model.response;

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
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageFeedbackWebResponse {

  private String locationPath;
  private List<String> systemFeedback = new ArrayList<>();
  private List<String> userFeedback = new ArrayList<>();
  private boolean edited;
}
