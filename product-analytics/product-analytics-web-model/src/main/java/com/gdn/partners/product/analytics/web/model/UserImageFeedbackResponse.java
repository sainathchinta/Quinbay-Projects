package com.gdn.partners.product.analytics.web.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
public class UserImageFeedbackResponse implements Serializable {

  private static final long serialVersionUID = 9159646129792028357L;

  private String locationPath;
  private String imageNotes;
  private List<String> imagePrediction = new ArrayList<>();
}
