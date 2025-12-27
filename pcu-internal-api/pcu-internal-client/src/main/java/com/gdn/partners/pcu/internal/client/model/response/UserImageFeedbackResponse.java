package com.gdn.partners.pcu.internal.client.model.response;

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
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserImageFeedbackResponse implements Serializable {

  private static final long serialVersionUID = 2669744256749361700L;
  private String locationPath;
  private String imageNotes;
  private List<String> imagePrediction = new ArrayList<>();
}
