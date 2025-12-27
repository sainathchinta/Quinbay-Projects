package com.gdn.partners.pcu.internal.client.model.response;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

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
public class OtherModelFeedbackResponse implements Serializable {

  private static final long serialVersionUID = -7250259667777086691L;
  private String contentNotes;
  private Set<String> contentFeedBack = new HashSet<>();
}
