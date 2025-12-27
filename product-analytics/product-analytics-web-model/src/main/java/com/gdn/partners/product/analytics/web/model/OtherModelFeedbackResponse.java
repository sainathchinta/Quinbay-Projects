package com.gdn.partners.product.analytics.web.model;

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
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class OtherModelFeedbackResponse implements Serializable {

  private static final long serialVersionUID = 9093027505820488863L;
  private String contentNotes;
  private Set<String> contentFeedBack = new HashSet<>();
}
