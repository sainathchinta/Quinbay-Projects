package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class EvidenceSubmittedDetailResponse implements Serializable {
  private static final long serialVersionUID = -3572144993251378986L;
  private List<String> evidenceFilePath = new ArrayList<>();
  private List<String> evidenceUrl = new ArrayList<>();
  private String evidenceSubmittedNotes;
  private String evidenceSubmittedBy;
}
