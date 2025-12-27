package com.gdn.x.mta.distributiontask.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class SubmitEvidenceRequest {

  private static final long serialVersionUID = 8637855752875399324L;
  private String productCode;
  private String productSku;
  private List<String> evidenceUrl = new ArrayList<>();
  private List<String> evidenceFilePath = new ArrayList<>();
  private String evidenceSubmittedNotes;
}
