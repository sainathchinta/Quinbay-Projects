package com.gdn.partners.pcu.external.client.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class SubmitEvidenceIPRRequest {
  private String productCode;
  private String productSku;
  private List<String> evidenceUrl = new ArrayList<>();
  private List<String> evidenceFilePath = new ArrayList<>();
  private String evidenceSubmittedNotes;
}
