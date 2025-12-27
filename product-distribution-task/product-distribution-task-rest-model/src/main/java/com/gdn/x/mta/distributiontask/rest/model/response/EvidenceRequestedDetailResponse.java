package com.gdn.x.mta.distributiontask.rest.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class EvidenceRequestedDetailResponse implements Serializable {

  private static final long serialVersionUID = 4006186947870397584L;
  private Date evidenceRequestedDate;
  private String evidenceRequestedReasons;
  private String violationType;
  private String evidenceRequestedNotes;
  private String reviewerNotes;
  private String evidenceRequestedBy;
}
