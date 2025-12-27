package com.gdn.x.mta.distributiontask.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkScreeningProductActionsDTO implements Serializable {

  private static final long serialVersionUID = -7655071934060115152L;
  private List<String> productCodes = new ArrayList<>();
  private String assignTo;
  private String assignedBy;
  private String correctionReason;
  private String additionalNotes;
  private String rejectionReason;
}
