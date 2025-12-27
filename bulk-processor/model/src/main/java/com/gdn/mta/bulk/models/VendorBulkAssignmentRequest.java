package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VendorBulkAssignmentRequest {
  private String productCode;
  private String assignedBy;
  private String assignedTo;
  private String assignmentType;
  private int rowNumber;
}
