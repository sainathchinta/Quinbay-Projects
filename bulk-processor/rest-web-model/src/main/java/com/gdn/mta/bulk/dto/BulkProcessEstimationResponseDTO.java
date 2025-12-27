package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@ToString
@Data
public class BulkProcessEstimationResponseDTO {
  private int hour;
  private long totalRecords;
  private long totalProcessingTime;
  private long totalProcesses;
  private String fetchType;
  private String processType;

  public BulkProcessEstimationResponseDTO(int hour, long totalRecords, String processType,
    long totalProcessingTime, long totalProcesses) {
    this.hour = hour;
    this.totalRecords = totalRecords;
    this.processType = processType;
    this.totalProcessingTime = totalProcessingTime;
    this.totalProcesses = totalProcesses;
  }
}
