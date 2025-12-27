package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@ToString
@Data
public class BulkProcessDataDTO {
  private String id;
  private int rowNumber;
  private String bulkProcessId;
  private String bulkProcessCode;
  private String parentCode;
}
