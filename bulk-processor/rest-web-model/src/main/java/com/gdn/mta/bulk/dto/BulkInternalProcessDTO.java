package com.gdn.mta.bulk.dto;

import org.apache.poi.ss.usermodel.Workbook;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@Builder
public class BulkInternalProcessDTO {
  private String filepath;
  private String directoryPath;
  private Workbook finalWorkbook;
}
