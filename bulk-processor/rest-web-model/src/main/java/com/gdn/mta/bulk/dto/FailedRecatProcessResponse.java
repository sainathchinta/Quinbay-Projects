package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FailedRecatProcessResponse implements Serializable {
  private static final long serialVersionUID = 1948859631939576046L;

  private String recatRequestCode;
  private int totalCount;
  private int successCount;
  private int errorCount;
}