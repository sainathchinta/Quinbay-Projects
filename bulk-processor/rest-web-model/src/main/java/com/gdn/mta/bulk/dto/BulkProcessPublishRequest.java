package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@Builder
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessPublishRequest implements Serializable {

  private static final long serialVersionUID = 8834610293068216473L;
  private String bulkProcessCode;
  private String businessPartnerCode;
  private String categoryCode;
}
