package com.gdn.mta.bulk.dto;

import java.io.Serializable;

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
public class BulkBasicInfoRequest implements Serializable {

  private static final long serialVersionUID = 2588699778829162902L;

  private String bulkProcessType;
  private String businessPartnerCode;
  private String fileName;
  private String updatedBy;
  private boolean instoreSeller;
  private boolean trustedSeller;
  private String bulkProcessCode;
  private String storeId;
  private String filePath;
  private boolean productVideoActivated;
  private boolean bopisEligible;
}
