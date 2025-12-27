package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.Map;

/**
 * Created by virajjasani on 02/08/16.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
public class BulkUpdateProcessDTO {

  private String bulkProcessType;
  private String businessPartnerCode;
  private byte[] fileContent;
  private String fileName;
  private String updatedBy;
  private String clientHost;
  private Map<String, Boolean> privilegedMap;

  public BulkUpdateProcessDTO(String bulkProcessType, String businessPartnerCode,
    byte[] fileContent,
    String fileName, Map<String, Boolean> privilegedMap, String updatedBy, String clientHost) {
    this.bulkProcessType = bulkProcessType;
    this.businessPartnerCode = businessPartnerCode;
    this.fileContent = fileContent;
    this.fileName = fileName;
    this.privilegedMap = privilegedMap;
    this.updatedBy = updatedBy;
    this.clientHost = clientHost;
  }
}
