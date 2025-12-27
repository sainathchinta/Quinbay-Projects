package com.gdn.mta.bulk.models;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class InternalBulkUploadDataDomainEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String updatedBy;
  private String processType;
  private String internalProcessDataRequestId;
  private List<String> internalProcessDataRequestIdList;

  public InternalBulkUploadDataDomainEventModel(String storeId, String updatedBy, String processType,
      String internalProcessDataRequestId) {
    this.storeId = storeId;
    this.updatedBy = updatedBy;
    this.processType = processType;
    this.internalProcessDataRequestId = internalProcessDataRequestId;
  }
}
