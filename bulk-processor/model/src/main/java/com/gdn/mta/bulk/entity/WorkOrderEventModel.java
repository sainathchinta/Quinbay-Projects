package com.gdn.mta.bulk.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkOrderEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 6268077733957446157L;
  private String id;
  private int rowNumber;
  private String bulkProcessId;
  private String bulkProcessCode;
  private String storeId;
  private String businessPartnerCode;
}
