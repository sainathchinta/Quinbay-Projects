package com.gdn.mta.bulk.models;

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
public class InternalBrandUpdateEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String newBrandCode;
  private String oldBrandCode;
  private String newBrandName;
  private String oldBrandName;
  private String processType;
  private String updatedBy;
  private boolean brandNameUpdate;
  private String internalProcessDataRequestId;
}
