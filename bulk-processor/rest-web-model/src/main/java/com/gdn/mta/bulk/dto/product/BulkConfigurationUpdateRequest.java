package com.gdn.mta.bulk.dto.product;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkConfigurationUpdateRequest extends GdnBaseDomainEventModel {

  private String storeId;
  private String requestId;
  private String updatedBy;
  private String bulkProcessType;
  private String bulkProcessCode;
  private String actionType;
  private String filePath;
}
