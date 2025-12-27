package com.gdn.mta.bulk.dto;

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
public class BulkRestrictedKeywordUploadModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -996973973583282867L;
  private String storeId;
  private String requestId;
  private String updatedBy;
  private String bulkProcessType;
  private String bulkProcessCode;
  private String actionType;
  private String filePath;
}
