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
public class BulkBrandAuthUploadModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1850284913340210490L;
  private String storeId;
  private String requestId;
  private String createdBy;
  private String bulkProcessType;
  private String bulkProcessCode;
  private String filePath;
}
