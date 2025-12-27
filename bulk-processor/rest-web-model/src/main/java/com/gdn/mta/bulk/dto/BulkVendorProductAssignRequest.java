package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

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
public class BulkVendorProductAssignRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -140335219432450164L;

  private String storeId;
  private String requestId;
  private String updatedBy;
  private String bulkProcessType;
  private String bulkProcessCode;
  private String assignmentType;
  private String filePath;
  private Map<String, List<String>> validUserRoleList;
  private String vendorCode;
  private String internalProcessRequestCode;
}
