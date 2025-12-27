package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
@JsonInclude

public class VendorAutoAssignmentRequest extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -5934748077511332494L;
  private String storeId;
  private VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest;
  private String internalProcessRequestCode;
  private List<String> assigneeList = new ArrayList<>();
  private String vendorEmail;
  private int requestedSkuCount;
  private boolean defaultSettingsEnabled;
  private String vendorCode;
}
