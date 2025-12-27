package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentFilterRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude

public class VendorAutoAssignmentRequest extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -6416354128227232570L;
  private String storeId;
  private VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest;
  private String internalProcessRequestCode;
  private List<String> assigneeList = new ArrayList<>();
  private String vendorEmail;
  private int requestedSkuCount;
  private boolean defaultSettingsEnabled;
  private String vendorCode;
}