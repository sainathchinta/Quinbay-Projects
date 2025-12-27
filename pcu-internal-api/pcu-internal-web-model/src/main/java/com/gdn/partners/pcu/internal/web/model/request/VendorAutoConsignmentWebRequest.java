package com.gdn.partners.pcu.internal.web.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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

public class VendorAutoConsignmentWebRequest implements Serializable {

  private static final long serialVersionUID = -3709520528760749056L;
  private VendorAutoAssignmentFilterWebRequest vendorAutoAssignmentFilterWebRequest;
  private List<String> assigneeList = new ArrayList<>();
  private String vendorEmail;
  private int requestedSkuCount;
  private boolean defaultSettingsEnabled;
}
