package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkMasterProductUpdateRequest extends BaseDTORequest {

  private static final long serialVersionUID = 2430128612854758752L;
  private List<SimpleMasterProductUpdateRequest> simpleMasterProductUpdateRequests = new ArrayList<>();

  public BulkMasterProductUpdateRequest() {}

  public BulkMasterProductUpdateRequest(List<SimpleMasterProductUpdateRequest> simpleMasterProductUpdateRequests) {
    super();
    this.simpleMasterProductUpdateRequests = simpleMasterProductUpdateRequests;
  }

  public List<SimpleMasterProductUpdateRequest> getSimpleMasterProductUpdateRequests() {
    return simpleMasterProductUpdateRequests;
  }

  public void setSimpleMasterProductUpdateRequests(
    List<SimpleMasterProductUpdateRequest> simpleMasterProductUpdateRequests) {
    this.simpleMasterProductUpdateRequests = simpleMasterProductUpdateRequests;
  }

  @Override
  public String toString() {
    return new StringBuilder("BulkMasterProductUpdateRequest [simpleMasterProductUpdateRequests=")
        .append(simpleMasterProductUpdateRequests).append(", toString()=").append(super.toString())
        .toString();
  }

}
