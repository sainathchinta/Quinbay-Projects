package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkMasterProductUpdateResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -5270091706017323774L;
  private List<SimpleMasterProductUpdateResponse> simpleMasterProductUpdateResponses = new ArrayList<>();

  public BulkMasterProductUpdateResponse() {}

  public BulkMasterProductUpdateResponse(List<SimpleMasterProductUpdateResponse> simpleMasterProductUpdateResponses) {
    super();
    this.simpleMasterProductUpdateResponses = simpleMasterProductUpdateResponses;
  }

  public List<SimpleMasterProductUpdateResponse> getSimpleMasterProductUpdateResponses() {
    return simpleMasterProductUpdateResponses;
  }

  public void setSimpleMasterProductUpdateResponses(
    List<SimpleMasterProductUpdateResponse> simpleMasterProductUpdateResponses) {
    this.simpleMasterProductUpdateResponses = simpleMasterProductUpdateResponses;
  }

  @Override
  public String toString() {
    return new StringBuilder("BulkMasterProductUpdateResponse [simpleMasterProductUpdateResponses=")
        .append(simpleMasterProductUpdateResponses).append(", toString()=").append(super.toString())
        .toString();
  }

}
