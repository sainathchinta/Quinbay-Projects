package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterProductUpdateResponseDTO {

  private String productCode;
  private Boolean updateSuccess;
  private String reasonOfFailure;
  private boolean isDimensionOrDgLevelUpdated;

  public SimpleMasterProductUpdateResponseDTO(String productCode) {
    this.productCode = productCode;
  }

  public SimpleMasterProductUpdateResponseDTO(String productCode, Boolean updateSuccess) {
    this.productCode = productCode;
    this.updateSuccess = updateSuccess;
  }

  public Boolean getUpdateSuccess() {
    return updateSuccess;
  }

  public void setUpdateSuccess(Boolean updateSuccess) {
    this.updateSuccess = updateSuccess;
  }

  public String getReasonOfFailure() {
    return reasonOfFailure;
  }

  public void setReasonOfFailure(String reasonOfFailure) {
    this.reasonOfFailure = reasonOfFailure;
  }

  public String getProductCode() {

    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public boolean isDimensionOrDgLevelUpdated() {
    return isDimensionOrDgLevelUpdated;
  }

  public void setDimensionOrDgLevelUpdated(boolean dimensionOrDgLevelUpdated) {
    isDimensionOrDgLevelUpdated = dimensionOrDgLevelUpdated;
  }
}
