package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterProductUpdateResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -2240221133665087698L;
  private String productCode;
  private Boolean updateSuccess;
  private String reasonOfFailure;
  private boolean isDimensionOrDgLevelUpdated;

  public SimpleMasterProductUpdateResponse() {}

  public static class Builder {

    private String productCode;
    private Boolean updateSuccess;
    private String reasonOfFailure;

    public SimpleMasterProductUpdateResponse.Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public SimpleMasterProductUpdateResponse.Builder updateSuccess(Boolean updateSuccess) {
      this.updateSuccess = updateSuccess;
      return this;
    }

    public SimpleMasterProductUpdateResponse.Builder reasonOfFailure(String reasonOfFailure){
      this.reasonOfFailure = reasonOfFailure;
      return this;
    }

    public SimpleMasterProductUpdateResponse build() {
      return new SimpleMasterProductUpdateResponse(this);
    }
  }

  protected SimpleMasterProductUpdateResponse(SimpleMasterProductUpdateResponse.Builder builder) {
    this.productCode = builder.productCode;
    this.updateSuccess = builder.updateSuccess;
    this.reasonOfFailure = builder.reasonOfFailure;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
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

  public boolean isDimensionOrDgLevelUpdated() {
    return isDimensionOrDgLevelUpdated;
  }

  public void setDimensionOrDgLevelUpdated(boolean dimensionOrDgLevelUpdated) {
    isDimensionOrDgLevelUpdated = dimensionOrDgLevelUpdated;
  }

  @Override public String toString() {
    return new StringBuilder("SimpleMasterProductUpdateResponse [productCode=").append(productCode)
        .append(", updateSuccess=").append(updateSuccess)
        .append(", reasonOfFailure=").append(reasonOfFailure)
        .append(", toString()=").append(super.toString()).toString();
  }
}
