package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemResponse extends BaseResponse {

  private static final long serialVersionUID = 7178641687458623948L;
  private String merchantCode;
  private List<SuccessOfflineItemResponse> successProducts;
  private List<FailedOfflineItemResponse> failedProducts;

  public OfflineItemResponse() {
    super();
  }

  public OfflineItemResponse(String merchantCode, List<SuccessOfflineItemResponse> successProducts,
      List<FailedOfflineItemResponse> failedProducts) {
    super();
    this.merchantCode = merchantCode;
    this.successProducts = successProducts;
    this.failedProducts = failedProducts;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public List<SuccessOfflineItemResponse> getSuccessProducts() {
    return successProducts;
  }

  public void setSuccessProducts(List<SuccessOfflineItemResponse> successProducts) {
    this.successProducts = successProducts;
  }

  public List<FailedOfflineItemResponse> getFailedProducts() {
    return failedProducts;
  }

  public void setFailedProducts(List<FailedOfflineItemResponse> failedProducts) {
    this.failedProducts = failedProducts;
  }

  @Override
  public String toString() {
    return "OfflineItemResponse{" + "merchantCode=" + merchantCode
        + ", successProducts=" + successProducts
        + ", failedProducts=" + failedProducts + '}';
  }
}
