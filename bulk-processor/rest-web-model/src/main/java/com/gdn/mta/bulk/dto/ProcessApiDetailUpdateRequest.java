package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.bulk.dto.product.ProductLevel3Request;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProcessApiDetailUpdateRequest extends GdnBaseDomainEventModel implements Serializable {
  
  private static final long serialVersionUID = 5532393728914664419L;

  private String requestId;
  private String merchantCode;
  private String remoteAddress;
  private String username;
  private ProductLevel3Request productDetailRequests;
  
  public ProcessApiDetailUpdateRequest() {
    super();
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getRemoteAddress() {
    return remoteAddress;
  }

  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public ProductLevel3Request getProductDetailRequests() {
    return productDetailRequests;
  }

  public void setProductDetailRequests(ProductLevel3Request productDetailRequests) {
    this.productDetailRequests = productDetailRequests;
  }
  
}
