package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.bulk.dto.product.ProductLevel3Request;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessApiDetailUpdateRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 6655926234595064566L;
  
  private String requestId;
  private String merchantCode;
  private String remoteAddress;
  private String username;
  private List<ProductLevel3Request> productDetailRequests;
  
  public BulkProcessApiDetailUpdateRequest() {
    super();
  }

  public BulkProcessApiDetailUpdateRequest(String requestId, String merchantCode,
      String remoteAddress, String username, List<ProductLevel3Request> productDetailRequests) {
    super();
    this.requestId = requestId;
    this.merchantCode = merchantCode;
    this.remoteAddress = remoteAddress;
    this.username = username;
    this.productDetailRequests = productDetailRequests;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
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

  public List<ProductLevel3Request> getProductDetailRequests() {
    return productDetailRequests;
  }

  public void setProductDetailRequests(List<ProductLevel3Request> productDetailRequests) {
    this.productDetailRequests = productDetailRequests;
  }
  
}
