package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown=true)
public class BulkProcessApiUpdateRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -2296408371616448070L;

  private String requestId;
  private String merchantCode;
  private String username;
  private String remoteAddress;
  private List<BulkProcessApiUpdateProductRequest> productRequests;

  public BulkProcessApiUpdateRequest() {
    super();
  }

  public BulkProcessApiUpdateRequest(String requestId, String merchantCode,
      List<BulkProcessApiUpdateProductRequest> productRequests) {
    super();
    this.requestId = requestId;
    this.merchantCode = merchantCode;
    this.productRequests = productRequests;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    BulkProcessApiUpdateRequest other = (BulkProcessApiUpdateRequest) obj;
    if (merchantCode == null) {
      if (other.merchantCode != null)
        return false;
    } else if (!merchantCode.equals(other.merchantCode))
      return false;
    if (productRequests == null) {
      if (other.productRequests != null)
        return false;
    } else if (!productRequests.equals(other.productRequests))
      return false;
    if (requestId == null) {
      if (other.requestId != null)
        return false;
    } else if (!requestId.equals(other.requestId))
      return false;
    return true;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public List<BulkProcessApiUpdateProductRequest> getProductRequests() {
    return productRequests;
  }

  public String getRequestId() {
    return requestId;
  }
  
  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getRemoteAddress() {
    return remoteAddress;
  }

  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((merchantCode == null) ? 0 : merchantCode.hashCode());
    result = prime * result + ((productRequests == null) ? 0 : productRequests.hashCode());
    result = prime * result + ((requestId == null) ? 0 : requestId.hashCode());
    return result;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setProductRequests(List<BulkProcessApiUpdateProductRequest> productRequests) {
    this.productRequests = productRequests;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Override
  public String toString() {
    return "BulkProcessApiUpdateRequest [requestId=" + requestId + ", merchantCode=" + merchantCode
        + ", productRequests=" + productRequests + "]";
  }

}
