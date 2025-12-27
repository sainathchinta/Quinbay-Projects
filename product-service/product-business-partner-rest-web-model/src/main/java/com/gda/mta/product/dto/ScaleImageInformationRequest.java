package com.gda.mta.product.dto;

import java.io.Serializable;

public class ScaleImageInformationRequest implements Serializable {

  private static final long serialVersionUID = -94776883874426711L;
  private String storeId;
  private String clientId;
  private String requestId;
  private String username;
  private String productCode;
  private String hashcode;

  public ScaleImageInformationRequest() {
    // do nothing
  }

  public ScaleImageInformationRequest(String storeId, String clientId, String requestId,
      String username, String productCode, String hashcode) {
    super();
    this.storeId = storeId;
    this.clientId = clientId;
    this.requestId = requestId;
    this.username = username;
    this.productCode = productCode;
    this.hashcode = hashcode;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getClientId() {
    return clientId;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getHashcode() {
    return hashcode;
  }

  public void setHashcode(String hashcode) {
    this.hashcode = hashcode;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ScaleImageInformationRequest [storeId=%s, clientId=%s, requestId=%s, username=%s, productCode=%s, hashCode=%s, getStoreId()=%s, getClientId()=%s, getRequestId()=%s, getUsername()=%s, getProductCode()=%s, getHashCode()=%s]",
            storeId, clientId, requestId, username, productCode, hashcode, getStoreId(),
            getClientId(), getRequestId(), getUsername(), getProductCode(), getHashcode());
  }

}
