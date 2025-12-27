package com.gdn.mta.bulk.models;

import java.io.Serializable;

public class MtaApiCreateProductQueueResult implements Serializable {
  
  private static final long serialVersionUID = 8609111244098369526L;
  private String type;
  private String productName;
  private String productCode;
  private String gdnSku;
  private String merchantSku;
  
  public MtaApiCreateProductQueueResult() {
    super();
  }
  
  public String getType() {
    return type;
  }

  public void setType(String type) {
    this.type = type;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }
  
}
