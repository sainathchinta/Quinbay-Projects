package com.gdn.mta.bulk.dto;


import lombok.Builder;

/**
 * Created by virajjasani on 27/07/16.
 */
@Builder
public class BulkUpdateErrorDTO {

  private String productName;
  private String productSku;
  private String pickupPointCode;
  private String reason;

  public BulkUpdateErrorDTO() {
  }

  public BulkUpdateErrorDTO(String productSku, String reason) {
    this.productSku = productSku;
    this.reason = reason;
  }

  public BulkUpdateErrorDTO(String productName, String productSku, String reason) {
    this.productName = productName;
    this.productSku = productSku;
    this.reason = reason;
  }

  public BulkUpdateErrorDTO(String productName, String productSku, String pickupPointCode, String reason) {
    this.productName = productName;
    this.productSku = productSku;
    this.pickupPointCode = pickupPointCode;
    this.reason = reason;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getReason() {
    return reason;
  }

  public void setReason(String reason) {
    this.reason = reason;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkUpdateErrorDTO{");
    sb.append("productName='").append(productName).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", reason='").append(reason).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
