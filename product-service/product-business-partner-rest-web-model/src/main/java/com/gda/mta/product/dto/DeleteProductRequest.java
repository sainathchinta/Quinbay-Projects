package com.gda.mta.product.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class DeleteProductRequest extends BaseRequest {

  private static final long serialVersionUID = -4024809278486591734L;
  private String productCode;
  private String productName;
  private String notes;
  private String state;

  public DeleteProductRequest() {
  }

  public DeleteProductRequest(final String productCode, final String notes) {
    this(productCode, null, notes, null);
  }

  public DeleteProductRequest(final String productCode, final String productName,
      final String notes) {
    this(productCode,productName,notes,null);
  }

  public DeleteProductRequest(String productCode, String productName, String notes, String state) {
    this.productCode = productCode;
    this.productName = productName;
    this.notes = notes;
    this.state = state;
  }

  public String getNotes() {
    return notes;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setNotes(final String notes) {
    this.notes = notes;
  }

  public void setProductCode(final String productCode) {
    this.productCode = productCode;
  }

  public void setProductName(final String productName) {
    this.productName = productName;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("productCode", productCode)
        .append("productName", productName)
        .append("notes", notes)
        .append("state", state)
        .toString();
  }
}
