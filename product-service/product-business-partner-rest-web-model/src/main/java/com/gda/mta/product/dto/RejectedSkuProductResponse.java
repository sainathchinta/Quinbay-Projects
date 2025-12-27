/**
 *
 */
package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

import java.util.Date;

/**
 * @author Poornima
 */
public class RejectedSkuProductResponse extends BaseResponse {

    private static final long serialVersionUID = 6641115138940497039L;

    private String productName;
    private String categoryName;
    private String brand;
    private Date submitDate;
    private String initiator;
    private String rejectedReason;
    private Date rejectedDate;
    private String productCode;

    public String getProductCode() { return productCode; }

    public void setProductCode(String productCode) { this.productCode = productCode; }


    public RejectedSkuProductResponse() {
        //no implementation
    }

  public RejectedSkuProductResponse(String productName, String categoryName, String brand,
      Date submitDate, String initiator, String rejectedReason, Date rejectedDate,
      String productCode) {
    super();
    this.productName = productName;
    this.categoryName = categoryName;
    this.brand = brand;
    this.submitDate = submitDate;
    this.initiator = initiator;
    this.rejectedReason = rejectedReason;
    this.rejectedDate = rejectedDate;
    this.productCode = productCode;
  }

  public RejectedSkuProductResponse(String productName, String categoryName, String brand,
      Date submitDate, String initiator, String rejectedReason, Date rejectedDate) {
    super();
    this.productName = productName;
    this.categoryName = categoryName;
    this.brand = brand;
    this.submitDate = submitDate;
    this.initiator = initiator;
    this.rejectedReason = rejectedReason;
    this.rejectedDate = rejectedDate;
  }

    public String getProductName() {
        return productName;
    }

    public void setProductName(String productName) {
        this.productName = productName;
    }

    public String getCategoryName() {
        return categoryName;
    }

    public void setCategoryName(String categoryName) {
        this.categoryName = categoryName;
    }

    public String getBrand() {
        return brand;
    }

    public void setBrand(String brand) {
        this.brand = brand;
    }

    public Date getSubmitDate() {
        return submitDate;
    }

    public void setSubmitDate(Date submitDate) {
        this.submitDate = submitDate;
    }

    public String getInitiator() {
        return initiator;
    }

    public void setInitiator(String initiator) {
        this.initiator = initiator;
    }

    public String getRejectedReason() {
        return rejectedReason;
    }

    public void setRejectedReason(String rejectedReason) {
        this.rejectedReason = rejectedReason;
    }

    public Date getRejectedDate() {
        return rejectedDate;
    }

    public void setRejectedDate(Date rejectedDate) {
        this.rejectedDate = rejectedDate;
    }

    @Override public String toString() {
        final StringBuilder sb = new StringBuilder("{RejectedSkuProductResponse - ");
        sb.append("productName").append(productName).append(" ");
        sb.append(", categoryName: ").append(categoryName);
        sb.append(", brand: ").append(brand);
        sb.append(", submitDate: ").append(submitDate);
        sb.append(", initiator: ").append(initiator);
        sb.append(", rejectedReason: ").append(rejectedReason);
        sb.append(", rejectedDate: ").append(rejectedDate);
        sb.append(", productCode: ").append(productCode);
        sb.append("}");
        return sb.toString();
    }

}
