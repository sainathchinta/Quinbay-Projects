package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.Date;

/**
 * @author Poornima
 */
public class RejectedSkuProductCollection implements Serializable {

    private static final long serialVersionUID = -4007116256547830816L;

    private String productName;
    private String categoryName;
    private String brand;
    private Date submitDate;
    private String initiator;
    private String rejectedReason;
    private Date rejectedDate;
    private String productCode;

    public RejectedSkuProductCollection() {
        //no implementation
    }

    public RejectedSkuProductCollection(String productName, String categoryName, String brand,
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

    public String getProductCode() { return productCode; }

    public void setProductCode(String productCode) { this.productCode = productCode;  }

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

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("RejectedSkuProductCollection{");
        sb.append("productName='").append(productName).append('\'');
        sb.append(", categoryName='").append(categoryName).append('\'');
        sb.append(", brand='").append(brand).append('\'');
        sb.append(", submitDate=").append(submitDate);
        sb.append(", initiator='").append(initiator).append('\'');
        sb.append(", rejectedReason='").append(rejectedReason).append('\'');
        sb.append(", rejectedDate=").append(rejectedDate);
        sb.append(", productCode='").append(productCode).append('\'');
        sb.append('}');
        return sb.toString();
    }
}
