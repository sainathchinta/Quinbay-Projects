package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Table(name = "PDT_PRODUCT_REVIEW_PERFORMANCE")
@Entity
public class ProductReviewPerformance extends GdnBaseEntity {

  private static final long serialVersionUID = 1L;

  @Column(name = "PRODUCT_CODE")
  private String productCode;

  @Column(name = "PRODUCT_NAME")
  private String productName;

  @Column(name = "CATEGORY_CODE")
  private String categoryCode;

  @Column(name = "DISTRIBUTION_TIME")
  private Long distributionTime = 0L;

  @Column(name = "IMAGE_TIME")
  private Long imageTime = 0L;

  @Column(name = "CONTENT_TIME")
  private Long contentTime = 0L;

  @Column(name = "QC_TIME")
  private Long qcTime = 0L;

  @Column(name = "DISTRIBUTION_COUNTER")
  private Integer distributionCounter = 0;

  @Column(name = "IMAGE_COUNTER")
  private Integer imageCounter = 0;

  @Column(name = "CONTENT_COUNTER")
  private Integer contentCounter = 0;

  @Column(name = "QC_COUNTER")
  private Integer qcCounter = 0;

  public ProductReviewPerformance() {}

  public ProductReviewPerformance(String productCode, String productName, String categoryCode, Long distributionTime,
      Long imageTime, Long contentTime, Long qcTime, Integer distributionCounter, Integer imageCounter,
      Integer contentCounter, Integer qcCounter) {
    super();
    this.productCode = productCode;
    this.productName = productName;
    this.categoryCode = categoryCode;
    this.distributionTime = distributionTime;
    this.imageTime = imageTime;
    this.contentTime = contentTime;
    this.qcTime = qcTime;
    this.distributionCounter = distributionCounter;
    this.imageCounter = imageCounter;
    this.contentCounter = contentCounter;
    this.qcCounter = qcCounter;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public Long getDistributionTime() {
    return distributionTime;
  }

  public void setDistributionTime(Long distributionTime) {
    this.distributionTime = distributionTime;
  }

  public Long getImageTime() {
    return imageTime;
  }

  public void setImageTime(Long imageTime) {
    this.imageTime = imageTime;
  }

  public Long getContentTime() {
    return contentTime;
  }

  public void setContentTime(Long contentTime) {
    this.contentTime = contentTime;
  }

  public Long getQcTime() {
    return qcTime;
  }

  public void setQcTime(Long qcTime) {
    this.qcTime = qcTime;
  }

  public Integer getDistributionCounter() {
    return distributionCounter;
  }

  public void setDistributionCounter(Integer distributionCounter) {
    this.distributionCounter = distributionCounter;
  }

  public Integer getImageCounter() {
    return imageCounter;
  }

  public void setImageCounter(Integer imageCounter) {
    this.imageCounter = imageCounter;
  }

  public Integer getContentCounter() {
    return contentCounter;
  }

  public void setContentCounter(Integer contentCounter) {
    this.contentCounter = contentCounter;
  }

  public Integer getQcCounter() {
    return qcCounter;
  }

  public void setQcCounter(Integer qcCounter) {
    this.qcCounter = qcCounter;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductReviewPerformance [productCode=");
    builder.append(productCode);
    builder.append(", productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", distributionTime=");
    builder.append(distributionTime);
    builder.append(", imageTime=");
    builder.append(imageTime);
    builder.append(", contentTime=");
    builder.append(contentTime);
    builder.append(", qcTime=");
    builder.append(qcTime);
    builder.append(", distributionCounter=");
    builder.append(distributionCounter);
    builder.append(", imageCounter=");
    builder.append(imageCounter);
    builder.append(", contentCounter=");
    builder.append(contentCounter);
    builder.append(", qcCounter=");
    builder.append(qcCounter);
    builder.append("]");
    return builder.toString();
  }

}
