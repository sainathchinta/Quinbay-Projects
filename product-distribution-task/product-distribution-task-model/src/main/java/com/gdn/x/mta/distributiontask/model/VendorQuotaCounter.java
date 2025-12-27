package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

@Table(name = "PDT_VENDOR_QUOTA_COUNTER")
@Entity
public class VendorQuotaCounter extends GdnBaseEntity {

  private static final long serialVersionUID = 1L;

  @JoinColumn(name = "VENDOR")
  @OneToOne
  private Vendor vendor;

  @Column(name = "TOTAL_REVIEW_IN_PROGRESS")
  private Integer totalReviewInProgress;

  public VendorQuotaCounter() {}

  public VendorQuotaCounter(Vendor vendor, Integer totalReviewInProgress) {
    this.vendor = vendor;
    this.totalReviewInProgress = totalReviewInProgress;
  }

  public Integer getTotalReviewInProgress() {
    return totalReviewInProgress;
  }

  public Vendor getVendor() {
    return vendor;
  }

  public void setTotalReviewInProgress(Integer totalReviewInProgress) {
    this.totalReviewInProgress = totalReviewInProgress;
  }

  public void setVendor(Vendor vendor) {
    this.vendor = vendor;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("VendorQuotaCounter [vendor=");
    builder.append(vendor);
    builder.append(", totalReviewInProgress=");
    builder.append(totalReviewInProgress);
    builder.append(", toString()=");
    builder.append(super.toString());
    builder.append("]");
    return builder.toString();
  }

}
