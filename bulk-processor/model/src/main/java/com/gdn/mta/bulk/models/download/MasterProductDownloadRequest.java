package com.gdn.mta.bulk.models.download;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterProductDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = 7271544538961326957L;
  private String filterName;
  private String productCode;
  private String categoryName;
  private String productName;
  private String categoryCode;
  private Boolean reviewPending;
  private String sortBy;

  public String getFilterName() {
    return filterName;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public String getProductName() {
    return productName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public Boolean getReviewPending() {
    return reviewPending;
  }

  public String getSortBy() {
    return sortBy;
  }

  public MasterProductDownloadRequest() {
  }

  public static class MasterProductDownloadBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private ProductListRequest productListRequest;
    private String filterName;
    private String productCode;
    private String categoryName;
    private String productName;
    private String categoryCode;
    private Boolean reviewPending;
    private String sortBy;

    public MasterProductDownloadBuilder() {}

    public MasterProductDownloadRequest.MasterProductDownloadBuilder filterName(String filterName) {
      this.filterName = filterName;
      return this;
    }

    public MasterProductDownloadRequest.MasterProductDownloadBuilder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public MasterProductDownloadRequest.MasterProductDownloadBuilder categoryName(String categoryName) {
      this.categoryName = categoryName;
      return this;
    }

    public MasterProductDownloadRequest.MasterProductDownloadBuilder productName(String productName) {
      this.productName = productName;
      return this;
    }

    public MasterProductDownloadRequest.MasterProductDownloadBuilder categoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public MasterProductDownloadRequest.MasterProductDownloadBuilder reviewPending(Boolean reviewPending) {
      this.reviewPending = reviewPending;
      return this;
    }

    public MasterProductDownloadRequest.MasterProductDownloadBuilder sortBy(String sortBy) {
      this.sortBy = sortBy;
      return this;
    }

    public MasterProductDownloadRequest build() {
      return new MasterProductDownloadRequest(this);
    }

  }

  public MasterProductDownloadRequest(MasterProductDownloadRequest.MasterProductDownloadBuilder builder) {
    super(builder);
    this.filterName = builder.filterName;
    this.productCode = builder.productCode;
    this.categoryName = builder.categoryName;
    this.categoryCode = builder.categoryCode;
    this.productName = builder.productName;
    this.reviewPending = builder.reviewPending;
    this.sortBy = builder.sortBy;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MasterProductDownloadRequest{");
    sb.append("filterName='").append(filterName).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", categoryName='").append(categoryName).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", reviewPending='").append(reviewPending).append('\'');
    sb.append(", sortBy='").append(sortBy).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
