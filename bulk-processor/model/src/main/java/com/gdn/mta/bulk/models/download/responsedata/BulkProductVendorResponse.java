package com.gdn.mta.bulk.models.download.responsedata;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;


public class BulkProductVendorResponse extends BulkDataResponse {
  List<DistributionProductResponse> responseList;

  public BulkProductVendorResponse() {}

  public BulkProductVendorResponse(List<DistributionProductResponse> responseList) {
    this.responseList = responseList;
  }

  public List<DistributionProductResponse> getResponseList() {
    return responseList;
  }

  public void setResponseList(List<DistributionProductResponse> responseList) {
    this.responseList = responseList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("responseList", responseList).toString();
  }
}
