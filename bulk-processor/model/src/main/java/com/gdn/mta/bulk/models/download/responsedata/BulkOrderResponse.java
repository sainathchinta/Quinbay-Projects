package com.gdn.mta.bulk.models.download.responsedata;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;

/**
 * Created by keshashah on 25/10/16.
 */
public class BulkOrderResponse extends BulkDataResponse {
  List<OrderItemSummaryResponse> responseList;

  public BulkOrderResponse(List<OrderItemSummaryResponse> responseList) {
    this.responseList = responseList;
  }

  public List<OrderItemSummaryResponse> getResponseList() {
    return responseList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("responseList", responseList).toString();
  }
}
