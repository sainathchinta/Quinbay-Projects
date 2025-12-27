package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by keshashah on 20/10/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderDownloadRequest extends BulkDownloadRequest {

  private static final long serialVersionUID = -6437913223720116680L;
  private OrderItemSummaryRequest orderRequest;
  private Map<String, String> orderStatusMap;
  private String sortBy;
  private String orderBy;
  private Boolean offline;
  private boolean showPaymentMethodColumn;
  private boolean showSalesmanInfo;

  public OrderItemSummaryRequest getOrderRequest() {
    return orderRequest;
  }

  public static class OrderBuilder extends BulkDownloadRequest.BulkRequestBuilder {

    private OrderItemSummaryRequest orderRequest;
    private Map<String, String> orderStatusMap;
    private String sortBy;
    private String orderBy;
    private Boolean offline;
    private boolean showPaymentMethodColumn;

    public OrderBuilder() {
    }

    public OrderBuilder orderRequest(OrderItemSummaryRequest orderRequest) {
      this.orderRequest = orderRequest;
      return this;
    }

    public OrderBuilder statusMap(Map<String, String> statusMap) {
      this.orderStatusMap = statusMap;
      return this;
    }

    public OrderBuilder sortBy(String sortBy) {
      this.sortBy = sortBy;
      return this;
    }

    public OrderBuilder orderBy(String orderBy) {
      this.orderBy = orderBy;
      return this;
    }

    public OrderBuilder offline(Boolean offline) {
      this.offline = offline;
      return this;
    }
    public OrderBuilder showPaymentMethodColumn(boolean showPaymentMethodColumn) {
      this.showPaymentMethodColumn = showPaymentMethodColumn;
      return this;
    }

    public OrderDownloadRequest build() {
      return new OrderDownloadRequest(this);
    }
  }

  protected OrderDownloadRequest(OrderBuilder orderBuilder) {
    super(orderBuilder);
    orderRequest = orderBuilder.orderRequest;
    orderStatusMap = orderBuilder.orderStatusMap;
    sortBy = orderBuilder.sortBy;
    orderBy = orderBuilder.orderBy;
    offline = orderBuilder.offline;
    showPaymentMethodColumn = orderBuilder.showPaymentMethodColumn;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("orderRequest", orderRequest)
        .append("orderStatusMap", orderStatusMap).append("requestId", getRequestId())
        .append("downloadType", getDownloadType()).append("fileType", getFileType())
        .append("bulkProcessEntity", getBulkProcessEntity()).append("emailCc", getEmailCc())
        .append("emailTo", getEmailTo()).append("filename", getFilename())
        .append("username", getUsername()).append("merchantId", getMerchantId())
        .append("sortBy", getSortBy()).append("orderBy", getOrderBy())
        .append("showPaymentMethodColumn", isShowPaymentMethodColumn())
        .append("offline", getOffline()).toString();
  }
}
