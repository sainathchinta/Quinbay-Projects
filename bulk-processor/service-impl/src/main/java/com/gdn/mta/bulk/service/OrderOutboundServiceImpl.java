package com.gdn.mta.bulk.service;

import java.util.Objects;

import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.mta.bulk.feignConfig.OrderFeign;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class OrderOutboundServiceImpl implements OrderOutboundService {

  @Autowired
  private OrderFeign orderFeign;

  @Override
  public GdnRestListResponse<OrderItemSummaryResponse> findOrderItemSummaryByFilter(String storeId, String requestId,
      String username, OrderItemSummaryRequest orderRequest, String orderBy, String sortBy, int page, int size) {
    GdnRestListResponse<OrderItemSummaryResponse> response =
        orderFeign.findOrderItemSummaryByFilter(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username,
            orderBy, sortBy, page, size, orderRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error Occurred while getting orders for request {}", orderRequest);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, GenericErrorMessages.SYSTEM_ERROR);
    }
    return response;
  }

  @Override
  public GdnRestListResponse<OfflinePackageSummaryResponse> findOfflinePackageSummary(String storeId, String requestId,
      String username, OrderItemSummaryRequest orderRequest, String orderBy, String sortBy, int page, int size) {
    GdnRestListResponse<OfflinePackageSummaryResponse> response =
        orderFeign.findOfflinePackageSummary(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username,
            orderBy, sortBy, page, size, orderRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error Occurred while getting orders for request {}", orderRequest);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, GenericErrorMessages.SYSTEM_ERROR);
    }
    return response;
  }

  @Override
  public GdnRestListResponse<OrderItemSummaryResponse> getOrderItemsForBulkDownload(String storeId,
      String requestId, String username, OrderItemSummaryRequest orderItemSummaryRequest, int page,
      int size) {
    GdnRestListResponse<OrderItemSummaryResponse> response =
        orderFeign.getOrderItemsForBulkDownload(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username,
            page, size, orderItemSummaryRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error Occurred while getting orders for request {}", orderItemSummaryRequest);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, GenericErrorMessages.SYSTEM_ERROR);
    }
    return response;
  }
}
