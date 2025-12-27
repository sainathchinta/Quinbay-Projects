package com.gdn.mta.bulk.service;

import java.util.Arrays;
import java.util.Collections;

import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.OrderFeign;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;

public class OrderOutboundServiceTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "api";
  private static final String CLIENT_ID = "x-bulk";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final int PAGE = 0;
  private static final int SIZE = 1;

  @InjectMocks
  private OrderOutboundServiceImpl orderOutboundService;

  @Mock
  private OrderFeign orderFeign;

  private OrderItemSummaryRequest orderItemSummaryRequest;
  private OrderItemSummaryResponse orderItemSummaryResponse;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    orderItemSummaryRequest = new OrderItemSummaryRequest();
    orderItemSummaryResponse = new OrderItemSummaryResponse();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(orderFeign);
  }

  @Test
  public void findOrderItemSummaryByFilterTest() {
    Mockito.when(
        orderFeign.findOrderItemSummaryByFilter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY,
            SORT_BY, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(orderItemSummaryResponse),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));

    orderOutboundService.findOrderItemSummaryByFilter(STORE_ID, REQUEST_ID, USERNAME, orderItemSummaryRequest, ORDER_BY,
        SORT_BY, PAGE, SIZE);

    Mockito.verify(orderFeign)
        .findOrderItemSummaryByFilter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY, SORT_BY, PAGE,
            SIZE, orderItemSummaryRequest);
  }

  @Test
  public void findOrderItemSummaryByFilterNullTest() {
    Mockito.when(
        orderFeign.findOrderItemSummaryByFilter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY,
            SORT_BY, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(null);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> orderOutboundService.findOrderItemSummaryByFilter(STORE_ID, REQUEST_ID, USERNAME,
              orderItemSummaryRequest, ORDER_BY, SORT_BY, PAGE, SIZE));
    } finally {
      Mockito.verify(orderFeign)
          .findOrderItemSummaryByFilter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY, SORT_BY, PAGE,
              SIZE, orderItemSummaryRequest);
    }
  }

  @Test
  public void findOrderItemSummaryByFilterSuccessFalseTest() {
    Mockito.when(
        orderFeign.findOrderItemSummaryByFilter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY,
            SORT_BY, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, false, Arrays.asList(orderItemSummaryResponse),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> orderOutboundService.findOrderItemSummaryByFilter(STORE_ID, REQUEST_ID, USERNAME,
              orderItemSummaryRequest, ORDER_BY, SORT_BY, PAGE, SIZE));
    } finally {
      Mockito.verify(orderFeign)
          .findOrderItemSummaryByFilter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY, SORT_BY, PAGE,
              SIZE, orderItemSummaryRequest);
    }
  }

  @Test
  public void findOfflinePackageSummaryTest() {
    Mockito.when(
        orderFeign.findOfflinePackageSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            ORDER_BY, SORT_BY, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.singletonList(
            OfflinePackageSummaryResponse.builder()
                .orderItems(Collections.singletonList(orderItemSummaryResponse)).build()),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    orderOutboundService.findOfflinePackageSummary(STORE_ID, REQUEST_ID, USERNAME,
        orderItemSummaryRequest, ORDER_BY, SORT_BY, PAGE, SIZE);
    Mockito.verify(orderFeign)
        .findOfflinePackageSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ORDER_BY,
            SORT_BY, PAGE, SIZE, orderItemSummaryRequest);
  }

  @Test
  public void findOfflinePackageSummaryNullTest() {
    Mockito.when(
        orderFeign.findOfflinePackageSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            ORDER_BY, SORT_BY, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> orderOutboundService.findOfflinePackageSummary(STORE_ID, REQUEST_ID, USERNAME,
              orderItemSummaryRequest, ORDER_BY, SORT_BY, PAGE, SIZE));
    } finally {
      Mockito.verify(orderFeign)
          .findOfflinePackageSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
              ORDER_BY, SORT_BY, PAGE, SIZE, orderItemSummaryRequest);
    }
  }

  @Test
  public void findOfflinePackageSummarySuccessFalseTest() {
    Mockito.when(
        orderFeign.findOfflinePackageSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            ORDER_BY, SORT_BY, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, false, null, new PageMetaData(PAGE, SIZE, SIZE),
            REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> orderOutboundService.findOfflinePackageSummary(STORE_ID, REQUEST_ID, USERNAME,
              orderItemSummaryRequest, ORDER_BY, SORT_BY, PAGE, SIZE));
    } finally {
      Mockito.verify(orderFeign)
          .findOfflinePackageSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
              ORDER_BY, SORT_BY, PAGE, SIZE, orderItemSummaryRequest);
    }
  }

  @Test
  public void getOrderItemsForBulkDownloadTest() {
    Mockito.when(
        orderFeign.getOrderItemsForBulkDownload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(orderItemSummaryResponse),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));

    orderOutboundService.getOrderItemsForBulkDownload(STORE_ID, REQUEST_ID, USERNAME, orderItemSummaryRequest, PAGE, SIZE);

    Mockito.verify(
        orderFeign).getOrderItemsForBulkDownload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE, orderItemSummaryRequest);
  }

  @Test
  public void getOrderItemsForBulkDownloadNullTest() {
    Mockito.when(
        orderFeign.getOrderItemsForBulkDownload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            orderItemSummaryRequest)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> orderOutboundService.getOrderItemsForBulkDownload(STORE_ID, REQUEST_ID, USERNAME,
              orderItemSummaryRequest, PAGE, SIZE));
    } finally {
      Mockito.verify(orderFeign)
          .getOrderItemsForBulkDownload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
              orderItemSummaryRequest);
    }
  }

  @Test
  public void getOrderItemsForBulkDownloadSuccessFalseTest() {
    Mockito.when(
        orderFeign.getOrderItemsForBulkDownload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE, orderItemSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, false, Arrays.asList(orderItemSummaryResponse),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> orderOutboundService.getOrderItemsForBulkDownload(STORE_ID, REQUEST_ID, USERNAME,
              orderItemSummaryRequest, PAGE, SIZE));
    } finally {
      Mockito.verify(orderFeign)
          .getOrderItemsForBulkDownload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
              orderItemSummaryRequest);
    }
  }
}
