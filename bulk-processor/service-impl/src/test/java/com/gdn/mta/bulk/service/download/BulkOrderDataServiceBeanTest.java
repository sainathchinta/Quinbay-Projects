package com.gdn.mta.bulk.service.download;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.mta.bulk.service.OrderHistoryOutboundService;
import com.gdn.x.neo.order.client.sdk.model.OrderItemStatus;
import com.gdn.x.neo.order.client.sdk.model.OrderType;
import com.gdn.x.neo.order.client.sdk.web.model.request.DateRangeRequest;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import org.apache.commons.lang.time.DateUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkOrderResponse;
import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.mta.bulk.service.OrderHistoryOutboundService;
import com.gdn.mta.bulk.service.OrderOutboundService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.neo.order.client.sdk.model.OrderItemStatus;
import com.gdn.x.neo.order.client.sdk.model.OrderType;
import com.gdn.x.neo.order.client.sdk.web.model.request.DateRangeRequest;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import org.apache.commons.lang.time.DateUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Created by keshashah on 14/11/16.
 */
public class BulkOrderDataServiceBeanTest {

  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";

  @Mock
  private OrderOutboundService orderOutboundService;

  @Mock
  private OrderHistoryOutboundService orderHistoryOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @InjectMocks
  private BulkOrderDataServiceBean bulkOrderDataServiceBean;
  private List<OrderItemSummaryResponse> responseList;
  private List<OfflinePackageSummaryResponse> offlineResponseList;

  private String ORDER_ID = "1234567";
  private String ORDER_ITEM_ID = "7654321";
  private String AWB_NUMBER = "AWB-123456";
  private String MERCHANT_SKU = "MRC-123456";
  private String STORE_ID = "10001";
  private static final String PICKUP_POINT_REQUEST = "PP-1,PP-2,PP-3,PP-4,PP-5";

  private Double PRICE = 10000.00;
  private Double MERCHANT_ADJUSTMENT = 3000.00;
  private static final String BULK_DOWNLOAD_BATCH_SIZE = "1";
  private static final String PICKUP_POINT_DOWNLOAD_BATCH_SIZE = "2";
  private static final String BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS = "SCAN_AND_GO_WEB,SCAN_AND_GO_IOS";
  private static final String MERCHANT_PICKUP_POINT_NAME = "pp-name";
  private static final String SHIP_STREET_ADDR = "addr";
  private static final String SHIP_CITY = "city";
  private static final String SHIP_PROVINCE = "province";
  private static final String DISC_NAME = "disc-name";
  private static final String DISC_CODE = "disc-code";
  private static final String SELLER_VOUCHER_NAME = "seller-voucher-name";
  private static final String SELLER_VOUCHER_ID = "seller-voucher-id";
  private static final String SHIPPING_VOUCHER_NAME = "shipping-voucher-name";
  private static final String SHIPPING_VOUCHER_ID = "shipping-voucher-id";
  private static final String ITEM_NOTES = "item-notes";
  private static final Date D_TIMESTAMP = new Date();

  private OrderDownloadRequest bulkDownloadRequest;
  private SystemParameterConfig solrSwitch;
  private SystemParameterConfig orderBatchSize;
  private SystemParameterConfig pickupPointBatchSize;
  private SystemParameterConfig scanAndGoChannels;
  private Map<String, SystemParameterConfig> systemParameterConfigMap = new HashMap<>();
  private OrderDownloadRequest.OrderBuilder orderBuilder = new OrderDownloadRequest.OrderBuilder();

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    responseList = new ArrayList<>();
    OrderItemSummaryResponse itemSummaryResponse = new OrderItemSummaryResponse();
    itemSummaryResponse.setOrderItemStatus(OrderItemStatus.BP.name());
    itemSummaryResponse = new OrderItemSummaryResponse();
    itemSummaryResponse.setOrderItemStatus(OrderItemStatus.BP.name());
    itemSummaryResponse.setStatusDescription(OrderItemStatus.BP.name());
    itemSummaryResponse.setOrderId(ORDER_ID);
    itemSummaryResponse.setOrderItemId(ORDER_ITEM_ID);
    itemSummaryResponse.setAwbNumber(AWB_NUMBER);
    itemSummaryResponse.setMerchantSku(MERCHANT_SKU);
    itemSummaryResponse.setPrice(PRICE);
    itemSummaryResponse.setMerchantAdjustment(MERCHANT_ADJUSTMENT);
    itemSummaryResponse.setShippingStreetAddress(SHIP_STREET_ADDR);
    itemSummaryResponse.setShippingCity(SHIP_CITY);
    itemSummaryResponse.setShippingProvince(SHIP_PROVINCE);
    itemSummaryResponse.setTotalOrderItemPrice(PRICE);
    itemSummaryResponse.setTotalOrderPrice(PRICE);
    itemSummaryResponse.setSellerDiscount(PRICE);
    itemSummaryResponse.setDiscountName(DISC_NAME);
    itemSummaryResponse.setDiscountCode(DISC_CODE);
    itemSummaryResponse.setSellerVoucher(PRICE);
    itemSummaryResponse.setMerchantVoucherName(SELLER_VOUCHER_NAME);
    itemSummaryResponse.setMerchantVoucherCode(SELLER_VOUCHER_ID);
    itemSummaryResponse.setShippingVoucherName(SHIPPING_VOUCHER_NAME);
    itemSummaryResponse.setShippingVoucherCode(SHIPPING_VOUCHER_ID);
    itemSummaryResponse.setItemNotes(ITEM_NOTES);
    itemSummaryResponse.setStatusDUpdatedTimestamp(D_TIMESTAMP);
    responseList.add(itemSummaryResponse);

    OrderItemSummaryResponse itemSummaryResponse2 = new OrderItemSummaryResponse();
    itemSummaryResponse2 = new OrderItemSummaryResponse();
    itemSummaryResponse2.setOrderItemStatus(OrderItemStatus.BP.name());
    itemSummaryResponse2.setOrderId(ORDER_ID);
    itemSummaryResponse2.setOrderItemId(ORDER_ITEM_ID);
    itemSummaryResponse2.setAwbNumber(AWB_NUMBER);
    itemSummaryResponse2.setMerchantSku(MERCHANT_SKU);
    itemSummaryResponse2.setPrice(PRICE);
    itemSummaryResponse2.setShippingStreetAddress(SHIP_STREET_ADDR);
    itemSummaryResponse2.setShippingCity(SHIP_CITY);
    itemSummaryResponse2.setShippingProvince(SHIP_PROVINCE);
    itemSummaryResponse2.setTotalOrderItemPrice(PRICE);
    itemSummaryResponse2.setTotalOrderPrice(PRICE);
    itemSummaryResponse2.setSellerDiscount(PRICE);
    itemSummaryResponse2.setDiscountName(DISC_NAME);
    itemSummaryResponse2.setDiscountCode(DISC_CODE);
    itemSummaryResponse2.setSellerVoucher(PRICE);
    itemSummaryResponse2.setMerchantVoucherName(SELLER_VOUCHER_NAME);
    itemSummaryResponse2.setMerchantVoucherCode(SELLER_VOUCHER_ID);
    itemSummaryResponse2.setShippingVoucherName(SHIPPING_VOUCHER_NAME);
    itemSummaryResponse2.setShippingVoucherCode(SHIPPING_VOUCHER_ID);
    itemSummaryResponse2.setItemNotes(ITEM_NOTES);
    itemSummaryResponse2.setStatusDUpdatedTimestamp(D_TIMESTAMP);
    responseList.add(itemSummaryResponse2);

    offlineResponseList = new ArrayList<>();
    offlineResponseList.add(new OfflinePackageSummaryResponse("package-id", responseList));

    Map<String, String> orderStatusMap = new HashMap<>();
    orderStatusMap.put(OrderItemStatus.BP.name(), OrderItemStatus.BP.getExternalDescription());
    orderBuilder.orderRequest(new OrderItemSummaryRequest());
    orderBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    orderBuilder.emailCC("kesha@xyz.com");
    orderBuilder.emailTo("kesha@xyz.com");
    orderBuilder.filename("test.xlsx");
    orderBuilder.downloadType(DownloadType.ALL);
    orderBuilder.merchant("m-123");
    orderBuilder.request("123");
    orderBuilder.orderBy(ORDER_BY);
    orderBuilder.sortBy(SORT_BY);
    orderBuilder.statusMap(orderStatusMap);
    bulkDownloadRequest = orderBuilder.build();
    solrSwitch =
      new SystemParameterConfig(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        Boolean.TRUE.toString(),
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    orderBatchSize =
      new SystemParameterConfig(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE, BULK_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    pickupPointBatchSize =
      new SystemParameterConfig(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE, PICKUP_POINT_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE);
    scanAndGoChannels = new SystemParameterConfig(
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS,
        BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS);
    systemParameterConfigMap.put(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
      solrSwitch);
    systemParameterConfigMap.put(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
      orderBatchSize);
    systemParameterConfigMap.put(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
      pickupPointBatchSize);
    systemParameterConfigMap.put(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS,
        scanAndGoChannels);
    Mockito.when(
        this.systemParameterConfigService.findValueByStoreIdAndVariables(eq(Constant.STORE_ID), eq(
          Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS))))
      .thenReturn(systemParameterConfigMap);
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch",
        Boolean.FALSE);
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistoryDaysLimit",
        365);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(orderOutboundService, systemParameterConfigService,
        orderHistoryOutboundService);
  }

  @Test
  public void getData_data_exist_success() throws Exception {
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_offline_data_exist_success() throws Exception {
    offlineResponseList.get(0).getOrderItems().get(0).setItemNotes("Notes");
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_clickAndCollect_data_exist_success() throws Exception {
    offlineResponseList.get(0).getOrderItems().get(0).setItemNotes("Notes");
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_scanAndGo_data_exist_success() throws Exception {
    offlineResponseList.get(0).getOrderItems().get(0).setBusinessChannel(
        BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS.split(Constant.COMMA)[0]);
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_off2On_data_exist_success() throws Exception {
    offlineResponseList.get(0).getOrderItems().get(0).setOrderType(OrderType.OFF2ON.getValue());
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_when_supermarketId_isPresent() throws Exception {
    offlineResponseList.get(0).getOrderItems().get(0).setOrderType(OrderType.B2C_RETAIL.getValue());
    offlineResponseList.get(0).getOrderItems().get(0).setSupermarketId(Constant.SUPERMARKET);
    offlineResponseList.get(0).getOrderItems().get(0).setPaymentMethod("Credit Card");
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertEquals(response.getResponseList().get(0).getOrderType(), Constant.SUPERMARKET);
    Assertions.assertEquals(response.getResponseList().get(0).getPaymentMethod(), "Credit Card");
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_off2On_data_exist_success_history() throws Exception {
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch",
        Boolean.TRUE);
    offlineResponseList.get(0).getOrderItems().get(0).setOrderType(OrderType.OFF2ON.getValue());
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    when(orderHistoryOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    bulkDownloadRequest.getOrderRequest().setStatusFPDateRange(new DateRangeRequest() {{
      setStartDate(DateUtils.addDays(new Date(), -400));
      setEndDate(new Date());
    }});
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    verify(orderHistoryOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(8, response.getResponseList().size());
  }

  @Test
  public void getData_off2On_data_exist_success_history_no_calls() throws Exception {
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch",
        Boolean.TRUE);
    offlineResponseList.get(0).getOrderItems().get(0).setOrderType(OrderType.OFF2ON.getValue());
    bulkDownloadRequest.setOffline(true);
    bulkDownloadRequest.getOrderRequest().setStatusFPDateRange(new DateRangeRequest() {{
      setStartDate(DateUtils.addDays(new Date(), 2));
      setEndDate(DateUtils.addDays(new Date(), -400));
    }});
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertTrue(response.getResponseList().isEmpty());
  }

  @Test
  public void getData_offlinePlus_data_exist_success() throws Exception {
    offlineResponseList.get(0).getOrderItems().get(0).setOrderType(OrderType.OFFLINE_PLUS.getValue());
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    bulkDownloadRequest.setOffline(true);
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(4, response.getResponseList().size());
  }

  @Test
  public void getData_NullOrder_WriteLog() throws Exception {
    when(orderOutboundService.findOrderItemSummaryByFilter(anyString(), anyString(), anyString(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1 , 0, 1), "123"));


    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);

    verify(orderOutboundService).findOrderItemSummaryByFilter(Mockito.eq(Constant.STORE_ID),
        Mockito.eq(bulkDownloadRequest.getRequestId()), Mockito.eq(bulkDownloadRequest.getUsername()),
        Mockito.any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), Mockito.eq(0),
      Mockito.eq(1));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertTrue(response.getResponseList().isEmpty());
    Assertions.assertEquals(0, response.getResponseList().size());
  }

  @Test
  public void getData_orderStatusNotFound_setDefaultStatus() throws Exception {
    List<OrderItemSummaryResponse> responseList = new ArrayList<>();
    OrderItemSummaryResponse itemSummaryResponse = new OrderItemSummaryResponse();
    itemSummaryResponse.setOrderItemStatus(OrderItemStatus.BP.name());
    responseList.add(itemSummaryResponse);
    ((OrderDownloadRequest) bulkDownloadRequest).getOrderStatusMap().remove(OrderItemStatus.BP.name());
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(0, 4000, 1), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);

    verify(orderOutboundService).findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));

    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse orderResponse = (BulkOrderResponse) dataResponse;
    Assertions.assertEquals(responseList.size(), orderResponse.getResponseList().size());
    Assertions.assertEquals("-", orderResponse.getResponseList().get(0).getOrderItemStatus());
  }

  @Test
  public void getDataSolrDisabledTest() throws Exception {
    DateRangeRequest dateRangeRequest = new DateRangeRequest();
    dateRangeRequest.setStartDate(DateUtils.addDays(D_TIMESTAMP, -2));
    dateRangeRequest.setEndDate(D_TIMESTAMP);
    OrderItemSummaryRequest orderItemSummaryRequest = new OrderItemSummaryRequest();
    orderItemSummaryRequest.setStatusFPDateRange(dateRangeRequest);
    orderBuilder.orderRequest(orderItemSummaryRequest);
    bulkDownloadRequest = orderBuilder.build();
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch", true);
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistoryDaysLimit", 3);
    solrSwitch.setValue(Boolean.FALSE.toString());
    when(orderOutboundService.getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt());
  }

  @Test
  public void getDataSolrDisabled_1stPageExceptionTest() throws Exception {
    solrSwitch.setValue(Boolean.FALSE.toString());
    when(orderOutboundService.getOrderItemsForBulkDownload(any(), any(), any(),
      any(OrderItemSummaryRequest.class), anyInt(), anyInt()))
      .thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2),
          "123"))
      .thenThrow(new ApplicationRuntimeException());
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).getOrderItemsForBulkDownload(any(), any(), any(),
      any(OrderItemSummaryRequest.class), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertEquals(2, response.getResponseList().size());
  }

  @Test
  public void getData_data_exist_success_2ndPageException() throws Exception {
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
      any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(),
      anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"))
      .thenThrow(new ApplicationRuntimeException());
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOrderItemSummaryByFilter(any(), any(), any(),
      any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertEquals(2, response.getResponseList().size());
  }

  @Test
  public void getData_offline_data_exist_success_2ndPageException() throws Exception {
    bulkDownloadRequest.setOffline(true);
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
      any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(),
      anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"))
      .thenThrow(new ApplicationRuntimeException());
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
      any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertEquals(2, response.getResponseList().size());
  }

  @Test
  public void getData_success_withPickupPoints() throws Exception {
    ((OrderDownloadRequest) bulkDownloadRequest).getOrderRequest()
      .setPickupPointCode(PICKUP_POINT_REQUEST);
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
      any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
      new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
      eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(6)).findOrderItemSummaryByFilter(any(), any(),
        any(),
      any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    BulkOrderResponse response = (BulkOrderResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(12, response.getResponseList().size());
  }

  @Test
  public void getDataFromXOrderAndOrderHistoryTest() throws Exception {
    DateRangeRequest dateRangeRequest = new DateRangeRequest();
    dateRangeRequest.setStartDate(DateUtils.addDays(D_TIMESTAMP, -5));
    dateRangeRequest.setEndDate(D_TIMESTAMP);
    OrderItemSummaryRequest orderItemSummaryRequest = new OrderItemSummaryRequest();
    orderItemSummaryRequest.setStatusFPDateRange(dateRangeRequest);
    orderBuilder.orderRequest(orderItemSummaryRequest);
    bulkDownloadRequest = orderBuilder.build();
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch", true);
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistoryDaysLimit", 3);
    solrSwitch.setValue(Boolean.FALSE.toString());
    when(orderOutboundService.getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    when(orderHistoryOutboundService.getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt());
    verify(orderHistoryOutboundService, times(2)).getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt());
  }

  @Test
  public void getDataFromOrderHistoryTest() throws Exception {
    DateRangeRequest dateRangeRequest = new DateRangeRequest();
    dateRangeRequest.setStartDate(DateUtils.addDays(D_TIMESTAMP, -10));
    dateRangeRequest.setEndDate(DateUtils.addDays(D_TIMESTAMP, -5));
    OrderItemSummaryRequest orderItemSummaryRequest = new OrderItemSummaryRequest();
    orderItemSummaryRequest.setStatusFPDateRange(dateRangeRequest);
    orderBuilder.orderRequest(orderItemSummaryRequest);
    bulkDownloadRequest = orderBuilder.build();
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch", true);
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistoryDaysLimit", 3);
    solrSwitch.setValue(Boolean.FALSE.toString());
    when(orderHistoryOutboundService.getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderHistoryOutboundService, times(2)).getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt());
  }

  @Test
  public void getDataFromOrderHistoryExceptionTest() throws Exception {
    DateRangeRequest dateRangeRequest = new DateRangeRequest();
    dateRangeRequest.setStartDate(DateUtils.addDays(D_TIMESTAMP, -10));
    dateRangeRequest.setEndDate(DateUtils.addDays(D_TIMESTAMP, -5));
    OrderItemSummaryRequest orderItemSummaryRequest = new OrderItemSummaryRequest();
    orderItemSummaryRequest.setStatusFPDateRange(dateRangeRequest);
    orderBuilder.orderRequest(orderItemSummaryRequest);
    bulkDownloadRequest = orderBuilder.build();
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistorySwitch", true);
    ReflectionTestUtils.setField(bulkOrderDataServiceBean, "orderBulkDownloadHistoryDaysLimit", 3);
    solrSwitch.setValue(Boolean.FALSE.toString());
    when(orderHistoryOutboundService.getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt())).thenReturn(
            new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"))
        .thenThrow(new ApplicationRuntimeException());
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderHistoryOutboundService, times(2)).getOrderItemsForBulkDownload(any(), any(), any(),
        any(OrderItemSummaryRequest.class), anyInt(), anyInt());
  }

  @Test
  public void getData_when_salesmanEmail_isNotEmpty_shouldSetShowSalesmanInfo() throws Exception {
    bulkDownloadRequest.setShowSalesmanInfo(false);
    responseList.get(0).setSalesmanEmail("salesman@example.com");
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    Assertions.assertTrue(bulkDownloadRequest.isShowSalesmanInfo());
  }

  @Test
  public void getData_when_salesman_info_isEmpty_shouldNotSetShowSalesmanInfo() throws Exception {
    bulkDownloadRequest.setShowSalesmanInfo(false);
    responseList.get(0).setSalesmanEmail(null);
    responseList.get(0).setSalesmanName(null);
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    Assertions.assertFalse(bulkDownloadRequest.isShowSalesmanInfo());
  }

  @Test
  public void getData_when_showSalesmanInfo_isAlreadyTrue_shouldRemainTrue() throws Exception {
    bulkDownloadRequest.setShowSalesmanInfo(true);
    responseList.get(0).setSalesmanEmail("salesman@example.com");
    when(orderOutboundService.findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(responseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOrderItemSummaryByFilter(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    Assertions.assertTrue(bulkDownloadRequest.isShowSalesmanInfo());
  }

  @Test
  public void getData_offline_when_salesmanName_isNotEmpty_shouldSetShowSalesmanInfo() throws Exception {
    bulkDownloadRequest.setOffline(Boolean.TRUE);
    bulkDownloadRequest.setShowSalesmanInfo(false);
    offlineResponseList.get(0).getOrderItems().get(0).setSalesmanName("salesman-name");
    when(orderOutboundService.findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt())).thenReturn(
        new GdnRestListResponse<>(offlineResponseList, new PageMetaData(1, 0, 2), "123"));
    BulkDataResponse dataResponse = bulkOrderDataServiceBean.getData(bulkDownloadRequest);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariables(eq(Constant.STORE_ID),
        eq(Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS)));
    verify(orderOutboundService, times(2)).findOfflinePackageSummary(any(), any(), any(),
        any(OrderItemSummaryRequest.class), eq(ORDER_BY), eq(SORT_BY), anyInt(), anyInt());
    Assertions.assertTrue(dataResponse instanceof BulkOrderResponse);
    Assertions.assertTrue(bulkDownloadRequest.isShowSalesmanInfo());
  }
}
