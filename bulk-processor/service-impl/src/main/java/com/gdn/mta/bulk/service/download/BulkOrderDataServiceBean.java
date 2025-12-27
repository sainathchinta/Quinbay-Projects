package com.gdn.mta.bulk.service.download;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkOrderResponse;
import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.mta.bulk.service.OrderHistoryOutboundService;
import com.gdn.mta.bulk.service.OrderOutboundService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.neo.order.client.sdk.model.OrderType;
import com.gdn.x.neo.order.client.sdk.web.model.request.DateRangeRequest;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by keshashah on 25/10/16.
 */
@Slf4j
@Service(value = "bulkOrderDataServiceBean")
public class BulkOrderDataServiceBean implements BulkProcessDataService {

  @Autowired
  private OrderOutboundService orderOutboundService;

  @Autowired
  private OrderHistoryOutboundService orderHistoryOutboundService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Value("${order.bulk.download.history.switch:false}")
  public boolean orderBulkDownloadHistorySwitch;

  @Value("${order.bulk.download.history.days.limit:365}")
  public int orderBulkDownloadHistoryDaysLimit;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws BulkDownloadException {
    log.info("Calling Get Data for orders for request {}", request);
    OrderDownloadRequest orderDownloadRequest = (OrderDownloadRequest) request;
    Map<String, SystemParameterConfig> variableToSystemParameterConfigMap =
      this.systemParameterConfigService.findValueByStoreIdAndVariables(Constant.STORE_ID,
        Arrays.asList(SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE,
          SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE,
          SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
          SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS));
    List<OrderItemSummaryResponse> orderItemSummaryResponseList =
      fetchOrderItemSummary(orderDownloadRequest, variableToSystemParameterConfigMap);
    return convertToBulkResponse(variableToSystemParameterConfigMap,
        orderItemSummaryResponseList, orderDownloadRequest);
  }

  private List<OrderItemSummaryResponse> fetchOrderItemSummary(
    OrderDownloadRequest orderDownloadRequest,
    Map<String, SystemParameterConfig> variableToSystemParameterConfigMap) {
    int pickupPointCodeBatchSize = Integer.parseInt(variableToSystemParameterConfigMap.get(
      SystemParameterConfigNames.PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE).getValue());
    int size = Integer.parseInt(variableToSystemParameterConfigMap.get(
      SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_PAGE_SIZE).getValue());
    boolean solrEnabled = Boolean.parseBoolean(variableToSystemParameterConfigMap.get(
      SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED).getValue());
    List<OrderItemSummaryResponse> orderItemSummaryResponseList = new ArrayList<>();
    List<String> pickupPointCodes = Arrays.asList(StringUtils.split(
      Optional.ofNullable(orderDownloadRequest.getOrderRequest().getPickupPointCode())
        .orElse(StringUtils.EMPTY), Constant.COMMA));
    List<List<String>> pickupPointCodeBatched = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      pickupPointCodeBatched = Lists.partition(pickupPointCodes, pickupPointCodeBatchSize);
    }
    int i = 0;
    do {
      if (CollectionUtils.isNotEmpty(pickupPointCodeBatched)) {
        orderDownloadRequest.getOrderRequest()
          .setPickupPointCode(StringUtils.join(pickupPointCodeBatched.get(i), Constant.COMMA));
      }
      if (BooleanUtils.toBooleanDefaultIfNull(orderDownloadRequest.getOffline(), false)) {
        orderItemSummaryResponseList.addAll(getOfflineOrderData(orderDownloadRequest, size));
      } else if (solrEnabled) {
        orderItemSummaryResponseList.addAll(getOrderDataFromSolr(orderDownloadRequest, size));
      } else {
        orderItemSummaryResponseList.addAll(getOrderDataFromDb(orderDownloadRequest, size));
      }
      i++;
    } while (i < pickupPointCodeBatched.size());
    if (CollectionUtils.isEmpty(orderItemSummaryResponseList)) {
      log.error("Bulk Download: Order Response is empty for request {} ", orderDownloadRequest);
    }
    return orderItemSummaryResponseList;
  }

  private List<OrderItemSummaryResponse> getOrderDataFromSolr(OrderDownloadRequest orderDownloadRequest, int size) {
    long totalElements = 0;
    int page = 0;
    GdnRestListResponse<OrderItemSummaryResponse> solrResponseList;
    List<OrderItemSummaryResponse> orderItemSummaryResponseList = new ArrayList<>();
    do {
      try {
        log.info("Fetching order data from solr, request : {}, page : {}, size : {},",
            orderDownloadRequest, page, size);
        solrResponseList =
          orderOutboundService.findOrderItemSummaryByFilter(Constant.STORE_ID, orderDownloadRequest.getRequestId(),
            orderDownloadRequest.getUsername(), orderDownloadRequest.getOrderRequest(),
            orderDownloadRequest.getOrderBy(), orderDownloadRequest.getSortBy(), page, size);
        orderItemSummaryResponseList.addAll(solrResponseList.getContent());
        totalElements = solrResponseList.getPageMetaData().getTotalRecords();
      } catch (Exception e) {
        log.error("Error while fetching order data from solr, request : {}, page : {}, size : {},"
          + " error - ", orderDownloadRequest, page, size, e);
      }
      page++;
    } while ((long) page * size < totalElements);
    return orderItemSummaryResponseList;
  }

  private List<OrderItemSummaryResponse> getOfflineOrderData(
      OrderDownloadRequest orderDownloadRequest, int size) {
    long totalElements = 0;
    int page = 0;
    List<OrderItemSummaryResponse> orderItemSummaryResponseList = new ArrayList<>();
    Date currentDate = new Date();
    DateRangeRequest dateRangeRequest = orderDownloadRequest.getOrderRequest().getStatusFPDateRange();
    if (!orderBulkDownloadHistorySwitch || dateRangeRequest.getEndDate()
        .after(DateUtils.addDays(currentDate, -1 * orderBulkDownloadHistoryDaysLimit))) {
      fetchOfflineData(orderDownloadRequest, size, page, orderItemSummaryResponseList,
          totalElements, Boolean.FALSE);
    }
    if (orderBulkDownloadHistorySwitch && dateRangeRequest.getStartDate()
        .before(DateUtils.addDays(currentDate, -1 * orderBulkDownloadHistoryDaysLimit))) {
      fetchOfflineData(orderDownloadRequest, size, page, orderItemSummaryResponseList,
          totalElements, Boolean.TRUE);
    }
    return orderItemSummaryResponseList;
  }

  private void fetchOfflineData(OrderDownloadRequest orderDownloadRequest, int size, int page,
      List<OrderItemSummaryResponse> orderItemSummaryResponseList, long totalElements, boolean history) {
    GdnRestListResponse<OfflinePackageSummaryResponse> solrResponseList;
    do {
      try {
        if (history) {
          solrResponseList = orderHistoryOutboundService.findOfflinePackageSummary(Constant.STORE_ID,
              orderDownloadRequest.getRequestId(), orderDownloadRequest.getUsername(),
              orderDownloadRequest.getOrderRequest(), orderDownloadRequest.getOrderBy(),
              orderDownloadRequest.getSortBy(), page, size);
        } else {
          solrResponseList = orderOutboundService.findOfflinePackageSummary(Constant.STORE_ID,
              orderDownloadRequest.getRequestId(), orderDownloadRequest.getUsername(),
              orderDownloadRequest.getOrderRequest(), orderDownloadRequest.getOrderBy(),
              orderDownloadRequest.getSortBy(), page, size);
        }

        List<OrderItemSummaryResponse> list = solrResponseList.getContent()
            .stream()
            .flatMap(packageInfo -> packageInfo.getOrderItems().stream())
            .collect(Collectors.toList());
        orderItemSummaryResponseList.addAll(list);
        totalElements = solrResponseList.getPageMetaData().getTotalRecords();
      } catch (Exception e) {
        log.error("Error while fetching offline order history data, request : {}, page : {}, size : {},"
            + " error - ", orderDownloadRequest, page, size, e);
      }
      page++;
    } while ((long) page * size < totalElements);
  }

  private List<OrderItemSummaryResponse> getOrderDataFromDb(OrderDownloadRequest orderDownloadRequest, int size) {
    long totalElements = 0;
    int page = 0;
    GdnRestListResponse<OrderItemSummaryResponse> dbResponseList = new GdnRestListResponse<>();
    List<OrderItemSummaryResponse> orderItemSummaryResponseList = new ArrayList<>();
    DateRangeRequest dateRangeRequest = orderDownloadRequest.getOrderRequest().getStatusFPDateRange();
    Date currentDate = new Date();
    if (!orderBulkDownloadHistorySwitch || dateRangeRequest.getEndDate()
        .after(DateUtils.addDays(currentDate, -1 * orderBulkDownloadHistoryDaysLimit))) {
      do {
        try {
          log.info("Fetching order data from X-order db, request : {}, page : {}, size : {},",
              orderDownloadRequest, page, size);
          dbResponseList = orderOutboundService.getOrderItemsForBulkDownload(Constant.STORE_ID,
              orderDownloadRequest.getRequestId(), orderDownloadRequest.getUsername(),
              orderDownloadRequest.getOrderRequest(), page, size);
          orderItemSummaryResponseList.addAll(dbResponseList.getContent());
          totalElements = dbResponseList.getPageMetaData().getTotalRecords();
        } catch (Exception e) {
          log.error(
              "Error while fetching order data from X-order db, request : {}, page : {}, size : {},"
                  + " error - ", orderDownloadRequest, page, size, e);
        }
        page++;
      } while ((long) page * size < totalElements);
    }

    totalElements = 0;
    page = 0;
    if (orderBulkDownloadHistorySwitch && dateRangeRequest.getStartDate()
        .before(DateUtils.addDays(currentDate, -1 * orderBulkDownloadHistoryDaysLimit))) {
      do {
        try {
          log.info("Fetching order data from Order-History db, request : {}, page : {}, size : {},",
              orderDownloadRequest, page, size);
          dbResponseList =
              orderHistoryOutboundService.getOrderItemsForBulkDownload(Constant.STORE_ID,
                  orderDownloadRequest.getRequestId(), orderDownloadRequest.getUsername(),
                  orderDownloadRequest.getOrderRequest(), page, size);
          orderItemSummaryResponseList.addAll(dbResponseList.getContent());
          totalElements = dbResponseList.getPageMetaData().getTotalRecords();
        } catch (Exception e) {
          log.error(
              "Error while fetching order data from Order-History db, request : {}, page : {}, "
                  + "size : {},"
                  + " error - ", orderDownloadRequest, page, size, e);
        }
        page++;
      } while ((long) page * size < totalElements);
    }

    return orderItemSummaryResponseList;
  }

  private BulkOrderResponse convertToBulkResponse(Map<String, SystemParameterConfig> paramMap,
      List<OrderItemSummaryResponse> responseList, OrderDownloadRequest request) {
    Set<String> scanAndGoBusinessChannels = Arrays.stream(
        paramMap.get(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS).getValue()
            .split(Constant.COMMA)).collect(Collectors.toSet());
    List<OrderItemSummaryResponse> orderResponses = new ArrayList<>();
      for (OrderItemSummaryResponse response : responseList) {

        if(StringUtils.isNotEmpty(response.getOrderId())){
          response.setOrderId("=\"" + response.getOrderId() + "\"");
        }
        
        if(StringUtils.isNotEmpty(response.getOrderItemId())){
          response.setOrderItemId("=\"" + response.getOrderItemId() + "\"");
        }
        
        if(StringUtils.isNotEmpty(response.getAwbNumber())){
          response.setAwbNumber("=\"" + response.getAwbNumber() + "\"");
        }
        
        if(StringUtils.isNotEmpty(response.getMerchantSku())){
          response.setMerchantSku("=\"" + response.getMerchantSku() + "\"");
        }
        
        if(response.getMerchantAdjustment() != null ){
          response.setPrice(response.getPrice() - response.getMerchantAdjustment());
        }

        if(StringUtils.isNotEmpty(response.getItemNotes())){
          response.setItemNotes("=\"" + response.getItemNotes() + "\"");
        }

        if (BooleanUtils.toBooleanDefaultIfNull(request.getOffline(), false)) {
          response.setOrderType(parseOfflineOrderCategory(scanAndGoBusinessChannels,
              response.getOrderType(), response.getBusinessChannel(), response.getSupermarketId()));
        }

        if(StringUtils.isNotEmpty(response.getSupermarketId())) {
          request.setShowPaymentMethodColumn(true);
          response.setPaymentMethod(response.getPaymentMethod());
        }
        log.info("salesman detail for request id {}, info {} with info {}", request.getRequestId(), response.getSalesmanEmail(), response);

        if(!request.isShowSalesmanInfo() &&
            (StringUtils.isNotBlank(response.getSalesmanEmail()) ||
            StringUtils.isNotBlank(response.getSalesmanName()))) {
          request.setShowSalesmanInfo(true);
        }

        String orderStatusDesc = response.getStatusDescription();
        if (StringUtils.isEmpty(orderStatusDesc)) {
          orderStatusDesc = Constant.DASH;
        }
        response.setOrderItemStatus(orderStatusDesc);
        orderResponses.add(response);
    }
    return new BulkOrderResponse(orderResponses);
  }


  private String parseOfflineOrderCategory(Set<String> scanAndGoBusinessChannels, String orderType,
      String businessChannel, String supermarketId) {
    if (OrderType.OFF2ON.getValue().equals(orderType)) {
      return Constant.INSTORE;
    } else if (OrderType.OFFLINE_PLUS.getValue().equals(orderType) || StringUtils.isNotEmpty(supermarketId)) {
      return Constant.SUPERMARKET;
    } else {
      if (scanAndGoBusinessChannels.contains(businessChannel)) {
        return Constant.SCAN_AND_GO;
      } else {
        return Constant.CLICK_AND_COLLECT;
      }
    }
  }
}
