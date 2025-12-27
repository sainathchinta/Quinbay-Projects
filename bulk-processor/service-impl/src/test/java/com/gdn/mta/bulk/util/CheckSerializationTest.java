package com.gdn.mta.bulk.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductDownloadRequest;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by keshashah on 03/11/16.
 */
public class CheckSerializationTest {

  @Test
  public void testOrderDownloadRequest() throws IOException {
    OrderDownloadRequest.OrderBuilder orderBuilder = new OrderDownloadRequest.OrderBuilder();
    Map<String,String> testMap = new HashMap<>();
    testMap.put("123", "abc");
    OrderItemSummaryRequest summaryRequest = new OrderItemSummaryRequest();
    summaryRequest.setOrderItemId("123");
    orderBuilder.orderRequest(summaryRequest);
    orderBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    orderBuilder.emailCC("kesha@xyz.com");
    orderBuilder.emailTo("kesha@xyz.com");
    orderBuilder.filename("test.xlsx");
    orderBuilder.downloadType(DownloadType.ALL);
    orderBuilder.merchant("m-123");
    orderBuilder.request("123");
    orderBuilder.statusMap(testMap);
    BulkDownloadRequest request = orderBuilder.build();
    ObjectMapper mapper = new ObjectMapper();
    String value = mapper.writeValueAsString(request);
    BulkDownloadRequest bulkDownloadRequest =
        mapper.readValue(value, new TypeReference<BulkDownloadRequest>() {
        });
//    Assertions.assertInstanceOf(OrderDownloadRequest.class, bulkDownloadRequest);
    OrderDownloadRequest request1 = (OrderDownloadRequest) bulkDownloadRequest;
    Assertions.assertEquals("123", request1.getOrderRequest().getOrderItemId());
    Assertions.assertNotNull(request1.getOrderStatusMap());

  }


  @Test
  public void testProductDownloadRequest() throws IOException {
    ProductDownloadRequest.ProductBuilder productBuilder = new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    BulkDownloadRequest request = productBuilder.build();
    ObjectMapper mapper = new ObjectMapper();
    String value = mapper.writeValueAsString(request);

    BulkDownloadRequest bulkDownloadRequest =
        mapper.readValue(value, new TypeReference<BulkDownloadRequest>() {
        });
//    Assertions.assertInstanceOf(BulkDownloadRequest.class, bulkDownloadRequest);
    Assertions.assertEquals("123", bulkDownloadRequest.getRequestId());

  }
}
