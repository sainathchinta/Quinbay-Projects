package com.gdn.partners.pbp.outbound.sellerLogistics;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.http.HttpStatus;

import com.blibli.oss.backend.common.model.response.Paging;
import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.partners.pbp.outbound.sellerLogistics.feign.SellerLogisticsFeign;
import com.gdn.seller.logistics.web.model.request.SaveSkuLogisticProductRequest;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.SaveSkuLogisticProductResponse;

public class SellerLogisticsOutboundBeanTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_DELIVERY_TYPE = "merchantDeliveryType";
  private static final String ERROR = "error";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String CHANNEL_ID = "web";
  private static final String CLIENT_ID = "pbp";
  private static final boolean IS_ACTIVE = true;

  @InjectMocks
  private SellerLogisticsOutboundBean sellerLogisticsOutboundBean;

  @Mock
  private SellerLogisticsFeign sellerLogisticsFeign;

  private Response<List<GetSkuLogisticProductResponse>> findResponse;
  private Response<SaveSkuLogisticProductResponse> saveResponse;
  private Map<String, List<String>> errorMap;
  private Map<String, String> contextMap;
  private SaveSkuLogisticProductRequest saveSkuLogisticProductRequest;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    findResponse = new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(),
        new ArrayList<>((Collections.singletonList(new GetSkuLogisticProductResponse()))),
        new Paging(), new HashMap<>(), new HashMap<>());
    saveResponse = new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(),
        new SaveSkuLogisticProductResponse(), new Paging(), new HashMap<>(), new HashMap<>());
    errorMap = new HashMap<>();
    contextMap = new HashMap<>();
    contextMap.put("storeId", STORE_ID);
    contextMap.put("requestId", REQUEST_ID);
    contextMap.put("username", USERNAME);
    MDC.setContextMap(contextMap);
    saveSkuLogisticProductRequest = new SaveSkuLogisticProductRequest();
    when(sellerLogisticsFeign.getSkuLogistics(ITEM_SKU, MERCHANT_CODE, MERCHANT_DELIVERY_TYPE,
        REQUEST_ID, CHANNEL_ID, STORE_ID, CLIENT_ID, USERNAME)).thenReturn(findResponse);
    when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest)).thenReturn(saveResponse);
  }

  @Test
  public void getSkuLogistics() throws Exception {
    sellerLogisticsOutboundBean.getSkuLogistics(ITEM_SKU, MERCHANT_CODE, MERCHANT_DELIVERY_TYPE);
    verify(sellerLogisticsFeign).getSkuLogistics(ITEM_SKU, MERCHANT_CODE, MERCHANT_DELIVERY_TYPE,
        REQUEST_ID, CHANNEL_ID, STORE_ID, CLIENT_ID, USERNAME);
  }

  @Test
  public void getSkuLogistics_nullResponse() throws Exception {
    try {
      when(sellerLogisticsFeign.getSkuLogistics(ITEM_SKU, MERCHANT_CODE, MERCHANT_DELIVERY_TYPE,
          REQUEST_ID, CHANNEL_ID, STORE_ID, CLIENT_ID, USERNAME)).thenReturn(null);

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.getSkuLogistics(ITEM_SKU, MERCHANT_CODE, MERCHANT_DELIVERY_TYPE);
      });
    } finally {
      verify(sellerLogisticsFeign).getSkuLogistics(ITEM_SKU, MERCHANT_CODE, MERCHANT_DELIVERY_TYPE,
          REQUEST_ID, CHANNEL_ID, STORE_ID, CLIENT_ID, USERNAME);
    }
  }

  @Test
  public void saveSkuLogistics() throws Exception {
    sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
    verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
  }

  @Test
  public void saveSkuLogistics_responseNoSuccess() throws Exception {
    try {
      when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest)).thenReturn(
              new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
                  new SaveSkuLogisticProductResponse(), new Paging(), errorMap, new HashMap<>()));

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
      });
    } finally {
      verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
    }
  }

  @Test
  public void saveSkuLogistics_responseNoSuccessErrorMapNull() throws Exception {
    try {
      when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest)).thenReturn(
              new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
                  new SaveSkuLogisticProductResponse(), new Paging(), null, new HashMap<>()));

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
      });
    } finally {
      verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
    }
  }

  @Test
  public void saveSkuLogistics_responseNoSuccessErrorMap() throws Exception {
    errorMap.put(ERROR, Arrays.asList(ERROR));
    try {
      when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest)).thenReturn(
              new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
                  new SaveSkuLogisticProductResponse(), new Paging(), errorMap, new HashMap<>()));

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
      });
    } finally {
      verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
    }
  }

  @Test
  public void saveSkuLogistics_responseNoSuccessErrorListEmpty() throws Exception {
    errorMap.put(ERROR, new ArrayList<>());
    try {
      when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest)).thenReturn(
              new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
                  new SaveSkuLogisticProductResponse(), new Paging(), errorMap, new HashMap<>()));

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
      });
    } finally {
      verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
    }
  }

  @Test
  public void saveSkuLogistics_responseNoSuccessErrorBlank() throws Exception {
    errorMap.put(ERROR, Arrays.asList(StringUtils.EMPTY));
    try {
      when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest)).thenReturn(
              new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
                  new SaveSkuLogisticProductResponse(), new Paging(), errorMap, new HashMap<>()));

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
      });
    } finally {
      verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
    }
  }

  @Test
  public void saveSkuLogistics_responseNull() throws Exception {
    try {
      when(sellerLogisticsFeign.saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest))
              .thenReturn(new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(), null,
                  new Paging(), new HashMap<>(), new HashMap<>()));

      Assertions.assertThrows(Exception.class, () -> {
        sellerLogisticsOutboundBean.saveSkuLogistics(saveSkuLogisticProductRequest, IS_ACTIVE);
      });
    } finally {
      verify(sellerLogisticsFeign).saveSkuLogistics(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, IS_ACTIVE, saveSkuLogisticProductRequest);
    }
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    verifyNoMoreInteractions(this.sellerLogisticsFeign);
  }
}
