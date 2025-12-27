package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.ProductAssemblyFeign;
import com.gdn.mta.bulk.models.MasterWarehouseResponse;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import com.gdn.partners.bulk.util.Constant;

public class ProductAssemblyOutboundServiceBeanTest {

  private static final String PAGE = "1";
  private static final String LIMIT = "500";

  @InjectMocks
  private ProductAssemblyOutboundServiceBean productAssemblyOutboundService;

  @Mock
  private ProductAssemblyFeign productAssemblyFeign;

  private SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest;
  private TransferRequest transferRequest;

  @BeforeEach()
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    simpleListAssemblyDisassemblyRequest = new SimpleListAssemblyDisassemblyRequest();
    transferRequest = new TransferRequest();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productAssemblyFeign);
  }

  @Test
  public void getWarehouseCodeAndFulfillmentCenterEmptyTest() {
    Mockito.when(productAssemblyFeign.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PAGE, LIMIT))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(
              Constant.STORE_ID, PAGE, LIMIT));
    } finally {
      Mockito.verify(productAssemblyFeign)
          .getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PAGE, LIMIT);
    }
  }

  @Test
  public void getWarehouseCodeAndFulfillmentCenterExceptionTest() {
    Mockito.when(productAssemblyFeign.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PAGE, LIMIT))
        .thenReturn(new GdnRestListResponse<>(null, null, false, Constant.REQUEST_ID));
    List<MasterWarehouseResponse> masterWarehouseResponseList = null;
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(
              Constant.STORE_ID, PAGE, LIMIT));
    } finally {
      Mockito.verify(productAssemblyFeign)
          .getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PAGE, LIMIT);
    }
  }

  @Test
  public void getWarehouseCodeAndFulfillmentCenterSuccessTest() {
    Mockito.when(productAssemblyFeign.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PAGE, LIMIT)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(new MasterWarehouseResponse()),
            new PageMetaData(0, 0, 0), Constant.REQUEST_ID));
    List<MasterWarehouseResponse> masterWarehouseResponseList =
        productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT);
    Mockito.verify(productAssemblyFeign)
        .getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PAGE, LIMIT);
    Assertions.assertNotNull(masterWarehouseResponseList);
  }

  @Test
  public void assemblyDisassemblyRequestTest() {
    Mockito.when(
            productAssemblyFeign.assemblyDisassemblyRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, Constant.USER_NAME, PAGE, simpleListAssemblyDisassemblyRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    GdnBaseRestResponse response =
        productAssemblyOutboundService.assemblyDisassemblyRequest(Constant.STORE_ID, PAGE, Constant.REQUEST_ID,
            Constant.USER_NAME, simpleListAssemblyDisassemblyRequest);
    Mockito.verify(productAssemblyFeign)
        .assemblyDisassemblyRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PAGE, simpleListAssemblyDisassemblyRequest);
    Assertions.assertNotNull(response);
  }

  @Test
  public void assemblyDisassemblyRequestExceptionTest() {
    Mockito.when(
            productAssemblyFeign.assemblyDisassemblyRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, Constant.USER_NAME, PAGE, simpleListAssemblyDisassemblyRequest))
        .thenReturn(new GdnBaseRestResponse(false));
    GdnBaseRestResponse response = null;
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productAssemblyOutboundService.assemblyDisassemblyRequest(Constant.STORE_ID, PAGE,
              Constant.REQUEST_ID, Constant.USER_NAME, simpleListAssemblyDisassemblyRequest));
    } finally {
      Mockito.verify(productAssemblyFeign)
          .assemblyDisassemblyRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, PAGE, simpleListAssemblyDisassemblyRequest);
    }
  }

  @Test
  public void transferRequestTest() {
    Mockito.when(productAssemblyFeign.transferRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, transferRequest)).thenReturn(new GdnBaseRestResponse(true));
    GdnBaseRestResponse response =
        productAssemblyOutboundService.transferRequest(Constant.STORE_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            transferRequest);
    Mockito.verify(productAssemblyFeign)
        .transferRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, transferRequest);
    Assertions.assertNotNull(response);
  }

  @Test
  public void transferRequestExceptionTest() {
    Mockito.when(productAssemblyFeign.transferRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, transferRequest)).thenReturn(new GdnBaseRestResponse(false));
    GdnBaseRestResponse response = null;
    try {
      Assertions.assertThrows(RuntimeException.class, () -> productAssemblyOutboundService.transferRequest(Constant.STORE_ID, Constant.REQUEST_ID, Constant.USER_NAME,
              transferRequest));
    } finally {
      Mockito.verify(productAssemblyFeign)
          .transferRequest(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, transferRequest);
    }
  }
}
