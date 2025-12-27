package com.gdn.mta.bulk.service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.feignConfig.ProductAnalyticsFeign;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.AutoApprovedAssigneeRequest;
import com.gdn.mta.bulk.models.AutoApprovedSelectedDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedListWebResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductAssigneeChangeResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

class ProductAnalyticsOutboundServiceImplTest {

  @InjectMocks
  private ProductAnalyticsOutboundServiceImpl outboundService;
  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  private static final String REQUEST_ID = "requestId";
  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "api";
  private static final String CLIENT_ID = "x-bulk";
  private static final String USERNAME = "x-bulk";
  private GdnRestListResponse<AutoApprovedListWebResponse> response;
  private AutoApprovedWebRequest request;
  private AutoApprovedAssigneeRequest assigneeRequest;
  private AutoApprovedSelectedDownloadRequest selectedDownloadRequest;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    response = new GdnRestListResponse<>();
    request = new AutoApprovedWebRequest();
    assigneeRequest = new AutoApprovedAssigneeRequest();
    selectedDownloadRequest = new AutoApprovedSelectedDownloadRequest();
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
  }

  @Test
  void fetchAutoApprovedProductsDownloadList_Success() {
    response.setSuccess(true);
    response.setContent(Collections.singletonList(new AutoApprovedListWebResponse()));
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsList(Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any()))
        .thenReturn(response);
    List<AutoApprovedListWebResponse> result = outboundService.fetchAutoApprovedProductsDownloadList(REQUEST_ID, 1, 10, request);
    Mockito.verify(productAnalyticsFeign).fetchAutoApprovedProductsList(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
    Assertions.assertEquals(1, result.size());
  }

  @Test
  void fetchAutoApprovedProductsDownloadList_Failure() {
    response.setSuccess(false);
    response.setContent(null);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsList(Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any()))
        .thenReturn(response);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> outboundService.fetchAutoApprovedProductsDownloadList(REQUEST_ID, 1, 10, request));
    Mockito.verify(productAnalyticsFeign).fetchAutoApprovedProductsList(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }

  @Test
  void fetchAutoApprovedProductsSelectedDownloadList_Success() {
    response.setSuccess(true);
    response.setContent(Collections.singletonList(new AutoApprovedListWebResponse()));
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsSelectedDownload(Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(response);
    List<AutoApprovedListWebResponse> result = outboundService.fetchAutoApprovedProductsSelectedDownloadList(REQUEST_ID, selectedDownloadRequest);
    Mockito.verify(productAnalyticsFeign).fetchAutoApprovedProductsSelectedDownload(Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    Assertions.assertEquals(1, result.size());
  }

  @Test
  void fetchAutoApprovedProductsSelectedDownloadList_Failure() {
    response.setSuccess(false);
    response.setContent(null);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedProductsSelectedDownload(Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(response);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> outboundService.fetchAutoApprovedProductsSelectedDownloadList(REQUEST_ID, selectedDownloadRequest));
    Mockito.verify(productAnalyticsFeign).fetchAutoApprovedProductsSelectedDownload(Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  void processUpdateAssigneeForAutoApprovedProducts_success() {
    GdnRestListResponse<ProductAssigneeChangeResponse> response = new GdnRestListResponse<>();
    Mockito.when(productAnalyticsFeign.updateAssignedTo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USERNAME, AutoApprovedAssigneeRequest.builder().build()))
        .thenReturn(response);
    String errorMessage = outboundService.processUpdateAssigneeForAutoApprovedProducts(REQUEST_ID,
        AutoApprovedAssigneeRequest.builder().build());
    Mockito.verify(productAnalyticsFeign)
        .updateAssignedTo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            AutoApprovedAssigneeRequest.builder().build());
    Assertions.assertNull(errorMessage);
  }

  @Test
  void validateResponse_NullResponse() {
    response = null;
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> outboundService.validateResponse(response, request));
  }

  @Test
  void validateResponse_Failure() {
    response.setSuccess(false);
    response.setContent(null);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> outboundService.validateResponse(response, request));
  }


  @Test
  public void processUpdateAssigneeForAutoApprovedProducts_successFalse() {
    GdnRestListResponse<ProductAssigneeChangeResponse> response = new GdnRestListResponse<>();
    response.setContent(
      Arrays.asList(new ProductAssigneeChangeResponse("MTA-1234", "Product Not Found")));
    Mockito.when(
      productAnalyticsFeign.updateAssignedTo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        AutoApprovedAssigneeRequest.builder().build())).thenReturn(response);
    String errorMessage = outboundService.processUpdateAssigneeForAutoApprovedProducts(REQUEST_ID,
      AutoApprovedAssigneeRequest.builder().build());
    Mockito.verify(productAnalyticsFeign)
      .updateAssignedTo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        AutoApprovedAssigneeRequest.builder().build());
    Assertions.assertEquals("Product Not Found", errorMessage);
  }

  @Test
  public void processUpdateAssigneeForAutoApprovedProducts_exception() {
    Mockito.when(productAnalyticsFeign.updateAssignedTo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, AutoApprovedAssigneeRequest.builder().build())).thenReturn(null);
    try {
      outboundService.processUpdateAssigneeForAutoApprovedProducts(REQUEST_ID,
          AutoApprovedAssigneeRequest.builder().build());
    } catch (Exception e) {
      Assertions.assertNotNull(e.getMessage());
      Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE,
          ((ApplicationRuntimeException) e).getErrorCodes());
    }
    Mockito.verify(productAnalyticsFeign)
        .updateAssignedTo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            AutoApprovedAssigneeRequest.builder().build());
  }
}