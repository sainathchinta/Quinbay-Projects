package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.feignConfig.MasterSkuReviewFeign;
import com.gdn.mta.bulk.models.download.ChangeAssigneeRequest;
import com.gdn.mta.bulk.models.download.DownloadInReviewAnchorsWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class MasterSkuReviewOutboundServiceImplTest {

  private static final String REQUEST_ID = "requestId";

  @Mock
  private MasterSkuReviewFeign masterSkuReviewFeign;

  @InjectMocks
  private MasterSkuReviewOutboundServiceImpl outboundService;

  private GdnRestListResponse<InReviewAnchorDownloadResponse> response;
  private DownloadInReviewAnchorsWebRequest request;
  private GdnBaseRestResponse gdnBaseRestResponse;
  private ChangeAssigneeRequest changeAssigneeRequest;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    response = new GdnRestListResponse<>();
    request = new DownloadInReviewAnchorsWebRequest();
    gdnBaseRestResponse = new GdnBaseRestResponse();
    changeAssigneeRequest = new ChangeAssigneeRequest();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(masterSkuReviewFeign);
  }

  @Test
  public void fetchAnchorMappingDownloadList_Success() {
    response.setSuccess(true);
    response.setContent(Collections.singletonList(new InReviewAnchorDownloadResponse()));
    Mockito.when(
        masterSkuReviewFeign.getInReviewAnchorsDownload(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any()))
      .thenReturn(response);
    List<InReviewAnchorDownloadResponse> result =
      outboundService.fetchAnchorMappingDownloadList(REQUEST_ID, 1, 10, request);
    Mockito.verify(masterSkuReviewFeign)
      .getInReviewAnchorsDownload(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void fetchAnchorMappingDownloadList_Failure() {
    response.setSuccess(false);
    response.setContent(null);
    Mockito.when(
        masterSkuReviewFeign.getInReviewAnchorsDownload(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any()))
      .thenReturn(response);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> outboundService.fetchAnchorMappingDownloadList(REQUEST_ID, 1, 10, request));
    Mockito.verify(masterSkuReviewFeign)
      .getInReviewAnchorsDownload(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }

  @Test
  public void fetchAnchorMappingDownloadList_NullResponse() {
    response = null;
    Mockito.when(
        masterSkuReviewFeign.getInReviewAnchorsDownload(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any()))
      .thenReturn(response);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> outboundService.fetchAnchorMappingDownloadList(REQUEST_ID, 1, 10, request));
    Mockito.verify(masterSkuReviewFeign)
      .getInReviewAnchorsDownload(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }

  @Test
  public void processBulkUploadAssigneeAction_Success() {
    gdnBaseRestResponse.setSuccess(true);
    Mockito.when(masterSkuReviewFeign.updateAssignedTo(Mockito.any(), Mockito.any(), Mockito.any(),
      Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(gdnBaseRestResponse);
    String result =
      outboundService.processBulkUploadAssigneeAction(REQUEST_ID, changeAssigneeRequest);
    Assertions.assertNull(result);
    Mockito.verify(masterSkuReviewFeign)
      .updateAssignedTo(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void processBulkUploadAssigneeAction_SuccessFalse() {
    gdnBaseRestResponse.setSuccess(false);
    gdnBaseRestResponse.setErrorMessage("error");
    Mockito.when(masterSkuReviewFeign.updateAssignedTo(Mockito.any(), Mockito.any(), Mockito.any(),
      Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(gdnBaseRestResponse);
    String result =
      outboundService.processBulkUploadAssigneeAction(REQUEST_ID, changeAssigneeRequest);
    Assertions.assertEquals("error", result);
    Mockito.verify(masterSkuReviewFeign)
      .updateAssignedTo(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void processBulkUploadAssigneeAction_NullResponse() {
    gdnBaseRestResponse = null;
    Mockito.when(masterSkuReviewFeign.updateAssignedTo(Mockito.any(), Mockito.any(), Mockito.any(),
      Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(gdnBaseRestResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> outboundService.processBulkUploadAssigneeAction(REQUEST_ID, changeAssigneeRequest));
    Mockito.verify(masterSkuReviewFeign)
      .updateAssignedTo(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any());
  }
}
