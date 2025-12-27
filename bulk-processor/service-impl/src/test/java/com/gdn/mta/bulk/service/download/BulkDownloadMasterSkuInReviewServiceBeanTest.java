package com.gdn.mta.bulk.service.download;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.AnchorMappingRequest;
import com.gdn.mta.bulk.models.download.MasterSkuInReviewDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuInReviewDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import com.gdn.mta.bulk.service.MasterSkuReviewOutboundService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

class BulkDownloadMasterSkuInReviewServiceBeanTest {
  private static final String REQUEST_ID = "requestId";

  @Mock
  private MasterSkuReviewOutboundService masterSkuReviewOutboundService;

  @InjectMocks
  private BulkDownloadMasterSkuInReviewServiceBean downloadMasterSkuInReviewServiceBean;

  private MasterSkuInReviewDownloadRequest request;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(downloadMasterSkuInReviewServiceBean,
      "masterSkuReviewDownloadInReviewLimit", 50);
    request = new MasterSkuInReviewDownloadRequest();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(masterSkuReviewOutboundService);
  }

  @Test
  void getData_Success() throws Exception {
    request.setRequestId(REQUEST_ID);
    request.setBulkProcessEntity(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD);
    List<InReviewAnchorDownloadResponse> mockResponseList =
      Arrays.asList(new InReviewAnchorDownloadResponse(), new InReviewAnchorDownloadResponse());
    Mockito.when(
      masterSkuReviewOutboundService.fetchAnchorMappingDownloadList(Mockito.any(), Mockito.anyInt(),
        Mockito.anyInt(), Mockito.any())).thenReturn(mockResponseList);
    BulkDataResponse result = downloadMasterSkuInReviewServiceBean.getData(request);
    Assertions.assertEquals(BulkMasterSkuInReviewDownloadResponse.class, result.getClass());
    Assertions.assertEquals(2,
      ((BulkMasterSkuInReviewDownloadResponse) result).getAnchorDownloadResponseList().size());
    Mockito.verify(masterSkuReviewOutboundService).fetchAnchorMappingDownloadList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt(), Mockito.any());  }

  @Test
  void getData_EmptyResponse() throws Exception {
    request.setRequestId(REQUEST_ID);
    request.setBulkProcessEntity(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD);
    Mockito.when(
      masterSkuReviewOutboundService.fetchAnchorMappingDownloadList(Mockito.any(), Mockito.anyInt(),
        Mockito.anyInt(), Mockito.any())).thenReturn(Collections.emptyList());
    BulkDataResponse result = downloadMasterSkuInReviewServiceBean.getData(request);
    Assertions.assertEquals(BulkMasterSkuInReviewDownloadResponse.class, result.getClass());
    Assertions.assertEquals(0,
      ((BulkMasterSkuInReviewDownloadResponse) result).getAnchorDownloadResponseList().size());
    Mockito.verify(masterSkuReviewOutboundService).fetchAnchorMappingDownloadList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }


  @Test
  void getMasterSkuInReviewDownloadData_WithClusterRequestList() throws Exception {
    request.setClusterRequestList(Collections.singletonList(new AnchorMappingRequest()));
    Mockito.when(
        masterSkuReviewOutboundService.fetchAnchorMappingDownloadList(Mockito.any(), Mockito.anyInt(),
          Mockito.anyInt(), Mockito.any()))
      .thenReturn(Collections.singletonList(new InReviewAnchorDownloadResponse()));
    BulkDataResponse result = downloadMasterSkuInReviewServiceBean.getData(request);
    Assertions.assertEquals(BulkMasterSkuInReviewDownloadResponse.class, result.getClass());
    Assertions.assertEquals(1,
      ((BulkMasterSkuInReviewDownloadResponse) result).getAnchorDownloadResponseList().size());
    Mockito.verify(masterSkuReviewOutboundService).fetchAnchorMappingDownloadList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }

  @Test
  void getMasterSkuInReviewDownloadData_WithoutClusterRequestList() throws Exception {
    request.setSize(150); // assuming a size greater than masterSkuReviewDownloadInReviewLimit
    request.setPage(0);
    Mockito.when(
        masterSkuReviewOutboundService.fetchAnchorMappingDownloadList(Mockito.any(), Mockito.anyInt(),
          Mockito.anyInt(), Mockito.any()))
      .thenReturn(Collections.singletonList(new InReviewAnchorDownloadResponse()))
      .thenReturn(Collections.emptyList());
    BulkDataResponse result = downloadMasterSkuInReviewServiceBean.getData(request);
    Assertions.assertEquals(BulkMasterSkuInReviewDownloadResponse.class, result.getClass());
    Assertions.assertEquals(1,
      ((BulkMasterSkuInReviewDownloadResponse) result).getAnchorDownloadResponseList().size());
    Mockito.verify(masterSkuReviewOutboundService, Mockito.times(2)).fetchAnchorMappingDownloadList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }
}
