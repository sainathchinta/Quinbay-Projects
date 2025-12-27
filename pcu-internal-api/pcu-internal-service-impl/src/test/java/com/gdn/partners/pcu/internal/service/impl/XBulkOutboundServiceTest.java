package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.client.model.response.StringResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;

import java.util.ArrayList;

@ExtendWith(MockitoExtension.class)
public class XBulkOutboundServiceTest {

  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String DEFAULT_RECAT_REQUEST_CODE = "RECAT_REQUEST_CODE";
  private static final String RECAT_REQUEST_CODE = "recat-request-code";
  private static final String CODE = "code";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String FILE = "originalFilename.xlsx";
  private static final String SCHEDULE_DATE = "20/08/2021 00:00:00";
  private RecatProcessSummaryRequest recatProcessSummaryRequest = new RecatProcessSummaryRequest();
  public static final String SELLER_CODE = "sellerCode";
  public static final String USER_NAME = "userName";
  public static final String PROCESS_TYPE = "processType";
  public static final String STORE_ID = "storeId";

  @Mock
  private XBulkFeign xBulkFeign;

  @InjectMocks
  private XBulkOutboundServiceImpl xBulkOutboundService;

  @BeforeEach
  public void init() {

  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(xBulkFeign);
  }

  @Test
  public void getPendingProcessCountTest() {
    when(xBulkFeign.countNumberOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new UploadProcessCount(0L), REQUEST_ID));
    UploadProcessCount processCount =
        xBulkOutboundService.getPendingProcessCount(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    verify(xBulkFeign).countNumberOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    Assertions.assertEquals(0, processCount.getCount());
  }

  @Test
  public void getFailedProductsMailTest() {
    when(xBulkFeign.getFailedProductsMail(DEFAULT_RECAT_REQUEST_CODE))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
        xBulkOutboundService.getFailedProductsMail(DEFAULT_RECAT_REQUEST_CODE);
    verify(xBulkFeign).getFailedProductsMail(DEFAULT_RECAT_REQUEST_CODE);
  }

  @Test
  public void uploadNewRecatRequestTest() {
    when(xBulkFeign.uploadNewRecatRequest(RECAT_REQUEST_CODE, FILE, SCHEDULE_DATE))
        .thenReturn(new GdnBaseRestResponse(true));
    xBulkOutboundService.uploadNewRecatRequest(RECAT_REQUEST_CODE, FILE, SCHEDULE_DATE);
    verify(xBulkFeign).uploadNewRecatRequest(RECAT_REQUEST_CODE, FILE, SCHEDULE_DATE);
  }
  @Test
  public void cancelRecatRequestTest() {
    when(xBulkFeign.cancelRecatRequest(RECAT_REQUEST_CODE, false)).thenReturn(new GdnBaseRestResponse(true));
    xBulkOutboundService.cancelRecatRequest(RECAT_REQUEST_CODE);
    verify(xBulkFeign).cancelRecatRequest(RECAT_REQUEST_CODE, false);
  }

  @Test
  public void getRecatProductStatusCountTest() {
    when(xBulkFeign.getProductStatusCounts(RECAT_REQUEST_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new RecatProductCountResponse(), REQUEST_ID));
    RecatProductCountResponse recatProductCountResponse =
        xBulkOutboundService.getRecatProductStatusCount(RECAT_REQUEST_CODE);
    verify(xBulkFeign).getProductStatusCounts(RECAT_REQUEST_CODE);
  }

  @Test
  public void getRecatProductSummaryByRecatRequestCodeTest() {
    when(xBulkFeign.getProductSummary(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(new RecatProductSummaryResponse()),
            new PageMetaData(1, 0, 1), REQUEST_ID));
    xBulkOutboundService
        .getRecatProductSummaryByRecatRequestCode(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryRequest());
    verify(xBulkFeign).getProductSummary(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryRequest());
  }

  @Test
  public void recatProcessFilterSummaryTest() {
    Mockito.when(xBulkFeign.getRecatProcessSummary(recatProcessSummaryRequest, PAGE, SIZE))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(),
            new PageMetaData(PAGE, SIZE, PAGE), REQUEST_ID));
    Page<RecatProcessSummaryResponse> response =
        xBulkOutboundService.recatProcessFilterSummary(recatProcessSummaryRequest, PAGE, SIZE);
    Mockito.verify(xBulkFeign).getRecatProcessSummary(recatProcessSummaryRequest, PAGE, SIZE);
  }

  @Test
  public void downloadStoreCopyUploadTemplate() {
    Mockito.when(xBulkFeign.downloadUploadTemplateForStoreCopy(CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new StringResponse(), REQUEST_ID));
    String file = xBulkOutboundService.downloadStoreCopyUploadTemplate(CODE);
    Mockito.verify(xBulkFeign).downloadUploadTemplateForStoreCopy(CODE);
  }

  @Test
  public void checkPendingDownloadProcessTest() {
    Mockito.when(xBulkFeign.getPendingBulkRequests(SELLER_CODE, USER_NAME, PROCESS_TYPE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BulkInternalPendingRequestResponse(), REQUEST_ID));
    xBulkOutboundService.getPendingProcesses(SELLER_CODE, USER_NAME, PROCESS_TYPE);
    Mockito.verify(xBulkFeign).getPendingBulkRequests(SELLER_CODE, USER_NAME, PROCESS_TYPE);
  }

  @Test
  public void checkPendingFilesForAutoAssignmentTest() {
    Mockito.when(xBulkFeign.checkPendingFiles(STORE_ID, USER_NAME, PROCESS_TYPE)).thenReturn(
            new GdnRestSimpleResponse<>(null, null, true, REQUEST_ID, new InternalProcessPendingFilesResponse()));
    xBulkOutboundService.checkPendingFilesForAutoAssignment(STORE_ID, USER_NAME, PROCESS_TYPE);
    Mockito.verify(xBulkFeign).checkPendingFiles(STORE_ID, USER_NAME, PROCESS_TYPE);
  }
}
