package com.gdn.partners.pcu.internal.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.model.response.StringResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.mta.bulk.dto.SimpleStringResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class XBulkFeignFallbackTest {

  private static final String DEFAULT_RECAT_REQUEST_CODE = "RECAT_REQUEST_CODE";
  private static final String CODE = "code";
  public static final String SELLER_CODE = "sellerCode";
  public static final String USER_NAME = "userName";
  public static final String PROCESS_TYPE = "processType";
  public static final String STORE_ID = "storeId";

  public static final String ENTITY_TYPE = "entity";
  public static final String STATUS = "pending";

  private XBulkFeignFallback xBulkFeignFallback = new XBulkFeignFallback();
  private static final String RECAT_REQUEST_CODE = "recat-request-code";

  @Test
  public void countNumberOfUploads() {
    GdnRestSingleResponse<UploadProcessCount> response =
        xBulkFeignFallback.countNumberOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getFailedProductsMailTest() {
    GdnBaseRestResponse response =
        xBulkFeignFallback.getFailedProductsMail(DEFAULT_RECAT_REQUEST_CODE);
  }

  @Test
  public void uploadNewRecatRequestTest() {
    GdnBaseRestResponse response =
        xBulkFeignFallback.uploadNewRecatRequest(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void cancelRecatRequestTest() {
    GdnBaseRestResponse response = xBulkFeignFallback.cancelRecatRequest(CODE, false);
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductStatusCounts() {
    GdnRestSingleResponse<RecatProductCountResponse> response =
        xBulkFeignFallback.getProductStatusCounts(RECAT_REQUEST_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductSummary() {
    GdnRestListResponse<RecatProductSummaryResponse> response =
        xBulkFeignFallback.getProductSummary(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getRecatProcessSummaryTest() {
    GdnRestListResponse<RecatProcessSummaryResponse> response =
        xBulkFeignFallback.getRecatProcessSummary(new RecatProcessSummaryRequest(), 0, 10);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void downloadUploadTemplateForStoreCopy() {
    GdnRestSingleResponse<StringResponse> response = xBulkFeignFallback.downloadUploadTemplateForStoreCopy(CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void bulkInternalProcessSummaryTest() {
    GdnRestListResponse<BulkInternalProcessSummaryResponse> response =
        xBulkFeignFallback.bulkInternalProcessSummary(new BulkInternalProcessSummaryRequest(), 0, 10);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void uploadNewBulkInternalProcessRequestTest() {
    GdnBaseRestResponse response =
        xBulkFeignFallback.uploadNewBulkInternalProcessRequest(new BulkInternalProcessUploadRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void bulkInternalProcessCancelRequestTest() {
    GdnBaseRestResponse response = xBulkFeignFallback.bulkInternalProcessCancelRequest(RECAT_REQUEST_CODE);
  }

  @Test
  public void getPendingBulkRequestsTest() {
    GdnRestSingleResponse<BulkInternalPendingRequestResponse> response =
        xBulkFeignFallback.getPendingBulkRequests(SELLER_CODE, USER_NAME, PROCESS_TYPE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void checkPendingFilesTest() {
    GdnRestSimpleResponse<InternalProcessPendingFilesResponse> response =
            xBulkFeignFallback.checkPendingFiles(STORE_ID, USER_NAME, PROCESS_TYPE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void abortPendingBulkProcess() {
    GdnBaseRestResponse response =
      xBulkFeignFallback.abortPendingBulkProcess(CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void clearInProgressDownloads() {
    GdnBaseRestResponse response =
      xBulkFeignFallback.clearInProgressDownloads(ENTITY_TYPE,STATUS);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
