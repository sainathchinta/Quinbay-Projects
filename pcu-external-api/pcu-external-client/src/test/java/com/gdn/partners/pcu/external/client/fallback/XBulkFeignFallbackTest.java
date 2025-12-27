package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.PickupPointCodesRequestDTO;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.dto.product.BulkProcessSubjectToVatRequest;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.model.BulkProcessExternalUploadRequest;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoRequest;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;

public class XBulkFeignFallbackTest {

  private XBulkFeignFallback xbpFeignFallback = new XBulkFeignFallback();

  @Test
  public void uploadForBulkUpdateTest() {
    GdnBaseRestResponse response =
        xbpFeignFallback.uploadForBulkUpdate(new BulkProcessV2Request());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void uploadForBulkUpdateEANTest() {
    GdnBaseRestResponse response =
        xbpFeignFallback.uploadForBulkUpdateEAN(new BulkProcessV2Request());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void uploadTest() {
    GdnBaseRestResponse response = xbpFeignFallback.upload(new BulkProcessUploadRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void externalUploadTest() {
    GdnBaseRestResponse response = xbpFeignFallback.externalUpload(new BulkProcessExternalUploadRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
  @Test
  public void bulkArchiveItemSkusTest() {
    GdnBaseRestResponse response = xbpFeignFallback.bulkArchiveItemSkus(new BulkProcessUpdateRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterPromoBulkProcessNotesByBulkProcessCodeTest() {
    GdnRestListResponse response = xbpFeignFallback.filterPromoBulkProcessNotesByBulkProcessCode(StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterWholeSaleConfigBulkProcessNotesByBulkProcessCodeTest() {
    GdnRestSingleResponse response = xbpFeignFallback.filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void checkPendingBulkRequestsByBusinessPartnerCodeTest() {
    GdnRestSingleResponse<BulkPendingRequestsResponse> response =
        xbpFeignFallback.checkPendingBulkRequestsByBusinessPartnerCode(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void downloadProductUnifiedTemplateTest(){
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response =
        xbpFeignFallback.downloadProductUnifiedTemplate(StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void findOneTest() {
    GdnRestSingleResponse<SystemParameterConfigResponse> response =
        xbpFeignFallback.findOne(StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void bulkUpdateOff2OnTest() {
    GdnBaseRestResponse response =
        xbpFeignFallback.bulkUpdateOff2On(new BulkProcessUpdateRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void bulkArchiveProductSkusTest() {
    GdnBaseRestResponse response = xbpFeignFallback.bulkArchiveProductSkus(new BulkProcessUpdateRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void createWorkOrderTest() {
    GdnBaseRestResponse response = xbpFeignFallback.createWorkOrder(new BulkProcessUpdateRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void bulkUploadSubjectToVatTest() {
    GdnBaseRestResponse response = xbpFeignFallback.bulkUploadSubjectToVat(new BulkProcessSubjectToVatRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void bulkUploadDeleteOfflineItemsTest() throws Exception {
    GdnBaseRestResponse response =
      xbpFeignFallback.bulkUploadDeleteOfflineItems(new BulkProcessDeleteOfflineItemRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void uploadForBulkUpsertOfflineItemsTest() throws Exception {
    GdnBaseRestResponse response =
        xbpFeignFallback.uploadForBulkUpsertOfflineItems(new BulkProcessUpsertOfflineItemRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void fetchBulkProcessStatusListingTest() throws Exception {
    GdnBaseRestResponse response =
      xbpFeignFallback.fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, Constants.REQUEST_ID, "bulkProcessType",
        Constants.BUSINESS_PARTNER_CODE, Collections.EMPTY_LIST, false,0, 50);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void checkQrGenerationAccessibleTest() throws Exception {
    GdnBaseRestResponse response =
        xbpFeignFallback.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void uploadQrCodeExcelTest() throws Exception {
    GdnBaseRestResponse response =
      xbpFeignFallback.uploadQrCodeExcel(Constants.STORE_ID, Constants.REQUEST_ID,
        new QrExcelUploadRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void downloadProductUnifiedTemplateV2Test(){
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response =
      xbpFeignFallback.downloadProductUnifiedTemplateV2(StringUtils.EMPTY,
        new PickupPointCodesRequestDTO());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBulkProcessByProcessCodeTest() throws Exception {
    GdnBaseRestResponse response =
        xbpFeignFallback.getBulkProcessByProcessCode(Constants.STORE_ID, Constants.CHANNEL_ID, Constants.CLIENT_ID,
            Constants.REQUEST_ID, Constants.USER_NAME, "bulkProcessCode");
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void uploadBulkBasicInfoFileTest() throws Exception {
    GdnBaseRestResponse response =
        xbpFeignFallback.uploadBulkBasicInfoFile(Constants.STORE_ID, Constants.CHANNEL_ID, Constants.CLIENT_ID,
            Constants.REQUEST_ID, Constants.USER_NAME, new BulkBasicInfoRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
}
