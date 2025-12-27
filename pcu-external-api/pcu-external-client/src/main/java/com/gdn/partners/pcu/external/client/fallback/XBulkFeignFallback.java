package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.PickupPointCodesRequestDTO;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkProcessSubjectToVatRequest;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.feign.XBulkFeign;
import com.gdn.partners.pcu.external.client.model.BulkProcessExternalUploadRequest;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoRequest;

import org.springframework.stereotype.Component;

import java.util.List;


@Component
public class XBulkFeignFallback implements XBulkFeign {
  @Override
  public GdnBaseRestResponse uploadForBulkUpdate(BulkProcessV2Request bulkRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse uploadForBulkUpdateEAN(BulkProcessV2Request bulkRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse upload(BulkProcessUploadRequest bulkRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse externalUpload(BulkProcessExternalUploadRequest bulkRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse bulkArchiveItemSkus(BulkProcessUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<BulkProcessNotesResponse> filterPromoBulkProcessNotesByBulkProcessCode(
    String bulkProcessCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<WholeSaleCountResponse> filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(
    String bulkProcessCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BulkPendingRequestsResponse> checkPendingBulkRequestsByBusinessPartnerCode(String type,
      String businessPartnerCode, String bulkProcessType) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<UnifiedBulkDownloadResponse> downloadProductUnifiedTemplate(
    String businessPartnerCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<UnifiedBulkDownloadResponse> downloadProductUnifiedTemplateV2(
    String businessPartnerCode, PickupPointCodesRequestDTO request) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }


  @Override
  public GdnRestSingleResponse<SystemParameterConfigResponse> findOne(String variable) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse bulkUpdateOff2On(BulkProcessUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse bulkArchiveProductSkus(BulkProcessUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse createWorkOrder(BulkProcessUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse bulkUploadSubjectToVat(BulkProcessSubjectToVatRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse bulkUploadDeleteOfflineItems(
    BulkProcessDeleteOfflineItemRequest bulkProcessDeleteOfflineItemRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse uploadForBulkUpsertOfflineItems(
    BulkProcessUpsertOfflineItemRequest bulkProcessUpsertOfflineItemRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<BulkProcessStatusListingResponse> fetchBulkProcessStatusListing(
    String storeId, String channelId, String clientId, String requestId, String bulkProcessType,
    String businessPartnerCode, List<String> bulkProcessCodes, boolean estimationsNeeded,
    Integer page, Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse checkQrGenerationAccessible(String businessPartnerCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse uploadQrCodeExcel(String storeId, String requestId,
    QrExcelUploadRequest qrExcelUploadRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
      false, null);
  }

  @Override
  public GdnRestSingleResponse<BulkProcessResponse> getBulkProcessByProcessCode(String storeId,
      String channelId, String clientId, String requestId, String username, String bulkProcessCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse uploadBulkBasicInfoFile(String storeId, String channelId, String clientId,
      String requestId, String username, BulkBasicInfoRequest bulkBasicInfoRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }
}
