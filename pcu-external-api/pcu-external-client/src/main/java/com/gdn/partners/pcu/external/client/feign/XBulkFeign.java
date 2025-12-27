package com.gdn.partners.pcu.external.client.feign;

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
import com.gdn.partners.pcu.external.client.factory.XBulkFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.model.BulkProcessExternalUploadRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoRequest;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;


@FeignClient(name = "xBulkFeign", url = "${service.x-bulk.endpoint}", fallbackFactory = XBulkFeignFallbackFactory.class)
public interface XBulkFeign {

  @RequestMapping(value = "/api/bulk-process/upload-bulk-update", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadForBulkUpdate(@RequestBody BulkProcessV2Request bulkRequest);

  @RequestMapping(value = "/api/bulk-process/upload-bulk-update-ean", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadForBulkUpdateEAN(@RequestBody BulkProcessV2Request bulkRequest);

  @RequestMapping(value = "/api/bulk-process/upload", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse upload(@RequestBody BulkProcessUploadRequest bulkRequest);

  @RequestMapping(value = "/api/bulk-process/external-upload", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse externalUpload(@RequestBody BulkProcessExternalUploadRequest bulkRequest);

  @RequestMapping(value = "/api/bulk-process/bulk-archive-items", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse bulkArchiveItemSkus(@RequestBody BulkProcessUpdateRequest request);

  @RequestMapping(value = "/api/bulk-process/bulk-update-off2on", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse bulkUpdateOff2On(@RequestBody BulkProcessUpdateRequest request);

  @RequestMapping(value = "/api/bulk-process/filter/bulk-process-code/{bulkProcessCode}/promoNotes", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BulkProcessNotesResponse> filterPromoBulkProcessNotesByBulkProcessCode(
      @PathVariable("bulkProcessCode") String bulkProcessCode);

  @RequestMapping(value = "/api/bulk-process/filter/bulk-process-code/{bulkProcessCode}/wholesaleConfig", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<WholeSaleCountResponse> filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(
      @PathVariable("bulkProcessCode") String bulkProcessCode);

  @RequestMapping(value = "/api/bulk-process/check-pending-bulk-requests", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BulkPendingRequestsResponse> checkPendingBulkRequestsByBusinessPartnerCode(
      @RequestParam("type") String type, @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("bulkProcessType") String bulkProcessType);

  @RequestMapping(value = "/api/bulk-process/download-unified-product", method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<UnifiedBulkDownloadResponse> downloadProductUnifiedTemplate(
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/bulk-process/download-unified-product-v2", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<UnifiedBulkDownloadResponse> downloadProductUnifiedTemplateV2(
    @RequestParam("businessPartnerCode") String businessPartnerCode,
    @RequestBody PickupPointCodesRequestDTO request);

  @RequestMapping(value = "/api/system-parameters/find-one", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SystemParameterConfigResponse> findOne(@RequestParam("variable") String variable);

  @RequestMapping(value = "/api/bulk-process/bulk-archive-productSkus", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse bulkArchiveProductSkus(@RequestBody BulkProcessUpdateRequest request);

  @RequestMapping(value = "/api/bulk-process/bulk-work-order-creation", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse createWorkOrder(@RequestBody BulkProcessUpdateRequest request);

  @RequestMapping(value = "/api/bulk-process/upload-subject-to-vat", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse bulkUploadSubjectToVat(@RequestBody BulkProcessSubjectToVatRequest request);

  @RequestMapping(value = "/api/bulk-process/upload-bulk-delete-offline-items", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse bulkUploadDeleteOfflineItems(
    BulkProcessDeleteOfflineItemRequest bulkProcessDeleteOfflineItemRequest);

  @RequestMapping(value = "/api/bulk-process/upload-bulk-upsert-offline-items", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadForBulkUpsertOfflineItems(
      BulkProcessUpsertOfflineItemRequest bulkProcessUpsertOfflineItemRequest);

  @RequestMapping(value = "/api/bulk-process/bulk-process-status-listing", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BulkProcessStatusListingResponse> fetchBulkProcessStatusListing(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("bulkProcessType") String bulkProcessType,
    @RequestParam("businessPartnerCode") String businessPartnerCode,
    @RequestParam(value = "bulkProcessCodes", required = false) List<String> bulkProcessCodes,
    @RequestParam(value = "estimationsNeeded", required = false) boolean estimationsNeeded,
    @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/bulk-process/check-qr-process-allowed", method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse checkQrGenerationAccessible(
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/bulk-process/upload-qr-excel", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadQrCodeExcel(@RequestParam("storeId") String storeId,
    @RequestParam("requestId") String requestId, QrExcelUploadRequest qrExcelUploadRequest);

  @RequestMapping(value = "/api/bulk-process/filter/bulk-process-code/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BulkProcessResponse> getBulkProcessByProcessCode(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @PathVariable("id") String bulkProcessCode);

  @PostMapping(value = "/api/bulk-process/upload-bulk-basic-info-file", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadBulkBasicInfoFile(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @RequestBody BulkBasicInfoRequest bulkBasicInfoRequest);
}
