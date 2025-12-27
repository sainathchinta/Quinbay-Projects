package com.gdn.partners.pcu.internal.client.feign;

import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import com.gdn.mta.bulk.dto.SimpleStringResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.partners.pcu.internal.client.factory.XBulkFeignFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.response.StringResponse;

@FeignClient(name = "xBulkFeign", url = "${service.x-bulk.endpoint}", fallbackFactory = XBulkFeignFallbackFactory.class)
public interface XBulkFeign {

  @RequestMapping(value = "/api/bulk-process/countNumberOfUploads", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<UploadProcessCount> countNumberOfUploads(@RequestParam("bulkProcessType") String bulkProcessType,
      @RequestParam("status") String status);

  @RequestMapping(value = "/api/recat-process/{recat-request-code}/get-failed-products-mail", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse getFailedProductsMail(@PathVariable("recat-request-code") String recatRequestCode);

  @RequestMapping(value = "/api/recat-process/request-summary", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RecatProcessSummaryResponse> getRecatProcessSummary(
      @RequestBody RecatProcessSummaryRequest recatProcessSummaryRequest,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/recat-process/upload-new-request", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadNewRecatRequest(@RequestParam("recatRequestCode") String recatRequestCode,
      @RequestParam("fileName") String fileName, @RequestParam("scheduledTime") String scheduledTime);

  @RequestMapping(value = "/api/recat-process/{recatRequestCode}/cancel-request", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse cancelRecatRequest(@PathVariable("recatRequestCode") String recatRequestCode,
      @RequestParam("forceUpdate") boolean forceUpdate);

  @RequestMapping(value = "/api/recat-process/{recat-request-code}/product-summary", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RecatProductSummaryResponse> getProductSummary(
      @PathVariable("recat-request-code") String recatRequestCode, @RequestParam("page") int page,
      @RequestParam("size") int size, @RequestBody RecatProductSummaryRequest recatProductSummaryRequest);

  @RequestMapping(value = "api/recat-process/{recatRequestCode}/product-status-counts", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<RecatProductCountResponse> getProductStatusCounts(
      @PathVariable("recatRequestCode") String recatRequestCode);


  @RequestMapping(value = "api/store-copy/{sellerCode}/template/download", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<StringResponse> downloadUploadTemplateForStoreCopy(
      @PathVariable("sellerCode") String sellerCode);

  @RequestMapping(value = "/api/bulkInternalProcess/summary", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BulkInternalProcessSummaryResponse> bulkInternalProcessSummary(
      @RequestBody BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/bulkInternalProcess/upload", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse uploadNewBulkInternalProcessRequest(
      @RequestBody BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest);

  @RequestMapping(value = "/api/bulkInternalProcess/cancel", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse bulkInternalProcessCancelRequest(@RequestParam("internalProcessRequestCode") String internalProcessRequestCode);

  @RequestMapping(value = "/api/bulk-process/in-progress-requests", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BulkInternalPendingRequestResponse> getPendingBulkRequests(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("username") String username,
      @RequestParam("downloadType") String downloadType);

  @RequestMapping(value = "/api/bulkInternalProcess/check-pending-files", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<InternalProcessPendingFilesResponse> checkPendingFiles(
      @RequestParam("storeId") String storeId, @RequestParam("username") String username, @RequestParam("processType") String processType);

  @RequestMapping(value = "/api/bulk-process/abort-pending-tasks", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse abortPendingBulkProcess(@RequestParam("id") String id);

  @RequestMapping(value = "/api/bulk-process/clear-in-progress-downloads", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse clearInProgressDownloads(@RequestParam("entityType") String entityType,
    @RequestParam("status") String status);
}

