package com.gdn.mta.bulk.controller;

import static com.gdn.mta.bulk.models.GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.service.InternalProcessService;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.mta.product.util.GdnRestSimpleResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@RestController
@Slf4j
@RequestMapping(value = BulkInternalProcessController.BASE_PATH)
@Tag(name = "BulkInternalProcessController", description = "Bulk internal process APIs")
public class BulkInternalProcessController {
  public static final String BASE_PATH = "/api/bulkInternalProcess/";
  public static final String PROCESS_NEW_FILE_REQUESTS = "process-new-file-requests";
  public static final String PROCESS_INTERNAL_REQUEST_DATA = "process-internal-request-data";
  public static final String DELETE_INTERNAL_REQUEST = "delete-internal-request";
  public static final String ABORT_PENDING_TASK = "abort-pending-task";
  public static final String FAIL_PENDING_TASK = "fail-pending-task";
  public static final String PROCESS_STATUS_UPDATE = "process-status-update";
  public static final String SUMMARY = "summary";
  public static final String UPLOAD = "upload";
  public static final String CANCEL_REQUEST = "cancel";
  public static final String PENDING_FILES = "check-pending-files";
  public static final String BULK_PRICE_RECOMMENDATION_UPLOAD = "bulk-price-recommendation-upload";

  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private InternalProcessService internalProcessService;

  @RequestMapping(value = PROCESS_NEW_FILE_REQUESTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 1 : Process new internal process requests", description = "Scheduler 1 : Process new internal process requests")
  public GdnBaseRestResponse processNewInternalProcessRequests(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username, @RequestParam String processType) {
    log.info("API to process new internal process requests requestId : {}, processType : {} ", requestId, processType);
    try {
      internalProcessServiceWrapper.processNewInternalProcessRequest(storeId, username, processType);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("API to process new internal process requests failed, requestId : {}, processType : {} ", requestId, processType, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = PROCESS_INTERNAL_REQUEST_DATA, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 2 : Process new internal process request",
      description = "Scheduler 2 : Process new internal process request")
  public GdnBaseRestResponse processInternalRequestData(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String processType) {
    log.info("Scheduler 2 : Process new store copy data for product creation requestId : {}", requestId);
    internalProcessServiceWrapper.processInternalProcessDataRequest(storeId, requestId, username, processType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = DELETE_INTERNAL_REQUEST, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 5 : Delete internal request", description = "Scheduler 5 : Delete internal request")
  public GdnBaseRestResponse deleteInternalRequest(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String processType) {
    log.info("Scheduler 5 : Delete internal request  : {}", requestId);
    internalProcessServiceWrapper.deleteOldBulkInternalProcessRequest(storeId, processType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ABORT_PENDING_TASK, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Abort pending bulk internal process tasks", description = "Abort Pending bulk internal processTasks")
  public GdnBaseRestResponse abortPendingBulkInternalProcess(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String processType) {
    try {
      internalProcessServiceWrapper.abortPendingBulkInternalProcessBefore(storeId, processType);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      log.error("Error while aborting bulk internal process pending tasks ", ex);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = FAIL_PENDING_TASK, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 3 Abort pending bulk internal process tasks", description = "Abort Pending bulk internal processTasks")
  public GdnBaseRestResponse failPendingBulkInternalProcess(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String processType) {
    try {
      internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(storeId, processType);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      log.error("Error while aborting bulk internal process pending tasks ", ex);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = PROCESS_STATUS_UPDATE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 4 : Internal Process status update",
      description = "Scheduler 4 : Internal Process status update to update final status of internal process")
  public GdnBaseRestResponse bulkInternalProcessStatusUpdate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String processType) {
    log.info("Internal Process status update requestId : {}", requestId);
    internalProcessServiceWrapper.processStatusUpdate(storeId, requestId, username, processType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = BULK_PRICE_RECOMMENDATION_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "API to upload Price Recommendation files", description = "API to upload "
    + "Price Recommendation files by BR")
  public GdnBaseRestResponse bulkPriceRecommendation(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestParam String processType, @RequestBody BulkProcessUpdateRequest request) throws Exception {
    log.info("Price recommendation upload request for requestId : {} and request {} and "
      + "processType : {} ", requestId, request, processType);
    try {
      GdnPreconditions
          .checkArgument(StringUtils.isNotBlank(request.getBulkProcessType()), BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = BulkProcessController.convertBulkProcessDTO(request);
      internalProcessServiceWrapper.preProcessBulkPriceRecommendationFile(bulkUpdateProcessDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception exception) {
      log.error("Error while uploading file for request : {} for process : {} , Error : ",
        request, processType, exception);
      return new GdnBaseRestResponse(exception.getMessage(), null, false, requestId);
    }
  }


  @RequestMapping(value = SUMMARY, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch Bulk Internal Process requests by various filters", description = "Fetch Bulk Internal Process requests by various filters")
  public GdnRestListResponse<BulkInternalProcessSummaryResponse> bulkInternalProcessSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching Bulk Internal Process request summary by filter : {}, page : {}, size :{}",
        bulkInternalProcessSummaryRequest, page, size);
    try {
      Page<BulkInternalProcessSummaryResponse> bulkInternalProcessSummaryResponsePage =
          this.internalProcessServiceWrapper
              .bulkInternalProcessSummary(storeId, bulkInternalProcessSummaryRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, bulkInternalProcessSummaryResponsePage.getContent(),
          new PageMetaData(size, page, bulkInternalProcessSummaryResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error fetching Bulk Internal Process summary with request : {}, Error : ",
          bulkInternalProcessSummaryRequest, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getCode(), e.getMessage(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = UPLOAD, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes
      = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Upload bulk internal process", description = "Upload bulk internal process")
  public GdnBaseRestResponse bulkInternalProcessUpload(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest) {
    log.info("Upload bulk internal process : {} ", bulkInternalProcessUploadRequest);
    try {
      setMdcParameters(storeId, channelId, clientId, requestId, username);
      this.internalProcessServiceWrapper.uploadBulkInternalProcess(storeId, bulkInternalProcessUploadRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while uploading Bulk Internal Process with request : {}, ",
          bulkInternalProcessUploadRequest, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  private static void setMdcParameters(String storeId, String channelId, String clientId,
      String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
  }

  @RequestMapping(value = CANCEL_REQUEST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk internal process cancel request", description = "Bulk internal process cancel request")
  public GdnBaseRestResponse bulkInternalProcessCancelRequest(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String internalProcessRequestCode) {
    log.info("Bulk internal process cancel request, username : {}, internalProcessRequestCode : {}", username,
        internalProcessRequestCode);
    try {
      this.internalProcessServiceWrapper
          .bulkInternalProcessCancelRequest(storeId, username, internalProcessRequestCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.info(
          "Error occurred while cancelling Bulk internal processes, username : {}, internalProcessRequestCode : {} ",
          username, internalProcessRequestCode, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = PENDING_FILES, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "checking pending files of a user for given process type", description = "checking pending files of a user for given process type")
  public GdnRestSimpleResponse<InternalProcessPendingFilesResponse> checkPendingFiles(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String processType) {
    log.info("Checking pending files of a user = {} for a processType = {}", username, processType);
    return new GdnRestSimpleResponse<>(null, null, true, requestId,
        internalProcessService. checkPendingFiles(storeId, username, processType));
  }
}

