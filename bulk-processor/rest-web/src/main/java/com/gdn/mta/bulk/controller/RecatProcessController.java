package com.gdn.mta.bulk.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.service.ProductRecatStatusService;
import com.gdn.mta.bulk.service.RecatProcessServiceWrapper;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@RestController
@Slf4j
@RequestMapping(value = RecatProcessController.BASE_PATH)
@Tag(name = "RecatProcessController", description = "Recat Process Service API")
public class RecatProcessController {

  private static final String ROOT = "/";
  public static final String BASE_PATH = ROOT + "api/recat-process";
  public static final String PROCESS_NEW_REQUESTS = ROOT + "process-new-requests";
  public static final String RECAT_REQUEST_CODE = ROOT + "{recat-request-code}";
  public static final String GET_FAILED_PRODUCTS_MAIL = ROOT + "get-failed-products-mail";
  public static final String UPLOAD_NEW_REQUEST = ROOT + "upload-new-request";
  public static final String GET_PRODUCT_COUNTS_BY_REQUEST_CODE = ROOT + "{recatRequestCode}/product-status-counts";
  public static final String PUBLISH_PENDING_PRODUCTS = ROOT + "publish-pending-products";
  public static final String REQUEST_FILTER_SUMMARY = ROOT + "request-summary";
  public static final String UPDATE_FINAL_STATUS = ROOT + "update-final-status";
  public static final String CANCEL_REQUEST = ROOT + "{recatRequestCode}/cancel-request";
  public static final String RECAT_PRODUCT_SUMMARY = ROOT + "{recat-request-code}/product-summary";

  @Autowired
  private RecatProcessServiceWrapper recatProcessServiceWrapper;

  @Autowired
  private ProductRecatStatusService productRecatStatusService;

  @RequestMapping(value = PROCESS_NEW_REQUESTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Process new recat requests", description = "Process new recat requests")
  public GdnBaseRestResponse processNewRecatRequests(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    log.info("API to process new Recat request requestId : {}", requestId);
    try {
      recatProcessServiceWrapper.processNewRecatProcess(storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("API to process new Recat request failed, requestId : {}", requestId, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = RECAT_REQUEST_CODE + GET_FAILED_PRODUCTS_MAIL, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get recat failed products mail", description = "get recat failed products mail")
  public GdnBaseRestResponse getFailedProductsMail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("recat-request-code") String recatRequestCode) {
    log.info("API to get recat failed products mail : {}", requestId);
    try {
      recatProcessServiceWrapper.getFailedProductsMail(storeId, recatRequestCode, username, requestId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error generating excel and sending mail for recat failed products, requestId : {} ", requestId, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = GET_PRODUCT_COUNTS_BY_REQUEST_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get product counts by recat request code", description = "Get product count by recat request code")
  public GdnRestSingleResponse<RecatProductCountResponse> getProductStatusCounts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("recatRequestCode") String recatRequestCode) {
    log.info("API to get product status counts requestId : {}, recatRequestCode : {}", requestId, recatRequestCode);
    try {
      RecatProductCountResponse recatProductCountResponse =
          recatProcessServiceWrapper.getProductCountsByRecatRequestCode(storeId, recatRequestCode);
      return new GdnRestSingleResponse<>(null, null, true, recatProductCountResponse, requestId);
    } catch (Exception e) {
      log.error("API to get product status counts failed, requestId : {}, recatRequestCode : {}", requestId,
          recatRequestCode, e);
      return new GdnRestSingleResponse<>(null, null, false, null, requestId);
    }
  }

  @RequestMapping(value = PUBLISH_PENDING_PRODUCTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Publish pending products", description = "Publish pending products")
  public GdnBaseRestResponse publishPendingProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    log.info("API to publish pending products requestId : {}", requestId);
    try {
      recatProcessServiceWrapper.publishPendingProducts(storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("API to publish pending products failed, requestId : {}", requestId, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = UPDATE_FINAL_STATUS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update final status", description = "Update final status")
  public GdnBaseRestResponse updateFinalStatus(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    log.info("API to update final status: {}", requestId);
    try {
      recatProcessServiceWrapper.updateRecatProcessFinalStatus(storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("API to update final status failed, requestId : {}", requestId, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = CANCEL_REQUEST, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Cancel recat request", description = "Cancel recat request")
  public GdnBaseRestResponse cancelRecatRequest(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("recatRequestCode") String recatRequestCode,
      @RequestParam(defaultValue = "false", required = false) boolean forceUpdate) {
    log.info("API to cancel recat-request-code : {} and force update : {}", recatRequestCode, forceUpdate);
    try {
      recatProcessServiceWrapper.cancelRecatRequest(storeId, recatRequestCode, forceUpdate, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.info("API to cancel recat-request-code : {} and force update : {} failed - ", recatRequestCode, forceUpdate,
          e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = UPLOAD_NEW_REQUEST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Create new recat process entry", description = "Create new recat process entry")
  public GdnBaseRestResponse uploadNewRecatRequest(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String recatRequestCode,
      @RequestParam String fileName, @RequestParam String scheduledTime) {
    log.info("API to upload new recat request, filename : {}, request code : {}", fileName,
        recatRequestCode);
    try {
      recatProcessServiceWrapper
          .uploadNewRecatRequest(storeId, recatRequestCode, fileName, scheduledTime);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error to generate new recat request, filename : {}, request code : {}, Error - ",
          fileName, recatRequestCode, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getCode(), e.getMessage(), false,
          requestId);
    }
  }

  @RequestMapping(value = REQUEST_FILTER_SUMMARY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Filter recat requests", description = "Filter recat requests")
  public GdnRestListResponse<RecatProcessSummaryResponse> getRecatProcessSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody RecatProcessSummaryRequest recatProcessSummaryRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size) {
    log.info("Fetching recat request summary by filter : {}, page : {}, size :{}",
        recatProcessSummaryRequest, page, size);
    try {
      Page<RecatProcessSummaryResponse> recatProcessSummaryResponses =
          this.recatProcessServiceWrapper
              .getRecatProcessSummary(storeId, recatProcessSummaryRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, recatProcessSummaryResponses.getContent(),
          new PageMetaData(size, page, recatProcessSummaryResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error fetching recat process summary with request : {}. Error - ",
          recatProcessSummaryRequest, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getCode(), e.getMessage(), false,
          null, null, requestId);
    }
  }

  @RequestMapping(value = RECAT_PRODUCT_SUMMARY, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get recat product summary", description = "Get recat product summary")
  public GdnRestListResponse<RecatProductSummaryResponse> getProductSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam int page, @RequestParam(defaultValue = "25") int size,
      @PathVariable("recat-request-code") String recatRequestCode,
      @RequestBody RecatProductSummaryRequest recatProductSummaryRequest) {
    log.info(
        "API to Get recat product summary requestId : {}, recatRequestCode : {}, page : {}, size : {}, recatProductSummaryRequest : {}",
        requestId, recatRequestCode, page, size, recatProductSummaryRequest);
    try {
      Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = productRecatStatusService
          .getRecatProductSummary(storeId, recatRequestCode, recatProductSummaryRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, recatProductSummaryResponsePage.getContent(),
          new PageMetaData(size, page, recatProductSummaryResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error(
          "API to Get recat product summary requestId : {}, recatRequestCode : {}, page : {}, size : {}, recatProductSummaryRequest : {}",
          requestId, recatRequestCode, page, size, recatProductSummaryRequest, e);
      return new GdnRestListResponse<>(null, null, false, null, null, requestId);
    }
  }

}
