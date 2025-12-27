package com.gdn.x.mta.distributiontask.controller;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.rest.model.ScheduledJobControllerPath;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductEmailService;
import com.gdn.x.mta.distributiontask.service.api.ScheduledJobService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Calendar;
import java.util.Date;
import java.util.Objects;

@RestController
@Slf4j
@RequestMapping(value = ScheduledJobControllerPath.BASE_PATH)
@Tag(name = "ScheduledJobController", description = "Scheduled Jobs")
public class ScheduledJobController {

  @Autowired
  private ScheduledJobService scheduledJobService;

  @Autowired
  private ProductEmailService productEmailService;

  @RequestMapping(value = ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Run post live config changes", description = "Run post live config changes")
  @ResponseBody
  public GdnBaseRestResponse runPostLiveReviewConfigChanges(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) Long dateInMillis) throws Exception {
    try {
      Date date;
      if (Objects.nonNull(dateInMillis)) {
        date = new Date(dateInMillis);
      } else {
        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.DATE, -1);
        date = calendar.getTime();
      }
      log.info("Running post live config changes for storeId : {}, date : {}", storeId, date);
      scheduledJobService.runPostLiveConfigChanges(storeId, requestId, username, date.getTime());
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while running post live config changes", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ScheduledJobControllerPath.AUTO_APPROVAL_OF_PENDING_PRODUCTS,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "auto approval of pending products", description = "auto approval of pending products")
  @ResponseBody
  public GdnBaseRestResponse autoApprovalOfPendingProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = Constants.CREATED_DATE) String orderProductsBy,
      @RequestParam(required = false, defaultValue = Constants.ASC) String orderProductsIn,
      @RequestParam(required = false, defaultValue = Constants.PENDING) String status) throws Exception {
    try {
      scheduledJobService.autoApprovePendingProducts(storeId, orderProductsBy, orderProductsIn, status);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while auto approval of pending products ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ScheduledJobControllerPath.RETRY_PRODUCTS_BY_ACTION,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "auto approval of pending products", description = "auto approval of pending products")
  @ResponseBody
  public GdnBaseRestResponse retryProductsByAction(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = Constants.CREATED_DATE) String orderProductsBy,
      @RequestParam(required = false, defaultValue = Constants.ASC) String orderProductsIn,
      @RequestParam(required = false, defaultValue = Constants.AUTO_NEED_REVISION) String action) throws Exception {
    try {
      scheduledJobService.retryProductsByAction(storeId, orderProductsBy, orderProductsIn, action);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while auto approval of pending products ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ScheduledJobControllerPath.Add_PRODUCT_TO_AUTO_APPROVAL, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "auto approval of pending products on config change", description = "auto approval of pending products on config change")
  @ResponseBody
  public GdnBaseRestResponse addProductToAutoApprovalConfigChange(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) throws Exception {
    try {
      scheduledJobService.addProductsForAutoApprovalOnConfigChange(storeId);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while auto approval on config change ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ScheduledJobControllerPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish pending common image migration records", description = "publish pending common image migration records")
  @ResponseBody
  public GdnBaseRestResponse publishCommonImageMigrationRecords(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("migrationType") String migrationType,
      @RequestBody ProductCodeListRequest productCodeListRequest) {
    try {
      scheduledJobService.publishPendingProductMigrationRecords(storeId, migrationType, productCodeListRequest);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while publishing events for backfilling of common image flag ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ScheduledJobControllerPath.SYNC_NEED_CORRECTION_PRODUCTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "sync need revision products", description = "sync need revision products")
  @ResponseBody
  public GdnBaseRestResponse syncNeedCorrectionProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = Constants.DEFAULT_NUM_OF_HOURS) int dataWindowInHours) throws Exception {
    try {
      log.info("Syncing need revision products {}", requestId);
      scheduledJobService.syncNeedCorrectionProducts(storeId, dataWindowInHours);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while sync of need revision products ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @Operation(summary = "Send scheduled Mailer for Evidence Requested Products")
  @GetMapping(value = ScheduledJobControllerPath.SEND_EVIDENCE_REQUESTED_MAIL, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse sendMailForEvidenceRequested(@RequestParam String storeId,
    @RequestParam String requestId, @RequestParam String productEmailType) {
    try {
      log.info("Sending suspension mail for requestId {} & productEmailType {} ", requestId,
        productEmailType);
      productEmailService.sendProductMailEventsToBusinessPartnersForSuspension(storeId,
        productEmailType);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception ex) {
      log.error("error while sending suspension mails ", ex);
      return new GdnBaseRestResponse(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }
}
