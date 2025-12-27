package com.gdn.mta.product.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductFbbMigrationService;
import com.gdn.mta.product.service.ScheduledJobService;
import com.gdn.mta.product.web.model.ScheduledJobControllerPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
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
  private ProductFbbMigrationService productFbbMigrationService;

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

  @RequestMapping(value = ScheduledJobControllerPath.MIGRATE_PRODUCTS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Run migration of products", description = "Run migration of products")
  @ResponseBody
  public GdnBaseRestResponse migrateProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody(required = false) SimpleListStringRequest request) throws Exception {
    log.info("Starting product migration scheduler  for storeId : {}, products : {}", storeId, request);
    if (Objects.nonNull(request) && CollectionUtils.isNotEmpty(request.getValue())) {
      scheduledJobService.migrateProducts(storeId, request.getValue());
    } else {
      scheduledJobService.migrateProducts(storeId, null);
    }
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ScheduledJobControllerPath.RETRY_MIGRATE_PRODUCTS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Retry migration of failed products", description = "Retry migration of failed products")
  @ResponseBody
  public GdnBaseRestResponse retryFailedMigratedProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody(required = false) SimpleListStringRequest request) throws Exception {
    log.info("Retrying product migration scheduler  for storeId : {}, products : {}", storeId, request);
    if (Objects.nonNull(request) && CollectionUtils.isNotEmpty(request.getValue())) {
      scheduledJobService.retryFailedMigratedProducts(storeId, request.getValue());
    } else {
      scheduledJobService.retryFailedMigratedProducts(storeId, null);
    }
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ScheduledJobControllerPath.UPDATE_MIGRATION_STATUS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update the migration status of products", description = "Update the migration status of products")
  @ResponseBody
  public GdnBaseRestResponse updateMigrationStatus(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) throws Exception {
    log.info("Starting scheduler to update the PENDING and IN_PROGRESS state {}", storeId);
    scheduledJobService.updateMigrationStatus(storeId);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ScheduledJobControllerPath.PUBLISH_IMAGE_QC_BACKLOG_PRODUCTS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update the migration status of products", description = "Update the migration status of products")
  @ResponseBody
  public GdnBaseRestResponse publishImageQcBacklogProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "updatedDate") String orderProductsBy, @RequestParam(defaultValue = "desc") String orderProductsIn)
      throws Exception {
    log.info("Publish image qc backlog products storeId : {}", storeId);
    scheduledJobService.publishImageQcBacklogProducts(storeId, orderProductsBy, orderProductsIn);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ScheduledJobControllerPath.SYNC_IN_REVIEW_PRODUCTS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Sync in review products", description = "Sync in review products")
  @ResponseBody
  public GdnBaseRestResponse syncReviewTypeProducts(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username) throws Exception {
    log.info("Syncing in review products {}", storeId);
    scheduledJobService.syncInReviewProducts(storeId);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ScheduledJobControllerPath.SYNC_ACTIVE_PRODUCTS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Sync in active products", description = "Sync in active products")
  @ResponseBody
  public GdnBaseRestResponse syncActiveProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) throws Exception {
    log.info("Syncing active products {}", storeId);
    scheduledJobService.syncActiveProducts(storeId);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ScheduledJobControllerPath.SYNC_PRE_LIVE_PRODUCTS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Sync in active products", description = "Sync in active products")
  @ResponseBody
  public GdnBaseRestResponse syncPreLiveProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) throws Exception {
    log.info("Syncing pre-live products {}", storeId);
    scheduledJobService.syncPreLiveProducts(storeId);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @PostMapping(value = ScheduledJobControllerPath.MIGRATE_FBB_PRODUCTS, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Migrate Fbb Products", description = "Migrate Fbb Products")
  @ResponseBody
  public GdnBaseRestResponse migrateFbbProducts(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @PathVariable("migrationType") String migrationType) {
    log.info("Migrating fbb products for type {}",migrationType);
    GdnPreconditions.checkArgument(org.apache.commons.lang.StringUtils.isNotEmpty(storeId),
      ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(org.apache.commons.lang.StringUtils.isNotEmpty(migrationType),
      ErrorMessages.MIGRATION_TYPE_MUST_NOT_BE_EMPTY);
    productFbbMigrationService.migrateFbbProducts(storeId, migrationType);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }


  @PutMapping(value = ScheduledJobControllerPath.ADD_DELETE_VARIANT_RETRY_PUBLISH, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add-delete-variant-retry-publish", description = "add-delete-variant-retry-publish")
  public GdnBaseRestResponse addDeleteVariantRetryPublish(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) String productCode) {
    try {
      scheduledJobService.addDeleteVariantRetryPublishEvents(storeId, productCode,requestId , username);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while running add delete variant publish scheduler ", e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }


}
