package com.gdn.x.productcategorybase.controller;

import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.SchedulerApiPath;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.service.SchedulerService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = SchedulerApiPath.BASE_PATH)
@Tag(name = "SchedulerController", description = "Scheduler rundeck apis")
public class SchedulerController {

  @Autowired
  private SchedulerService schedulerService;

  @RequestMapping(value = SchedulerApiPath.BACKFILL_PRODUCT_ATTRIBUTE_EXTRACTED, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Backfill product attribute extracted values", description = "Backfill "
      + "product attribute extracted values")
  public GdnBaseRestResponse backfillProductExtractedAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String status) {
    schedulerService.processPublishingProductAttributeExtractionBackFilling(storeId, username,
        status);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = SchedulerApiPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish pending common image migration records", description = "publish "
      + "pending common image migration records")
  public GdnBaseRestResponse publishCommonImageMigrationRecords(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("migrationType") String migrationType,
      @RequestBody(required = false) ProductCodesRequest productCodesRequest) {
    try {
      schedulerService.publishPendingProductMigrationRecords(storeId, migrationType, productCodesRequest);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while publishing events for backfilling of common image flag ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = SchedulerApiPath.DELETE_ARCHIVED_PRODUCT_DATA, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Delete archived product data", description = "Delete archived product data")
  public GdnBaseRestResponse deleteArchivedProductData(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    try {
      schedulerService.deleteArchiveProductData(storeId);
    } catch (Exception e) {
      log.error("Exception caught while deleteArchivedProductData ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }


  @RequestMapping(value = SchedulerApiPath.PUBLISH_REJECTED_PRODUCTS_FOR_DELETION, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Publish rejected products for deletion", description = "Publish rejected "
      + "products for deletions")
  public GdnBaseRestResponse publishRejectedProductForDeletion(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    schedulerService.publishRejectedProductForDeletion(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = SchedulerApiPath.ACTIVATE_BRAND_AUTHORISATION, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Activate upcoming brand authorisation", description = "Activate upcoming brand authorisation")
  public GdnBaseRestResponse activateBrandAuthorisation(@RequestParam String storeId,
    @RequestParam String requestId, @RequestParam int daysThreshold) {
    try {
      schedulerService.activateUpcomingBrandAuthorisation(storeId, daysThreshold);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while publishing events for brand authorisation activation ", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @PostMapping(value = SchedulerApiPath.UPDATE_PRODUCT_MIGRATION_STATUS, produces =
    MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update Product Migration status",
    description = "Update Product Migration status")
  public GdnBaseRestResponse updateProductMigrationStatus(@RequestParam String storeId,
    @RequestParam String requestId, @RequestBody ProductMigrationRequest productMigrationRequest) {
    try {
      schedulerService.updateProductMigrationStatus(storeId, requestId, productMigrationRequest);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      log.error("Exception caught while updating product migration status for productCode : {} ",
        productMigrationRequest.getProductCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @PostMapping(value = SchedulerApiPath.SEND_NEAR_EXPIRY_MAIL, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Send Near Expiry mail for brand authorisation", description = "Send Near "
      + "Expiry mail for brand authorisation")
  public GdnBaseRestResponse sendNearExpiryMailForBrandAuthorisation(@RequestParam String storeId,
      @RequestParam String requestId) {
    schedulerService.sendNearExpiryMails(storeId);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }
}
