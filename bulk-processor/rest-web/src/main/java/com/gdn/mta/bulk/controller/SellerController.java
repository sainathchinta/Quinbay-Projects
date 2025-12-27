package com.gdn.mta.bulk.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.dto.product.DormantSellerProductUpdateRequest;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.mta.bulk.service.DormantSellerServiceWrapper;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@RestController
@Slf4j
@RequestMapping(value = SellerController.BASE_PATH)
@Tag(name = "SellerController", description = "Seller Service API")
public class SellerController {

  public static final String BASE_PATH = "/api/seller";
  public static final String PROCESS_PENDING_SELLER = "/process-pending-seller";
  public static final String UPDATE_VIEWCONFIG_ITEM_SKU = "/update-viewconfig-pending-items";
  public static final String UPDATE_SELLER_STATUS = "/update-seller-status";
  public static final String DELETE_DORMANT_SELLER_PRODUCT = "/delete-dormant-seller-product";
  public static final String OVERRIDE_DORMANT_SELLER_PRODUCT_STATUS = "/override-dormant-seller-product-status";
  public static final String RETRY_DORMANT_SELLER_PROCESS = "/retry-dormant-seller-process";
  public static final String NOTIFY_STUCK_DORMANT_PROCESS = "/notify-stuck-process";
  public static final String UPDATE_DORMANT_SELLER_EVENT = "/update-dormant-seller-event";

  @Autowired
  private DormantSellerService dormantSellerService;

  @Autowired
  private DormantSellerServiceWrapper dormantSellerServiceWrapper;

  @RequestMapping(value = PROCESS_PENDING_SELLER, method = RequestMethod.GET,
    produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "API start processing of pending events", description = "API start processing of pending events")
  public GdnBaseRestResponse processPendingDormantSellerEvent(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestParam String processType) {
    log.info("Started process of pending seller events :{} for processType : {}", username, processType);
    try {
      this.dormantSellerServiceWrapper.processPendingDormantSellerEvent(storeId, requestId,
        username, processType);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error processing pending seller events, error - ", e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = UPDATE_VIEWCONFIG_ITEM_SKU, method = RequestMethod.GET,
    produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "API to process pending items of dormant seller", description = "API to process pending items of dormant seller")
  public GdnBaseRestResponse updateViewConfigForItems(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestParam String processType) {
    log.info("Started update of item skus for dormant seller : {}", username);
      this.dormantSellerService.updateViewConfigForItemsOfDormantSeller(storeId, requestId,
        username, processType);
      return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = UPDATE_SELLER_STATUS, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "API update dormant seller status based on processed product", description = "API update dormant seller status based on processed product")
  public GdnBaseRestResponse updateDormantSellerStatus(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    log.info("Started update dormant seller status based on processed product by : {}", username);
    try {
      this.dormantSellerService.updateDormantSellerStatus(storeId, requestId, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error updating dormant seller status based on processed product, error - {}",e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = DELETE_DORMANT_SELLER_PRODUCT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "API update dormant seller status based on processed product",
      description = "API update dormant seller status based on processed product")
  public GdnBaseRestResponse deleteDormantSellerProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam int pageSize, @RequestParam int batchSize, @RequestParam int days) {
    log.info("Started update dormant seller status based on processed product by : {}", username);
    try {
      this.dormantSellerService.deleteDormantSellerProduct(storeId, requestId, username, pageSize, batchSize, days);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error updating dormant seller status based on processed product, error - {}", e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = OVERRIDE_DORMANT_SELLER_PRODUCT_STATUS, method = RequestMethod.POST, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "API to manually update dormant seller product status",
    description = "API to manually update dormant seller product status")
  public GdnBaseRestResponse overrideDormantSellerProductStatus(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username,
    @RequestBody List<DormantSellerProductUpdateRequest> dormantSellerProductUpdateRequestList) {
    log.info("Override dormant seller product status by requst : {}", dormantSellerProductUpdateRequestList);
    try {
      this.dormantSellerService.overrideDormantSellerProductStatus(storeId, requestId, username,
        dormantSellerProductUpdateRequestList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error updating dormant seller product status, error - {}", e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = RETRY_DORMANT_SELLER_PROCESS, method = RequestMethod.PUT, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Retry dormant seller event", description = "Retry dormant seller event")
  public GdnBaseRestResponse retryDormantSellerProcess(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    log.info("Retry dormant seller event");
    try {
      this.dormantSellerService.retryDormantSeller(storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error retry dormant seller event, error - {} ", e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = NOTIFY_STUCK_DORMANT_PROCESS, method = RequestMethod.PUT, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Notify dormant seller stuck event", description = "Notify dormant seller stuck event")
  public GdnBaseRestResponse notifyStuckDormantSellerProcess(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    log.info("Notify dormant seller stuck event");
    try {
      this.dormantSellerService.notifyStuckDormantProcess(storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error Notify dormant seller stuck event, error - {} ", e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = UPDATE_DORMANT_SELLER_EVENT, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "API to update dormant seller event", description = "API to update dormant seller event")
  public GdnBaseRestResponse updateDormantSellerEvent(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String status, @RequestParam List<String> businessPartnerCodes,
      @RequestParam(required = false) boolean abortDataEntries) {
    log.info("Started update Dormant Seller Event Username: {} businessPartnerCodes : {}, status : {}", username,
        businessPartnerCodes, status);
    this.dormantSellerService.updateDormantSellerEvent(storeId, requestId, username, status,
        businessPartnerCodes, abortDataEntries);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
