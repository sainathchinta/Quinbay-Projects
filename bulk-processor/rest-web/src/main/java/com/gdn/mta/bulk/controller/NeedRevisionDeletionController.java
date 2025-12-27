package com.gdn.mta.bulk.controller;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.service.NeedRevisionDeletionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;

@RestController
@Slf4j
@RequestMapping(value = NeedRevisionDeletionController.BASE_PATH)
@Tag(name = "needRevisionDeletionController", description = "need-revision-deletion")
public class NeedRevisionDeletionController {

  public static final String BASE_PATH = "/api/needRevisionDeletion";
  public static final String FETCH_AND_POPULATE_NR_BUSINESS_PARTNER_CODES = "/fetchAndPopulateNRBusinessPartnerCodes";
  public static final String PROCESS_NEED_REVISION_DELETION = "/process-need-revision-deletion";
  public static final String SEND_NOTIFICATION = "/sendNotificationForNeedRevisionDeletion";
  public static final String FETCH_BUSINESS_PARTNER_NR_PRODUCTS = "/fetch-business-partner-products";

  @Autowired
  private NeedRevisionDeletionService needRevisionDeletionService;

  @RequestMapping(value = FETCH_AND_POPULATE_NR_BUSINESS_PARTNER_CODES, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 0 : fetch and populate nr business partner codes", description = "Scheduler 0 : fetch and populate nr business partner codes")
  public GdnBaseRestResponse processNewInternalProcessRequests(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    log.info("API to fetch and populate nr business partner codes with request id : {}", requestId);
    try {
      needRevisionDeletionService.populateNRBusinessPartnerCodes(storeId, requestId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("API to fetch and populate nr business partner codes failed, requestId : {}, ", requestId, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = PROCESS_NEED_REVISION_DELETION, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 2 : Process Data table for Need Revision Deletion", description
    = "Process Data table for Need Revision Deletion")
  public GdnBaseRestResponse processNeedRevisionDeletion(@RequestParam String storeId,
    @RequestParam Integer fetchProcessCountForDeletion, @RequestParam String requestId) {
    log.info("API to Process Data table for Need Revision Deletion ran at {} ", new Date());
    try {
      needRevisionDeletionService.processNeedRevisionDeletion(storeId, fetchProcessCountForDeletion);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("API to fetch and populate nr business partner codes failed, requestId : {}, ", requestId, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }



  @RequestMapping(value = SEND_NOTIFICATION, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler 3 : send notification for need revision deletion", description = "Scheduler 0 : fetch and populate nr business partner codes")
  public GdnBaseRestResponse sendNotificationForNeedRevisionDeletion(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) throws Exception {
    log.info("Starting scheduler to send notification for processed data : {}, username : {}", requestId, username);
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion(storeId, requestId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = FETCH_BUSINESS_PARTNER_NR_PRODUCTS, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary =
                 "Scheduler 1 : Fetch products of a business partner code and save in NR data table", description =
                 "Scheduler 1 : Fetch products of a business partner code and save in data table")
  public GdnBaseRestResponse fetchBusinessPartnerProductsAndSave(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    try {
      needRevisionDeletionService.fetchProductsOfABusinessPartner(storeId, requestId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception exception) {
      log.error("Error while fetching products of businessPartners ", exception);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }


}
