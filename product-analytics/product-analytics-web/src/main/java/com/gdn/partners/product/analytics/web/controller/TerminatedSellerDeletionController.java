package com.gdn.partners.product.analytics.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionApiPath;
import com.gdn.partners.product.analytics.service.ImageDeleteService;
import com.gdn.partners.product.analytics.service.TerminatedSellerDeletionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@Tag(name = "Terminated Seller Deletion Apis", description = "Terminated Seller Deletion Apis")
@RequestMapping(value = TerminatedSellerDeletionApiPath.BASE_PATH)
@RequiredArgsConstructor
public class TerminatedSellerDeletionController {

  private final TerminatedSellerDeletionService terminatedSellerDeletionService;
  private final ImageDeleteService imageDeleteService;

  @Operation(summary = "Scheduler to publish events to delete products of terminated sellers")
  @GetMapping(value = TerminatedSellerDeletionApiPath.PUBLISH_EVENTS)
  public GdnBaseRestResponse publishEvents(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false) boolean publishForAGPDeletion) {
    terminatedSellerDeletionService.publishEventsForProductDeletion(storeId, publishForAGPDeletion);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary = "Scheduler to delete images")
  @GetMapping(value = TerminatedSellerDeletionApiPath.DELETE_IMAGES)
  public GdnBaseRestResponse deleteImages(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @RequestParam(required = false) String productCode) {
    imageDeleteService.deleteImagesOfProduct(storeId, productCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}