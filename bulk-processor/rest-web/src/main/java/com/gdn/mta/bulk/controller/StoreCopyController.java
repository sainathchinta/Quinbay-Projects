package com.gdn.mta.bulk.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.SimpleStringResponse;
import com.gdn.mta.bulk.service.StoreCopyServiceWrapper;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@RestController
@Slf4j
@RequestMapping(value = StoreCopyController.BASE_PATH)
@Tag(name = "StoreCopyController", description = "Store Copy Service API")
public class StoreCopyController {

  private static final String ROOT = "/";
  public static final String BASE_PATH = ROOT + "api/store-copy";
  public static final String DOWNLOAD_TARGET_SELLER_TEMPLATE = "/{sellerCode}/template/download";

  @Autowired
  private StoreCopyServiceWrapper storeCopyServiceWrapper;

  @RequestMapping(value = DOWNLOAD_TARGET_SELLER_TEMPLATE, method = RequestMethod.GET,
                  produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get target seller template", description = "get target seller template")
  public GdnRestSingleResponse<SimpleStringResponse> downloadTargetSellerTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("sellerCode") String sellerCode) {
    log.info("API to download target seller template requestId : {}, sellerCode : {} ", requestId, sellerCode);
    try {
      String filePath = storeCopyServiceWrapper.downloadTargetSellerTemplate(storeId, sellerCode, username, requestId);
      return new GdnRestSingleResponse(null, null, true, new SimpleStringResponse(filePath), requestId);
    } catch (Exception e) {
      log.error("Error generating excel for store copy upload template, requestId : {} , sellerCode {} ", requestId,
          sellerCode, e);
      return new GdnRestSingleResponse(null, null, false, null, requestId);
    }
  }
}
