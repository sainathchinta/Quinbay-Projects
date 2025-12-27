package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.service.VideoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.model.UtilityApiPath;
import com.gdn.partners.pcu.external.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = UtilityApiPath.BASE_PATH)
@Tag(name ="Utility API")
public class UtilityController {

  @Autowired
  private ProductService productService;

  @Autowired
  private VideoService videoService;

  @Operation(summary = "API to republish L3 event to AGP")
  @GetMapping(value = UtilityApiPath.REPUBLISH_L3_TO_AGP, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse republishProductsToAgp(@PathVariable("productSku") String productSku) {
    log.info("API to republish L3 event to AGP for productSku : {}  ",
        productSku);
    productService.republishProductToAgp(productSku);
    return new BaseResponse(null, null, true, null);
  }

  @Operation(summary = "API to republish L4 event to AGP")
  @GetMapping(value = UtilityApiPath.REPUBLISH_L4_TO_AGP, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse republishItemsToAgp(@PathVariable("itemSku") String itemSku) {
    log.info("API to republish L4 event to AGP for itemSku : {}  ",
        itemSku);
    productService.republishItemToAgp(itemSku);
    return new BaseResponse(null, null, true, null);
  }

  @Operation(summary = "API to republish L5 event to AGP")
  @GetMapping(value = UtilityApiPath.REPUBLISH_L5_TO_AGP, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse republishItemPickupPointToAgp(@PathVariable("itemSku") String itemSku,
      @PathVariable("pickupPointCode") String pickupPointCode,
      @RequestParam(required = false, defaultValue = "true") boolean republishToAgp) {
    log.info("API to republish L5 event to AGP for itemSku : {}  pickupPointCode : {}",
        itemSku, pickupPointCode);
    productService.republishItemPickupPointToAgp(itemSku, pickupPointCode, republishToAgp);
    return new BaseResponse(null, null, true, null);
  }

  @Operation(summary = "Retry video compression for a specific video id")
  @PostMapping(value = UtilityApiPath.RETRY_VIDEO_COMPRESSION, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse retryVideoCompression(@PathVariable("videoId") String videoId)
    throws Exception {
    log.info("Retrying video compression : {}  ", videoId);
    videoService.retryVideoCompression(videoId);
    return new BaseResponse(null, null, true, null);
  }

  @Operation(summary = "Generate fingerprint for a video")
  @PostMapping(value = UtilityApiPath.GENERATE_FINGERPRINT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse generateFingerPrint(@PathVariable("videoId") String videoId)
    throws Exception {
    log.info("Generating fingerprint for video id {} ", videoId);
    videoService.generateFingerPrint(videoId);
    return new BaseResponse(null, null, true, null);
  }

  @Operation(summary = "Reindex brand by brand request code ")
  @PostMapping(value = UtilityApiPath.REINDEX_BRAND_COLLECTION_BY_BRAND_REQUEST_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse reindexBrandSolrCollection(@PathVariable("brandRequestCode") String brandRequestCode)
    throws Exception {
    log.info("Reindex brand code : {}  ", brandRequestCode);
    productService.reindexBrandCollection(brandRequestCode);
    return new BaseResponse(null, null, true, null);
  }

}
