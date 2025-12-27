package com.gdn.x.productcategorybase.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.AggregateApiPath;
import com.gdn.x.productcategorybase.AggregateCommandDesc;
import com.gdn.x.productcategorybase.service.AggregateService;
import com.gdn.x.productcategorybase.service.AsyncProcessor;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = AggregateApiPath.BASE_PATH)
@Tag(name = "AggregateController", description = "Aggregate Service API")
public class AggregateController {

  @Autowired
  private AggregateService aggregateService;

  @Autowired
  private AsyncProcessor asyncProcessor;

  @RequestMapping(value = AggregateApiPath.PUBLISH_ALL_PRODUCTS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all products")
  
  public GdnBaseRestResponse publishAllProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int startPage) {
    asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_CONTROLLER,
        () -> aggregateService.publishPageOfProducts(storeId,startPage));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = AggregateApiPath.PUBLISH_ALL_PRODUCT_CATEGORIES, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all product categories")
  
  public GdnBaseRestResponse publishAllProductCategories(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int startPage) {
    asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_CATEGORY_CONTROLLER,
        () -> aggregateService.publishPageOfProductCategories(storeId,startPage));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = AggregateApiPath.PUBLISH_ALL_PRODUCT_ATTRIBUTES, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all product attributes")
  
  public GdnBaseRestResponse publishAllProductAttributes(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int startPage) {
    asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_ATTRIBUTE_CONTROLLER,
        () -> aggregateService.publishPageOfProductAttributes(storeId,startPage));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = AggregateApiPath.PUBLISH_ALL_IMAGES, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all images")
  
  public GdnBaseRestResponse publishAllImages(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int startPage) {
    asyncProcessor.submitWithBackoff(AggregateCommandDesc.IMAGE_CONTROLLER,
        () -> aggregateService.publishPageOfImages(storeId,startPage));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = AggregateApiPath.PUBLISH_ALL_PRODUCT_ITEMS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all product items")
  
  public GdnBaseRestResponse publishAllProductItems(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int startPage) {
    asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_ITEM_CONTROLLER,
        () -> aggregateService.publishPageOfProductItems(storeId,startPage));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

}
