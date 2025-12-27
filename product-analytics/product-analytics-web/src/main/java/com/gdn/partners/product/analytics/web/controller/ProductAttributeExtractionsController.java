package com.gdn.partners.product.analytics.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.product.analytics.model.ProductAttributeExtractionsApiPath;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import com.gdn.partners.product.analytics.web.model.request.ProductAttributeExtractionsRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@Tag(name = "Product Attribute Extractions Apis", description = "Product Attribute Extractions "
    + "Apis")
@RequestMapping(value = ProductAttributeExtractionsApiPath.BASE_PATH)
@RequiredArgsConstructor
public class ProductAttributeExtractionsController {

  private final ProductAttributeExtractionsService productAttributeExtractionsService;

  @Operation(summary = "Scheduler to fetch and publish events for product-attribute "
      + "extractions")
  @GetMapping(value = ProductAttributeExtractionsApiPath.PUBLISH_EVENTS)
  public GdnBaseRestResponse publishEvents(@RequestParam String storeId,
      @RequestParam int batchSize) {
    productAttributeExtractionsService.publishEventsForProductAttributeExtractions(storeId,
        batchSize);
    return new GdnBaseRestResponse(null, null, true, null);
  }

  @Operation(summary = "Scheduler to fetch and publish events for product-attribute extractions "
      + "by productSku")
  @PostMapping(value = ProductAttributeExtractionsApiPath.PUBLISH_EVENTS_BY_PRODUCT_SKU)
  public GdnBaseRestResponse publishEventsByProductSku(@RequestParam String storeId,
      @RequestBody ProductAttributeExtractionsRequest productAttributeExtractionsRequest) {
    productAttributeExtractionsService.publishEventsForProductAttributeExtractionsByProductSku(
        storeId, productAttributeExtractionsRequest.getProductSkuList());
    return new GdnBaseRestResponse(null, null, true, null);
  }
}