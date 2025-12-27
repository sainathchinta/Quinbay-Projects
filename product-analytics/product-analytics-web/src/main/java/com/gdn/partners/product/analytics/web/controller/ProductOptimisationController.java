package com.gdn.partners.product.analytics.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.model.ProductOptimisationApiPath;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationListResponse;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationSuggestionResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.product.analytics.web.model.ProductCountsWebResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationUpdateStatusRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@RestController
@Tag(name = "Product Optimisation Controller", description = "Product Optimisation Controller")
@RequestMapping(value = ProductOptimisationApiPath.BASE_PATH)
@RequiredArgsConstructor
public class ProductOptimisationController {

  private final ProductOptimisationService productOptimisationService;

  @PostMapping(value = ProductOptimisationApiPath.FILTER, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch the list of product to be optimised for a seller")
  public GdnRestListResponse<ProductOptimisationListResponse> filter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size,
      @RequestBody ProductOptimisationListRequest productOptimisationListRequest) {
    Page<ProductOptimisationListResponse> responses =
        productOptimisationService.fetchProductOptimisationList(storeId,
            productOptimisationListRequest, page, size);
    return new GdnRestListResponse<>(null, null, true, responses.getContent(),
        new PageMetaData(size, page, responses.getTotalElements()), requestId);
  }

  @Operation(summary = "Get product count")
  @GetMapping(value = ProductOptimisationApiPath.PRODUCT_COUNT, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<ProductCountsWebResponse> productCount(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String sellerCode) {
    return new GdnRestSingleResponse<>(
        productOptimisationService.getProductCounts(storeId, sellerCode), requestId);
  }

  @Operation(summary = "Update status")
  @PostMapping(value = ProductOptimisationApiPath.UPDATE_STATUS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateStatus(@RequestParam String requestId,
      @RequestBody ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest) {
    productOptimisationService.updateStatusForProductOptimisation(
        productOptimisationUpdateStatusRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary = "Provide feedback")
  @PostMapping(value = ProductOptimisationApiPath.PROVIDE_FEEDBACK, produces =
               MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse submitSuggestionFeedback(@RequestParam String requestId, @RequestBody
      ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest) {
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary = "SuggestionDetail")
  @GetMapping(value = ProductOptimisationApiPath.SUGGESTION_DETAIL, produces =
              MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ProductOptimisationSuggestionResponse> showSuggestionDetails(
      @RequestParam String requestId, @RequestParam String storeId,
      @RequestParam String productSku) {
    List<ProductOptimisationSuggestionResponse> productOptimisationSuggestionResponses =
        productOptimisationService.showSuggestionDetails(storeId, productSku);
    return new GdnRestListResponse<>(productOptimisationSuggestionResponses,
        new PageMetaData(0, 0, productOptimisationSuggestionResponses.size()), requestId);
  }

  @Operation(summary = "Clear seller cache")
  @GetMapping(value = ProductOptimisationApiPath.CLEAR_CACHE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse clearSellerCache(@PathVariable String sellerCode,
    @RequestParam String requestId) {
    productOptimisationService.clearSellerLevelCache(sellerCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}