package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ProductOptimisationApiPath;
import com.gdn.partners.pcu.external.service.ProductOptimisationService;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductCountsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationSuggestionResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@RestController
@RequestMapping(value = ProductOptimisationApiPath.BASE_PATH)
@Tag(name ="Product Optimisation API")
@RequiredArgsConstructor
public class ProductOptimisationController {

  private final ProductOptimisationService productOptimisationService;
  private final MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary = "Get product count")
  @GetMapping(value = ProductOptimisationApiPath.PRODUCT_COUNT, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<ProductCountsWebResponse> productCount() {
    String requestId = mandatoryParameterHelper.getRequestId();
    return new GdnRestSingleResponse<>(
        productOptimisationService.getProductCount(mandatoryParameterHelper.getStoreId(),
            requestId, mandatoryParameterHelper.getBusinessPartnerCode()), requestId);
  }

  @PostMapping(value = ProductOptimisationApiPath.FILTER, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch list of products to be optimized with filter")
  public ListBaseResponse<ProductOptimisationListResponse> filter(@RequestParam int page,
      @RequestParam int size,
      @RequestBody ProductOptimisationListWebRequest productOptimisationListWebRequest) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String sellerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Fetch list of products for seller:{} with filter:{}", sellerCode,
        productOptimisationListWebRequest);
    Page<ProductOptimisationListResponse> response =
        productOptimisationService.fetchProductOptimisationList(sellerCode,
            productOptimisationListWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "Provide Feedback")
  @PostMapping(value = ProductOptimisationApiPath.PROVIDE_FEEDBACK, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse provideFeedback(
      @RequestBody ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest) {
    this.productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @GetMapping(value = ProductOptimisationApiPath.SUGGESTION_DETAILS, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "SuggestionDetail")
  public ListBaseResponse<ProductOptimisationSuggestionResponse> suggestionDetails(
      @RequestParam String productSku) {
    String requestId = mandatoryParameterHelper.getRequestId();
    List<ProductOptimisationSuggestionResponse> responses =
        productOptimisationService.getSuggestionDetails(productSku);
    return new ListBaseResponse<>(null, null, true, requestId, responses, null);
  }

  @Operation(summary = "Update status")
  @PostMapping(value = ProductOptimisationApiPath.UPDATE_STATUS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateStatus(@RequestBody
  ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest) {
    String requestId = mandatoryParameterHelper.getRequestId();
    productOptimisationService.updateStatusForProductOptimisation(requestId,
        productOptimisationUpdateStatusRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
