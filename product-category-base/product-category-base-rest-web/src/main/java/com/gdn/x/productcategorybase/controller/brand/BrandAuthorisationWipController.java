package com.gdn.x.productcategorybase.controller.brand;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.BrandAuthorisationWipPath;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthorisationWipListResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthWipDetailResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipListRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthServiceWrapper;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipServiceWrapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = BrandAuthorisationWipPath.BASE_PATH)
@Tag(name = "BrandAuthorisationWipController", description = "Brand Authorisation Wip Service API")
@RequiredArgsConstructor
public class BrandAuthorisationWipController {

  private final BrandAuthorisationWipService brandAuthorisationWipService;
  private final BrandAuthorisationWipServiceWrapper brandAuthorisationWipServiceWrapper;
  private final BrandAuthServiceWrapper brandAuthServiceWrapper;


  @PostMapping(value = BrandAuthorisationWipPath.APPROVE, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Approve Brand Authorisation Wip", description = "Approve Brand "
      + "Authorisation Wip")
  public GdnBaseRestResponse approve(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest) {
    log.info("Approve Brand Authorisation Wip for Seller : {} ",
        brandAuthorisationWipActionRequest.getSellerCode());
      brandAuthorisationWipServiceWrapper.brandAuthorisationWipAction(storeId, username,
          brandAuthorisationWipActionRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = BrandAuthorisationWipPath.DETAILS, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Fetch Brand Auth Wip Detail", description = "Fetch Brand Auth Wip Detail")
  public GdnRestSingleResponse<BrandAuthWipDetailResponse> fetchBrandAuthWipDetail(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam("status") String status,
      @RequestParam("id") String id) {
    return new GdnRestSingleResponse<>(
        brandAuthorisationWipService.fetchBrandAuthWipDetails(storeId, status, id), requestId);
  }

  @PostMapping(value = BrandAuthorisationWipPath.CREATE_BRAND_AUTH, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Create New Brand Authorisation Request", description = "Create New Brand "
      + "Authorisation Request")
  public GdnRestSingleResponse<BrandAuthCreateWipResponse> createRequest(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody BrandAuthCreateWipRequest request) {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      log.info("Adding brand auth for seller {} and brand code {} ", request.getSellerCode(),
          request.getBrandCode());
      BrandAuthCreateWipResponse response =
          brandAuthServiceWrapper.createBrandAuthWip(request, storeId, username);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (ApplicationRuntimeException ex) {
      log.error(
          "#Exception while creating brand auth for seller code : {} and brand code : {} with "
              + "exception",
          request.getSellerCode(), request.getBrandCode(), ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ex.getErrorCodes().getCode(), false, null,
          requestId);
    } catch (Exception ex) {
      log.error("Exception while creating brand auth for seller code : {} and brand code : {} and"
          + " exception ", request.getSellerCode(), request.getBrandCode(), ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @PostMapping(value = BrandAuthorisationWipPath.SUBMIT_AUTHORISATION_REQUEST, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Submit brand authorisation request", description = "Submit brand authorisation request")
  public GdnBaseRestResponse submitBrandAuthorisationRequest(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestBody BrandAuthUpdateRequest brandAuthUpdateRequest)
    throws Exception {
    log.info("Brand Authorisation request by seller : {} for brand : {} ",
      brandAuthUpdateRequest.getSellerCode(), brandAuthUpdateRequest.getBrandCode());
    brandAuthorisationWipServiceWrapper.submitBrandAuthorisationRequest(storeId, username,
      brandAuthUpdateRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = BrandAuthorisationWipPath.VALIDATE_BRAND_REQUEST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Validate Brand Authorisation Request", description = "Validate Brand "
      + "Authorisation Request")
  public GdnRestSingleResponse<SimpleBooleanResponse> validateBrandAuthRequest(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam boolean edited,
      @PathVariable("brandCode") String brandCode, @PathVariable("sellerCode") String sellerCode) {
    return new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(
        brandAuthorisationWipService.validateBrandAuthRequest(storeId, brandCode, sellerCode, edited)),
        requestId);
  }

  @PostMapping(value = BrandAuthorisationWipPath.FILTER_SUMMARY, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch list of brand authorisation wip", description = "Fetch list of "
      + "brand authorisation wip")
  public GdnRestListResponse<BrandAuthorisationWipListResponse> filterSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody BrandAuthorisationWipListRequest request,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size) {
    log.info("Api to fetch brand authorisation wip list by filter request = {} ", request);
    Page<BrandAuthorisationWipListResponse> response =
        brandAuthorisationWipService.getBrandAuthorisationWipListResponse(storeId, request,
            PageRequest.of(page, size));
    return new GdnRestListResponse<>(null, null, true, response.getContent(),
        new PageMetaData(size, page, response.getTotalElements()), requestId);
  }

  @GetMapping(value = BrandAuthorisationWipPath.CREATION_ELIGIBILITY, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Checks for the eligibility of the seller to create brand-auth request",
             description = "Checks for the eligibility of the seller to create brand-auth request")
  public GdnRestSingleResponse<SimpleBooleanResponse> checkEligibility(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String sellerCode) {
    log.info("Api to check for the eligibility of the seller to create brand-auth wip request for "
            + "seller:{}", sellerCode);
    return new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(
        brandAuthorisationWipServiceWrapper.checkEligibility(storeId, sellerCode)),
        requestId);
  }
}
