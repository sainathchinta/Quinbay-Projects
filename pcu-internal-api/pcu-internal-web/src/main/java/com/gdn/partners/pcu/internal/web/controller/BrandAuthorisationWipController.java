package com.gdn.partners.pcu.internal.web.controller;

import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthValidationResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthWipDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthCreateWipResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.partners.pcu.internal.model.BrandAuthorisationWipPath;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationWipService;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthorisationWipActionWebRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@Tag(name = "Brand Authorisation Wip Controller", description = "Brand Authorisation Wip Controller ")
@RestController
@RequestMapping(value = BrandAuthorisationWipPath.BASE_PATH)
@RequiredArgsConstructor
public class BrandAuthorisationWipController {

  private final BrandAuthorisationWipService brandAuthorisationWipService;
  private final ClientParameterHelper clientParameterHelper;


  @GetMapping(value = BrandAuthorisationWipPath.DETAILS, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Fetch Brand Auth Wip Detail", description = "Fetch Brand Auth Wip Detail")
  public GdnRestSingleResponse<BrandAuthWipDetailResponse> fetchBrandAuthWipDetail(
      @RequestParam("status") String status, @RequestParam("id") String id) {
    return new GdnRestSingleResponse<>(
        brandAuthorisationWipService.fetchBrandAuthWipDetails(clientParameterHelper.getStoreId(),
            status, id, clientParameterHelper.getBusinessPartnerCode()),
        clientParameterHelper.getRequestId());
  }

    @GetMapping(value = BrandAuthorisationWipPath.VALIDATE_BRAND_REQUEST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Validate Brand Authorisation Request", description = "Validate Brand "
      + "Authorisation Request")
  public SingleBaseResponse<BrandAuthValidationResponse> validateBrandAuthRequest(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam boolean edited,
      @PathVariable("brandCode") String brandCode, @PathVariable("sellerCode") String sellerCode) {
      return new SingleBaseResponse<>(null, null, true, requestId, new BrandAuthValidationResponse(
        brandAuthorisationWipService.validateBrandAuthRequest(storeId, brandCode, sellerCode,
          edited)));
  }

  @Operation(summary = "Action Api for Brand Authorisation Wip", description = "Approves, Rejects"
      + " and send for Need revision Brand Authorisation Wip for a seller with brandCode")
  @PostMapping(value = BrandAuthorisationWipPath.ACTION, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse brandAuthorisationWipAction(
      @RequestBody BrandAuthorisationWipActionWebRequest brandAuthorisationWipActionWebRequest) {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    log.info("Invoking action for Brand Authorisation Wip for brand code : {}",
        brandAuthorisationWipActionWebRequest.getBrandCode());
    brandAuthorisationWipService.brandAuthorisationWipAction(storeId, username,
        brandAuthorisationWipActionWebRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = BrandAuthorisationWipPath.CREATE_BRAND_AUTH, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Create New Brand Authorisation Request", description = "Create New Brand "
      + "Authorisation Request")
  public GdnRestSingleResponse<BrandAuthCreateWipResponse> createBrandAuthRequest(
      @RequestBody BrandAuthCreateWipRequest request) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    BrandAuthCreateWipRequest brandCreateRequest = request;
    brandCreateRequest.setSellerCode(this.clientParameterHelper.getBusinessPartnerCode());
    log.info("Adding brand auth for seller {} and brand code {} ", brandCreateRequest.getSellerCode(),
        request.getBrandCode());
    BrandAuthCreateWipResponse response =
        brandAuthorisationWipService.createBrandAuthRequest(storeId, username, requestId, brandCreateRequest);
    return new GdnRestSingleResponse<>(null, null, true, response, requestId);
  }

  @PostMapping(value = BrandAuthorisationWipPath.UPDATE_BRAND_AUTH, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update Brand Authorisation Wip Request", description = "Update Brand "
      + "Authorisation Wip Request")
  public GdnBaseRestResponse updateBrandAuthorisationWipRequest(
      @RequestBody BrandAuthUpdateRequest request) {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    String sellerCode = this.clientParameterHelper.getBusinessPartnerCode();
    request.setSellerCode(sellerCode);
    log.info("Update brand auth wip for seller {} and brand code {} ", request.getSellerCode(),
        request.getBrandCode());
    brandAuthorisationWipService.updateBrandAuthWip(storeId, username, request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = BrandAuthorisationWipPath.FILTER_SUMMARY, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Brand Authorisation Wip List", description = "Brand "
      + "Authorisation Wip List")
  public ListBaseResponse<BrandAuthorisationWipListResponse> filterSummary(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size,
      @RequestBody BrandAuthorisationWipListRequest request) {
    String requestId = this.clientParameterHelper.getRequestId();
    String sellerCode = this.clientParameterHelper.getBusinessPartnerCode();
    request.setSellerCode(sellerCode);
    log.info("Fetching brand authorisation list for seller code {}",
        clientParameterHelper.getBusinessPartnerCode());
    Page<BrandAuthorisationWipListResponse> response =
        brandAuthorisationWipService.getBrandAuthorisationWipList(page, size, request);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @GetMapping(value = BrandAuthorisationWipPath.CREATION_ELIGIBILITY, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check for the eligibility to create new brand auth request", description
      = "Check for the eligibility to create new brand auth request")
  public SingleBaseResponse<SimpleBooleanResponse> checkEligibility() {
    String sellerCode = this.clientParameterHelper.getBusinessPartnerCode();
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Check for the eligibility to create new brand auth request for seller:{}",
        sellerCode);
    return new SingleBaseResponse<>(null, null, true, requestId,
        new SimpleBooleanResponse(brandAuthorisationWipService.checkEligibility(sellerCode)));
  }
}
