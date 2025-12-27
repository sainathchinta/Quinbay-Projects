package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.core.security.annotation.Authorize;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ProductBusinessPartnerApiPath;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.ProductBusinessPartnerService;
import com.gdn.partners.pcu.external.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.external.web.model.request.CopyProductItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductBusinessPartnerResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointSummaryWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 09/12/2018 AD.
 */

@Slf4j
@Tag(name ="Product Business Partner API")
@RestController
@RequestMapping(value = ProductBusinessPartnerApiPath.BASE_PATH)
public class ProductBusinessPartnerController {

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary ="Create Product using flow2")
  @PostMapping(value = ProductBusinessPartnerApiPath.CREATE,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public SingleBaseResponse<CreateProductBusinessPartnerResponse> create(
      @RequestBody ProductBusinessPartnerWebRequest request) throws Exception {
    SingleBaseResponse<CreateProductBusinessPartnerResponse> response =
        productBusinessPartnerService
            .create(ConverterUtil.toProductBusinessPartnerServiceRequest(request, mandatoryParameterHelper));
    response.setRequestId(mandatoryParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary ="Get businessPartner profile by business partner code")
  @GetMapping(value = ProductBusinessPartnerApiPath.GET_BUSINESS_PARTNER_PROFILE_BY_ID,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BusinessPartnerProfileWebResponse> getBusinessPartnerProfileById() {
    String requestId = mandatoryParameterHelper.getRequestId();
    BusinessPartnerProfileWebResponse response = productBusinessPartnerService
        .getBusinessPartnerProfile(mandatoryParameterHelper.getBusinessPartnerCode());
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="Copy Product using flow2")
  @PostMapping(value = ProductBusinessPartnerApiPath.COPY_PRODUCT_ITEMS,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse copyProductItems(@RequestBody CopyProductItemWebRequest request) {
    GdnBaseRestResponse response = productBusinessPartnerService
      .copy(ConverterUtil.toProductCopyRequest(request, mandatoryParameterHelper), false);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(),
      response.isSuccess(), mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="Copy Product using flow2")
  @PostMapping(value = ProductBusinessPartnerApiPath.COPY_ALL_PRODUCT_ITEMS,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse copyAllProductItems(@RequestBody CopyProductItemWebRequest request) {
    GdnBaseRestResponse response = productBusinessPartnerService
      .copyAll(ConverterUtil.toProductCopyRequest(request, mandatoryParameterHelper));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(),
      response.isSuccess(), mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="Retry Copy Product using flow2")
  @Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_FBB-CENTER"})
  @PostMapping(value = ProductBusinessPartnerApiPath.RETRY_COPY_PRODUCT_ITEMS,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse retryCopyProductItems(@RequestBody CopyProductItemWebRequest request) {
    GdnBaseRestResponse response = productBusinessPartnerService
      .copy(ConverterUtil.toProductCopyRequest(request, mandatoryParameterHelper), true);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(),
      response.isSuccess(), mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="Products available to copy from linked Account")
  @PostMapping(value = ProductBusinessPartnerApiPath.PRODUCT_ITEMS_TO_COPY,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<AvailableToCopyProductDetailsResponse> productsAvailableToBeCopied(
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
    @RequestBody ProductLevel3SummaryRequest filterRequest) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String linkedPartnerCode = mandatoryParameterHelper.getLinkedBusinessPartnerCode();

    return productBusinessPartnerService
      .productsAvailableToDirectCopy(businessPartnerCode, linkedPartnerCode, page, size, filterRequest);
  }

  @Operation(summary ="Check if any product is mapped to businessPartner")
  @GetMapping(value = ProductBusinessPartnerApiPath.IS_PRODUCT_MAPPED_TO_MERCHANT, produces = MediaType
      .APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Boolean> isProductsMappedToBusinessPartnerCode() {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    boolean response = productBusinessPartnerService.isProductMappedToMerchant(businessPartnerCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="Fetch pickup point detail summary")
  @PostMapping(value = ProductBusinessPartnerApiPath.FETCH_PICKUP_POINT_DETAIL_SUMMARY, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<PickupPointSummaryWebResponse> fetchPickupPointDetailSummary(
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size,
    @RequestBody PickupPointSummaryWebRequest pickupPointSummaryWebRequest) throws Exception {
    String storeId = mandatoryParameterHelper.getStoreId();
    String requestId = mandatoryParameterHelper.getRequestId();
    RequestHelper.validateAuthorisation(mandatoryParameterHelper.getBusinessPartnerCode(),
      pickupPointSummaryWebRequest.getBusinessPartnerCode());
    Page<PickupPointSummaryWebResponse> pickupPointSummaryWebResponses =
        pickupPointService.getPickupPointSummaryFilter(storeId, page, size, pickupPointSummaryWebRequest);
    return new ListBaseResponse(null, null, true, requestId, pickupPointSummaryWebResponses.getContent(),
        new Metadata(page, size, pickupPointSummaryWebResponses.getTotalElements()));
  }

  @Operation(summary ="save default pickup points and configurations")
  @PostMapping(value = ProductBusinessPartnerApiPath.SAVE_DEFAULT_PICKUP_POINT_AND_CONFIG, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse saveDefaultPickupPointsAndConfigurations(@PathVariable("merchantCode") String merchantCode,
      @RequestBody DefaultConfigurationAndPickupPointRequest request) {
    String storeId = mandatoryParameterHelper.getStoreId();
    String requestId = mandatoryParameterHelper.getRequestId();
    productBusinessPartnerService.updateDefaultConfigurationsAndPickupPoints(storeId, merchantCode, request);
    return new BaseResponse(null, null, true, requestId);
  }

}
