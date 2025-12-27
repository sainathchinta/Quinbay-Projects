package com.gdn.x.productcategorybase.controller.brand;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthHistoryService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthServiceWrapper;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.BrandAuthorisationPath;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Validated
@Slf4j
@RestController
@RequestMapping(value = BrandAuthorisationPath.BASE_PATH)
@Tag(name = "BrandAuthorisationController", description = "Brand Authorisation Service API")
public class BrandAuthorisationController {

  @Autowired
  private BrandAuthorisationService brandAuthorisationService;

  @Autowired
  private BrandAuthHistoryService brandAuthHistoryService;

  @Autowired
  private BrandAuthServiceWrapper brandAuthServiceWrapper;

  @RequestMapping(value = BrandAuthorisationPath.VALID, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if brand is authorised", description = "Check if brand is authorised")
  public GdnRestSingleResponse<SimpleBooleanResponse> checkBrandAuthBySellerCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("sellerCode") String sellerCode,
      @RequestParam String brandCode, @RequestParam(defaultValue = "false") boolean toTakeDown) {
    try {
      boolean authorised = this.brandAuthorisationService.checkBrandAuthBySellerCode(storeId,
        sellerCode, brandCode, toTakeDown);
      return new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(authorised), requestId);
    } catch (Exception e) {
      log.error("Error while checking brand auth for seller code :{}, brand code :{} ", sellerCode, brandCode,
          e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.GET_AUTHORISATIONS, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if brand is authorised", description = "Check if brand is authorised")
  public GdnRestListResponse<BrandAuthFilterResponse> getAuthorisations(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "25") int size, @RequestBody BrandAuthFilterRequest request) {
    try {
      log.info("Get authorisations for BrandAuthFilterRequest = {} ", request);
      Page<BrandAuthFilterResponse> brandAuthorisationPage =
          this.brandAuthorisationService.findBrandAuthorisationByFilter(storeId, request, page, size);
      return new GdnRestListResponse<>(null, null, true, brandAuthorisationPage.getContent(),
          new PageMetaData(size, page, brandAuthorisationPage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching brand auths request :{} ", request, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.TAKE_DOWN_BASED_ON_BRAND, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if product has to be taken down based on brand", description = "Check if product has to be taken down based on brand")
  public GdnRestSingleResponse<SimpleBooleanResponse> checkTakeDownBasedOnBrand(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductBrandValidationRequest productBrandValidationRequest) {
    try {
      log.info("checking if the product has to be taken down based on productBrandValidationRequest = {} ",
          productBrandValidationRequest);
      boolean takeDown =
          this.brandAuthorisationService.takeDownProductBasedOnBrand(storeId, productBrandValidationRequest);
      return new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(takeDown), requestId);
    } catch (Exception e) {
      log.error("Error while checking take down of product based on productBrandValidationRequest = {} ",
          productBrandValidationRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }


  @RequestMapping(value = BrandAuthorisationPath.BRAND_AUTHORISE_DETAIL_BY_BRAND_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetches Brand Authorisation detail by Brand code", description = "Details to be fetched for Brand Authorisation Edit")
  public GdnRestSingleResponse<BrandAuthorisationDetailResponse> getBrandAuthorisationDetailByCode(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username, @RequestParam String sellerCode,
    @PathVariable(value = "brandCode") String brandCode) {
    log.info("Fetching Brand Authorisation Details for requestId : {} for Brand Code : {}",
      requestId, brandCode);
    try {
      BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
        this.brandAuthorisationService.getBrandAuthDetailByBrandCodeAndSellerCode(storeId,
          brandCode, sellerCode);
      if (Objects.nonNull(brandAuthorisationDetailResponse)) {
        log.info("BrandAuthorisationDetailResponse for brand code : {} is {}", brandCode,
          brandAuthorisationDetailResponse);
        return new GdnRestSingleResponse<>(null, null, true, brandAuthorisationDetailResponse,
          requestId);
      } else {
        log.warn("BrandAuthorisationDetailResponse for brandCode : {} was found to be null",
          brandCode);
        return new GdnRestSingleResponse<>(null, null, true, null, requestId);
      }

    } catch (Exception e) {
      log.error("Error while fetching brand Auth Details for seller Code : {} and brand code : {} ",
        sellerCode, brandCode, e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.CREATE, method = RequestMethod.POST, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Create New Brand Authorisation", description = "Create New Brand Authorisation")
  public GdnRestSingleResponse<BrandAuthCreateResponse> create(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestBody BrandAuthCreateRequest request) {
    try {
      log.info("Adding brand auth for seller {} and brand code {} ", request.getSellerCode(),
        request.getBrandCode());
      BrandAuthCreateResponse response =
        brandAuthServiceWrapper.createBrandAuthAndEvictCache(request, storeId, username);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception ex) {
      log.error("Exception while creating brand auth for seller code : {} and brand code : {} and"
        + " exception ", request.getSellerCode(), request.getBrandCode(), ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.DELETE, method = RequestMethod.POST, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete Brand Authorisation", description = "delete mapping for brand authorisation")
  public GdnBaseRestResponse delete(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestBody @NotEmpty(message = "BrandAuth Delete Request List cannot be empty") List< @Valid BrandAuthDeleteRequest> brandAuthDeleteRequestList) {
    log.info("Proceeding with deletion Brand Authorisation mapping for brandAuthDeleteRequestList : {} ", brandAuthDeleteRequestList.toString());
    try {
      this.brandAuthServiceWrapper.deleteBrandAuthAndEvictCache(storeId, username, brandAuthDeleteRequestList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error Encountered with Delete Request for requestId : {}, error :", requestId,
        e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getMessage(), false,
        requestId);
    } catch (Exception e) {
      log.error("Error Encountered with Delete Request for requestId : {}, error :", requestId,
        e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.HISTORY_FILTER_SUMMARY, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to fetch history of a brand auth")
  public GdnRestListResponse<BrandAuthHistoryResponse> getBrandAuthHistory(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody BrandAuthHistoryRequest brandAuthHistoryRequest,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching brand auth history for {}", brandAuthHistoryRequest);
    try {
      Page<BrandAuthHistoryResponse> brandAuthHistoryResponse = this.brandAuthHistoryService
        .getBrandAuthHistory(storeId, brandAuthHistoryRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, brandAuthHistoryResponse.getContent(),
        new PageMetaData(size, page, brandAuthHistoryResponse.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error Encountered with fetching history Request for requestId : {}, error : {}",
        requestId, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false,
        null, null, requestId);
    } catch (Exception e) {
      log.error("Error Encountered with fetching history Request for requestId : {}, error : {}",
        requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.GET_BRAND_AUTH_BY_IDS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetches Brand Authorisation detail by id", description = "Details to be fetched"
    + " for Brand Authorisation by id")
  public GdnRestListResponse<BrandAuthBulkDownloadResponse> getBrandAuthDetailsById(
    @RequestParam String storeId, @RequestParam String requestId,
    @RequestBody BrandAuthBulkDownloadRequest brandAuthBulkDownloadRequest) {
    log.info("Fetching Brand Authorisation Details for requestId : {} for request : {}", requestId,
      brandAuthBulkDownloadRequest);
    try {
      List<BrandAuthBulkDownloadResponse> brandAuthBulkResponse = this.brandAuthorisationService
        .getBrandAuthBulkResponse(storeId, brandAuthBulkDownloadRequest.getIds());
      if (CollectionUtils.isNotEmpty(brandAuthBulkResponse)) {
        return new GdnRestListResponse<>(null, null, true, brandAuthBulkResponse, null, requestId);
      }
      return new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, requestId);
    } catch (Exception e) {
      log.error("Error while fetching brand Auth Details by id ", e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = BrandAuthorisationPath.UPDATE, method = RequestMethod.POST, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update Brand Authorisation", description = "Update Brand Authorisation")
  public GdnBaseRestResponse update(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestBody BrandAuthUpdateRequest updateRequest) {
    log.info("Updating Authorisation for Seller : {} ", updateRequest.getSellerCode());
    try {
      this.brandAuthServiceWrapper.editBrandAuthDetailsAndEvictCache(storeId, updateRequest, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException ae) {
      log.error("Error with processing update Request for seller : {} and brandCode : {} ",
        updateRequest.getSellerCode(), updateRequest.getBrandCode(), ae);
      return new GdnBaseRestResponse(ae.getErrorMessage(), ae.getErrorCodes().getCode(), false,
        requestId);
    } catch (Exception e) {
      log.error("Error with processing update Request for seller : {} and brandCode : {} ",
        updateRequest.getSellerCode(), updateRequest.getBrandCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }
}
