package com.gdn.mta.product.controller;

import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.CogsDataResponse;
import com.gda.mta.product.dto.CogsUpdateRequests;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemL5ListingRequest;
import com.gda.mta.product.dto.ItemSkuPpCodeRequest;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductL3ListingRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gda.mta.product.dto.response.ProductL3BasicResponse;
import com.gda.mta.product.dto.response.ProductL3ListingResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductAndItemPickupPontL5Response;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.service.ProductLevel3V2Wrapper;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.service.ProductLevel3V2Service;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.web.model.ProductLevel3ControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductLevel3V2ControllerPath;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pbp.helper.ResponseHelper;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.PostMapping;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static org.assertj.core.util.Preconditions.checkArgument;

@RestController("ProductLevel3ControllerV2")
@RequestMapping(value = ProductLevel3V2ControllerPath.BASE_PATH)
@Slf4j
@Tag(name = "ProductLevel3ControllerV2")
public class ProductLevel3V2Controller {

  @Autowired
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductLevel3V2Wrapper productLevel3V2Wrapper;

  @Value("${preOrder.maximum.days}")
  private int preOrderMaximumDays;

  @Value("${preOrder.working.maximum.week}")
  private int preOrderMaximumWeek;

  @Value("${combine.content.and.logistics.update}")
  private boolean combineContentAndLogisticsUpdate;

  @Value("${webp.conversion.enabled}")
  private boolean webpConversionEnabled;

  @Value("${combine.pre.order.update}")
  private boolean combinePreOrderUpdate;

  @Value("${combine.l3.and.l5.validation}")
  private boolean combineL3AndL5Validation;

  @RequestMapping(value = ProductLevel3V2ControllerPath.ITEM_LISTING_UPDATE_V2, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update l5 listing at controller", description = "update l5 listing")
  public GdnBaseRestResponse itemListingUpdateV2(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestParam(required = false) boolean isExternalOnly,
      @RequestBody ProductLevel3QuickEditV2Request request) throws Exception {
    log.info("l5 listing update request: {} ", request);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      ApiErrorCode apiErrorCode =
          this.productLevel3V2Service.productQuickEditV2(storeId, productSku, request, isExternalOnly);
      if (Objects.nonNull(apiErrorCode)) {
        return new GdnBaseRestResponse(apiErrorCode.getDesc(), apiErrorCode.getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      log.error("error updating active product info. {} ", e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.GET_L3_LISTING, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch l3 listing at controller", description = "Fetch l3 listing")
  public ListBaseResponse<ProductL3ListingResponse> getL3ListingV2(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody ProductL3ListingRequest request) throws Exception {
    log.info("l3 listing request: {} ", request);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      Page<ProductL3ListingResponse> productL3ListingResponses =
          this.productLevel3V2Service.getProductL3List(storeId, request, page, size);
      return new ListBaseResponse<>(null, null, true, requestId, productL3ListingResponses.getContent(),
          new Metadata(page, size, productL3ListingResponses.getTotalElements()));
    } catch (Exception e) {
      log.error("error getting product info. Request - {}, error - ", request, e);
      return new ListBaseResponse(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId, null, new Metadata(page, size, 0L));
    }
  }


  @RequestMapping(value = ProductLevel3V2ControllerPath.PRODUCT_EDIT_INFO, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update edited product info content at controller", description = "update edited product info content at controller")
  @ResponseBody
  public GdnRestSingleResponse<EditProductV2Response> updateEditedProductInfo(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productSku") String productSku,
      @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal,
      @RequestBody ProductL3UpdateRequest request) throws Exception {

    log.info("Updating l3 product info request {} ", request.toString());
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    EditProductV2Response editResponse = new EditProductV2Response();
    try {
      if (webpConversionEnabled && !request.isNeedCorrection()) {
        CommonUtils.overrideImageExtensionToWebp(request);
        log.info("Overriding image extension to webp for productSku : {} and request {} ", productSku, request);
      }
      ProductEditValidationDTO productEditValidationDTO =
          productLevel3V2Wrapper.editProductDetails(requestId, request, isOnlyExternal,
              combineContentAndLogisticsUpdate, combinePreOrderUpdate);
      editResponse = ResponseHelper.toEditProductV2Response(productEditValidationDTO.getEditProductResponse());
      if (productEditValidationDTO.isL5ValidationFailed() || productEditValidationDTO.isNeedCorrection()) {
        return new GdnRestSingleResponse<>(null, null, true, editResponse,
            requestId);
      }
      if (Objects.nonNull(editResponse.getApiErrorCode())) {
        log.error("Error while updating the edit info for productSku : {} and error code : {}",
            productSku, editResponse);
        return new GdnRestSingleResponse<>(editResponse.getApiErrorCode().getDesc(),
            editResponse.getApiErrorCode().getCode(), false, editResponse, requestId);
      }
    } catch (ApiDataNotFoundException e) {
      log.error("ApiDataNotFoundException Exception caught while updating edit info with request {} ", request, e);
      return new GdnRestSingleResponse<>(e.getErrorMsg(),
          Optional.ofNullable(e.getErrorCode()).orElse(ApiErrorCode.SYSTEM_ERROR).getCode(), Boolean.FALSE, null,
          requestId);
    } catch (Exception ex) {
      log.error("Exception caught while updating edit info with request {} ", request, ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, editResponse, requestId);
    }
    return new GdnRestSingleResponse<>(null, null, true, editResponse, requestId);
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.FETCH_L3_DETAILS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch L3 full details by product sku", description = "Fetch L3 full details "
      + "by product sku")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3DetailsV2Response> fetchProductL3DetailsByProductSku(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "true") boolean concatValueAndValueTypes,
      @RequestParam(defaultValue = "true", required = false) boolean needInventoryData,
      @PathVariable("productSku") String productSku) {
    checkArgument(StringUtils.isNotBlank(productSku),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      ProductLevel3DetailsV2Response response = productLevel3V2Service
          .fetchL3ProductDetailsByProductSku(storeId, productSku, concatValueAndValueTypes, needInventoryData);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching the L3 detail for productSku : {}, requestId : {}", productSku,
          requestId, e);
      return new GdnRestSingleResponse(e.getMessage(), e.getErrorCodes().getCode(), false,
          null, requestId);
    } catch (Exception e) {
      log.error("Error while fetching the L3 detail for productSku : {}, requestId : {}", productSku,
          requestId, e);
      return new GdnRestSingleResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.FETCH_L4_DETAILS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch L4 details by product sku", description = "Fetch L4 details " + "by product sku")
  @ResponseBody
  public GdnRestListResponse<ItemSummaryL4Response> fetchL4DetailsByProductSku(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", required = false) Integer size, @PathVariable("productSku") String productSku) {
    checkArgument(StringUtils.isNotBlank(productSku),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      Page<ItemSummaryL4Response> itemSummaryL4ResponsePage =
          productLevel3V2Service.getItemSummaryL4Response(storeId, productSku, page, size);
      return new GdnRestListResponse<>(null, null, true, itemSummaryL4ResponsePage.getContent(),
          new PageMetaData(itemSummaryL4ResponsePage.getPageable().getPageSize(), page,
              itemSummaryL4ResponsePage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching the L4 detail for productSku : {}, requestId : {}", productSku, requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    } catch (Exception e) {
      log.error("Error while fetching the L4 detail for productSku : {}, requestId : {}", productSku, requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.LISTING_UPDATE, method = RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "listing update for MTA-api", description = "partial update of requested fields")
  public GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> listingPartialUpdate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestBody ProductLevel3QuickEditV2Request request) throws Exception {
    log.info("listingUpdate for productSku : {}, with request : {}", productSku, request);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    if (CollectionUtils.isNotEmpty(request.getQuickEditV2Requests())) {
      for (QuickEditV2Request quickEditV2Request : request.getQuickEditV2Requests()) {
        GdnPreconditions.checkArgument(StringUtils.isNotBlank(quickEditV2Request.getItemSku()),
            ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(
            StringUtils.isNotBlank(quickEditV2Request.getPickupPointCode()),
            ProductLevel3ControllerErrorMessage.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      }
    }
    try {
      ItemPriceStockQuickUpdateResponse response =
          this.productLevel3V2Service.quickEditPatching(storeId, productSku, request);
      if (Objects.isNull(response.getApiErrorCode())) {
        return new GdnRestSingleResponse(null, null, true, response, requestId);
      } else {
        return new GdnRestSingleResponse<>(response.getApiErrorCode().getDesc(),
            response.getApiErrorCode().getCode(), false, null, requestId);
      }
    } catch (Exception e) {
      log.error("error updating active product info. {} ", e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.FETCH_L5_DETAILS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Return L5 details", description = "Return L5 details")
  public GdnRestListResponse<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "true", required = false) boolean needInventoryData,
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "50") int size,
      @RequestBody ItemL5ListingRequest itemL5ListingRequest) {
    log.info("Fetching l5 listing for l3 : {}", itemL5ListingRequest);
    try {
      Page<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponsePage =
          productLevel3V2Service.getItemPickupPointL3Listing(storeId, username, requestId, page, size,
              itemL5ListingRequest, needInventoryData);
      return new GdnRestListResponse<>(null, null, true,
          itemPickupPointListingL3ResponsePage.getContent(),
          new PageMetaData(size, page, itemPickupPointListingL3ResponsePage.getTotalElements()),
          requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching l5 listing for l3 : {}, error - ", itemL5ListingRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
          null, null, requestId);
    } catch (Exception e) {
      log.error("Error while fetching l5 listing for l3 : {}, error - ", itemL5ListingRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.CREATE_DEFAULT_L5_FBB, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Create Default Fbb l5", description = "Create Default Fbb l5")
  public GdnRestSingleResponse<FbbCreatePickupPointResponse> getItemPickupPointL3Listing(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody FbbCreatePickupPointRequest fbbCreatePickupPointRequest) {
    log.info("Creating default Fbb L5 with  FbbCreatePickupPointRequest : {}", fbbCreatePickupPointRequest);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      FbbCreatePickupPointResponse fbbCreatePickupPointResponse =
          productLevel3V2Service.createDefaultFbbPickupPoint(fbbCreatePickupPointRequest);
      return new GdnRestSingleResponse<>(fbbCreatePickupPointResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while creating Default Fbb L5 with itemSku = {},  default pickupPoint = {}",
          fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId(), e);
      return new GdnRestSingleResponse(e.getMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error while creating Default Fbb L5 with itemSku = {},  default pickupPoint = {}",
          fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId(), e);
      return new GdnRestSingleResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @PostMapping(value = ProductLevel3V2ControllerPath.GET_PRODUCT_L5_DETAILS, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Return L5 details", description = "Return L5 details")
  public GdnRestListResponse<ProductAndItemPickupPontL5Response> getProductL5Details(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "true", required = false) boolean needInventoryData,
      @RequestBody List<ItemSkuPpCodeRequest> itemSkusRequest) {
    log.info("Fetching l5 details for request {} ", itemSkusRequest);
    try {
      List<ProductAndItemPickupPontL5Response> itemPickupPointListingL3ResponsePage =
          productLevel3V2Service.getProductDetailsByItemSkuAndPickupPointCode(storeId,
              itemSkusRequest, needInventoryData);
      return new GdnRestListResponse<>(null, null, true, itemPickupPointListingL3ResponsePage,
          new PageMetaData(itemPickupPointListingL3ResponsePage.size(), 0,
              itemPickupPointListingL3ResponsePage.size()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching l5 details for request : {}, error - ", itemSkusRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
          null, null, requestId);
    } catch (Exception e) {
      log.error("Error while fetching l5 details for request : {}, error - ", itemSkusRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.UPDATE_BRAND_OF_PRODUCT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "brand update for a product", description = "brand update for a product")
  public GdnBaseRestResponse updateBrandOfProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode, @RequestBody BrandUpdateRequest brandUpdateRequest) {
    log.info("Updating brand of product having productCode : {} , oldBrandCode = {} , newBrandCode = {} ", productCode,
        brandUpdateRequest.getOldBrandCode(), brandUpdateRequest.getNewBrandCode());
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      productLevel3V2Service.updateBrandDataOfProduct(productCode, brandUpdateRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while updating brand of product having productCode : {}, oldBrandCode = {} , newBrandCode = {} ",
          productCode, brandUpdateRequest.getOldBrandCode(), brandUpdateRequest.getNewBrandCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.PRODUCTS_COUNT, method = RequestMethod.GET)
  @Operation(summary = "Get total products Count", description = "Get total products count")
  GdnRestSingleResponse<ProductCountResponse> getProductCount(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("businessPartnerCode") String businessPartnerCode) throws Exception {
    try {
      return new GdnRestSingleResponse<>(
          new ProductCountResponse(productLevel3V2Service.getProductCount(storeId, businessPartnerCode, false)), requestId);
    } catch (Exception e) {
      log.error("Error while getting product count for businessPartnerCode {} error {} ", businessPartnerCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.BASIC_DETAILS, method = RequestMethod.GET)
  @Operation(summary = "Get product basic detail", description = "Get product basic detail")
  GdnRestSingleResponse<ProductL3BasicResponse> getProductBasicDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) throws Exception {
    try {
      return new GdnRestSingleResponse<>(productLevel3V2Service.getProductL3BasicResponse(storeId, productCode),
          requestId);
    } catch (Exception e) {
      log.error("Error while getting product basic details for productCode {} error  ", productCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.ELIGIBILITY_FOR_NEED_REVISION_DELETION,
                  method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "eligibility of deletion of need revision product", description =
      "eligibility of deletion of need revision product")
  @ResponseBody
  public GdnRestListResponse<NeedRevisionEligibilityResponse> eligibilityForNeedRevisionDeletion(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String username,
      @RequestBody List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequest) {
    try {
      List<NeedRevisionEligibilityResponse> needRevisionEligibilityResponseList =
          productLevel3V2Service.eligibilityForNeedRevisionDeletion(storeId,
              needRevisionEligibilityRequest,true);
      return new GdnRestListResponse<>(null, null, true, needRevisionEligibilityResponseList,
          new PageMetaData(needRevisionEligibilityResponseList.size(), 0,
              needRevisionEligibilityResponseList.size()), requestId);
    } catch (Exception e) {
      log.error("Error while eligibility for request : {}, error - ",
          needRevisionEligibilityRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, null, requestId);
    }
  }

  @PostMapping(value = ProductLevel3V2ControllerPath.PRODUCT_MASTER_DATA_EDIT_INFO, consumes = MediaType.APPLICATION_JSON_VALUE,
    produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update master product info", description = "update master product info by productSku")
  public GdnBaseRestResponse updatedProductMaterData(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username,
    @PathVariable("productSku") String productSku,
    @RequestBody ProductMasterDataEditRequest masterDataEditRequest) throws Exception {
    log.info("Updating Master product for product : {} info request {} ", productSku,
      masterDataEditRequest.toString());
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    ApiErrorCode apiErrorCode;
    try {
      apiErrorCode = productLevel3V2Wrapper.editProductMasterData(storeId, requestId, username,
          masterDataEditRequest);
    } catch (ApplicationRuntimeException ae) {
      log.error("ApplicationRuntimeException caught while updating edit info with request {} ",
        masterDataEditRequest, ae);
      return new GdnRestSingleResponse<>(
        ae.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY),
        ErrorCategory.VALIDATION.getCode(), true, null, requestId);
    } catch (Exception ex) {
      log.error("Exception caught while updating edit info with request {} ", masterDataEditRequest,
        ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
        false, null, requestId);
    }
    return new GdnRestSingleResponse<>(
      Optional.ofNullable(apiErrorCode).map(ApiErrorCode::getDesc).orElse(null),
      Optional.ofNullable(apiErrorCode).map(ApiErrorCode::getCode).orElse(null), true, null,
      requestId);
  }

  private void setMandatoryParameters(String storeId, String channelId, String clientId, String requestId,
      String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.COGS,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update cogs value at L5", description =
      "Update cogs value at L5")
  @ResponseBody
  public GdnBaseRestResponse updateCogsValue(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String username,
      @PathVariable("productSku") String productSku,
      @RequestBody CogsUpdateRequests cogsUpdateRequests) {
    log.info("Updating COGS value for productSku: {} with request: {}", productSku, cogsUpdateRequests);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      productLevel3V2Service.updateCogsValue(productSku, cogsUpdateRequests);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while updating COGS value for productSku: {}, error: ", productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3V2ControllerPath.COGS,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get cogs for productSku", description =
      "Get cogs for productSku")
  @ResponseBody
  public GdnRestListResponse<CogsDataResponse> getCogsData(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size) {
    log.info("Getting COGS data for productSku: {} with page: {}, size: {}", productSku, page, size);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      List<CogsDataResponse> cogsDataResponses = productLevel3V2Service.getCogsData(productSku, page, size);
      return new GdnRestListResponse<>(null, null, true, cogsDataResponses,
          new PageMetaData(size, page, cogsDataResponses.size()), requestId);
    } catch (Exception e) {
      log.error("Error while getting COGS data for productSku: {}, error: ", productSku, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null, requestId);
    }
  }
}