package com.gdn.mta.product.controller;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import com.gda.mta.product.dto.ProductSkuAndPickupPointCodeRequest;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductCodeAndSkuRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.ProductSkuDetailResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.service.ProductL3Service;
import com.gdn.mta.product.web.model.ProductL3ControllerPath;
import com.gdn.partners.pbp.controller.productlevel3.ProductLevel3ControllerErrorMessage;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

import java.util.List;


@RestController
@RequestMapping(value = ProductL3ControllerPath.BASE_PATH)
@Tag(name = "ProductL3Controller", description = "New product Level 3 Service API")
@Slf4j
public class ProductL3Controller {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductL3Controller.class);

  @Autowired
  private ProductL3Service productL3Service;

  @RequestMapping(value = ProductL3ControllerPath.GET_L3_PRODUCT_DETAIL, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get Product L3 details by product sku", description = "Get Product L3 details "
    + "by product sku")
  @ResponseBody
  public GdnRestSingleResponse<ProductL3DetailsResponse> getProductL3DetailsByProductSku(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection,
      @RequestParam(required = false, defaultValue = "true") boolean concatenateValueWithValueType) {
    checkArgument(StringUtils.isNotBlank(productSku),
      ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    try {
      ProductL3DetailsResponse response =
          productL3Service.getL3ProductDetailsByProductSku(storeId, productSku, isNeedCorrection, true,
              concatenateValueWithValueType);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching the L3 detail for productSku : {}, requestId : {}", productSku, requestId, e);
      return new GdnRestSingleResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductL3ControllerPath.GET_L5_PRODUCT_DETAIL, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Return auto approval type", description = "Return auto approval type")
  public GdnRestListResponse<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "50") int size,
      @RequestParam(required = false, defaultValue = "true") boolean onlyDefaultViewConfig,
      @RequestBody ItemPickupPointListingL3Request itemPickupPointListingL3Request,
      @RequestParam(required = false, defaultValue = "true") boolean concatenateValueWithValueType) {
    LOGGER.info("Fetching l5 listing for l3 : {}", itemPickupPointListingL3Request);
    try {
      Page<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponsePage =
          productL3Service.getItemPickupPointL3Listing(storeId, username, requestId, page, size,
              itemPickupPointListingL3Request, onlyDefaultViewConfig, concatenateValueWithValueType, true);
      return new GdnRestListResponse<>(null, null, true, itemPickupPointListingL3ResponsePage.getContent(),
          new PageMetaData(size, page, itemPickupPointListingL3ResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching l5 listing for l3 : {}, error - ", itemPickupPointListingL3Request, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductL3ControllerPath.GET_L5_BY_PRODUCT_SKUS, method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get L5 by summary filter", description = "Get L5 by summary filter")
  public GdnRestListResponse<ProductLevel3SummaryResponse> getL5SummaryByProductSkus(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam(value = "page", defaultValue = "0") int page,
    @RequestParam(value = "size", defaultValue = "50") int size,
    @RequestParam String businessPartnerCode,
    @RequestParam(required = false, defaultValue = "false") boolean onlineOrCnc,
    @RequestBody ProductSkuAndPickupPointCodeRequest productSkuListWithPickupPoints) {
    log.info("Fetching l5 listing for productSkus : {}, page : {}, size : {}",
        productSkuListWithPickupPoints.getValue(), page, size);
    try {
      Page<ProductLevel3SummaryResponse> productLevel3SummaryResponses =
        productL3Service.getItemPickupPointByProductSkus(page, size,
            productSkuListWithPickupPoints.getValue(), businessPartnerCode, onlineOrCnc, productSkuListWithPickupPoints.getPickupPointCodes());
      return new GdnRestListResponse<>(null, null, true, productLevel3SummaryResponses.getContent(),
        new PageMetaData(size, page, productLevel3SummaryResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching l5 listing for productSkus : {}, error - ",
          productSkuListWithPickupPoints.getValue(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        null, null, requestId);
    }
  }

  @RequestMapping(value = ProductL3ControllerPath.DELETE_L5_BY_PICKUP_POINT_CODE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Delete in progress l5 for a pp code", description = "Delete in progress l5 for a pp code")
  public GdnRestListResponse<DeleteInProgressL5Response> deleteL5ByPickupPointCode(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username,
    @RequestBody ItemSkuPickupPointRequest itemSkuPickupPointRequest) {
    log.info("Deleting in-progress L5 for pp code : {} & business partner {} ",
      itemSkuPickupPointRequest.getPickupPointCode(),
      itemSkuPickupPointRequest.getBusinessPartnerCode());
    try {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      List<DeleteInProgressL5Response> deleteInProgressL5ResponseList =
        productL3Service.deleteInProgressL5ForDeletePickupPoint(storeId, itemSkuPickupPointRequest);
      return new GdnRestListResponse<>(null, null, true, deleteInProgressL5ResponseList,
        new PageMetaData(0, 0, deleteInProgressL5ResponseList.size()), requestId);
    } catch (Exception e) {
      log.error("Error while deleting in-progress L5 for pp code : {} & business partner {} ",
        itemSkuPickupPointRequest.getPickupPointCode(),
        itemSkuPickupPointRequest.getBusinessPartnerCode(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        null, null, requestId);
    }
  }

  @PostMapping(value = ProductL3ControllerPath.GET_PRODUCT_SKU_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product sku detail response", description = "Get product sku detail response")
  public GdnRestSingleResponse<ProductSkuDetailResponse> getProductSkuDetailResponse(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username,
    @RequestBody ProductCodeAndSkuRequest request) {
    log.info("Fetching product sku detail for productSku = {} ", request.getProductSku());
    try {
      return new GdnRestSingleResponse<>(
        productL3Service.getProductSkuDetailResponse(storeId, request), requestId);
    } catch (Exception e) {
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        null, requestId);
    }
  }
}
