package com.gdn.x.product.rest.web.controller.api;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.BulkDownloadProductBasicInfoResponse;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.DefaultPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.request.ProductLevel3SummaryRequest;
import com.gdn.x.product.service.api.ProductWrapperService;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductV2ApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.request.DistributionInfoByOmniChannelSkusRequest;
import com.gdn.x.product.rest.web.model.response.DistributionInfoByOmniChannelSkusResponse;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ProductServiceV2;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = ProductV2ApiPath.BASE_PATH)
@Tag(name = "ProductV2", description = "Product V2 Service API")
public class ProductV2Controller {

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductServiceV2 productServiceV2;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ProductWrapperService productWrapperService;

  @RequestMapping(value = ProductV2ApiPath.GET_PRODUCT_AND_ITEMS, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items",
      description = "get a product with all the items from database")
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam boolean showDeleted,
      @RequestParam String productSku, @RequestParam String pickupPointCode, @RequestParam(defaultValue = "false") boolean combineOthersBundlings,
      @RequestParam(required = false) boolean off2On,
      @RequestParam(defaultValue = "false") boolean includeForceReview, @RequestParam(required = false, defaultValue = "true") boolean needProductData,
      @RequestParam(required = false) String fetchViewConfigByChannel) {
    log.info("Get product and items with productSku = {}", productSku);
    try {
      ProductItemsVo productItemsVo = this.productService
          .getProductAndItemDetails(storeId, requestId, username, productSku, pickupPointCode, showDeleted,
              combineOthersBundlings, off2On, needProductData, includeForceReview, fetchViewConfigByChannel);
      ProductAndItemsResponse productAndItemsResponse =
          this.modelConverter.convertToProductAndItemsResponse(productItemsVo, fetchViewConfigByChannel, true);
      return new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, requestId);
    } catch (ApiIncorrectInputDataException e) {
      log.error("error in getProductAndItems api with productSku : {} , pickupPointCode : {} , {} ",  productSku,
          pickupPointCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCode(), false, null, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("error in getProductAndItems api with productSku : {} , pickupPointCode : {} , {} ",  productSku,
          pickupPointCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("error in getProductAndItems api with productSku : {} , pickupPointCode : {} , {} ",  productSku,
          pickupPointCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ApiErrorCodes.SYSTEM_ERROR.getErrorCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and single item by itemSku and pickupPointCode", description = "get"
    + " a product with single matched itemSku and pickupPointCode from database")
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSkuAndPickupPointCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam String pickupPointCode,
      @RequestParam(required = false, defaultValue = "false") boolean includeMarkForDelete,
      @RequestParam(required = false) String fetchViewConfigByChannel) {
    log.info(
        "#getProductAndSingleItemByItemSkuAndPickupPointCode with itemSku = {}, pickupPointCode = {}, includeMarkForDelete = {} ",
        itemSku, pickupPointCode, includeMarkForDelete);
    try {
      ProductItemsVo productItemsVo =
          this.productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(storeId, username, requestId,
              itemSku, pickupPointCode, includeMarkForDelete);
      return new GdnRestSingleResponse<>(null, null, true,
          this.modelConverter.convertToProductAndItemsResponseWithConvertPreOrderDetails(
              productItemsVo, true, fetchViewConfigByChannel, true),
          requestId);
    } catch (ApiIncorrectInputDataException e) {
      log.error(
          "##getProductAndSingleItemByItemSkuAndPickupPointCode with itemSku = {}, pickupPointCode = {}, includeMarkForDelete = {} ",
          itemSku, pickupPointCode, includeMarkForDelete, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCode(), false, null,
          requestId);
    } catch (Exception e) {
      log.error(
          "##getProductAndSingleItemByItemSkuAndPickupPointCode with itemSku = {}, pickupPointCode = {}, includeMarkForDelete = {} ",
          itemSku, pickupPointCode, includeMarkForDelete, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and single item by itemSku",
    description = "get a product with single matched itemSku from database")
  public GdnRestSingleResponse<ProductAndItemDataResponse> getProductAndSingleItemByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam(required = false, defaultValue = "false") boolean includeMarkForDelete) {
    log.info("#getProductAndSingleItemByItemSku with itemSku = {}, includeMarkForDelete = {} ", itemSku,
        includeMarkForDelete);
    try {
      ProductItemsVo productItemsVo =
          this.productServiceV2.getProductAndSingleItemByItemSku(storeId, username, requestId, itemSku,
              includeMarkForDelete);
      return new GdnRestSingleResponse<>(null, null, true,
          this.modelConverter.convertToProductAndItemDataResponse(productItemsVo), requestId);
    } catch (Exception e) {
      log.error("##getProductAndSingleItemByItemSkuAndPickupPointCode with itemSku = {}, includeMarkForDelete = {} ",
          itemSku, includeMarkForDelete, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false,
          null, requestId);
    }
  }


  @RequestMapping(value = {ProductV2ApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS},
      method = {RequestMethod.POST}, consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product Info by itemSku")
  public GdnRestListResponse<ProductAndItemInfoResponseV2> getProductInfoByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody GetProductInfoRequestV2 request) throws Exception {
    try {
      List<ProductItemsVo> productAndItemsVOs =
          productServiceV2.getProductAndItemsByItemSkus(storeId, username, requestId, request.getItemSkus(),
              request.isPristine(), request.isOff2On());
      List<ProductAndItemInfoResponseV2> productInfoResponses =
          productAndItemsVOs.stream().map(e -> modelConverter.convertToProductAndItemInfoResponseV2(e)).collect(toList());
      return new GdnRestListResponse<>(productInfoResponses,
          new PageMetaData(productInfoResponses.size(), 0, productInfoResponses.size()), requestId);
    } catch (Exception e) {
      log.error("#getProductInfoByItemSku error with request {}", request, e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.UPDATE_CNC_ACTIVATED_FLAG, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update cnc activated flag at L3 and L4",
      description = "update cnc activated flag at L3 and L4")
  public GdnBaseRestResponse updateCncActivationFlag(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody SimpleListStringRequest simpleListStringRequest) {
    try {
      log.info("update cnc activated flag at L3 and L4 for item skus : {}", simpleListStringRequest.getValue());
      this.productServiceV2.updateCncFlagAtProductAndItemLevel(storeId, username, simpleListStringRequest.getValue());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("update cnc activated flag at L3 and L4 for item skus : {} ", simpleListStringRequest.getValue(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.VALIDATE_DUPLICATE_PRODUCT_BY_SELLER_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "validate duplicate product by seller sku",
    description = "validate duplicate product by seller sku")
  public GdnRestSingleResponse<DuplicateProductDetailsResponse> validateDuplicateProductBySellerSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String sellerSku,
      @PathVariable("merchantCode") String merchantCode) {
    log.info("validate duplicate product by seller sku = {} , merchantCode = {} ", sellerSku, merchantCode);
    try {
      DuplicateProductDetailsResponse duplicateProductDetailsResponse = productServiceV2.validateDuplicateProductBySellerSku(storeId, merchantCode, sellerSku);
      if (Objects.nonNull(duplicateProductDetailsResponse)) {
        return new GdnRestSingleResponse<>(ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU, null, false,
            duplicateProductDetailsResponse, requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, null, requestId);
      }
    } catch (Exception e) {
      log.error("error while validate duplicate product by seller sku = {} , includeMarkForDelete = {} ", sellerSku,
          merchantCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.GET_PRODUCT_L3_DETAIL_BY_PRODUCT_SKU_OR_PRODUCT_CODE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get prd_product detail by productSku or productCode",
    description = "get prd_product detail by productSku or productCode")
  public GdnRestListResponse<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductSkuAndProductCodeRequest request) {
    log.info("get prd_product detail by productSku or productCode. requestId : {} , prdProductRequest : {}", requestId,
        request);
    try {
      List<PrdProductResponse> prdProductResponseList =
          this.productServiceV2.getPrdProductDetailByProductSkuOrProductCode(storeId, request);
      return new GdnRestListResponse<>(null, null, Boolean.TRUE, prdProductResponseList, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting prd_product detail by productSku or productCode. prdProductRequest : {} ", request,
          e);
      return new GdnRestListResponse<>(e.getMessage(), null, Boolean.FALSE, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.GET_PRODUCT_AND_ITEMS_FOR_VIEW, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items",
    description = "Get Product And Items details Summary Response for PDP")
  public GdnRestSingleResponse<ProductAndItemsSummaryResponseV2> getProductAndItemsForView(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam boolean showDeleted, @RequestParam String productSku,
    @RequestParam String pickupPointCode,
    @RequestParam(defaultValue = "false") boolean combineOthersBundlings,
    @RequestParam(required = false) boolean off2On,
    @RequestParam(defaultValue = "false") boolean includeForceReview,
    @RequestParam(required = false, defaultValue = "true") boolean needProductData) {
    log.info("Invoking Get product and items for View with productSku = {} ", productSku);
    try {
      ProductAndItemsSummaryResponseV2 productAndItemsResponse =
        this.productServiceV2.getProductAndItemsForView(storeId, requestId, username, productSku,
          pickupPointCode, showDeleted, combineOthersBundlings, off2On, needProductData,
          includeForceReview);
      if(StringUtils.isEmpty(productAndItemsResponse.getStoreId())){
        productAndItemsResponse.setStoreId(storeId);
      }
      return new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, requestId);
    } catch (Exception e) {
      log.error("#getProductAndItemsForView with productSku {} , {}", productSku, e.getMessage(),
        e);
      return new GdnRestSingleResponse<>(e.getMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @PostMapping(value = ProductApiPath.GET_DEFAULT_PICKUP_POINT_CODE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get default pickup point code", description = "get default pickup point code")
  public GdnRestListResponse<ProductSkuPickupPointResponse> getDefaultPickupPointCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody DefaultPickupPointRequest request) {
    try {
      log.info("getting default pickup point codes for product-skus  : {} and republish : {} ",
          request.getSkus(), request.isRepublish());
      List<ProductSkuPickupPointResponse> productSkuPickupPointResponses = itemPickupPointService.
          getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(storeId, request.getSkus(), request.isRepublish());
      return new GdnRestListResponse<>(null, null, Boolean.TRUE,
          productSkuPickupPointResponses, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting default pickup point codes for product-skus: {} ", request.getSkus(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @GetMapping(value = ProductV2ApiPath.GET_BASIC_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
    produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get basic product and item by item sku and pickup point code", description =
    "get basic product and item by item sku and pickup point code")
  public GdnRestSingleResponse<BasicProductAndItemResponse> getBasicProductAndItemDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String itemSku, @RequestParam String pickupPointCode,
      @RequestParam(defaultValue = "false") boolean specificationNeeded, @RequestParam(defaultValue = "false") boolean descriptionNeeded,
      @RequestParam(required = false) String fetchViewConfigByChannel) {
    try {
      log.info("get basic product and item by request-id : {} item-sku : {} and pickup-point-code  : {} ", requestId, itemSku, pickupPointCode);
      BasicProductAndItemDTO basicProductAndItemDTO = productServiceV2
          .getBasicProductAndItemDetails(storeId, username, requestId, itemSku, pickupPointCode, specificationNeeded, descriptionNeeded);
      BasicProductAndItemResponse basicProductAndItemResponse = modelConverter.toBasicProductAndItemResponse(basicProductAndItemDTO, fetchViewConfigByChannel);
      return new GdnRestSingleResponse<>(null, null, true, basicProductAndItemResponse, requestId);
    } catch (Exception e) {
      log.error("Error while getting default pickup point codes for request-id : {} item-sku : {}, pickup-point-code : {}  ", requestId, itemSku, pickupPointCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @PostMapping(value = ProductV2ApiPath.GET_MIN_AND_MAX_PRICE,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get the minimum and maximum price for the given list of skus")
  public GdnRestListResponse<PriceRangeResponse> getPriceRangeForWebSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody List<String> listOfSkus) {
    try {
      log.info("Get Price Range For WebSku with storeId : {}, skuList: {}", storeId, listOfSkus);
      List<PriceRangeResponse> priceRangeForSkus = productServiceV2.getPriceRangeForSkus(storeId, merchantCode, listOfSkus);
      return new GdnRestListResponse<>(priceRangeForSkus,
          new PageMetaData(priceRangeForSkus.size(), 0, priceRangeForSkus.size()), requestId);
    } catch (Exception e) {
      log.error("Error while getting PriceRange for request-id: {} skuList: {} ", requestId, listOfSkus, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.GET_PRODUCT_DETAILSFOR_HALAL_PRODUCTS_BY_PRODUCT_SKU_LIST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get prd_product detail by productSku or productCode", description = "get prd_product detail by productSku or productCode")
  public GdnRestListResponse<HalalProductResponse> getProductDetailsByProductSkuList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<String> productSkuList) {
    try {
      log.info("getting halal products using product sku list : {} ", productSkuList);
      List<HalalProductResponse> halalProductResponseList =
          this.productServiceV2.getHalalProductResponseByProductSkus(storeId, productSkuList);
      return new GdnRestListResponse<>(null, null, Boolean.TRUE, halalProductResponseList, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting product details by productSkuList: {} ", productSkuList, e);
      return new GdnRestListResponse<>(e.getMessage(), null, Boolean.FALSE, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.UPDATE_PRODUCT_HALAL_CONFIG, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update halal config of product", description = "update halal config of product")
  public GdnBaseRestResponse updateHalalConfigOfProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productSku") String productSku, @RequestParam(required = true) String curationStatus) {
    try {
      log.info("update Halal config of productSku = {} and curationStatus = {} ", productSku, curationStatus);
      this.productServiceV2.updateHalalConfigOfProduct(storeId, productSku, curationStatus, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("update halal config failed for productSku = {} , curationStatus = {} ", productSku, curationStatus, e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.BASIC_PRODUCT_INFO, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "basic product info", description = "basic product info")
  public GdnRestSingleResponse<BasicProductResponse> getBasicProductInfo(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productSku") String productSku, @RequestParam(required = false, defaultValue = "false") boolean sharedProductInfoNeeded) {
    try {
      log.info("Fetching basic product details for productSku : {} ", productSku);
      BasicProductResponse basicProductResponse = productServiceV2.getBasicProductDetails(storeId, productSku, sharedProductInfoNeeded);
      return new GdnRestSingleResponse(null, null, true, basicProductResponse, requestId);
    } catch (ApiIncorrectInputDataException e) {
      log.error("Product not found for productSku : {} ",  productSku, e);
      return new GdnRestSingleResponse(e.errorMessage, e.errorCode, false, null, requestId);
    } catch (Exception e) {
      log.error("Failed to fetch basic product info for productSku : {} ", productSku, e);
      return new GdnRestSingleResponse(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductV2ApiPath.UPDATE_PRODUCT_AND_ITEM_PICKUP_POINT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product content and item pickup points", description = "update product combine request")
  public GdnRestSingleResponse<CombinedEditItemResponse> updateEditedProductAndItemPickupPoint(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "false") boolean updateCategory,
      @PathVariable("productSku") String productSku,
      @RequestBody ProductDetailPageEditRequest productDetailPageEditRequest) {
    log.info("Combined update edited product for productSku : {} with request = {}", productSku,
        productDetailPageEditRequest);
    try {
      MandatoryRequestParam mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username,
              username);
      CombinedEditItemResponse combinedEditItemResponse =
          this.productWrapperService.updateEditedProductCombined(requestId, updateCategory,
              productDetailPageEditRequest, productSku, mandatoryRequestParam);
      if (Objects.nonNull(combinedEditItemResponse.getApiErrorCode())) {
        return new GdnRestSingleResponse<>(combinedEditItemResponse.getApiErrorCode().getDesc(),
            combinedEditItemResponse.getApiErrorCode().getCode(), false, combinedEditItemResponse, requestId);
      }
      return new GdnRestSingleResponse<>(null, null, true, combinedEditItemResponse, requestId);
    } catch (ApiIncorrectInputDataException e) {
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCode(), false, new CombinedEditItemResponse(),
          requestId);
    } catch (Exception e) {
      log.error("Error occurred while performing combined update for edited product for productSku : {} , error is : ",
          productSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, new CombinedEditItemResponse(), requestId);
    }
  }

  @PostMapping(value = ProductV2ApiPath.GET_PRODUCT_BASIC_DETAILS_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product basic details", description = "get product basic details")
  public GdnRestListResponse<ProductBasicResponse> getProductBasicDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody SimpleListStringRequest request,
      @RequestParam(required = false, defaultValue = "false") boolean needSalesCategorydata) {
    try {
      log.info("getting product basic details for product-skus  : {} ", request);
      List<ProductBasicResponse> productSkuPickupPointResponses =
          productService.findProductBasicDetailsByProductSku(storeId, request, needSalesCategorydata);
      return new GdnRestListResponse<>(null, null, Boolean.TRUE, productSkuPickupPointResponses, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting product basic details for product-skus: {} ", request.getValue(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @PostMapping(value = ProductV2ApiPath.GET_PRODUCT_BASIC_DETAILS_BY_ITEM_SKU, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product basic details by itemSkus", description = "get product basic details by itemSkus")
  public GdnRestListResponse<ProductBasicResponse> getProductBasicDetailsByItemSkus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody SimpleListStringRequest request) {
    try {
      log.info("getting product basic details for product-skus  : {} ", request);
      List<ProductBasicResponse> productSkuPickupPointResponses =
          productService.findProductBasicDetailsByItemSkus(storeId, request);
      return new GdnRestListResponse<>(null, null, Boolean.TRUE, productSkuPickupPointResponses, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting product basic details for item-skus: {} ", request.getValue(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }


  @GetMapping(value = ProductV2ApiPath.GET_PRODUCT_SKU_LIST_BY_SIZE_CHART_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product sku list by size chart code", description = "product sku list by size chart code")
  public GdnRestListResponse<ProductSkuSizeChartResponse> findProductSkuListBySizeChartCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "50") int size, @RequestParam String sizeChartCode) {
    log.info("getProductSkuListBySizeChartCode : {} ", sizeChartCode);
    try {
      Page<ProductSkuSizeChartResponse> productSkuSizeChartResponses =
          productService.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
      return new GdnRestListResponse<>(productSkuSizeChartResponses.getContent(),
          new PageMetaData(size, page, productSkuSizeChartResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error finding products with size chart code : {}, error ", sizeChartCode, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @PostMapping(value = ProductV2ApiPath.MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product basic details by itemSkus", description = "get product basic details by itemSkus")
  public GdnBaseRestResponse migrateProductAndL5DetailByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductAndL5MigrationRequest request) {
    try {
      log.info("Migrate product and L5 detail for request : {} ", request);
      productService.migrateProductAndL5DetailByProductSku(storeId, request);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while migrating product and L5 detail for request : {} ", request, e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @PostMapping(value = ProductV2ApiPath.UPDATE_PRODUCT_MASTER_FIELDS_INFO, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product master fields info by product code", description = "get product master fields info"
      + "by product code and generate product score")
  public GdnBaseRestResponse updateProductMasterFieldsInfo(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductBasicMasterFieldsRequest request) {
    String errorMessage = StringUtils.EMPTY;
    boolean success = false;
    try {
      log.info("Update master data request for product: {}, request: {} ", request.getProductSku(), request);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getProductSku()),
          ProductErrorCodesEnum.PRODUCT_SKUS_MUST_NOT_BE_BLANK.getCode());
      productService.updateMasterDataInfo(storeId, requestId, username, request);
      success = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error invoking updateProductMasterFieldsInfo: {} ", request.getProductSku(), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, success, null, requestId);
  }

  @PostMapping(value = ProductV2ApiPath.GET_PRODUCT_BASIC_INFO_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product basic info by productSkus", description = "get product basic info by productSkus")
  public GdnRestSingleResponse<BulkDownloadProductBasicInfoResponse> getProductBasicInfoByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductLevel3SummaryRequest request) {
    String errorMessage = StringUtils.EMPTY;
    boolean success = false;
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse = new BulkDownloadProductBasicInfoResponse();
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getProductSkuList()),
          ProductErrorCodesEnum.PRODUCT_SKUS_MUST_NOT_BE_BLANK.getCode());
      log.info("#getProductBasicInfoByProductSku with productSkus = {}, ", request.getProductSkuList());
      bulkDownloadProductBasicInfoResponse = productService.getProductBasicInfoByProductSkus(storeId, request);
      success = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error invoking getProductBasicInfoByProductSku: {} ", request.getProductSkuList(), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, success, bulkDownloadProductBasicInfoResponse, requestId);
  }

  @PostMapping(value = ProductV2ApiPath.UPDATE_COGS, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update cogs value", description = "Update cogs value"
      + "by product code and generate product score")
  public GdnBaseRestResponse updateCogsValue(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CogsUpdateListRequest request) {
    String errorMessage = StringUtils.EMPTY;
    boolean success = false;
    try {
      log.info("Update insured amount request: {} ", request);
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getListRequest()),
          ProductErrorCodesEnum.REQUESTS_MUST_NOT_BE_EMPTY.getCode());
      itemPickupPointService.updateInsuredAmountInItemPickupPoint(storeId, request.getListRequest());
      success = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error invoking updateProductMasterFieldsInfo: {} ", request, e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, success, null, requestId);
  }

  @GetMapping(value = ProductV2ApiPath.GET_COGS, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get cogs data", description = "Get cogs data for a product with pagination")
  public GdnRestListResponse<CogsResponse> getCogsData(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable String productSku, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    String errorMessage = StringUtils.EMPTY;
    boolean success = false;
    Page<CogsResponse> response = null;
    try {
      log.info("Get cogs data request for productSku: {}, page: {}, size: {}", productSku, page, size);
      response = itemPickupPointService.getCogsData(storeId, productSku, page, size);
      success = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error invoking getCogsData: {} ", productSku, e);
    }
    return new GdnRestListResponse<>(errorMessage, null, success,
        Optional.ofNullable(response).orElse(new PageImpl<>(new ArrayList<>())).getContent(),
        new PageMetaData(size, page,
            Optional.ofNullable(response).orElse(new PageImpl<>(new ArrayList<>())).getTotalElements()), requestId);
  }

  @PostMapping(value = ProductV2ApiPath.GET_DISTRIBUTION_INFO_BY_OMNICHANNEL_SKUS,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get distribution info by omnichannel skus", description = "get distribution info by omnichannel skus")
  public GdnRestSingleResponse<DistributionInfoByOmniChannelSkusResponse> getDistributionInfoByOmniChannelSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody DistributionInfoByOmniChannelSkusRequest request) {
    try {
      log.info("getDistributionInfoByOmniChannelSkus requestId: {}, sellerCode: {}, size: {}", requestId,
          request.getSellerCode(), Optional.ofNullable(request.getOmnichannelSkuCodes()).map(List::size).orElse(0));
      DistributionInfoByOmniChannelSkusResponse response =
          productServiceV2.checkOmniChannelSkusInSeller(storeId, requestId, username, request);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (Exception e) {
      log.error("Error in getDistributionInfoByOmniChannelSkus for requestId: {}", requestId, e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), false, null, requestId);
    }
  }
}
