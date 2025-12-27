package com.gdn.x.product.rest.web.controller.api;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.product.exception.SolrCustomException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.ProductIdentifierWrapper;
import com.gdn.x.product.rest.web.model.ProductWrapper;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ProductScoreRetryDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductsAndItemsResponse;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailRequest;
import com.gdn.x.product.rest.web.model.request.SimpleProductListRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimplePristineProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductListResponse;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ProductApiPath.PRODUCT_LIST)
@Tag(name = "Product List Controller", description = "Product Search with bulk parameter API")
public class ProductListController {
  private static final Logger LOG = LoggerFactory.getLogger(ProductListController.class);

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductRetryEventPublishService productRetryEventPublishService;

  private static final String SUSPENDED = "Suspended";


  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_ALL, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all product and items sorted by productCode",
      description = "get a product with all the items from database and master data")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getAllProductAndItemsSortByProductCodeAsc(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int page, @RequestParam int size) {
    ProductListController.LOG.info("Get product and items sort by productCode asc");
    try {
      boolean needCategoryHierarchy = true;
      MasterDataDetailWithProductAndItemsResponseVo result =
          this.productSearchService.getAllProductAndItemsSortByProductCodeAsc(storeId, username,
              requestId, needCategoryHierarchy, PageRequest.of(page, size));
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(null, null,
          true, this.modelConverter.convertToMasterDataDetailResponse(result), requestId);
    }
    catch (SolrCustomException e){
      ProductListController.LOG.error(
        "#getAllProductAndItemsSortByProductCodeAsc, errorMessage {}", e.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
    catch (Exception e) {
      ProductListController.LOG.error(
          "#getAllProductAndItemsSortByProductCodeAsc, errorMessage {}", e.getMessage(), e);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_ALL_SKU, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all product and items sorted by productCode",
      description = "get a product with all the items from database and master data")
  public GdnRestSingleResponse<SimpleProductListResponse> getAllProductSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    ProductListController.LOG.info("getAllProductSku starts");
    try {
      List<Product> product = this.productService.getAllSimpleProduct();
      this.modelConverter.convertToSimpleProductResponses(product);
      return new GdnRestSingleResponse<SimpleProductListResponse>(
          this.modelConverter.convertToSimpleProductResponses(product), requestId);
    } catch (Exception e) {
      ProductListController.LOG.error("#getAllProductSku, errorMessage {}", e.getMessage(), e);
      return new GdnRestSingleResponse<SimpleProductListResponse>(e.getMessage(), e.getMessage(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by set of product catentry id",
      description = "get a product with all the items from database and master data")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByProductCatentryIds(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest productCatentryIdsRequest) {
    ProductListController.LOG.info("Get product and items with productCatentryIds = {}",
        new Object[] {productCatentryIdsRequest});
    try {
      boolean needCategoryHierarchy = true;
      MasterDataDetailWithProductAndItemsResponseVo result =
          this.productSearchService.getProductAndItemsByProductCatentryIds(storeId, username,
              requestId, productCatentryIdsRequest.getValue(), needCategoryHierarchy);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(null, null,
          true, this.modelConverter.convertToMasterDataDetailResponse(result), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductListController.LOG.error(
          "#getListOfProductByProductCatentryIds with productCatentryIds {} , {}",
          productCatentryIdsRequest, e.getMessage());
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductListController.LOG.error(
          "#getListOfProductByProductCatentryIds with productCatentryIds {} , {}",
          productCatentryIdsRequest, e.getMessage(), e);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CODES,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by set of product code",
      description = "get a product with all the items from database and master data")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest productCodesRequest) {
    ProductListController.LOG.info("Get product and items with productCodes = {}",
        new Object[] {productCodesRequest});
    try {
      boolean needCategoryHierarchy = true;
      MasterDataDetailWithProductAndItemsResponseVo result =
          this.productSearchService.getProductAndItemsByProductCodes(storeId, username, requestId,
              productCodesRequest.getValue(), needCategoryHierarchy);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(null, null,
          true, this.modelConverter.convertToMasterDataDetailResponse(result), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductListController.LOG.error(
          "#getProductAndItemsByProductCode with productCodesRequest {} , {}", productCodesRequest,
          e.getMessage());
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductListController.LOG.error(
          "#getProductAndItemsByProductCode with productCodesRequest {} , {}", productCodesRequest,
          e.getMessage(), e);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_BY_PRODUCT_SKUS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by set of product sku",
      description = "get a product with all the items from database and master data")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByProductSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest productSkusRequest) {
    ProductListController.LOG.info("Get product and items with productSkusRequest = {}",
        new Object[] {productSkusRequest});
    try {
      boolean needCategoryHierarchy = true;
      MasterDataWithProductItemsVo result =
          this.productSearchService.getProductAndItemsByProductSkus(storeId, username, requestId,
              productSkusRequest.getValue(), needCategoryHierarchy);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(null, null,
          true, this.modelConverter.convertToMasterDataWithProductItemsVo(result), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductListController.LOG.error("#getListOfProductByProductSkus with productCode {} , {}",
          productSkusRequest, e.getMessage());
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductListController.LOG.error("#getListOfProductByProductSkus with productCode {} , {}",
          productSkusRequest, e.getMessage(), e);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_CATALOG_BY_MERCHANT_CODE,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product for oxford product catalog",
      description = "get a product catalog detail for oxford")
  public GdnRestListResponse<ProductDetailResponse> getProductsForOfficialStore(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size,
      @RequestBody ProductDetailRequest productDetailRequest) {

    try {
      Page<ProductDetailVo> productDetailVos = this.productSearchService.getProductsForOfficialStore(storeId, clientId,
          modelConverter.toOfficialStoreRequestVO(productDetailRequest), PageRequest.of(page, size));
      return new GdnRestListResponse<>(null, null, true,
          modelConverter.convertToProductDetailResponse(productDetailVos.getContent()),
          new PageMetaData(size, page, productDetailVos.getTotalElements()), requestId);
    } catch (SolrCustomException e) {
      LOG.error(
        "Error while getting product detail official store with merchantCode {} and brands {} with Error: {}",
        productDetailRequest.getMerchantCodes(), productDetailRequest.getBrands(), e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, null, null, requestId);
    }
    catch (ApplicationRuntimeException e) {
      LOG.error(
        "Error while getting product detail official store with merchantCode {} and brands {} with Error: {}",
        productDetailRequest.getMerchantCodes(), productDetailRequest.getBrands(), e);
      return new GdnRestListResponse<>(e.getErrorMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, null, null, requestId);
    }
    catch (Exception e) {
      LOG.error(
          "Error while getting product detail official store with merchantCode {} and brands {} with Error: {}",
          productDetailRequest.getMerchantCodes(), productDetailRequest.getBrands(), e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_PRODUCT_BY_SIMPLE_REQUEST,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by set of product sku",
      description = "get a product with all the items from database and master data")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductBySimpleProductRequests(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleProductListRequest simpleProductRequestList) {
    ProductListController.LOG.info("Get product and items with productSkusRequest = {}",
        new Object[] {simpleProductRequestList});
    try {
      MasterDataDetailWithProductAndItemsResponseVo result =
          this.productSearchService.getListOfProductsBySimpleProductRequests(storeId, username,
              requestId,
              this.modelConverter.convertToSimpleProductRequestVo(simpleProductRequestList));
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(null, null,
          true, this.modelConverter.convertToMasterDataDetailResponse(result), requestId);
    } catch (Exception e) {
      ProductListController.LOG.error(
          "#getListOfProductBySimpleProductRequests with request {} , {}",
          simpleProductRequestList, e.getMessage(), e);
      return new GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_BY_PRODUCT_WRAPPER, method = RequestMethod
      .POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by productCode and PristineId", description = "get a "
      + "product with all the items from database and master data only return the sync one")
  public GdnRestListResponse<SimpleProductsAndItemsResponse>
  getProductAndItemsByProductWrapper(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductWrapper productWrapper) {
    ProductListController.LOG
        .info("Get product and items with productWrapper :{}", productWrapper);
    List<SimpleProductsAndItemsResponse> response = null;
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    boolean status = false;
    try {
      if (CollectionUtils.isNotEmpty(productWrapper.getPristineIds())) {
        LOG.debug("getting ProductAndItems data by pristineIds : {}",
            productWrapper.getPristineIds());
        result = this.productSearchService
            .getProductAndItemsByPristineIds(storeId, username, requestId,
                productWrapper.getPristineIds());
      } else {
        LOG.debug("getting ProductAndItems data by productcode : {}, productSkus :{}",
            productWrapper.getProductCodes(), productWrapper.getProductSkus());
        result = this.productSearchService
            .getProductAndItemsByProductCodesAndProductSkus(storeId, username, requestId,
                productWrapper.getProductCodes(), productWrapper.getProductSkus());
      }
      response = this.modelConverter.convertToSimpleProductsAndItemsDTO(result);
      status = true;
    } catch (ApplicationRuntimeException e) {
      errorCode = ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode();
      errorMessage = e.getMessage();
      LOG.error("Error while getting ProductAndItems data by ProductWrapper {}", productWrapper);
      LOG.error("Error : ", e);
    } catch (Exception e) {
      errorCode = ProductErrorCodesEnum.INTERNAL_SERVER.getCode();
      errorMessage = ProductErrorCodesEnum.INTERNAL_SERVER.getMessage();
      LOG.error("Error while getting ProductAndItems data by ProductWrapper {}", productWrapper);
      LOG.error("Error : ", e);
    }
    return new GdnRestListResponse<>(errorMessage, errorCode, status, response, null, requestId);
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_LIST_BY_PRISTINE_IDS, method = RequestMethod
      .POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "get productCodes and skus by PristineIds",
    description = "get productCodes and skus by PristineIds")
  @ResponseBody
  public GdnRestListResponse<SimplePristineProductResponse> getAllProductCodesAndSkusByPristineIds(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody Set<String> pristineIds) {
    ProductListController.LOG.info("Get productcodes and skus by pristineIds :{}", pristineIds);
    List<SimplePristineProductResponse> response = null;
    List<SimplePristineProductRequestVo> result = null;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    boolean status = false;
    try {
      if (CollectionUtils.isNotEmpty(pristineIds)) {
        result = this.productSearchService
            .getProductCodesAndSkusByPristineIds(storeId, username, requestId, pristineIds);
      }
      response = this.modelConverter.convertToSimplePristineProductResponse(result);
      status = true;
    } catch (ApplicationRuntimeException e) {
      errorCode = ProductErrorCodesEnum.GET_PRODUCT_CODES_AND_PRODUCT_SKUS.getCode();
      errorMessage = e.getMessage();
      LOG.error("Error while getting productcodes and skus by pristineIds :{}", pristineIds);
      LOG.error("Error : ", e);
    } catch (Exception e) {
      errorCode = ProductErrorCodesEnum.INTERNAL_SERVER.getCode();
      errorMessage = ProductErrorCodesEnum.INTERNAL_SERVER.getMessage();
      LOG.error("Error while getting productcodes and skus by pristineIds :{}", pristineIds);
      LOG.error("Error : ", e);
    }
    return new GdnRestListResponse<>(errorMessage, errorCode, status, response, null, requestId);
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS,
                  method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
                  consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get productMasterDataDetail by productCodes and productSkus ",
                description = "Rollback Api: Get productMasterDataDetail by productCodes and productSkus. It contains limited fields.")
  public GdnRestListResponse<SimpleProductMasterDataDetailResponse>
  getProductMasterDataDetailByProductCodesAndSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductIdentifierWrapper productIdentifierWrapper) {
    ProductListController.LOG
        .info("Get productMasterDataDetail by productCodes :{} and productSkus :{}",
            productIdentifierWrapper.getProductCodes(), productIdentifierWrapper.getProductSkus());
    List<SimpleProductMasterDataDetailResponse> response = null;
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    boolean status = false;
    try {
        result = this.productSearchService
            .getProductMasterDataDetailByProductCodesAndSkus(storeId, username, requestId,
                productIdentifierWrapper.getProductCodes(), productIdentifierWrapper.getProductSkus());

      response = this.modelConverter.convertToSimpleProductMasterDataDetailResponse(result);
      status = true;
    } catch (ApplicationRuntimeException e) {
      errorCode = ProductErrorCodesEnum.GET_PRODUCT_MASTER_DATA_DETAIL.getCode();
      errorMessage = e.getMessage();
      LOG.error(
          "Error while getting productMasterDataDetail by productCodes :{} and productSkus :{}",
          productIdentifierWrapper.getProductCodes(), productIdentifierWrapper.getProductSkus());
      LOG.error("Error : ", e);
    } catch (Exception e) {
      errorCode = ProductErrorCodesEnum.INTERNAL_SERVER.getCode();
      errorMessage = ProductErrorCodesEnum.INTERNAL_SERVER.getMessage();
      LOG.error(
          "Error while getting productMasterDataDetail by productCodes :{} and productSkus :{}",
          productIdentifierWrapper.getProductCodes(), productIdentifierWrapper.getProductSkus());
      LOG.error("Error : ", e);
    }
    return new GdnRestListResponse<>(errorMessage, errorCode, status, response, null, requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODES,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get products  for oxford product catalog",
      description = "get active products for merchant and category codes")
  public GdnRestListResponse<ActiveProductResponse> getProductsForMerchantAndCategoryCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size,
      @RequestBody ActiveProductRequest activeProductRequest) {

    try {
      Page<ActiveProductDetailVo> activeProductDetailVos =
          this.productSearchService.getActiveProductsListForMerchant(storeId,
              modelConverter.toActiveProductsRequestVO(activeProductRequest),
              PageRequest.of(page, size));
      return new GdnRestListResponse<>(null, null, true,
          modelConverter.convertToActiveProductResponse(activeProductDetailVos.getContent()),
          new PageMetaData(size, page, activeProductDetailVos.getTotalElements()), requestId);
    } catch (SolrCustomException e) {
      LOG.error(
        "Error while getting product detail official store with merchantCode {} and category codes {} with Error: {}",
        activeProductRequest.getMerchantCode(), activeProductRequest.getCategoryCodes(), e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, null, requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error(
        "Error while getting product detail official store with merchantCode {} and category codes {} with Error: {}",
        activeProductRequest.getMerchantCode(), activeProductRequest.getCategoryCodes(), e);
      return new GdnRestListResponse<>(e.getErrorMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, null, null, requestId);

    } catch (Exception e) {
      LOG.error(
          "Error while getting product detail official store with merchantCode {} and category codes {} with Error: {}",
          activeProductRequest.getMerchantCode(), activeProductRequest.getCategoryCodes(), e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_LIST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get the L3 level product list with status", description = "get the L3 level "
    + "product list with status")
  public GdnRestListResponse<ActiveProductResponse> getProductListByStatus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size,
      @RequestBody ActiveProductRequest activeProductRequest) {
    try {
      LOG.info("Getting the product list for suspension for request : {}", activeProductRequest);
      Page<ActiveProductDetailVo> activeProductDetailVos = this.productSearchService
          .getActiveProductsListForSuspension(storeId, modelConverter.toActiveProductsRequestVO(activeProductRequest),
              PageRequest.of(page, size));
      return new GdnRestListResponse<>(null, null, true,
          modelConverter.convertToActiveProductResponse(activeProductDetailVos.getContent()),
          new PageMetaData(size, page, activeProductDetailVos.getTotalElements()), requestId);
    } catch (SolrCustomException e) {
      LOG.error("Error while getting the product list for request {} with Error: {}", activeProductRequest,
          activeProductRequest, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false,
          null, null, requestId);
    } catch (Exception e) {
      LOG.error("Error while getting the product list for request {} with Error: {}", activeProductRequest,
          activeProductRequest, e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_SUSPENDED_ITEM_LIST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get the item list with suspended status", description = "get the item list with"
    + " suspended status")
  public GdnRestListResponse<ItemSummaryResponse> getSuspendedItemList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size,
      @RequestBody ActiveProductRequest activeProductRequest) {
    try {
      LOG.info("Getting the suspended item list for request : {}", activeProductRequest);
      ActiveProductsRequestVO activeProductsRequestVO = modelConverter.toActiveProductsRequestVO(activeProductRequest);
      activeProductsRequestVO.setStatus(SUSPENDED);
      Page<ItemInfoVO> itemInfoVOS = this.productSearchService
          .getSuspendedItemList(storeId, activeProductsRequestVO, PageRequest.of(page, size));
      return new GdnRestListResponse<>(null, null, true,
          modelConverter.convertToItemSummaryResponse(itemInfoVOS.getContent()),
          new PageMetaData(size, page, itemInfoVOS.getTotalElements()), requestId);
    } catch (SolrCustomException e) {
      LOG.error("Error while getting suspended item list for request {} with Error: {}", activeProductRequest,
          activeProductRequest, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, null, requestId);
    } catch (ApplicationRuntimeException e){
      LOG.error("Error while getting suspended item list for request {} with Error: {}", activeProductRequest,
        activeProductRequest, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false,
        null, null, requestId);}
    catch (Exception e) {
      LOG.error("Error while getting suspended item list for request {} with Error: {}", activeProductRequest,
          activeProductRequest, e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "generate productScore by productSku", description = "generate productScore by "
    + "productSku")
  public GdnRestSingleResponse<L3VersionResponse> generateProductScoreByProductSkuOrProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @RequestParam(required = false, defaultValue = "false") boolean updateCategory,
    @RequestParam(required = false) String productSku,
    @RequestParam(required = false) String productCode) {
    ProductListController.LOG
        .info("Generate productScore for productSku : {} and productCode: {} ", productSku, productCode);
    try {
      checkArgument(StringUtils.isNotBlank(productCode) || StringUtils.isNotBlank(productSku),
          ErrorMessages.PRODUCT_CODE_AND_PRODUCT_SKU_MUST_NOT_BE_NULL);
      List<ProductAndItemsVO> productAndItemsVOS =
        this.productService.generateProductScoreByProductSku(storeId, productSku, productCode,
          requestId, username, updateCategory, null);
      Long l3Version = null;
      if (CollectionUtils.isNotEmpty(productAndItemsVOS) && Objects.nonNull(productAndItemsVOS.get(0).getProduct())) {
        l3Version = productAndItemsVOS.get(0).getProduct().getVersion();
      }
      return new GdnRestSingleResponse<>(null, null, true,
          new L3VersionResponse(l3Version), requestId);
    } catch (Exception e) {
      ProductListController.LOG
          .error("generateProductScoreByProductSku productSku :{} , productCode : {}, errorMessage {}", productSku,
              productCode, e.getMessage(), e);
      retryGenerateProductScore(storeId, productSku, productCode, username, requestId,
        updateCategory);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, new L3VersionResponse(), requestId);
    }
  }

  private void retryGenerateProductScore(String storeId, String productSku, String productCode,
    String userName, String requestId, boolean updateCategory) {
    if (StringUtils.isNotBlank(productCode) || StringUtils.isNotBlank(productSku)) {
      try {
        String message = objectMapper.writeValueAsString(
          ProductScoreRetryDTO.builder().storeId(storeId).productCode(productCode)
            .productSku(productSku).updateCategory(updateCategory).userName(userName)
            .requestId(requestId).build());
        ProductRetryEventPublish productRetryEventPublish =
            ProductRetryEventPublish.builder().clearCache(Boolean.FALSE).retryCount(0).identifier(message).retryPublishStatus(RetryPublishStatus.PENDING)
                .topicName(DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME).build();
        this.productRetryEventPublishService.insertToRetryPublish(productRetryEventPublish);
      } catch (Exception e) {
        LOG.error("Exception while processing product score retry , productCode : {} ", productCode);
      }
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_MASTER_DATA_FIELDS_IN_PRODUCT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update master data fields in product", description = "update master data fields"
    + " in product")
  public GdnBaseRestResponse updateMasterDataFieldsInProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false) String productSku) {
    ProductListController.LOG.info("Update master data fields in product for productSku : {} ", productSku);
    try {
      checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
      this.productService.updateMasterDataFieldsInProduct(storeId, productSku);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductListController.LOG.error("Update master data field in product for productSku :{} failed , errorMessage {}",
          productSku, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }
}
