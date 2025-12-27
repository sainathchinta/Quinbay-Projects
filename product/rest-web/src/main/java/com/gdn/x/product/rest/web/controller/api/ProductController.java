package com.gdn.x.product.rest.web.controller.api;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.request.PromoEligibilityRequest;
import com.gdn.x.product.rest.web.model.response.PromoEligibilityResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.MDC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductsToItemCatalogMapping;
import com.gdn.x.product.model.vo.ReviewProductDetailVO;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.request.ProductBundleCreationRequest;
import com.gdn.x.product.service.api.ProductWrapperService;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.AgpConstant;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.CategoryBrandRequest;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionProductActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.SalesCatalogRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategorySequenceRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsAvailabilityResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.ProductsToItemCatalogMappingResponse;
import com.gdn.x.product.rest.web.model.response.ReviewProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductResponse;
import com.gdn.x.product.rest.web.model.util.GdnRestSimpleResponse;
import com.gdn.x.product.service.api.ProductCenterHistoryService;
import com.gdn.x.product.service.api.ProductDataArchivalService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.executor.api.AsyncProcessor;
import com.gdn.common.web.param.MandatoryRequestParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ProductApiPath.PRODUCT)
@Tag(name = "Product Controller", description = "Product Service API")
@Slf4j
public class ProductController {
  private static final Logger LOG = LoggerFactory.getLogger(ProductController.class);
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductCenterHistoryService productCenterHistoryService;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private AsyncProcessor asyncProcessor;

  @Autowired
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Autowired
  private ProductDataArchivalService productDataArchivalService;

  @Value("${shared.product.details.from.db}")
  private boolean sharedProductDetailsFromDb;

  @RequestMapping(value = ProductApiPath.ADD, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add a new product",
      description = "add a new product level 3 from existing product in master data")
  public GdnBaseRestResponse addProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductDTO productDTO) {
    ProductController.LOG.info("Add product with productDTO = {}", new Object[] {productDTO});

    try {
      this.productService.addProduct(storeId, requestId, username,
          this.modelConverter.convertProductDTOToProduct(productDTO));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#addProduct with productDTO {} , {}", productDTO, e.getMessage(),
          e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_PRODUCT.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ADD_PRODUCT_AND_ITEMS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add a new product of merchant with its items",
      description = "add a new product and items (level 3) synchronized")
  public GdnRestSingleResponse<AddProductAndItemsResponse> addProductAndItems(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductAndItemActivationRequest productAndItemActivationRequest) {
    log.info("Add product with productAndItemActivationRequest = {}", productAndItemActivationRequest);
    try {
      AddProductAndItemsResponseVo addProductAndItemsResponseVo =
          this.productService.addProductAndItems(storeId, requestId, username,
              this.modelConverter.convertToProductAndItemsRequestVO(productAndItemActivationRequest));
      return this.modelConverter.convertToAddProductAndItemsResponse(requestId,
          addProductAndItemsResponseVo);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("#addProductAndItems with productAndItemActivationRequest {} , {}",
          productAndItemActivationRequest, e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.ADD_PRODUCT_AND_ITEMS.getCode(), false,
          null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#addProductAndItems with productAndItemActivationRequest {} , {}",
          productAndItemActivationRequest, e.getMessage(), e);
      ProductErrorCodesEnum errorCode = ProductErrorCodesEnum.ADD_PRODUCT_AND_ITEMS;
      return new GdnRestSingleResponse<>(errorCode.getMessage(), errorCode.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ADD_SALES_CATEGORY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add product sales category", description = "add sales category of product")
  public GdnBaseRestResponse addProductSalesCatalog(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String catalogCode,
      @RequestParam String newCategoryCode, @RequestBody SimpleListStringRequest productSkus) {
    ProductController.LOG.info("add sales catalog with productSkus = {}",
        new Object[] {productSkus});
    try {
      this.productService.updateProductSalesCategory(storeId, requestId, username,
          productSkus.getValue(), catalogCode, null, newCategoryCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#addProductSalesCatalog with catalogCode:{}, productSkus:{} , {}", catalogCode,
          productSkus.getValue(), e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.ADD_SALES_CATEGORY.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ALTER_SALES_CATEGORY_SEQUENCE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "alter sales category sequence",
      description = "alter sales category sequence on specified product sku")
  public GdnBaseRestResponse alterSalesCategorySequence(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku,
      @RequestBody List<SalesCategorySequenceRequest> salesCategorySequenceRequests) {
    ProductController.LOG.info("Alter sales category sequence = {}",
        new Object[] {salesCategorySequenceRequests});
    try {
      this.productService.alterSalesCategorySequence(storeId, productSku, this.modelConverter
          .convertRequestListToModel(salesCategorySequenceRequests, SalesCategorySequence.class));
      return new GdnBaseRestResponse(requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#alterSalesCategorySequence error with request = {}, error = {}",
          salesCategorySequenceRequests, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.ALTER_SALES_CATEGORY.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DELETE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete product and its items",
      description = "delete a product with all the items by setting mark for delete true")
  public GdnBaseRestResponse deleteProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku) {
    ProductController.LOG.info("Delete product and its items with productSku = {}",
        new Object[] {productSku});

    try {
      this.productService.deleteProduct(storeId, productSku);

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#deleteProduct with productSku {} , {}", productSku,
          e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.DELETE_PRODUCT.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DELETE_SALES_CATEGORY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete product sales category", description = "delete sales category of product")
  public GdnBaseRestResponse deleteProductSalesCatalog(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String catalogCode,
      @RequestParam String oldCategoryCode, @RequestBody SimpleListStringRequest productSkus) {
    ProductController.LOG.info("delete sales catalog with productSkus = {}",
        new Object[] {productSkus});
    try {
      this.productService.updateProductSalesCategory(storeId, requestId, username,
          productSkus.getValue(), catalogCode, oldCategoryCode, null);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#deleteProductSalesCatalog with catalogCode:{}, productSkus:{} , {}", catalogCode,
          productSkus.getValue(), e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.DELETE_SALES_CATEGORY.getCode(), false, requestId);
    }
  }


  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items",
      description = "get a product with all the items from database")
  @ResponseBody
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam boolean showDeleted,
      @RequestParam String productSku, @RequestParam(defaultValue = "false") boolean combineOthersBundlings,
      @RequestParam(required = false) boolean off2On, @RequestParam(defaultValue = "false") boolean includeForceReview,
      @RequestParam(required = false, defaultValue = "true") boolean needProductData,
      @RequestParam(defaultValue = "false") boolean isMigrateAndSyncProduct) {
    ProductController.LOG.info("Get product and items with productSku = {}",
        new Object[] {productSku});
    try {
      ProductItemsVo productItemsVo = this.productService
          .getProductAndItems(storeId, requestId, username, productSku, showDeleted,
            combineOthersBundlings, off2On, needProductData, includeForceReview, isMigrateAndSyncProduct);
      ProductAndItemsResponse productAndItemsResponse =
        this.modelConverter.convertToProductAndItemsResponse(productItemsVo, null, false);
      return new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("#getProductAndItems with productSku {} , {}", productSku,
          e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#getProductAndItems with productSku {} , {}", productSku,
          e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS_AVAILABILITY,
      method = RequestMethod.POST, consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product and items availability",
      description = "get a product with data buyable, discoverable, and weight")
  @ResponseBody
  public GdnRestSingleResponse<ProductAndItemsAvailabilityResponse> getProductAndItemsAvailability(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest productSkus) {
    try {
      Map<String, List<ProductAndItemsVO>> productAndItemsVO = this.productService
          .getProductAndItemsAvailability(storeId, requestId, username, productSkus.getValue());
      Map<String, List<ProductAndItemsResponse>> productAndItemsResponse = new HashMap<>();
      ProductAndItemsAvailabilityResponse response =
          new ProductAndItemsAvailabilityResponse(productAndItemsResponse);
      for (Entry<String, List<ProductAndItemsVO>> entry : productAndItemsVO.entrySet()) {
        List<ProductAndItemsResponse> productAndItemResponses = entry.getValue().stream()
            .map(modelConverter::convertToProductAndItemsDTO).collect(Collectors.toList());
        productAndItemsResponse.put(entry.getKey(), productAndItemResponses);
      }
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("#getProductAndItems with productSku {} , {}", productSkus,
          e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#getProductAndItems with productSku {} , {}", productSkus,
          e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by catentry id",
      description = "get a product with all the items from database")
  @ResponseBody
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsByProductCatentryId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCatentryId) {
    ProductController.LOG.info("Get product and items with productCatentryId = {}",
        new Object[] {productCatentryId});
    try {
      ProductAndItemsVO productAndItemsVO = this.productSearchService
          .getProductAndItemsByProductCatentryId(storeId, username, requestId, productCatentryId);
      return new GdnRestSingleResponse<>(null, null, true,
          this.modelConverter.convertToProductAndItemsDTO(productAndItemsVO), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error(
          "#getProductAndItemsByProductCatentryId with productCatentryId {} , {}",
          productCatentryId, e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#getProductAndItemsByProductCatentryId with productCatentryId {} , {}",
          productCatentryId, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by productCode",
      description = "get a product with all the items from database and master data")
  @ResponseBody
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getProductAndItemsByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCode) {
    ProductController.LOG.info("Get product and items with productCode = {}",
        new Object[] {productCode});

    try {
      MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchService
          .getProductAndItemsByProductCode(storeId, username, requestId, productCode, true);
      return new GdnRestSingleResponse<>(null, null,
          true, this.modelConverter.convertToMasterDataDetailResponse(result), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("#getProductAndItemsByProductCode with productCode {} , {}",
          productCode, e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#getProductAndItemsByProductCode with productCode {} , {}",
          productCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by productCode",
      description = "get a product with all the items from database and master data only return the sync one")
  @ResponseBody
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getProductAndItemsSyncByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestParam(defaultValue = "false") boolean combineOthersBundlings,
      @RequestParam(required = false) boolean off2On) {
    ProductController.LOG.info("Get product and items with productCode sync = {}",
        new Object[] {productCode});

    try {
      MasterDataWithProductItemsVo result = this.productSearchService
          .getMasterDataWithProductItemsVo(storeId, username, requestId, productCode, true, combineOthersBundlings,
              off2On);
      MasterDataDetailWithProductAndItemsResponse masterDataDetailWithProductAndItemsResponse =
        this.modelConverter.convertToMasterDataWithProductItemsVo(result);
      return new GdnRestSingleResponse<>(null, null,
          true, masterDataDetailWithProductAndItemsResponse, requestId);
    } catch (SolrCustomException e){
      ProductController.LOG.error("#getProductAndItemsSyncByProductCode with productCode {} , {}",
        productCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
    catch (Exception e) {
      ProductController.LOG.error("#getProductAndItemsSyncByProductCode with productCode {} , {}",
          productCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_MASTER_DATA_AND_PRODUCT_ITEM_DATA,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get master data and product and item data",
      description = "Get master data and product and item data by productCode and itemSku")
  @ResponseBody
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemResponse> getMasterDataAndProductAndItemData(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestParam String itemSku, @RequestParam(defaultValue = "false") boolean combineOthersBundlings,
  @RequestParam(required = false) boolean off2On) {
    ProductController.LOG.info("getMasterDataAndProductAndItemData productCode = {}, itemSku = {}", productCode,
        itemSku);

    try {
      MasterDataDetailWithProductAndItemResponseVo result = this.productSearchService
          .getMasterDataAndProductAndItemData(storeId, username, requestId, productCode, itemSku,
              true, combineOthersBundlings, off2On);
      return new GdnRestSingleResponse<>(null, null,
          true, this.modelConverter.toMasterDataDetailWithProductAndItemResponse(result), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("#getMasterDataAndProductAndItemData with productCode = {}, itemSku = {}",
          productCode, itemSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#getMasterDataAndProductAndItemData with productCode = {}, itemSku = {}, {}",
          productCode, itemSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEMSKU,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and single item by itemSku",
      description = "get a product with single matched itemSku from database")
  @ResponseBody
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam(required = false) boolean instantPickup, @RequestParam(required = false) String pickupPointCode,
      @RequestParam(required = false, defaultValue = "true") boolean convertPreOrderDetails,
      @RequestParam(required = false) boolean fetchMfdTrueItem) {
    ProductController.LOG.info("getProductAndSingleItemByItemSku with itemSku = {}",
        new Object[] {itemSku});
    try {
      ProductItemsVo productItemsVo = this.productService.getProductAndSingleItemByItemSku(storeId, requestId, username,
          itemSku, true, instantPickup, pickupPointCode, fetchMfdTrueItem);
      return new GdnRestSingleResponse<>(null, null, true,
          this.modelConverter.convertToProductAndItemsResponseWithConvertPreOrderDetails(
              productItemsVo, convertPreOrderDetails, null, false), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("#getProductAndSingleItemByItemSku with itemSku {} , {}", itemSku,
          e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#getProductAndSingleItemByItemSku with itemSku {} , {}", itemSku,
          e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and single item by itemSku",
      description = "get a product with single matched itemSku from database")
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductDetailAndSingleItemByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam(required = false) boolean instantPickup, @RequestParam(required = false) String pickupPointCode) {
    ProductController.LOG.info("getProductDetailAndSingleItemByItemSku with itemSku = {}",
        new Object[] {itemSku});
    try {
      ProductAndItemsVO productAndItemsVO = this.productService
          .getProductDetailAndSingleItemByItemSku(storeId, requestId, username, itemSku, true, instantPickup, pickupPointCode);
      LOG.info("productAndItemsVO : {}", productAndItemsVO);
      return new GdnRestSingleResponse<>(null, null, true,
          this.modelConverter.convertToProductAndItemsDTO(productAndItemsVO), requestId);
    } catch (SolrCustomException e) {
      ProductController.LOG.error(
        "#getProductDetailAndSingleItemByItemSku failed with itemSku {}, error : ", itemSku, e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error(
        "#getProductDetailAndSingleItemByItemSku failed with itemSku {}, error : ", itemSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
        "#getProductDetailAndSingleItemByItemSku failed with itemSku {}, error : ", itemSku,
        e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get top x product that does not have sales catalog mapping",
      description = "will return x first occurence with distinct map of master catalog code - product skus")
  public GdnRestListResponse<ProductsToItemCatalogMappingResponse> getProductsWithoutSalesCatalog(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int month, @RequestParam int year,
      @RequestParam(defaultValue = "20") Integer size) {
    ProductController.LOG.info("Get product sku and  sync = {}", new Object[] {});
    Sort sort = Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE);
    int page = 0;
    Page<ProductsToItemCatalogMapping> result = null;
    try {
      result = this.productSearchService.getProductAndItemWithoutSalesCatalog(storeId, username,
      requestId,
        month, year, PageRequest.of(page, size, sort));
    }
    catch (SolrCustomException e){
      ProductController.LOG.error("#getProductsWithoutSalesCatalog Failed for request {} , error-",
        requestId, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
    catch (Exception e){
      ProductController.LOG.error("#getProductsWithoutSalesCatalog Failed for request {} , error- ",
        requestId, e);
      return new GdnRestListResponse<>(e.getMessage(),
        e.getMessage(), false, requestId);
    }
    List<ProductsToItemCatalogMappingResponse> content = this.modelConverter
        .convertListToResponse(result.getContent(), ProductsToItemCatalogMappingResponse.class);
    return new GdnRestListResponse<>(content,
        new PageMetaData(result.getSize(), result.getNumber(), result.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG_DAY_RANGE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get top x product that does not have sales catalog mapping",
      description = "will return x first occurence with distinct map of master catalog code - product skus")
  public GdnRestListResponse<ProductsToItemCatalogMappingResponse> getProductsWithoutSalesCatalogDayRange(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int fromDay, @RequestParam int toDay, @RequestParam int month,
      @RequestParam int year, @RequestParam(defaultValue = "20") Integer size) {
    ProductController.LOG.info("Get product sku and  sync = {}", new Object[] {});
    Sort sort = Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE);
    int page = 0;
    Page<ProductsToItemCatalogMapping> result =
        this.productSearchService.getProductAndItemWithoutSalesCatalogPerDay(storeId, username,
            requestId, fromDay, toDay, month, year, PageRequest.of(page, size, sort));
    List<ProductsToItemCatalogMappingResponse> content = this.modelConverter
        .convertListToResponse(result.getContent(), ProductsToItemCatalogMappingResponse.class);
    return new GdnRestListResponse<>(content,
        new PageMetaData(result.getSize(), result.getNumber(), result.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = {ProductApiPath.GET_PRODUCT_BY_ITEM_SKUS_CACHED},
      method = {RequestMethod.POST}, consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<SimpleProductResponse> getSimpleProductByItemSkuCached(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest itemSkus) throws Exception {
    LOG.debug("getProductByItemSku with itemSku : {} ", new Object[] {itemSkus});
    try {
      List<ProductAndItemsVO> productAndItemsVOs = productService.getProductAndItemsByItemSkus(
          storeId, requestId, username, new HashSet<>(itemSkus.getValue()));
      List<SimpleProductResponse> simpleProductResponses = productAndItemsVOs.stream().map(
          e -> modelConverter.convertToSimpleProductResponse(e.getProduct(), e.getItems().get(0)))
          .filter(e -> e != null).collect(toList());
      return new GdnRestListResponse<SimpleProductResponse>(simpleProductResponses,
          new PageMetaData(simpleProductResponses.size(), 0, simpleProductResponses.size()),
          requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("#getSimpleProductByItemSkuCached error with itemSkus = {}, with error = {}",
          itemSkus, e.getMessage());
      return new GdnRestListResponse<SimpleProductResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_SIMPLE_PRODUCT.getCode(), false, requestId);
    } catch (Exception e) {
      LOG.error("#getSimpleProductByItemSkuCached error with itemSkus {}", itemSkus, e);
      return new GdnRestListResponse<SimpleProductResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_SIMPLE_PRODUCT.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS},
      method = {RequestMethod.POST}, consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ProductAndItemInfoResponse> getProductInfoByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody GetProductInfoRequest request) throws Exception {
    LOG.debug("getProductInfoByItemSkus with request : {} ", request);
    try {
      MandatoryRequestParam mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId,
              requestId, username, username);
      List<ProductItemsVo> productAndItemsVOs = productSearchService
          .getProductAndItemsInfoForActiveItem(mandatoryRequestParam, request.getItemSkus(),
              request.isPristine(), request.isFullFetch(), request.isNeedWholesaleData(),
              request.isCombineOthersBundlings(), request.isOff2On());
      List<ProductAndItemInfoResponse> productInfoResponses = productAndItemsVOs.stream()
          .map(e -> modelConverter.convertToProductAndItemInfoResponse(e)).collect(toList());
      return new GdnRestListResponse<>(productInfoResponses,
        new PageMetaData(productInfoResponses.size(), 0, productInfoResponses.size()), requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("#getProductInfoByItemSku error with itemSkus = {}, with request = {}",
          request, e.getMessage());
      return new GdnRestListResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, requestId);
    } catch (Exception e) {
      LOG.error("#getProductInfoByItemSku error with request {}", request, e);
      return new GdnRestListResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.MOVE_SALES_CATEGORY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "move product sales category", description = "move sales category of product")
  public GdnBaseRestResponse moveProductSalesCatalog(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String catalogCode,
      @RequestParam String oldCategoryCode, @RequestParam String newCategoryCode,
      @RequestBody SimpleListStringRequest productSkus) {
    ProductController.LOG.info("move sales catalog with productSkus = {}",
        new Object[] {productSkus});
    try {
      this.productService.updateProductSalesCategory(storeId, requestId, username,
          productSkus.getValue(), catalogCode, oldCategoryCode, newCategoryCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#moveProductSalesCatalog with catalogCode:{}, productSkus:{} , {}", catalogCode,
          productSkus.getValue(), e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.MOVE_SALES_CATEGORY.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.SYNCHRONIZE, method = RequestMethod.POST, produces = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "synchronize product", description = "synchronize product with all the items")
  public GdnRestSingleResponse<ProductAndItemsResponse> synchronizeProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku) {
    ProductController.LOG.info("Synchronize product with productSku = {}", new Object[] {productSku});

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,  requestId);
    try {
      ProductAndItemsVO productAndItemsVO = this.productService.synchronizeProduct(storeId, productSku);
      if (Objects.nonNull(productAndItemsVO)) {
        return new GdnRestSingleResponse<>(null, null, true,
            this.modelConverter.convertToProductAndItemsDTO(productAndItemsVO), requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, null, requestId);
      }
    } catch (SolrCustomException e){
      ProductController.LOG.error("#synchronizeProduct with productSku {} , {} ", productSku,
        e.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
    catch (Exception e) {
      ProductController.LOG.error("#synchronizeProduct with productSku {} , {}", productSku, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.SYNCHRONIZE_PRODUCT.getCode(), false,
          null, requestId);
    }
  }

  @PostMapping(value = ProductApiPath.UNSYNCHRONIZE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "unsynchronize product", description = "unsynchronize product with all the items")
  public GdnRestSingleResponse<ProductAndItemsResponse> unsynchronizeProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku,
      @RequestParam boolean overwriteExistingMasterData) {
    ProductController.LOG.info("Unsynchronize product with productSku = {}", new Object[] {productSku});

    try {
      ProductAndItemsVO productAndItemsVO = this.productService
          .unsynchronizeProduct(storeId, requestId, username, productSku, overwriteExistingMasterData);
      if (Objects.nonNull(productAndItemsVO)) {
        return new GdnRestSingleResponse<>(null, null, true,
            this.modelConverter.convertToProductAndItemsDTO(productAndItemsVO), requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, null, requestId);
      }
    } catch (SolrCustomException e) {
      ProductController.LOG.error("#unsynchronizeProduct with productSku {} , {} ", productSku,
        e.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#unsynchronizeProduct with productSku {} , {}", productSku,
        e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
        ProductErrorCodesEnum.UNSYNCHRONIZE_PRODUCT.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PUBLISH_ALL_PRODUCTS, method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all products")
  public GdnBaseRestResponse publishAllProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username){
      asyncProcessor.submitWithBackoff(AgpConstant.COMMAND_PUBLISH_ALL_PRODUCTS, () ->
          this.productService.publishAllProducts(storeId));
      return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.REPUBLISH_PRODUCTS_TO_AGP, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "republish products to AGP")
  public GdnBaseRestResponse republishProductsToAgp(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<String> productSkus){
    asyncProcessor.submitWithBackoff(AgpConstant.COMMAND_REPUBLISH_PRODUCTS_TO_AGP, () ->
        this.productService.republishProductsToAgp(storeId,productSkus));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE, method = RequestMethod.POST, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product", description = "update product excluding synchronization")
  public GdnRestSingleResponse<ProductResponse> updateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal,
      @RequestBody ProductRequest productRequest) {
    ProductController.LOG.info("Update product with productRequest = {}", new Object[] {productRequest});

    try {
      Product product =
          this.productService.updateProduct(storeId, requestId, this.modelConverter.convertToProduct(productRequest),
              isOnlyExternal);

      return new GdnRestSingleResponse<>(null, null, true, this.modelConverter.convertProductToProductResponse(product),
          requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#updateProduct with productRequest {} , {}", productRequest, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.UPDATE_PRODUCT.getCode(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_MASTER_CATALOG, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product master catalog", description = "update master catalog of product")
  public GdnBaseRestResponse updateProductMasterCatalog(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCode, @RequestBody MasterCatalogRequest masterCatalogRequest) {
    ProductController.LOG.info("Update product master catalog with masterCatalogRequest = {}",
        new Object[] {masterCatalogRequest});
    try {
      this.productService.updateProductMasterCatalog(storeId, productCode,
          this.modelConverter.convertToMasterCatalog(masterCatalogRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#updateProductMasterCatalog with productCode:{}, masterCatalogRequest:{} , {}",
          productCode, masterCatalogRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_MASTER_CATALOG.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_SALES_CATALOG, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product sales catalog", description = "update sales catalog of product")
  public GdnBaseRestResponse updateProductSalesCatalog(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestParam boolean replace, @RequestBody SalesCatalogRequest salesCatalogRequest) {
    ProductController.LOG.info("Update product sales catalog with salesCatalogRequest = {}",
        new Object[] {salesCatalogRequest});
    try {
      this.productService.updateProductSalesCatalogByProductCode(storeId, productCode,
          this.modelConverter.convertToSalesCatalog(salesCatalogRequest), replace);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#updateProductSalesCatalog with productCode:{}, salesCatalogRequest:{} , {}", productCode,
          salesCatalogRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_SALES_CATALOG.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_COUNT_BY_BRAND, method = RequestMethod.GET,
    produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get count of product by brand", description = "get count of product by "
    + "brand")
  public GdnRestSingleResponse<SimpleLongResponse> getProductsCountByBrand(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam String brand) {
    ProductController.LOG.info("Get count of product by brand name with storeId:{}", storeId);
    try {
      Long productCount = this.productService.getProductsCountByBrand(storeId, brand);
      SimpleLongResponse response = new SimpleLongResponse(productCount);
      return new GdnRestSingleResponse<SimpleLongResponse>(null, null, true, response, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#getCountProductsByBrandName with brandName:{}", brand);
      return new GdnRestSingleResponse<SimpleLongResponse>(e.getMessage(), null, false, null,
        requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRISTINEID, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by PristineId", description = "get a product with all "
      + "the items from database and master data only return the sync one")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>
  getProductAndItemsByPristineId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String pristineId, @RequestParam String defaultSku) {
    MasterDataDetailWithProductAndItemsResponse response = null;
    try {
      ProductController.LOG.info("Get product and items with pristineId : {}", pristineId);
      MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchService
          .getPristineProductAndItemsInfoByItemSku(storeId, username, requestId, defaultSku, true, false, false);
      Optional.ofNullable(result.getProductAndItems()).orElse(new ArrayList<>()).forEach(
          productAndItemsVO -> this.productService
              .checkProductAndItemsForForceReview(Arrays.asList(productAndItemsVO.getProduct()),
                  productAndItemsVO.getItems()));
      response = this.modelConverter.convertToMasterDataDetailResponse(result);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while #getProductAndItemsSyncByPristineId pristineId : {}", pristineId, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRISTINE_PRODUCT_AND_ITEMS.getCode(), false, response,
          requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while #getProductAndItemsSyncByProductCode with pristineId : {}",
              pristineId, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, response, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get products and items response by pristineId",
      description = "get products with all the items in database")
  public GdnRestSingleResponse<PristineProductAndItemsResponse>
  getProductAndItemsResponseByPristineId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String id,  @RequestParam(required = false) boolean off2On) {
    PristineProductAndItemsResponse response = null;
    try {
      ProductController.LOG.info("Get product and items with pristineId : {}", id);
      PristineProductAndItemsResponseVO result = this.productSearchService
          .getProductAndItemsResponseByPristineId(storeId, username, requestId, id, off2On);
      response = this.modelConverter.convertToPristineProductAndItemsResponse(result);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while #getProductAndItemsResponseByPristineId id : {}", id, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, response, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while #getProductAndItemsResponseByPristineId with id : {}", id, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, response, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRISTINE_PRODUCT_AND_ITEMS_INFO_BY_ITEM_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get MasterDataDetailWithProductAndItemsResponse by itemSku", description = "get"
    + " a product with all the items from database and master data ")
  public GdnRestSingleResponse<PristineMasterDataDetailResponse> getPristineProductAndItemsInfoByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String itemSku,
      @RequestParam(defaultValue = "false") boolean combineOthersBundlings,
      @RequestParam(required = false) boolean off2On) {
    ProductController.LOG.info("Get MasterDataDetailWithProductAndItemsResponse by itemSku :{}", itemSku);
    PristineMasterDataDetailResponse response = null;
    try {
      MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchService
          .getPristineProductAndItemsInfoByItemSku(storeId, username, requestId, itemSku, true, combineOthersBundlings,
              off2On);
      response = this.modelConverter.convertToPristineMasterDataDetailResponse(result);

      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while #MasterDataDetailWithProductAndItemsResponse by itemSku : {}", itemSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, response, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while #MasterDataDetailWithProductAndItemsResponse by itemSku : {}", itemSku, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, response, requestId);
    }
  }


  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID,
                  method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items by productCode or pristineId", description =
      "get a product with all the items from database and master data ")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>
  getProductAndItemsByProductCodeOrPristineId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String id, @RequestParam(required = false) String defaultSku) {
    ProductController.LOG.info("Get product and items with id :{}", id);
    MasterDataDetailWithProductAndItemsResponse response = null;
    try {
      if (skuValidator.isPristineId(id)) {
        ProductController.LOG.info("Get product and items with pristineId : {}", id);
        MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchService
            .getPristineProductAndItemsInfoByItemSku(storeId, username, requestId, defaultSku, true, false, false);
        response = this.modelConverter.convertToMasterDataDetailResponse(result);
      } else {
        ProductController.LOG.info("Get product and items with productCode : {}", id);
        MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchService
            .getProductAndItemsSyncByProductCodes(storeId, username, requestId, id, true, false, false);
        response = this.modelConverter.convertToMasterDataDetailResponse(result);
      }
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while #getProductAndItemsByProductCodeOrPristineId id : {}", id, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, response, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while #getProductAndItemsByProductCodeOrPristineId with id : {}", id, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, response, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ADD_PRODUCT_ATTRIBUTE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add product attribute",
      description = "add product attribute for recategorization")
  public GdnBaseRestResponse addProductAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestBody ProductAttributeRequest productAttributeRequest) {
    ProductController.LOG.info("Add Product Attribute  productAttributeRequest = {}",
        new Object[] {productAttributeRequest});
    try {
      this.productService.addProductAttribute(storeId, productCode, this.modelConverter
          .convertProductAttributeRequestToMasterDataProductAttribute(productAttributeRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#addProductAttribute with productCode:{}, productAttributeResponse:{} , {}", productCode,
          productAttributeRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.ADD_PRODUCT_ATTRIBUTE.getCode(), false, requestId);
    }
  }


  @RequestMapping(value = ProductApiPath.UPDATE_MASTER_CATALOG_BY_PRODUCT_SKU, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product master catalog", description = "update master catalog of product")
  public GdnBaseRestResponse updateProductMasterCatalogByProductSku(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productSku, @RequestBody MasterCatalogRequest masterCatalogRequest) {
    ProductController.LOG.info("Update product master catalog by product sku with masterCatalogRequest = {}",
        new Object[] {masterCatalogRequest});
    try {
      this.productService.updateProductMasterCatalogByProductSku(storeId, productSku,
          this.modelConverter.convertToMasterCatalog(masterCatalogRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#updateProductMasterCatalog with productSku:{}, masterCatalogRequest:{} , {}",
          productSku, masterCatalogRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_MASTER_CATALOG.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_SALES_CATALOG_BY_PRODUCT_SKU, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product sales catalog", description = "update sales catalog of product")
  public GdnBaseRestResponse updateProductSalesCatalogByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku,
      @RequestParam boolean replace, @RequestBody SalesCatalogRequest salesCatalogRequest) {
    ProductController.LOG.info("Update product sales catalog by product sku with salesCatalogRequest = {}",
        new Object[] {salesCatalogRequest});
    try {
      this.productService.updateProductSalesCatalogByProductSku(storeId, productSku,
          this.modelConverter.convertToSalesCatalog(salesCatalogRequest), replace);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#updateProductSalesCatalog with productSku:{}, salesCatalogRequest:{} , {}", productSku,
          salesCatalogRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_SALES_CATALOG.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ADD_PRODUCT_ATTRIBUTE_BY_PRODUCT_SKU, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add product attribute",
      description = "add product attribute for recategorization")
  public GdnBaseRestResponse addProductAttributeByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku,
      @RequestBody ProductAttributeRequest productAttributeRequest) {
    ProductController.LOG.info("Add Product Attribute by product sku productAttributeRequest = {}",
        new Object[] {productAttributeRequest});
    try {
      this.productService.addProductAttributeByProductSku(storeId, productSku, this.modelConverter
          .convertProductAttributeRequestToMasterDataProductAttribute(productAttributeRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error(
          "#addProductAttribute with productSku:{}, productAttributeResponse:{} , {}", productSku,
          productAttributeRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.ADD_PRODUCT_ATTRIBUTE.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_DETAIL_FOR_REVIEW, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get Product Detail For Review",
                description = "Get Product Detail For Review")
  public GdnRestSingleResponse<ReviewProductDetailResponse> getProductDetailForReview(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String id, @RequestParam(required = false) String itemSku) {

    ReviewProductDetailResponse response = new ReviewProductDetailResponse();
    ReviewProductDetailVO result = null;
    try {
      if (skuValidator.isProductSku(id)) {
        ProductController.LOG.info("Get Product Detail for Review by productSku : {}", id);
        result = this.productSearchService
            .getProductDetailForReviewByProductSku(storeId, username, requestId, id, itemSku);
      } else {
        ProductController.LOG.info("Get product and items with productCode : {}", id);
        result = this.productSearchService
            .getProductDetailForReviewByProductCode(storeId, username, requestId, id, itemSku);
      }
      BeanUtils.copyProperties(result, response);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while Get Product Detail for Review by id : {}", id, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_REVIEW_PRODUCT_DETAIL.getCode(), false, response, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while Get Product Detail for Review by id : {}", id, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, response, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.SUSPEND}, method = {RequestMethod.POST}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "suspend/reactivate product", description = "suspend/reactivate product")
  public GdnBaseRestResponse toggleSuspensionProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productSku, @RequestParam boolean suspendProduct) {
    ProductController.LOG.info("Suspend product = {} with storeId = {}, and productSku = {}",
        new Object[] {suspendProduct, storeId, productSku});
    try {
      this.productService.toggleSuspensionProduct(storeId, productSku, username, suspendProduct, requestId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while suspending/reactivating the product with productSku : {}", productSku, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
        ProductErrorCodesEnum.SUSPEND_PRODUCT.getCode(), false, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while suspending/reactivating the product with productSku : {}", productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.SUSPEND_PRODUCT.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product by product code and merchant code", description = "get product by "
    + "product code and merchant code ")
  public GdnRestListResponse<ProductResponse> getProductByProductCodeAndMerchantCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCode, @RequestParam String merchantCode) {
    ProductController.LOG.info("Get product by product code {} and merchant code {}", productCode, merchantCode);
    List<ProductResponse> response = null;
    try {
      List<Product> products =
          this.productService.findByStoreIdAndProductCodeAndMerchantCode(storeId, productCode, merchantCode);
      if (CollectionUtils.isEmpty(products)) {
        return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
      }
      response = this.modelConverter.convertToProductResponseList(products);
      return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Error while getting the product by product code {} and merchant code {}", productCode, merchantCode,
              e);
      return new GdnRestListResponse<ProductResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_SIMPLE_PRODUCT.getCode(), false, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while getting the product by product code {} and merchant code {}", productCode, merchantCode,
              e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_UNMAPPED_SKU, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get unmapped product skus", description = "get unmapped product skus")
  public GdnRestListResponse<UnmappedSkuResponse> getUnmappedProductSkus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<String> request) {
    ProductController.LOG.info("Get unmapped product sku for categories = {}", request);
    try {
      List<UnmappedSkuResponse> response = this.productService.getUnmappedProductSkus(storeId, request);
      return new GdnRestListResponse<>(response, new PageMetaData(0, response.size(), response.size()), requestId);
    } catch (Exception e) {
      ProductController.LOG.error("#get unmapped productSkus for category codes", request, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_UNMAPPED_PRODUCT_SKUS.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get Product Details for product center by productSku", description = "Get "
    + "Product Details for product center by productSku")
  public GdnRestSingleResponse<ProductCenterDetailResponse> getProductDetailsForProductCenter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String productSku,
      @RequestParam(defaultValue = "true", required = false) boolean needBusinessPartnerData) {
    ProductController.LOG.info("Get Product Details for product center by productSku :{}", productSku);
    ProductCenterDetailResponse response = null;
    try {
      response = productService.getProductDetailsForProductCenter(storeId, productSku, needBusinessPartnerData);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("Error while #getProductDetailsForProductCenter by productSku : {}", productSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER.getCode(), false, response, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("Error while #getProductDetailsForProductCenter by productSku : {}", productSku, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, response, requestId);
    }
  }

  //api to update the product code from migrated product code using productSku
  @RequestMapping(value = {ProductApiPath.MIGRATE_PRODUCT_SKU}, method = {RequestMethod.PUT}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update migrated product_code", description = "update migrated product_code")
  public GdnBaseRestResponse updateMigratedProductCode(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productSku, @RequestParam(required = false) String newProductCode,
      @RequestParam boolean rollback) {
    ProductController.LOG
        .info("UpdateMigratedProductCode- productSku:{}, newProductCode:{}, rollback:{}", productSku,
            newProductCode, rollback);
    try {
      this.productService.updateMigratedProductCode(username, requestId, storeId, productSku, newProductCode, rollback);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error while migrating the productCode: {} for the product with productSku : {}", newProductCode,
              productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_MIGRATED_PRODUCT_CODE.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_CENTER_HISTORY, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product center history by productSku", description = "Get product center "
    + "history by productSku")
  public GdnRestListResponse<ProductCenterHistoryResponse> getProductCenterHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("productSku") String productSku,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    ProductController.LOG.info("Get product center history by productSku :{}", productSku);
    try {
      Page<ProductCenterHistoryResponse> response = this.productCenterHistoryService
          .getProductCenterHistoryByStoreIdAndProductSku(storeId, productSku, page, size);
      return new GdnRestListResponse<>(null, null, true, response.getContent(),
          new PageMetaData(size, page, response.getTotalElements()), requestId);
    } catch (Exception e) {
      ProductController.LOG.error("Error while #getProductCenterHistory by productSku : {}", productSku, e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_SALES_CATEGORY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update sales category", description = "Update sales category")
  public GdnBaseRestResponse updateSalesCategory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productSku, @RequestBody SalesCategoryMappingUpdateRequest request) {
    ProductController.LOG.info("Update sales category by productSku {} and request {}", productSku, request);
    try {
      this.productService.updateSalesCategory(storeId, productSku, request, requestId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG.error("Error while Updating sales category with productSku : {}", productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_SALES_CATEGORY.getCode(), false,
          requestId);
    } catch (Exception e) {
      ProductController.LOG.error("Error while Updating sales category with productSku : {}", productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_L3_COUNT_BY_PRODUCTCODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get l3 count by product code", description = "Get l3 count by product code")
  public GdnRestSingleResponse<SimpleLongResponse> getL3CountByProductCode(@RequestParam String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode) {
    ProductController.LOG.info("Get l3 count by productCode : {}", productCode);
    try {
      Long productL3Count = this.productService.getProductL3CountByProductCode(productCode);
      SimpleLongResponse response = new SimpleLongResponse(productL3Count);
      return new GdnRestSingleResponse<SimpleLongResponse>(null, null, true, response, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Error getting l3 count from solr with productCode:{}, error message:{} , {}", productCode,
              e.getMessage(), e);
      return new GdnRestSingleResponse<SimpleLongResponse>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PINPOINT_STATUS_BY_PRODUCT_SKU,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get pickup point codes which not tagged to geolocation",
      description = "Get pickup point codes which not tagged to geolocation")
  public GdnRestSingleResponse<ProductPickupPointListResponse> getPickupPointCodesByProductSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productSku") String productSku) {
    ProductController.LOG.info("Fetch pickup points without geolocation by productSku");
    try {
      return new GdnRestSingleResponse<>(null, null, true,
          this.productService.getPickupPointCodesByProductSku(storeId, productSku), requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Exception finding pickup point code for L3 : {}, error - ", productSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_COUNT_BY_TYPE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product count by Type", description = "Get product count by Type")
  public GdnRestSingleResponse<ProductCountResponse> getProductCountByType(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String type, @RequestParam String merchantCode) {
    ProductController.LOG.info("Fetch pickup points without geolocation by productSku");
    try {
      ProductCountResponseVo productCountResponseVo =
          this.productWrapperService.getProductCountByType(type, merchantCode, true);
      ProductCountResponse productCountResponse = new ProductCountResponse();
      BeanUtils.copyProperties(productCountResponseVo, productCountResponse);
      return new GdnRestSingleResponse<>(null, null, true, productCountResponse, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Exception finding count for L3 type : {}, merchantCode : {}, error - ", type, merchantCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_SECONDARY_COUNTS, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get secondary product count by Type", description = "Get secondary product "
    + "count by Type")
  public GdnRestSingleResponse<ProductCountResponse> getSecondaryCounts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String type, @RequestParam String merchantCode) {
    ProductController.LOG.info("Get secondary product count by Type");
    try {
      ProductCountResponseVo productCountResponseVo =
          this.productWrapperService.getProductCountByType(type, merchantCode, false);
      ProductCountResponse productCountResponse = new ProductCountResponse();
      BeanUtils.copyProperties(productCountResponseVo, productCountResponse);
      return new GdnRestSingleResponse<>(null, null, true, productCountResponse, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("Exception finding count for L3 type : {}, merchantCode : {}, error - ", type,
          merchantCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }


  @RequestMapping(value = ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product and items", description = "get a product with all the items from "
    + "database")
  public GdnRestSingleResponse<ProductL3Response> getProductDetailsByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku) {
    ProductController.LOG.info("Get product L3 detail with productSku = {}", new Object[] {productSku});
    try {
      ProductAndItemsVO productAndItemsVO =
          this.productService.getProductDetailsByProductSku(storeId, requestId, username, productSku);
      ProductL3Response productL3Response = this.modelConverter.convertToProductL3Response(productAndItemsVO);
      List<String> promoLabels = productService.getPromoLabels(productAndItemsVO, productL3Response);
      productL3Response.setPromoLabels(promoLabels);
      if (sharedProductDetailsFromDb) {
        productL3Response.setProductEditable(!productService.isSharedProduct(storeId,
            productL3Response.getProductCode(), false, productL3Response.getMerchantCode()).getResult());
      } else {
        long productCount = productService.getProductL3CountByProductCode(productL3Response.getProductCode());
        productL3Response.setProductEditable(productCount > 1 ? false : true);
      }
      return new GdnRestSingleResponse<>(null, null, true, productL3Response, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductController.LOG
          .error("Getting product details by product sku failed productSku : {} , {}", productSku, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_BY_PRODUCT_SKU.getCode(),
          false, null, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("Getting product details by product sku failed productSku : {} , {}", productSku, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_BY_PRODUCT_SKU.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_TYPE_BY_PRODUCT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product type by product code", description = "get product type by product "
    + "code")
  public GdnRestSingleResponse<ProductTypeResponse> getProductDetailsByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable(value = "productCode") String productCode) {
    ProductController.LOG.info("Get product type detail with productCode = {}", new Object[] {productCode});
    try {
      ProductTypeResponse productTypeResponse =
          this.productService.getProductTypeByProductCode(storeId, requestId, username, productCode);
      return new GdnRestSingleResponse<>(null, null, true, productTypeResponse, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("get product type by product code failed productCode : {} , {}", productCode, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_TYPE_BY_PRODUCT_CODE.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.BULK_UPDATE_OFF2ON_ACTIVE_FLAG_BY_PRODUCT_SKUS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update off2on active flag", description = "Update off2on active flag")
  public GdnRestSingleResponse<SimpleListStringResponse> bulkUpdateOff2OnActiveFlagByProductSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(required = false) boolean updateOff2OnHistory,
      @RequestBody SimpleStringBooleanMapRequest request) {
    try {
      List<String> productSkus =
          this.productService.updateOff2OnFlagByProductSkus(storeId, request.getStringBooleanMap(), username, channelId, clientId, requestId, updateOff2OnHistory);
      return new GdnRestSingleResponse<>(null, null, true, new SimpleListStringResponse(productSkus), requestId);
    } catch (Exception e) {
      ProductController.LOG.error("Error while Updating off2On active flag for requestId: {} ", requestId, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false,
          new SimpleListStringResponse(
              request.getStringBooleanMap().entrySet().stream().map(Entry::getKey).collect(Collectors.toList())),
          requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.PRODUCT_ARCHIVE}, method = {
    RequestMethod.POST}, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "archive product", description = "archiving product")
  public GdnBaseRestResponse toggleArchiveProduct(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam Boolean doArchive,
    @PathVariable("productSku") String productSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku),
      "Product sku can not be empty or blank");
    try {
      EditItemResponse editItemResponse =
          this.productService.toggleArchiveProduct(storeId, username, productSku, doArchive,
              doArchive ? Constants.SOURCE_PRODUCT_ARCHIVAL : StringUtils.EMPTY);
      if (Objects.nonNull(editItemResponse.getApiErrorCode())) {
        LOG.error("Error while updating the edit info for productSku : {} and error code : {}",
          productSku, editItemResponse);
        return new GdnBaseRestResponse(editItemResponse.getApiErrorCode().getDesc(),
          editItemResponse.getApiErrorCode().getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      ProductController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
        ProductErrorCodesEnum.ARCHIVE_PRODUCT.getCode(), false, requestId);

    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_COUNT_BY_CATEGORIES, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get count of product by category", description = "get count of product by "
      + "category")
  public GdnRestSimpleResponse<Long> getProductsCountByCategory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String merchantCode, @RequestBody CategoryBrandRequest categoryBrandRequest) {
    ProductController.LOG.info("Get count of product by categories with storeId:{}", storeId);
    try {
      Long productCount = this.productService.getProductsCountByCategory(storeId, categoryBrandRequest, merchantCode);
      return new GdnRestSimpleResponse<Long>(null, null, true, requestId, productCount);
    } catch (Exception e) {
      ProductController.LOG.error("getCountProductsByCategory with category & brand :{}", categoryBrandRequest.toString(), e);
      return new GdnRestSimpleResponse<Long>(e.getMessage(), null, false, requestId,null);
    }
  }


  @RequestMapping(value = ProductApiPath.UPDATE_EDITED_PRODUCT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product", description = "update product excluding synchronization")
  public GdnBaseRestResponse updateEditedProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "false") boolean updateCategory,
      @RequestBody ProductEditRequest productEditRequest) {
    ProductController.LOG.info("Update edited product with productRequest = {}", new Object[] {productEditRequest});
    try {
      Product product = null;
      if (Objects.nonNull(productEditRequest.getProductRequest())) {
        product = this.modelConverter.convertToProduct(productEditRequest.getProductRequest());
      }
      Map<String, ItemViewConfig> itemViewConfigMap = new HashMap<>();
      if (!CollectionUtils.isEmpty(productEditRequest.getItemViewConfigAndItemSkuListRequest())) {
        itemViewConfigMap =
            this.modelConverter.convertToItemViewConfigMap(productEditRequest.getItemViewConfigAndItemSkuListRequest());
      }
      this.productService.updateEditedProduct(requestId, product, itemViewConfigMap, updateCategory,
          false, null, productEditRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG
          .error("#updateSpecialAttributes failed for productSku : {} ", productEditRequest.getProductSku(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_PRODUCT_SPECIAL_ATTRIBUTES_FAILED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ACTIVATE_NEED_CORRECTION, method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product on need correction", description = "update product on need "
    + "correction")
  public GdnRestSimpleResponse<ActivateNeedRevisionResponse> activateOnNeedCorrection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody NeedCorrectionProductActivationRequest activationRequest) {
    ProductController.LOG.info("Update product on need correction = {}", new Object[] {activationRequest});
    try {
      ActivateNeedRevisionResponse activateNeedRevisionResponse =
          productService.activateProductAndItemsOnNeedCorrection(storeId, username, requestId, activationRequest);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, activateNeedRevisionResponse);
    } catch (Exception e) {
      ProductController.LOG.error(e.getMessage(), e);
      return new GdnRestSimpleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.ACTIVATE_PRODUCT_NEED_CORRECTION.getCode(), false, requestId, null);
    }
  }

  @RequestMapping(value = ProductApiPath.SCHEDULER_TO_RETRY_PUBLISH, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to retry publish", description = "Scheduler to retry publish")
  public GdnBaseRestResponse retryPublishScheduler(@RequestParam String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    ProductController.LOG.info("Scheduler to retry publish : {}", requestId);
    this.productRetryEventPublishService.schedulerRetryPublish(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.DELETE_ARCHIVED_PRODUCT_DATA, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to delete archived product data", description = "Scheduler to delete "
    + "archived product data")
  public GdnBaseRestResponse deleteArchivedProductData(@RequestParam String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    ProductController.LOG.info("Scheduler to delete archived product data : {}", requestId);
    this.productDataArchivalService.deleteProductArchivedData(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.MAKE_PRODUCT_BUNDLE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "make product bundle", description = "make product bundle")
  public GdnBaseRestResponse makeProductBundle(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("itemSku") String itemSku, @RequestBody ProductBundleCreationRequest productBundleCreationRequest) {
    try {
      this.productService.makeProductBundleFromBundleRecipe(storeId, itemSku, productBundleCreationRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductController.LOG.error("product bundle creation failed for productSku :{}", itemSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.BUNDLE_CREATION_FAILED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.RECONCILE_PRODUCT_VARIANTS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "reconcile product variant info", description = "reconcile product variant info")
  public GdnRestListResponse<ItemPickupPointCodeResponse> reconcileProductVariants(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("productSku") String productSku,
      @RequestBody AddDeleteVariantRetryRequest addDeleteVariantRetryRequest) {
    log.info("Reconcile product data request : {} ", addDeleteVariantRetryRequest);
    try {
      List<ItemPickupPointCodeResponse> itemPickupPointCodeResponses =
          this.productService.reconcileProductVariants(storeId, requestId, username, addDeleteVariantRetryRequest);
      return new GdnRestListResponse<>(null, null, true, itemPickupPointCodeResponses,
          new PageMetaData(0, itemPickupPointCodeResponses.size(), itemPickupPointCodeResponses.size()), requestId);
    } catch (Exception e) {
      ProductController.LOG.error("error while reconcile product variants : {} ", addDeleteVariantRetryRequest, e);
      return new GdnRestListResponse<>(e.getMessage(),
          ProductErrorCodesEnum.PRODUCT_VARIANT_RECONCILIATION_FAILED.getCode(), false, new ArrayList<>(),
          new PageMetaData(0, 0, 0), requestId);
    }
  }


  @RequestMapping(value = ProductApiPath.SHARED_PRODUCT, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "API to check whether product is shared or not", summary = "API to check whether product is shared or not")
  public GdnRestSingleResponse<SimpleBooleanResponse> isSharedProduct(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @PathVariable(value = "sellerCode") String sellerCode,
    @PathVariable("productCode") String productCode,
    @RequestParam(required = false, defaultValue = "false") boolean findByProductSku) {
    log.info("Checking product is shared or not {} ", productCode);
    return new GdnRestSingleResponse<>(null, null, true,
      productService.isSharedProduct(storeId, productCode, findByProductSku, sellerCode),
      requestId);
  }

  @PostMapping(value = ProductApiPath.GET_CAMPAIGN_STATUS_BY_BUSINESS_PARTNER_PRODUCT_SKU_MAP,
               produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "API to check whether product "
      + "is in campaign or not", summary = " API  to check whether product is in campaign or not")
  public GdnRestSingleResponse<PromoEligibilityResponse> isPromoItemAvailable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody PromoEligibilityRequest promoEligibilityRequest) {
    log.info("isPromoItemAvailable for :  {} ", promoEligibilityRequest);
    return new GdnRestSingleResponse<>(null, null, true,
        productService.isPromoItemAvailable(storeId,
            promoEligibilityRequest.getBusinessPartnerAndProductSkuList()), requestId);
  }
}
