package com.gdn.partners.pcu.external.web.controller;

import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.model.SortOrder;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ProductApiPath;
import com.gdn.partners.pcu.external.properties.EstimatedPriceProperties;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.CategoryService;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.ProductL3Service;
import com.gdn.partners.pcu.external.service.ProductLevel3WipService;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.external.web.controller.util.ExcelTemplateUtil;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkProductSkuRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistoryUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ListingUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceChangeCompatibleRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SubmitEvidenceIPRWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpcStatusWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.WholeSaleDetailListWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.BrandPredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BulkPendingRequestsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerPickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CogsValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistoryUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventoryWarehouseStockWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemL5ListingResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.LogAuditTrailUpdatedProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderPlacedWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PriceChangeCompatibleResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemNameWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3MasterWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductSystemParameterSwitchWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoUpdateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SimpleCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UnifiedBulkDownloadWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UniquePickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesaleCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoResponse;
import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.validator.constraints.NotBlank;
import org.hibernate.validator.constraints.NotEmpty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;


@Slf4j
@Tag(name = "Product API")
@RestController
@RequestMapping(value = ProductApiPath.BASE_PATH)
@Validated
public class ProductController {

  private static final String BLANK_TYPE_ERR = "type request parameter cannot be blank";
  private static final String BLANK_SEARCH_KEY_ERR = "search key request parameter cannot be blank";
  private static final String BLANK_PRODUCT_BUSINESS_PARTNER_ID_ERR =
      "product business partner ID request parameter cannot be blank";
  private static final String BLANK_GDN_SKU_ERR = "Gdn Sku request parameter cannot be blank";
  private static final String ACTIVE = "ACTIVE";
  private static final String XLSX_EXTENSION = ".xlsx";

  @Autowired
  private ProductService productService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private EstimatedPriceProperties estimatedPriceProperties;

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private ProductL3Service productL3Service;

  @Autowired
  private GCSProperties gcsProperties;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${validate.business.partner.code.for.security.enabled}")
  private boolean validateBusinessPartnerCodeForSecurityEnabled;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Operation(summary ="Get Estimated Price By ItemCode")
  @PostMapping(value = ProductApiPath.GET_ESTIMATED_PRICE_BY_ITEM_CODES, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<EstimateItemPriceWebResponse> getEstimatedPriceByItemCodes(
      @RequestBody List<String> itemCodes) {
    if (CollectionUtils.isNotEmpty(itemCodes)) {
      List<EstimateItemPriceWebResponse> responseList =
          productService.getEstimatedPriceByItemCodes(itemCodes, estimatedPriceProperties.getLowestPriceCoefficient());
      return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
          responseList, new Metadata());
    } else {
      return new ListBaseResponse<EstimateItemPriceWebResponse>(ErrorMessages.ERR_ITEM_CODE_NULL,
          ErrorCategory.INVALID_FORMAT.getMessage(), false,
          mandatoryParameterHelper.getRequestId(), Collections.emptyList(), new Metadata());
    }
  }

  @Operation(summary ="Product Detail API")
  @GetMapping(value = ProductApiPath.INFO, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductDetailWebResponse> getProductInfo(@PathVariable String productId) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : Get product info by product Id : {}", productId);
    ProductDetailWebResponse productDetailWebResponse = productService.getProductDetailsByProductId(productId);
    return new SingleBaseResponse<>(null, null, true, requestId, productDetailWebResponse);
  }

  @Operation(summary ="Product Item Details by product name and category codes API")
  @PostMapping(value = ProductApiPath.FILTER_BY_PRODUCT_NAME_AND_CATEGORY_CODES, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductItemDetailWebResponse> getProductItemByNameAndCategoryCodes(
      @RequestParam("productName") @Valid @NotBlank String productName, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody @Valid List<String> categoryCodes) {
    String requestId = mandatoryParameterHelper.getRequestId();
    boolean isOnlyExternal = Boolean.valueOf(mandatoryParameterHelper.isExternalOnly());
    log.info("Method : Get product items info by product name : {} and category codes : {}", productName, categoryCodes);
    Page<ProductItemDetailWebResponse> response =
        productService.getProductItemsByKeywordAndCategoryCodes(productName, categoryCodes, page, size, isOnlyExternal);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Product Item Details by upc code and category Ids API")
  @PostMapping(value = ProductApiPath.FILTER_BY_UPC_CODE_AND_CATEGORY_IDS, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductItemDetailWebResponse> getProductItemByUPCCodeAndCategoryIds(
      @RequestParam("upcCode") @Valid @NotBlank String upcCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody @Valid @NotEmpty List<String> categoryIds) {
    String requestId = mandatoryParameterHelper.getRequestId();
    boolean isOnlyExternal = Boolean.valueOf(mandatoryParameterHelper.isExternalOnly());
    log.info("Method : Get product items info by UPC Code : {} and category Ids : {}", upcCode, categoryIds);
    Page<ProductItemDetailWebResponse> response =
        productService.getProductItemsByUPCCodeAndCategoryIds(upcCode, categoryIds, page, size, isOnlyExternal);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Create Product using flow1")
  @PostMapping(value = ProductApiPath.CREATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public SingleBaseResponse<CreateProductResponse> createProduct(@RequestParam(required = false) String flowType,
      @RequestBody ProductCreationWebRequest request)
      throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Create product request with requestId : {}, productCode : {} for merchantCode : {}",
        mandatoryParameterHelper.getRequestId(), request.getProductCode(),
        request.getBusinessPartnerCode());
    request.setExternalUser(Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly()));
    CreateProductResponse response =
        productService.createProduct(mandatoryParameterHelper.getUsername(),
            ConverterUtil.toProductCreationRequest(request, mandatoryParameterHelper, false, false), businessPartnerCode, flowType);
    return new SingleBaseResponse<>(null, null, true,
        mandatoryParameterHelper.getRequestId(), response);
  }

  @Operation(summary ="Create Product using flow1 V2")
  @PostMapping(value = ProductApiPath.ADD, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public SingleBaseResponse<CreateProductResponse> createProductV2(@RequestParam(required = false) String flowType,
      @RequestBody ProductCreationWebRequest request)
      throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    request.setExternalUser(Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly()));
    log.info("Create product V2 request with requestId : {}, productCode : {} for merchantCode : {}",
        mandatoryParameterHelper.getRequestId(), request.getProductCode(),
        request.getBusinessPartnerCode());
    CreateProductResponse response = productService
        .createProductV2(mandatoryParameterHelper.getUsername(),
            ConverterUtil.toProductCreationRequest(request, mandatoryParameterHelper, true, ranchIntegrationEnabled),
            businessPartnerCode, flowType);
    return new SingleBaseResponse<>(null, null, true,
        mandatoryParameterHelper.getRequestId(), response);
  }

  @Operation(summary ="Category suggestions")
  @GetMapping(value = ProductApiPath.GET_CATEGORY_SUGGESTIONS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<CategorySuggestionWebResponse> getCategorySuggestions(@RequestParam String keyword,
      @RequestParam(required = false) boolean isUPCCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "100") Integer size) {
    Pageable pageRequest = PageRequest.of(page, size);
    log.info("Method : Get Category suggestions by keyword : {}", keyword);
    String requestId = mandatoryParameterHelper.getRequestId();
    boolean isOnlyExternal = Boolean.valueOf(mandatoryParameterHelper.isExternalOnly());
    return productService.getCategorySuggestions(keyword, pageRequest, requestId, isOnlyExternal);
  }

  @Operation(summary ="Category suggestions")
  @GetMapping(value = ProductApiPath.GET_TOP_CATEGORY_SUGGESTIONS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<List<SimpleCategoryWebResponse>> getTopCategorySuggestions(@RequestParam String keyword,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "5") Integer size) {
    Pageable pageRequest = PageRequest.of(page, size);
    log.info("Method : Get Top Category suggestions by keyword : {}", keyword);
    String requestId = mandatoryParameterHelper.getRequestId();
    List<List<SimpleCategoryWebResponse>> response =
        productService.getTopCategorySuggestions(keyword, pageRequest, requestId);
    return new ListBaseResponse<>(null, null, true, requestId, response,
        new Metadata(page, response.size(), (long) size));
  }

  @Operation(summary ="Product item suggestions by item name and category id")
  @GetMapping(value = ProductApiPath.FILTER_BY_ITEM_NAME_AND_CATEGORY_ID, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductItemDetailWebResponse> getProductItemSuggestionsByItemNameAndCategoryId(
      @RequestParam("categoryId") @Valid @NotBlank(message = "Category Id cannot be blank") String categoryId,
      @RequestParam("productName") @Valid @NotBlank(message = "productName cannot be blank") String productName,
      @RequestParam(value = "page", defaultValue = "0") Integer page,
      @RequestParam(value = "size", defaultValue = "10") Integer size) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : Get product suggestions by name : {} and category Id : {}", productName, categoryId);
    List<ProductItemDetailWebResponse> response =
        productService.getProductItemSuggestionsByItemNameAndCategoryId(productName, categoryId, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response, new Metadata());
  }

  @Operation(summary ="Get products count")
  @GetMapping(value = ProductApiPath.GET_PRODUCTS_COUNT, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Integer> getProductsCount(@RequestParam("viewable") boolean viewable) {
    String requestId = mandatoryParameterHelper.getRequestId();
    int response = productService.getProductsCount(viewable);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="Get Level 3 product and Level 4 item detail")
  @GetMapping(value = ProductApiPath.DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductLevel3WipDetailWebResponse> getProductDetailByProductSku(
      @PathVariable("productSku") String productSku, @RequestParam(required = false) boolean isActive) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    String requestId = mandatoryParameterHelper.getRequestId();
    ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse =
        productLevel3WipService.findProductDetailByProductSku(productSku, isActive);
    return new SingleBaseResponse<>(null, null, true, requestId, productLevel3WipDetailWebResponse);
  }

  @Operation(summary ="Get products by merchant and category codes API")
  @PostMapping(value = ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ActiveProductWebResponse> getProductsByMerchantAndCategoryCode(
      @RequestBody ActiveProductWebRequest activeProductWebRequest) {
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : Get active products by merchant : {} and category code : {}", merchantCode,
        activeProductWebRequest.getCategoryCode());
    Page<ActiveProductWebResponse> response =
        productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest, merchantCode);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(activeProductWebRequest.getPage(), activeProductWebRequest.getSize(),
            response.getTotalElements()));
  }

  @Operation(summary = "V2 api to fetch product List based on the filters provided")
  @PostMapping(value = ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODE_V2, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductL3ListingWebResponse> getProductsByMerchantAndCategoryCodeV2(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "25") Integer size,
      @RequestBody ProductL3ListingWebRequest webRequest) throws Exception {
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : Get products by merchant and categoryCodes v2 : {} and request : {}", merchantCode,
        webRequest.getCategoryCodes());
    Page<ProductL3ListingWebResponse> response =
        productService.getProductListByMerchantAndCategoryCodeV2(webRequest, merchantCode, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Get suspended items by merchant and category codes API")
  @PostMapping(value = ProductApiPath.GET_SUSPENDED_ITEMS, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<SuspensionWebResponse> getSuspendedItems(@RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "25") Integer size, @RequestBody SuspensionWebRequest suspensionWebRequest) {
    suspensionWebRequest
        .setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : Get suspended items by merchant : {} and category code : {}",
        suspensionWebRequest.getBusinessPartnerCode(), suspensionWebRequest.getCategoryCode());
    Page<SuspensionWebResponse> response =
        productService.getSuspendedItemListByMerchantAndCategoryCode(suspensionWebRequest, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Fetch the product counts")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductCountWebResponse> getProductCounts(@RequestParam @NotBlank String type) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : Get product counts for the type: {} and merchant code: {}", type, merchantCode);
    ProductCountWebResponse response = productService.getProductCounts(type, merchantCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @PostMapping(value = ProductApiPath.GET_INACTIVE_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductLevel3SummaryResponse> getInactiveProducts(
      @RequestParam("type") @Valid @NotBlank(message = BLANK_TYPE_ERR) String type,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @RequestBody InActiveProductWebRequest request) {
    request.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    log.info("Get inactive items of type : {} by merchant : {}", type, request.getBusinessPartnerCode());
    Page<ProductLevel3SummaryResponse> response = productService
        .findSummaryByFilter(request, PageRequest.of(page, size),
            new SortOrder(request.getSortBy(), request.getOrderBy()), type);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Get the active item list")
  @PostMapping(value = ProductApiPath.GET_ACTIVE_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductLevel3SummaryWebResponse> getActiveProducts(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @RequestBody ActiveProductWebRequest request) {
    request.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    log.info("Get active items for merchant : {}", request.getBusinessPartnerCode());
    Page<ProductLevel3SummaryWebResponse> response =
        productService.getActiveProductList(request, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @PostMapping(value = ProductApiPath.GET_INPROCESS_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<InProcessWebResponse> getInprocessProducts(@RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "50") Integer size, @RequestBody InProcessProductWebRequest request) {
    request.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    log.info("Get in-process products for merchant : {}", request.getBusinessPartnerCode());
    Page<InProcessWebResponse> response = productService
        .getInprocessProductList(request, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Get the active product name suggestions/search")
  @GetMapping(value = ProductApiPath.GET_ACTIVE_NAME_SUGGESTIONS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ItemDetailWebResponse> getActiveProductsNameList(@RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "50") Integer size, @RequestParam(required = false) boolean isProductName,
      @RequestParam("searchKey") @Valid @NotBlank(message = BLANK_SEARCH_KEY_ERR) String searchKey,
      @RequestParam(required = false, defaultValue = "true") boolean inStock) {
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Get active items name suggestions for merchant : {} and for searchKey; {}", merchantCode, searchKey);
    Page<ItemDetailWebResponse> response =
        productService.getActiveProductNameList(searchKey, merchantCode, isProductName, PageRequest.of(page, size), inStock);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="To archive/unarchive the list of itemSkus")
  @PostMapping(value = ProductApiPath.TOGGLE_ARCHIVE_ITEMS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse toggleArchiveItems(@RequestParam("doArchive") boolean doArchive,
      @RequestBody @Valid @NotEmpty List<String> itemSkus) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : toggle archive items for itemSkus : {} , doArchive : {}", itemSkus, doArchive);
    productService.toggleArchiveItems(itemSkus, doArchive, businessPartnerCode);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="Get the product update audit logs")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_UPDATE_AUDIT_LOGS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<LogAuditTrailUpdatedProductWebResponse> getProductUpdateAuditLogs(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @RequestParam("gdnSku") @Valid @NotBlank(message = BLANK_GDN_SKU_ERR) String gdnSku) throws ParseException {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(gdnSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    log.info("invoking product product update logs at controller. GdnSku : {}", gdnSku);
    Page<LogAuditTrailUpdatedProductResponse> productUpdateLogs =
        this.productService.getProductUpdateLogs(gdnSku, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        ResponseHelper.toLogAuditTrailUpdatedProductWebResponse(productUpdateLogs.getContent()),
    new Metadata(page, size, productUpdateLogs.getTotalElements()));
  }

  @Operation(summary ="API to view the product details in external")
  @GetMapping(value = ProductApiPath.GET_ITEM_SKU_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductLevel3MasterWebResponse> filterDetailByGdnSku(
      @PathVariable("itemSku") String itemSku) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(itemSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    log.info("Method : API to view product details for itemSku : {}", itemSku);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        productService.findDetailByGdnSku(businessPartnerCode, itemSku);
    return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        productLevel3MasterWebResponse);
  }

  @Operation(summary ="API to unsynchronize a product by productSku and itemSku")
  @PutMapping(value = ProductApiPath.UNSYNCHRONIZE_PRODUCT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse unsynchronize(@PathVariable("productSku") String productSku,
      @PathVariable("itemSku") String itemSku) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    GdnPreconditions.checkArgument(itemSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    log.info("Method : API to unsynchronize product for productSku : {} and itemSku : {}", productSku, itemSku);
    productService.unsynchronizeProduct(businessPartnerCode, productSku, itemSku);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to synchronize a product by productSku and itemSku")
  @PutMapping(value = ProductApiPath.SYNCHRONIZE_PRODUCT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse synchronize(@PathVariable("productSku") String productSku,
      @PathVariable("itemSku") String itemSku) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    GdnPreconditions.checkArgument(itemSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    log.info("Method : API to synchronize product for productSku : {} and itemSku : {}", productSku, itemSku);
    productService.synchronizeProduct(businessPartnerCode, productSku, itemSku);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to bulk delete the in-progress products")
  @PostMapping(value = ProductApiPath.DELETE_IN_PROGRESS_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<PostLiveProductCountResponse> bulkDeleteProductWip(
      @RequestBody BulkDeleteProductWipRequest request) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions
        .checkArgument(Objects.nonNull(request) && CollectionUtils.isNotEmpty(request.getProductLevel1Ids()),
            ErrorMessages.ERR_EMPTY_PRODUCT_LIST);
    log.info("Method : Bulk delete product WIP for request : {} and business partner code: {}", request,
        businessPartnerCode);
    PostLiveProductCountResponse response = productService.bulkDeleteProductWip(businessPartnerCode, request);
    return new SingleBaseResponse<>(null, null,
        true, mandatoryParameterHelper.getRequestId(), response);
  }

  @Operation(summary ="API to update the item skus of a page")
  @PostMapping(value = ProductApiPath.UPDATE_BULK_SUMMARY, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Map<String, ProductLevel3SummaryResponse>> updateBulkSummary(
      @RequestBody Map<String, ProductLevel3UpdateSummaryRequest> request) {
    GdnPreconditions.checkArgument(Objects.nonNull(request) && !CollectionUtils.sizeIsEmpty(request),
        ErrorMessages.ERR_EMPTY_PRODUCT_LIST);
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : update bulk summary for request: {}", request);
    Map<String, ProductLevel3SummaryResponse> response = productService.updateBulkSummary(businessPartnerCode, request);
    return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary ="API to update the item skus using excel")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateTemplateBulkUpdate(@RequestParam MultipartFile request,
      @RequestParam("isOnlyExternalUser") boolean isOnlyExternalUser) throws IOException {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(ExcelTemplateUtil.isInExcelMimeType(request), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
    log.info("invoking upload bulk update product template at controller. Request : {}", request);
    this.bulkProcessService.uploadBulkUpdate(businessPartnerCode, username, isOnlyExternalUser, request);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to update the ean/upc code using excel")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_EAN_UPDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateTemplateBulkUpdateEan(@RequestParam MultipartFile request) throws IOException {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(ExcelTemplateUtil.isInExcelMimeType(request), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
    log.info("invoking upload bulk update EAN / UPC template at controller. Request : {}", request);
    this.bulkProcessService.uploadBulkUpdateEAN(businessPartnerCode, username, request);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary = "API to update master info using bulk")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_MASTER_INFO, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateTemplateBulkUpdateMasterInfo(
      @RequestBody BulkBasicInfoWebRequest request) throws IOException {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(!request.getFileName().isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(request.getFileName().toLowerCase().endsWith(XLSX_EXTENSION), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
    log.info("invoking upload bulk update product master info template at controller, request : {}", request);
    this.bulkProcessService.uploadBulkUpdateMasterInfo(businessPartnerCode, username, request);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to upload a file")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateTemplateBulkUpload(@RequestParam MultipartFile request) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    log.info("Uploading the file at controller. Request : {}", request);
    if (gcsProperties.isEnabled()) {
      fileStorageService.uploadFileToGcs(request, username);
    }
    String filename = ExcelTemplateUtil.updateTemplateBulkUpload(request, username, gcsProperties.isEnabled());
    return new BaseResponse(filename, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to upload a VAT file")
  @PostMapping(value = ProductApiPath.UPLOAD_SUBJECT_TO_VAT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateSubjectToVat(@RequestParam MultipartFile request) throws Exception {
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    log.info("Uploading the VAT file at controller. Request : {}", request);
    String bulkProcessCode = UUID.randomUUID().toString();
    String filename = fileStorageService
        .uploadSubjectToVatFile(request, bulkProcessCode);
    bulkProcessService
        .uploadBulkSubjectToVatSkus(mandatoryParameterHelper.getBusinessPartnerCode(), filename,
            bulkProcessCode);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to get moving price from cogs value for multivariant products")
  @GetMapping(value = ProductApiPath.GET_COGS_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<CogsValueWebResponse> getCogsValue(@PathVariable("materialCode") String materialCode)
      throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Get moving price from cogs value for multivariant products, materialCode: {} ", materialCode);
    CogsValueWebResponse cogsValueWebResponse = new CogsValueWebResponse();
    Double cogsValue = productService.getCogsValue(materialCode, businessPartnerCode);
    if (Objects.nonNull(cogsValue)) {
      cogsValueWebResponse.setCogsValue(cogsValue);
      return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
          cogsValueWebResponse);
    } else {
      return new SingleBaseResponse<>(null, null, false, mandatoryParameterHelper.getRequestId(),
          null);
    }
  }

  @Operation(summary ="API to retry the activation of in-process product")
  @PostMapping(value = ProductApiPath.RETRY_CREATE_WIP, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse retryCreateWIP(@RequestBody List<String> productBusinessPartnerIds) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productBusinessPartnerIds),
        ErrorMessages.ERR_EMPTY_PRODUCT_BUSINESS_PARTNER_LIST);
    log.info("Method : retry create for productBusinessPartnerIds : {}", productBusinessPartnerIds);
    productService.retryCreate(productBusinessPartnerIds);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary = "API to upload all the files")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_UPLOAD_ALL, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse uploadTemplateBulkUploadProductWip(
    @RequestParam(required = false) String categoryCode, @RequestBody List<String> fileNames,
    @RequestParam(required = false) String processType) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(!fileNames.isEmpty(), ErrorMessages.EMPTY_FILE);
    if (StringUtils.isNotBlank(processType)) {
      GdnPreconditions.checkArgument(
        Constants.VALID_PROCESS_TYPE_FOR_CREATION.contains(processType),
        String.format(ErrorMessages.CREATION_INVALID_PROCESS_TYPE, processType,
          Constants.VALID_PROCESS_TYPE_FOR_CREATION));
    }
    log.info("Method : upload template bulk upload all for files : {} with processType : {} ",
      fileNames, processType);
    bulkProcessService.uploadExcelFile(businessPartnerCode, username, fileNames, processType);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary = "API to upload external template files")
  @PostMapping(value = ProductApiPath.EXTERNAL_BULK_UPLOAD_ALL, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse uploadExternalTemplate(@RequestBody Map<String, String> files,
      @RequestParam String zipFileName, @RequestParam(required = false) String pickupPointCode,
      @RequestParam String bulkProcessCode) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(bulkProcessCode),ErrorMessages.PROCESS_CODE_CANNOT_BE_EMPTY);
    log.info(
        "Method: external upload all for files : {} with pickupPointCode : {} and bulkProcessCode"
            + " : {} ", files, pickupPointCode, bulkProcessCode);
    bulkProcessService.uploadExternalFiles(businessPartnerCode, username, zipFileName, files,
        pickupPointCode, bulkProcessCode);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to delete a file")
  @PostMapping(value = ProductApiPath.DELETE_TEMPLATE_BULK_DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deleteBulkTemplate(@RequestParam @NotBlank(message = ErrorMessages.INVALID_REQUEST) String fileName) throws IOException {
    String username = mandatoryParameterHelper.getUsername();
    log.info("Deleting the file at controller. Filename : {}", fileName);
    String filename = ExcelTemplateUtil.deleteBulkTemplate(fileName, username);
    return new GdnBaseRestResponse(filename, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to delete a folder")
  @PostMapping(value = ProductApiPath.DELETE_ALL_TEMPLATE_BULK_DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deleteAllTemplateBulkProductFile() throws IOException {
    String username = mandatoryParameterHelper.getUsername();
    log.info("Deleting the folder at controller for username  : {}", username);
    ExcelTemplateUtil.deleteFolder(username);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to download selected products")
  @PostMapping(value = ProductApiPath.DOWNLOAD_TEMPLATE_BULK_UPDATE)
  public void downloadTemplateBulkUpdate(HttpServletResponse servletResponse,
      @RequestParam("isOnlyExternalUser") boolean isOnlyExternalUser, @RequestBody BulkRequest request)
      throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(!request.getGdnSkus().isEmpty(), ErrorMessages.GDN_SKUS_MUST_NOT_BE_BLANK);
    log.info("Method: API to download selected products for business partner : {} and username : {} and gdnSkus ; {}",
        businessPartnerCode, username, request.getGdnSkus());
    productService
        .downloadTemplateBulkUpdate(username, businessPartnerCode, isOnlyExternalUser, request, servletResponse);
  }

  @Operation(summary ="API to download selected products by productSku")
  @PostMapping(value = ProductApiPath.DOWNLOAD_TEMPLATE_BULK_UPDATE_BY_PRODUCT_SKU)
  public void downloadTemplateBulkUpdateByProductSku(HttpServletResponse servletResponse,
      @RequestParam("isOnlyExternalUser") boolean isOnlyExternalUser,
      @RequestBody BulkProductSkuRequest bulkProductSkuRequest) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(bulkProductSkuRequest.getProductSkus()),
            ErrorMessages.GDN_SKUS_MUST_NOT_BE_BLANK);
    log.info("Method: API to download selected products for business partner : {} and username : {} and productSkus ; {}",
        businessPartnerCode, username, bulkProductSkuRequest.getProductSkus());
    productService
        .downloadTemplateBulkUpdateByProductSku(businessPartnerCode, isOnlyExternalUser,
            bulkProductSkuRequest.getProductSkus(), servletResponse, 0, 50);
  }

  @Operation(summary ="API to download multipickuppoint template")
  @PostMapping(value = ProductApiPath.DOWNLOAD_TEMPLATE_MULTIPICKUPPOINT)
  public void downloadTemplateMultiPickuppoint(HttpServletResponse servletResponse) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method: API to download multipickuppoint template for business partner : {} and username : {}",
        businessPartnerCode, username);
    productService.downloadTemplateForMultiPickupPointTemplate(businessPartnerCode, servletResponse);
  }

  @Operation(summary ="API to download all products as per the request")
  @PostMapping(value = ProductApiPath.DOWNLOAD_ALL_BULK_PRODUCTS)
  public GdnBaseRestResponse downloadAllBulkProducts(
    @RequestParam(required = false, defaultValue = "0") Integer productSize,
    @RequestParam("isOnlyExternalUser") boolean isOnlyExternalUser,
    @RequestBody ProductSummaryWebRequest request) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method: API to download all products for business partner : {} and request : {} and productSize : {}",
        businessPartnerCode, request, productSize);
    productService
        .downloadAllProduct(username, isOnlyExternalUser, businessPartnerCode, request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary ="API to download all products as per the request with EAN/UPC")
  @PostMapping(value = ProductApiPath.DOWNLOAD_ALL_BULK_PRODUCTS_EAN)
  public GdnBaseRestResponse downloadAllBulkProductsEANUPC(
          @RequestParam(required = false, defaultValue = "0") Integer productSize,
          @RequestParam("isOnlyExternalUser") boolean isOnlyExternalUser,
          @RequestBody ProductSummaryWebRequest request) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method: API to download all products with EAN/UPC for business partner : " +
                    "{} and request : {} and productSize : {}",
            businessPartnerCode, request, productSize);
    productService
            .downloadAllProductWithEAN(username, isOnlyExternalUser, businessPartnerCode, request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary ="API to download all products basic info as per the request")
  @PostMapping(value = ProductApiPath.DOWNLOAD_ALL_BULK_PRODUCTS_BASIC_INFO)
  public GdnBaseRestResponse downloadAllBulkBasicInfo(
      @RequestBody ProductSummaryWebRequest request) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method: API to download all products basic info for business partner : {} and request : {} ",
        businessPartnerCode, request);
    productService
        .downloadAllProductBasicInfo(username, businessPartnerCode, request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary ="API to get the stock web site")
  @GetMapping(value = ProductApiPath.STOCK_INFO_WEBSITE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductLevel3StockInfoWebSiteResponse> getStockInfoWebSite(
      @RequestParam("webMerchantCode") @NotBlank(message = ErrorMessages.ERR_MER_CODE_NULL) String webMerchantCode,
      @RequestParam("webItemSku") @NotBlank(message = ErrorMessages.ERR_ITEM_SKU_NULL) String webItemSku) {
    log.info("Method : get stock info web site for business partner : {} and item sku : {}", webMerchantCode,
        webItemSku);
    ProductLevel3StockInfoWebSiteResponse productLevel3StockInfoWebSiteResponse =
        productService.getStockInfoWebSite(webMerchantCode, webItemSku);
    return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        productLevel3StockInfoWebSiteResponse);
  }

  @Operation(summary ="API to download template for a category")
  @PostMapping(value = ProductApiPath.DOWNLOAD_PRODUCT_TEMPLATE)
  public void downloadProductTemplate(HttpServletResponse servletResponse,
      @RequestParam("categoryId") @NotBlank String categoryId) throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    boolean isOnlyExternal = Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly());
    log.info("Method: API to download template for business partner : {} and category ID : {}", businessPartnerCode,
        categoryId);
    productService.downloadProductTemplate(username, businessPartnerCode, categoryId, servletResponse, isOnlyExternal);
  }

  @Operation(summary ="API to get the stock web site")
  @GetMapping(value = ProductApiPath.CHECK_SYNC_STOCK_MODE_AND_PRODUCT_PERMISSION, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Boolean> checkSyncStockModeAndProductPermission() {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : check the sync stock mode and product permission for business partner : {}",
        businessPartnerCode);
    boolean response = productService.checkSyncStockModeAndProductPermission(businessPartnerCode);
    return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary ="API to get categories by category codes")
  @PostMapping(value = ProductApiPath.GET_CATEGORIES_BY_CATEGORY_CODES)
  public ListBaseResponse<CategoryWebResponse> getCategoriesByCategoryCodes(@RequestBody List<String> categoryCodes)
      throws Exception {
    GdnPreconditions.checkArgument(!categoryCodes.isEmpty(), ErrorMessages.CATEGORY_CODES_MUST_NOT_BE_EMPTY);
    log.info("Method: API to get the category details for category codes : {}", categoryCodes.toString());
    List<CategoryWebResponse> response = categoryService.getCategoriesByCategoryCodes(categoryCodes);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response, new Metadata());
  }

  @Operation(summary ="API to update the item sku")
  @PostMapping(value = ProductApiPath.UPDATE_SUMMARY, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductLevel3SummaryResponse> updateSummary(@RequestParam("gdnSku") String gdnSku,
      @RequestBody ProductLevel3UpdateSummaryRequest request) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(gdnSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(request.getPrices()) && Objects.nonNull(request.getPrices().get(0)),
            ErrorMessages.REQUIRED_PARAMETER);
    GdnPreconditions.checkArgument(request.getPrices().get(0).getPrice() >= request.getPrices().get(0).getSalePrice(),
        ErrorMessages.SELLING_PRICE_MUST_BE_LOWER_THAN_REGULAR_PRICE);
    log.info("Method : update summary for gdnSku : {} request: {}", gdnSku, request);
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        productService.updateSummary(gdnSku, businessPartnerCode, request);
    return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        productLevel3SummaryResponse);
  }

  @Operation(summary ="API to update the product details")
  @PutMapping(value = ProductApiPath.UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse update(@RequestBody ProductLevel3WebRequest productLevel3WebRequest)
      throws ApplicationRuntimeException {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productLevel3WebRequest.getProductSku().startsWith(businessPartnerCode),
        ErrorMessages.INVALID_GDN_SKU);
    GdnPreconditions
        .checkArgument(productLevel3WebRequest.getDescription().length() <= Constants.MAXIMUM_DESCRIPTION_LENGTH,
            ErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
    if (Objects.nonNull(productLevel3WebRequest.getUniqueSellingPoint())) {
      String uspWithoutTags = ConverterUtil.getFilterUSP(productLevel3WebRequest.getUniqueSellingPoint());
      GdnPreconditions.checkArgument(uspWithoutTags.length() <= Constants.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
          ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    String isOnlyExternal = mandatoryParameterHelper.isExternalOnly();
    log.info("Method : update for request: {}", productLevel3WebRequest);
    productService.updateProductDetails(productLevel3WebRequest, businessPartnerCode, isOnlyExternal);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API for archive products")
  @PostMapping(value = ProductApiPath.BULK_ARCHIVE_PRODUCTS_EXTERNAL)
  public GdnBaseRestResponse bulkArchiveItemSkus(@RequestParam MultipartFile request) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    boolean isSuccess = false;
    String returnMessage = "";
    try {
      GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
      GdnPreconditions
          .checkArgument(ExcelTemplateUtil.isInExcelMimeType(request), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
      log.info("invoking upload bulk archive product template at controller. Request : {}", request);
      this.bulkProcessService.uploadBulkUpdateForBulkArchive(username, businessPartnerCode, request);
      isSuccess = true;
    } catch (Exception e) {
      returnMessage = e.getMessage();
      log.error("error invoking upload bulk archive product template at controller. Request Id: {}", requestId, e);
    }
    return new GdnBaseRestResponse(returnMessage, null, isSuccess, requestId);
  }

  @Operation(summary ="API for updating Off2OnChannelActive for productSkus")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_UPDATE_IN_STORE)
  public GdnBaseRestResponse uploadTemplateForBulkUpdateInStoreFlag(@RequestParam MultipartFile request) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    boolean isSuccess = false;
    String returnMessage = "";
    try {
      GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
      GdnPreconditions
          .checkArgument(ExcelTemplateUtil.isInExcelMimeType(request), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
      log.info("invoking upload bulk archive product template at controller. Request : {}", request);
      this.bulkProcessService.uploadBulkUpdateForInStoreUpdate(username, businessPartnerCode, request);
      isSuccess = true;
    } catch (Exception e) {
      returnMessage = e.getMessage();
      log.error("error invoking upload bulk archive product template at controller. Request Id: {}", requestId, e);
    }
    return new GdnBaseRestResponse(returnMessage, null, isSuccess, requestId);
  }

  @Operation(summary ="API for image update")
  @PostMapping(value = ProductApiPath.IMAGE_UPDATE)
  public GdnBaseRestResponse updateImage(@RequestBody UpdateImageRequest request) throws Exception {
    log.info("invoking update product image at controller. Request : {}", request);
    this.productService.updateProductImage(request,
        Boolean.valueOf(mandatoryParameterHelper.isExternalOnly()));
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to fetch bulk related notes for promo updated products")
  @GetMapping(value = ProductApiPath.FETCH_PROMO_BULK_NOTES)
  public ListBaseResponse<PromoUpdateProductResponse> fetchBulkPromoNotes(
      @PathVariable("bulkProcessCode") String bulkProcessCode) {
    log.info("Fetching promo related notes for bulkProcessCode : {}", bulkProcessCode);
    List<PromoUpdateProductResponse> response = this.bulkProcessService.fetchPromoUpdatedProductNotes(bulkProcessCode);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response, new Metadata(0, response.size(), Long.valueOf(response.size())));

  }

  @Operation(summary ="Api to validate the price bulk update")
  @PostMapping(value = ProductApiPath.CHECK_PRICE_CHANGE_COMPATIBILITY)
  public ListBaseResponse<PriceChangeCompatibleResponse> checkPriceChangeCompatibility(
      @RequestBody List<PriceChangeCompatibleRequest> priceChangeCompatibleRequests) throws Exception {
    log.info("Validating price update for  : {}", priceChangeCompatibleRequests);
    String storeId = mandatoryParameterHelper.getStoreId();
    String requestId = mandatoryParameterHelper.getRequestId();
    List<PriceChangeCompatibleResponse> response =
        this.productService.getPriceChangeCompatibility(storeId, requestId, priceChangeCompatibleRequests);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response, new Metadata(0, response.size(), Long.valueOf(response.size())));
  }

  @Operation(summary ="Api to check wholesale promo status on bulk update")
  @PostMapping(value = ProductApiPath.GET_WHOLESALE_PROMO_STATUS)
  public ListBaseResponse<WholesalePromoResponse> getWholesalePromoStatus(@RequestBody List<WholeSaleDetailListWebRequest>
      wholeSaleDetailListWebRequests) throws Exception {
    log.info("Api to get wholesale promo status on bulk update : {}", wholeSaleDetailListWebRequests);
    String storeId = mandatoryParameterHelper.getStoreId();
    String requestId = mandatoryParameterHelper.getRequestId();
    List<WholesalePromoResponse> response = this.productService.getWholesalePromoStatus(storeId, requestId, wholeSaleDetailListWebRequests);
    return new ListBaseResponse<>(null, null, true, requestId, response,
        new Metadata(0, response.size(), Long.valueOf(response.size())));
  }

  @Operation(summary ="API to fetch wholesale related count")
  @GetMapping(value = ProductApiPath.FETCH_WHOLESALE_CONFIG_COUNT)
  public GdnRestSimpleResponse<WholesaleCountWebResponse> fetchWholeSaleConfigCount(
      @PathVariable("bulkProcessCode") String bulkProcessCode) {
    log.info("Fetching wholesale related notes for bulkProcessCode : {}", bulkProcessCode);
    WholesaleCountWebResponse response = this.bulkProcessService.fetchWholeSaleConfigCount(bulkProcessCode);
    return new GdnRestSimpleResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary ="API to check pending requests by BP Code")
  @GetMapping(value = ProductApiPath.CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE, produces = MediaType
      .APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BulkPendingRequestsWebResponse> checkPendingBulkProcessByMerchantCode(
      @RequestParam(required = false, defaultValue = Constants.BULK_UPLOAD_TYPE) String type,
      @RequestParam(required = false, defaultValue = StringUtils.EMPTY) String bulkProcessType) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Invoking bulk process method to check bulk update requests status by businessPartnerCode. "
        + "businessPartnerCode : {}", businessPartnerCode);
    BulkPendingRequestsWebResponse response =
        bulkProcessService.checkPendingBulkProcess(type, businessPartnerCode, bulkProcessType);
    return new SingleBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary ="API to check youtube url is active or not")
  @GetMapping(value = ProductApiPath.GET_YOUTUBE_URL_STATUS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<YouTubeAPIWebResponse> checkYouTubeUrlStatus(
      @RequestParam("youTubeUrl") @Valid @NotBlank(message = "Youtube url must not be blank") String youTubeUrl)
      throws Exception {
    log.info("Method : API to validate youtube url : {}", youTubeUrl);
    YouTubeAPIWebResponse response = this.productService.validateYouTubeUrl(youTubeUrl);
    return new SingleBaseResponse(null, null, true, mandatoryParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary ="API to fetch minimum price")
  @GetMapping(value = ProductApiPath.GET_MINIMUM_PRICE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<Integer> getMinimumPrice() {
    log.info("Invoking API to fetch minimum price");
    Integer response = productService.getMinimumPrice();
    return new GdnRestSimpleResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary ="Get brand predefined allowed attribute value based on brandCode, status and value")
  @GetMapping(value = ProductApiPath.GET_BRAND_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BrandPredefinedAttributeValueWebResponse> getBrandPredefinedAllowedAttributeValueDetail(
      @PathVariable("brandCode") String brandCode, @PathVariable("status") String status,
      @RequestParam("value") String value) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : get brand Predefined AllowedAttribute value id for status : {}, brandCode : {}, value : {}",
        status, brandCode, value);
    List<BrandPredefinedAttributeValueWebResponse> response =
        productService.getBrandPredefinedAllowedAttributeValueDetail(brandCode, status);
    return new ListBaseResponse<>(null, null, true, requestId, response,
        new Metadata(0, response.size(), (long) response.size()));
  }

  @Operation(summary ="API to update price and stock information of item by itemSku")
  @PostMapping(value = ProductApiPath.UPDATE_PRICE_AND_STOCK)
  public BaseResponse updatePriceAndStock(
      @RequestBody ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest) {
    log.info("Update price and stock information for item : {}", productPriceAndStockUpdateWebRequest.getItemSku());
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    return new BaseResponse(null, null,
        productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, businessPartnerCode),
        mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to download unified product template")
  @GetMapping(value = ProductApiPath.DOWNLOAD_UNIFIED_PRODUCT_TEMPLATE)
  public GdnRestSimpleResponse<UnifiedBulkDownloadWebResponse> downloadUnifiedProductTemplate() throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("API to download unified template for business partner : {}", businessPartnerCode);
    UnifiedBulkDownloadWebResponse unifiedBulkDownloadWebResponse = new UnifiedBulkDownloadWebResponse();
    boolean isSuccess = false;
    String returnMessage = StringUtils.EMPTY;
    try {
      unifiedBulkDownloadWebResponse = bulkProcessService.downloadProductUnifiedTemplate(businessPartnerCode);
      log.info("Template path formed {} ",unifiedBulkDownloadWebResponse.getFilePath());
      if(!unifiedBulkDownloadWebResponse.getFilePath().startsWith(Constants.ROOT)){
        unifiedBulkDownloadWebResponse
          .setFilePath(Constants.ROOT + unifiedBulkDownloadWebResponse.getFilePath());
      }
      isSuccess = true;
    } catch (Exception e) {
      returnMessage = e.getMessage();
      log.error("error invoking download unified bulk download template", e);
    }
    log.info("API to download unified template completed with Success status : {} ", isSuccess);
    return new GdnRestSimpleResponse<>(returnMessage, null, isSuccess,
        mandatoryParameterHelper.getRequestId(), unifiedBulkDownloadWebResponse);
  }

  @Operation(summary ="Get product score rules")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_SCORE_RULES, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductScoreRuleWebResponse> getProductScoreRules(
      @RequestParam(required = false, name = "categoryCode") String categoryCode) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : get product score rule for category: {}", categoryCode);
    ProductScoreRuleWebResponse productScoreRuleWebResponse = productService.getProductScoreRule(categoryCode);
    return new SingleBaseResponse<>(null, null, true, requestId, productScoreRuleWebResponse);
  }

  @Operation(summary ="API to get all switches from product system paramter")
  @GetMapping(value = ProductApiPath.GET_SYSTEM_PARAMETER_SWITCH)
  public SingleBaseResponse<ProductSystemParameterSwitchWebResponse> getSystemParamterSwitches() {
    log.info("Fetching switch values from PBP system parameter");
    return new SingleBaseResponse<>(null, null,
    true, this.mandatoryParameterHelper.getRequestId(), this.productService
        .getSystemParameterSwitches());
  }

  @Operation(summary ="API to get upcCode and images of all L4 mapped to L3")
  @GetMapping(value = ProductApiPath.GET_UPC_CODE_IMAGES)
  public SingleBaseResponse<UpcCodeAndImagesWebResponse> getUpcCodeAndImages(
      @RequestParam(required = true, name = "productSku") String productSku) {
    log.info("Fetching upcCode and images of all L4 mapped to L3");
    String storeId = mandatoryParameterHelper.getStoreId();
    String requestId = mandatoryParameterHelper.getRequestId();
    return new SingleBaseResponse<>(null, null, true, requestId,
        this.productService.getUpcCodeAndImages(storeId, requestId, productSku));
  }

  @Operation(summary ="API to edit the product info")
  @PutMapping(value = ProductApiPath.EDIT_PRODUCT_INFO, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<EditProductWebResponse> editProductInfo(@PathVariable("productSku") String productSku,
      @RequestBody ProductEditInfoWebRequest productEditInfoWebRequest) throws ApplicationRuntimeException {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productEditInfoWebRequest.getProductSku().startsWith(businessPartnerCode),
        ErrorMessages.INVALID_GDN_SKU);
    GdnPreconditions
        .checkArgument(productEditInfoWebRequest.getDescription().length() <= Constants.MAXIMUM_DESCRIPTION_LENGTH,
            ErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
    if (Objects.nonNull(productEditInfoWebRequest.getUniqueSellingPoint())) {
      String uspWithoutTags = ConverterUtil.getFilterUSP(productEditInfoWebRequest.getUniqueSellingPoint());
      GdnPreconditions.checkArgument(uspWithoutTags.length() <= Constants.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
          ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    log.info("Method : edit product info for productSku : {}", productSku);
    boolean isOnlyExternal = Boolean.valueOf(mandatoryParameterHelper.isExternalOnly());
    EditProductWebResponse editProductWebResponse =
        productService.editProductInfo(productEditInfoWebRequest, businessPartnerCode, isOnlyExternal);
    return new SingleBaseResponse<>(null, null, Objects.isNull(editProductWebResponse.getApiErrorCode()) ? true : false,
        mandatoryParameterHelper.getRequestId(), editProductWebResponse);
  }

  @Operation(summary ="Get all item list for l3")
  @PostMapping(value = ProductApiPath.GET_PRODUCT_VARIANTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductLevel3SummaryDetailsWebResponse> getProductLevel3Variants(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @RequestBody ProductLevel3VariantsWebRequest request) {
    request.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    log.info("Get all item list for l3 for merchant : {}", request.getBusinessPartnerCode());
    if (request.isNeedCorrection()) {
      GdnPreconditions
          .checkArgument(StringUtils.isNotEmpty(request.getProductCode()), ErrorMessages.ERROR_PRODUCT_CODE_NOT_EMPTY);
    }
    Page<ProductLevel3SummaryDetailsWebResponse> response =
        productService.getProductLevel3VariantList(request, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API to if successful order placed on the productSKU")
  @GetMapping(value = ProductApiPath.IS_NAME_EDITABLE)
  public SingleBaseResponse<OrderPlacedWebResponse> isNameEditable(@PathVariable("productSku") String productSku)
      throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    return new SingleBaseResponse<>(null, null, true, requestId,
        this.productService.checkSuccessfullOrderPlacedForProductSku(productSku));
  }

  @Operation(summary ="API to get pickup points without pinpoint for given L3")
  @GetMapping(value = ProductApiPath.GET_PINPOINT_STATUS_BY_L3)
  public SingleBaseResponse<String> getPinpointStatusByL3(@PathVariable("productSku") String productSku) {
    log.info("Get pickup points for L3 which have no geolocation mapped for product : {}", productSku);
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    return new SingleBaseResponse<>(null, null, true, requestId,
        this.productService.getPinpointStatusByProductSku(productSku, businessPartnerCode));
  }

  @Operation(summary ="API to fetch pickup-points by l3")
  @GetMapping(value = ProductApiPath.GET_PICKUP_POINT_CODES)
  public ListBaseResponse<PickupPointCodeWebResponse> getPickupPointsByProductSkuCode(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam(required = true, name = "productSku") String productSku,
      @RequestParam(defaultValue = "false", name = "needCorrection") boolean needCorrection,
      @RequestParam(defaultValue = "false", name = "fbbActivated") boolean fbbActivated) throws Exception {
    log.info(
        "Test Fetching pickup-point codes of all L4 mapped to L3 : {}, page : {}, size : {}, " + "needCorrection : {}, "
            + " fbbActivated : {}", productSku, page, size, needCorrection, fbbActivated);
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode =
      mandatoryParameterHelper.getBusinessPartnerCode();
    Page<PickupPointCodeWebResponse> response =
        this.productService.getPickupPointCodesByProductSku(page, size, productSku,
          needCorrection, businessPartnerCode, fbbActivated);
    return new ListBaseResponse(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API to fetch all distinct pickup-points by l3")
  @GetMapping(value = ProductApiPath.GET_UNIQUE_PICKUP_POINT_CODES)
  public SingleBaseResponse<UniquePickupPointCodeWebResponse> getUniquePickupPointsByProductSkuCode(
      @RequestParam(required = true, name = "productSku") String productSku) throws Exception {
    log.info("Fetching all distinct pickup-point codes mapped to L3 : {}", productSku);
    String requestId = mandatoryParameterHelper.getRequestId();
    UniquePickupPointCodeWebResponse uniquePickupPointCodeWebResponse =
        this.productService.getUniquePickupPointCodesByProductSku(productSku);
    return new SingleBaseResponse<>(null, null, true, requestId, uniquePickupPointCodeWebResponse);
  }

  @Operation(summary ="API to update price and stock for L4s in external PDP")
  @PutMapping(value = ProductApiPath.EDIT_PRICE_STOCK_INFO, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ItemsPriceStockImagesUpdateWebResponse> editItemsPriceStockImages(
      @RequestBody UpdateItemsPriceStockImagesWebRequest request) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Edit product items request with requestId : {}, for merchantCode : {}, request : {}", requestId, request,
        businessPartnerCode);
    ItemsPriceStockImagesUpdateWebResponse response =
        this.productService.updateItemsPriceStockImages(businessPartnerCode, request);
    return new SingleBaseResponse(null, null, true, requestId, response);
  }

  @Operation(summary ="API to history summary")
  @PostMapping(value = ProductApiPath.GET_PRODUCT_EDIT_HISTORY_SUMMARY)
  public ListBaseResponse<HistorySummaryWebResponse> getProductEditHistorySummary(
      @RequestParam("page") int page, @RequestParam("size") int size,
      @RequestBody HistorySummaryWebRequest request) throws Exception {
    log.info("Fetching product edit history. Request: {} ", request);
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(request.getProductSku().startsWith(businessPartnerCode),
        ErrorMessages.INVALID_GDN_SKU);
    String requestId = mandatoryParameterHelper.getRequestId();
    Page<HistorySummaryWebResponse> response = this.productService.getProductEditHistorySummary(request, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API to update the product details")
  @PutMapping(value = ProductApiPath.EDIT_LOGISTICS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateLogistics(@RequestBody ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection)
      throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productLevel3UpdateWebRequest.getProductSku().startsWith(businessPartnerCode),
        ErrorMessages.INVALID_GDN_SKU);
    String isOnlyExternal = mandatoryParameterHelper.isExternalOnly();
    log.info("Method : update logistics for request: {}", productLevel3UpdateWebRequest);
    productService.updateLogistics(productLevel3UpdateWebRequest, businessPartnerCode, isOnlyExternal, isNeedCorrection);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="Get product order status by productCode")
  @GetMapping(value = ProductApiPath.GET_ORDER_STATUS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<OrderStatusWebResponse> getOrderStatus(@PathVariable("productCode") String productCode)
      throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method : get product order status by productCode : {}", productCode);
    OrderStatusWebResponse orderStatusWebResponse = productService.getOrderStatusByProductCode(productCode);
    return new SingleBaseResponse<>(null, null, true, requestId, orderStatusWebResponse);
  }

  @Operation(summary ="API to update the pickup points")
  @PutMapping(value = ProductApiPath.UPDATE_PICKUP_POINTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<PickupPointUpdateWebResponse> updatePickupPoints(@RequestBody PickupPointUpdateWebRequest pickupPointUpdateWebRequest)
      throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    GdnPreconditions.checkArgument(pickupPointUpdateWebRequest.getProductSku().startsWith(businessPartnerCode),
        ErrorMessages.INVALID_GDN_SKU);
    log.info("Method : update pick up point request for product sku: {}", pickupPointUpdateWebRequest.getProductSku());
    PickupPointUpdateWebResponse response =
        productService.updatePickupPoints(pickupPointUpdateWebRequest, businessPartnerCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="API to fetch variants name by l3")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_VARIANTS_NAME)
  public ListBaseResponse<ProductItemNameWebResponse> getProductVariantsNameByProductSku(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam(required = true, name = "productSku") String productSku)
      throws Exception {
    log.info("Fetching variants name of all L4 mapped to L3 : {}, page : {}, size : {}", productSku, page, size);
    String requestId = mandatoryParameterHelper.getRequestId();
    Page<ProductItemNameWebResponse> response =
        this.productService.getProductVariantsNameByProductSku(productSku, page, size);
    return new ListBaseResponse(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API to fetch the detail for a product by product sku")
  @GetMapping(value = ProductApiPath.GET_L3_DETAIL)
  public SingleBaseResponse<ProductLevel3DetailWebResponse> getL3DetailByProductSku(
      @PathVariable("productSku") String productSku,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection) throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching the L3 details for productSku : {}, businessPartnerCode : {}, isNeedCorrection: {}", productSku,
        businessPartnerCode, isNeedCorrection);
    ProductLevel3DetailWebResponse productLevel3DetailWebResponse =
        this.productService.getL3DetailByProductSku(productSku, businessPartnerCode, isNeedCorrection);
    return new SingleBaseResponse<>(null, null, true, requestId, productLevel3DetailWebResponse);
  }

  @Operation(summary ="API to fetch inventory summary for reserved stock")
  @GetMapping(value = ProductApiPath.GET_INVENTORY_SUMMARY)
  public SingleBaseResponse<InventorySummaryWebResponse> getInventorySummary(@PathVariable("itemSku") String itemSku,
      @RequestParam("isWareHouse") boolean isWareHouse) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Fetching inventory summary for itemSku : {}", itemSku);
    InventorySummaryWebResponse inventorySummaryWebResponse =
        this.productService.getInventorySummary(itemSku, isWareHouse, merchantCode);
    return new SingleBaseResponse<>(null, null, true, requestId, inventorySummaryWebResponse);
  }

  @Operation(summary ="Fetch the L3 product counts")
  @GetMapping(value = ProductApiPath.GET_L3_PRODUCT_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductL3CountWebResponse> getL3ProductCounts(@RequestParam @NotBlank String type) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : Get the L3 product counts for the type: {} and merchant code: {}", type, merchantCode);
    ProductL3CountWebResponse response = productService.getL3ProductCounts(type, merchantCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Deprecated
  @Operation(summary ="API to update the product details")
  @PutMapping(value = ProductApiPath.UPDATE_ITEM_LISTING, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateItemListing(@PathVariable("productSku") String productSku,
      @RequestBody ListingUpdateWebRequest listingUpdateWebRequest) throws Exception {
    log.info("Method :updateItemListing for request: {}", listingUpdateWebRequest);
    productService.updateItemListing(productSku, listingUpdateWebRequest.getQuickEditRequests());
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="To archive/unarchive the list of productSkus")
  @PostMapping(value = ProductApiPath.TOGGLE_ARCHIVE_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse toggleArchiveProducts(@RequestParam("doArchive") boolean doArchive,
      @RequestBody @Valid @NotEmpty List<String> productSkus) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : toggle archive products for products : {} , doArchive : {}", productSkus, doArchive);
    productService.toggleArchiveProducts(productSkus, doArchive, businessPartnerCode);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Deprecated
  @Operation(summary ="API to get L3 product list summary")
  @PostMapping(value = ProductApiPath.GET_L3_PRODUCT_LIST)
  public ListBaseResponse<ProductLevel3ListingWebResponse> getFilterSummaryL3(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @RequestBody ProductSummaryWebRequest request) throws Exception {
    log.info("Fetching product level3 list. Request : {} ", request);
    String requestId = mandatoryParameterHelper.getRequestId();
    request.setMerchantCode(mandatoryParameterHelper.getBusinessPartnerCode());
    Page<ProductLevel3ListingWebResponse> response = this.productService.getProductL3List(request, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API for uploading template for bulk archive productSkus")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_ARCHIVE)
  public BaseResponse bulkArchiveProductSkus(@RequestParam MultipartFile request) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(ExcelTemplateUtil.isInExcelMimeType(request), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
    log.info("invoking upload bulk archive product template with request : {} , {}", requestId, request);
    this.bulkProcessService.uploadBulkUpdateForBulkArchiveProductSkus(username, businessPartnerCode, request);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary ="API for uploading template for bulk workorder creation")
  @PostMapping(value = ProductApiPath.UPLOAD_TEMPLATE_BULK_WORKORDER)
  public BaseResponse bulkWorkorderCreation(@PathVariable("type") String type, @RequestParam MultipartFile request) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String username = mandatoryParameterHelper.getUsername();
    log.info("invoking upload bulk workorder template with type : {} , {}", requestId, type);
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(type), ErrorMessages.EXCEL_WORKORDER_TYPE_IS_INVALID);
    this.bulkProcessService.uploadBulkForWorkOrderCreation(username, type, businessPartnerCode, request);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary ="API to get L3 product list summary")
  @GetMapping(value = ProductApiPath.GET_ITEMS_BY_PRODUCT_SKU)
  public ListBaseResponse<ProductLevel3SummaryWebResponse> getItemsByProductSku(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @PathVariable("productSku") String productSku) throws Exception {
    log.info("Fetching the items for product sku {} ", productSku);
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    Page<ProductLevel3SummaryWebResponse> response =
        productService.getItemsByProductSku(productSku, businessPartnerCode, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API to get vendor notes")
  @GetMapping(value = ProductApiPath.GET_VENDOR_NOTES, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<VendorNotesResponse> getVendorNotes(@PathVariable("productCode") String productCode) throws Exception {
    log.info("Fetching the vendor notes for product code {} ", productCode);
    String requestId = mandatoryParameterHelper.getRequestId();
    VendorNotesResponse response = productService.getVendorNotes(productCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="API to update vendor notes")
  @PutMapping(value = ProductApiPath.UPDATE_VENDOR_NOTES, consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public BaseResponse updateVendorNotes(@PathVariable("productCode") String productCode,
      @RequestBody VendorNotesRequest request) throws Exception {
    log.info("Updating the vendor notes for product code {} ", productCode);
    String requestId = mandatoryParameterHelper.getRequestId();
    productService.updateVendorNotes(productCode, request);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary ="API to submit need revision product")
  @PutMapping(value = ProductApiPath.NEED_REVISION_SUBMIT, consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public SingleBaseResponse<EditProductWebResponse> submitNeedRevisionProduct(
      @RequestBody NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest) {
    log.info("Sumbit need for revision product. productCode : {}, productSku : {} ",
        needRevisionSubmitWebRequest.getProductCode(), needRevisionSubmitWebRequest.getProductSku());
    String requestId = mandatoryParameterHelper.getRequestId();
    EditProductWebResponse editProductWebResponse =
        productService.submitNeedForRevisionProduct(needRevisionSubmitWebRequest);
    return new SingleBaseResponse<>(null, null, Objects.isNull(editProductWebResponse.getApiErrorCode()) ? true : false,
        requestId, editProductWebResponse);
  }

  @Operation(summary = "API to appeal product")
  @PostMapping(value = ProductApiPath.APPEAL_PRODUCT, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public SingleBaseResponse<Boolean> appealProductFromInProgress(
      @Valid @RequestBody AppealProductWebRequest appealProductWebRequest) {
    log.info("Appeal product from in-progress tab. productCode : {}, productSku : {} ",
        appealProductWebRequest.getProductCode(), appealProductWebRequest.getProductSku());
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    boolean response =
        productService.appealProductsInProgress(appealProductWebRequest, businessPartnerCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="API to update product info")
  @PutMapping(value = ProductApiPath.UPDATE_PRODUCT_INFO, consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public SingleBaseResponse<EditProductWebResponse> updateProductInfo(@PathVariable("productSku") String productSku,
      @RequestBody UpdateProductLevel3InfoRequest request) throws Exception {
    log.info("Updating the product info productSku {} ", productSku);
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    if (StringUtils.isNotEmpty(request.getDescription())) {
      String updatedDescription =
          new String(Base64.getDecoder().decode(request.getDescription()));
      GdnPreconditions.checkArgument(updatedDescription.length() <= Constants.MAXIMUM_DESCRIPTION_LENGTH,
          ErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
    }
    if (Objects.nonNull(request.getUniqueSellingPoint())) {
      String uspWithoutTags =
          ConverterUtil.getFilterUSP(new String(Base64.getDecoder().decode(request.getUniqueSellingPoint())));
      GdnPreconditions.checkArgument(uspWithoutTags.length() <= Constants.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
          ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    log.info("Method : edit product info for productSku : {}", productSku);
    boolean isOnlyExternal = Boolean.valueOf(mandatoryParameterHelper.isExternalOnly());
    EditProductWebResponse editProductWebResponse =
        productService.updateProductInfo(request, businessPartnerCode, isOnlyExternal, productSku);
    log.info("productsku : {}, editProductWebResponse : {}", productSku, editProductWebResponse);
    return new SingleBaseResponse<>(null, null, Objects.isNull(editProductWebResponse.getApiErrorCode()) ? true : false,
        mandatoryParameterHelper.getRequestId(), editProductWebResponse);
  }

  @Operation(summary ="API to update image for MTA-API")
  @PostMapping(value = ProductApiPath.UPDATE_IMAGES, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ItemsPriceStockImagesUpdateWebResponse> updateImages(
      @PathVariable("productSku") String productSku, @RequestBody ProductImageEditWebRequest request) throws Exception {
    log.info("Updating images for product sku {} ", productSku);
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(Constants.PRODUCT_SKU_PATTERN.matcher(productSku).matches(),
        ErrorMessages.INVALID_PATTERN_SKU);
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    ItemsPriceStockImagesUpdateWebResponse response =
        productService.updateImages(productSku, businessPartnerCode, request);
    return new SingleBaseResponse(null, null, true, requestId, response);
  }

  @PostMapping(value = ProductApiPath.GET_L4_ITEM_LIST_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ItemLevel4ListingWebResponse> getL4ItemListByProductSku(
    @RequestParam(defaultValue = "0") Integer page, @RequestParam(required = false) Integer size,
    @RequestBody ItemLevel4WebRequest request) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    log.info("Method : get l4 item list by product sku product info for productSku : {}",
      request.getProductSkus());
    RequestHelper.validateProductSkuByBusinessPartnerCode(validateBusinessPartnerCodeForSecurityEnabled,
        mandatoryParameterHelper.getBusinessPartnerCode(), request.getProductSkus());
    Page<ItemLevel4ListingWebResponse> response =
        productService.getL4ItemListByProductSku(storeId, requestId, page, size, request);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, response.getPageable().getPageSize(), response.getTotalElements()));
  }

  @Operation(summary ="API to fetch the L3 details of a product by product sku")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_DETAILS)
  public SingleBaseResponse<ProductL3DetailWebResponse> getL3ProductDetailsByProductSku(
      @PathVariable("productSku") String productSku,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection,
      @RequestParam(required = false) boolean isFbbFetchRequired,
      @RequestParam(required = false, defaultValue = "true") boolean concatenateValueWithValueType) throws Exception {
    String storeId = mandatoryParameterHelper.getStoreId();
    String businessPartnerCode =
      mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching product L3 details for productSku : {}, businessPartnerCode : {}, "
      + "isNeedCorrection: {}", productSku, businessPartnerCode, isNeedCorrection);
    ProductL3DetailWebResponse productL3DetailWebResponse = this.productL3Service
      .getL3DetailsByProductSku(storeId, isFbbFetchRequired, businessPartnerCode, isNeedCorrection,
        productSku, concatenateValueWithValueType);
    return new SingleBaseResponse<>(null, null, true, requestId, productL3DetailWebResponse);
  }

  @Operation(summary ="API to fetch product update history")
  @PostMapping(value = ProductApiPath.GET_PRODUCT_UPDATE_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE, consumes =
    MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<HistoryUpdateWebResponse> getProductUpdateHistory(
    @RequestBody HistoryUpdateWebRequest historyUpdateWebRequest,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size)
    throws Exception {
    log.info("Fetching update product history for request {} ", historyUpdateWebRequest);
    String requestId = mandatoryParameterHelper.getRequestId();
    String productSku = StringUtils.defaultIfEmpty(historyUpdateWebRequest.getProductSku(), StringUtils.EMPTY);
    RequestHelper.validateProductSkuByBusinessPartnerCode(validateBusinessPartnerCodeForSecurityEnabled,
        mandatoryParameterHelper.getBusinessPartnerCode(), Set.of(productSku));
    Page<HistoryUpdateWebResponse> response =
      this.productService.getProductUpdateHistory(historyUpdateWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
      new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Get all item pickup point list for l3")
  @PostMapping(value = ProductApiPath.GET_PRODUCT_ITEM_PICKUP_POINT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ItemPickupPointListingL3WebResponse> getProductItemPickupPointsByProductSku(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size,
      @RequestParam(required = false, defaultValue = "true") boolean onlyDefaultViewConfig,
      @RequestParam(required = false, defaultValue = "true") boolean concatenateValueWithValueType,
      @PathVariable("productSku") String productSku, @RequestBody ItemPickupPointListingL3WebRequest request) {
    request.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    log.info("Get item pickup point list for l3 for merchant : {}, request : {} ", request.getBusinessPartnerCode(),
        request);
    Page<ItemPickupPointListingL3WebResponse> response =
        productService.getItemPickupPointListingByProductSku(page, size, onlyDefaultViewConfig, productSku, request, concatenateValueWithValueType);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="API to fetch Business Partner Pickup Point response from Pickup point codes")
  @RequestMapping(value = ProductApiPath.GET_PICKUP_DETAIL_BY_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ListBaseResponse<BusinessPartnerPickupPointWebResponse> getPickupDetailByCodes(
    @RequestBody List<String> pickupPointCodes) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching business partner pickupPoint details for pickupPointCodes : {} ", pickupPointCodes);
    List<BusinessPartnerPickupPointWebResponse> webResponses =
      productService.getPickupDetailByCode(pickupPointCodes, mandatoryParameterHelper.getBusinessPartnerCode());
    return new ListBaseResponse<>(null, null, true, requestId, webResponses,
      new Metadata(0, webResponses.size(), (long) webResponses.size()));
  }

  @Operation(summary ="API to bulk delete offline item")
  @PostMapping(value = ProductApiPath.OFFLINE_ITEM_DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deleteBulkOfflineItem(@RequestParam MultipartFile file)
    throws Exception {
    String username = mandatoryParameterHelper.getUsername();
    String businessPartnerCode =
      mandatoryParameterHelper.getBusinessPartnerCode();
    String clientId = mandatoryParameterHelper.getClientId();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Deleting the offline items with filename : {}", file.getOriginalFilename());
    GdnPreconditions.checkArgument(!file.isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(ExcelTemplateUtil.isInExcelMimeType(file),
      ErrorMessages.EXCEL_FILE_TYPE_INVALID);
    bulkProcessService
      .uploadBulkDeleteOfflineItems(requestId, businessPartnerCode, username, clientId, file);
    return new GdnBaseRestResponse(file.getOriginalFilename(), null, true,
      mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to edit the product info")
  @PutMapping(value = ProductApiPath.EDIT_L5_PRICE_STOCK, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ItemsPriceStockImagesUpdateWebResponse> editL5ProductStockAndPrice(
      @RequestBody ProductVariantUpdateWebRequest productVariantUpdateWebRequest) throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(productVariantUpdateWebRequest.getProductSku().startsWith(businessPartnerCode),
        ErrorMessages.INVALID_GDN_SKU);
    log.info("Method : edit product info L5 for productSku : {} with request : {} ",
        productVariantUpdateWebRequest.getProductSku(), productVariantUpdateWebRequest);
    ItemsPriceStockImagesUpdateWebResponse editProductWebResponse =
        productService.editL5ProductStockAndPrice(productVariantUpdateWebRequest, businessPartnerCode);
    return new SingleBaseResponse<>(null, null, CollectionUtils.isEmpty(editProductWebResponse.getVariantsErrorList()),
        mandatoryParameterHelper.getRequestId(), editProductWebResponse);
  }

  @Operation(summary ="API to get all all internal template download paths")
  @GetMapping(value = ProductApiPath.GET_ALL_EXTERNAL_TEMPLATE_DOWNLOAD_PATHS)
  @ResponseBody
  public SingleBaseResponse<TemplateDownloadFilePathWebResponse> getExternalDownloadTemplateFilePaths() {
    log.info("Fetching all all internal template download paths");
    return new SingleBaseResponse<>(null, null, true, this.mandatoryParameterHelper.getRequestId(),
        this.productService.getExternalDownloadTemplateFilePaths());
  }

  @Operation(summary ="API to get item basic details by product sku")
  @GetMapping(value = ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ListBaseResponse<ItemDetailWebResponse> getItemBasicDetailsByProductSku(
      @RequestParam("productSku") String productSku,
      @RequestParam(required = false, defaultValue = "true") Boolean fetchAllDetails) {
    log.info("Get item basic deatils by productSku : {} ", productSku);
    productSku = StringUtils.defaultIfEmpty(productSku, StringUtils.EMPTY);
    List<ItemDetailWebResponse> itemBasicDetails = productService.getItemBasicDetails(productSku,
        fetchAllDetails);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        itemBasicDetails, new Metadata(0, itemBasicDetails.size(), (long) itemBasicDetails.size()));
  }

  @Operation(summary = "API to get basic item details for given item skus")
  @PostMapping(value = ProductApiPath.GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ListBaseResponse<ItemDetailWebResponse> getBulkItemDetailByItemSkus(
      @RequestParam(required = false, defaultValue = "false") boolean fetchBundleRecipe,
      @RequestBody List<String> itemSkus) throws Exception {
    log.info("Get basic item details for itemSkus : {} ", itemSkus);
    List<ItemDetailWebResponse> itemBasicDetails =
        productService.getItemBasicDetails(itemSkus, fetchBundleRecipe);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        itemBasicDetails, new Metadata(0, itemBasicDetails.size(), (long)itemBasicDetails.size()));
  }

  @Operation(summary = "API to get item L5 details by product sku")
  @GetMapping(value = ProductApiPath.GET_ITEM_L5_DETAILS_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ListBaseResponse<ItemL5ListingResponse> getItemL5DetailsByProductSku(
      @RequestParam(value = "page", required = false) Integer page, @RequestParam(required = false) Integer size,
      @RequestParam("productSku") String productSku,
      @RequestParam(value = "cncActivated", required = false) Boolean cncActivated,
      @RequestParam(value = "fetchOnlyBundleVariants", required = false, defaultValue = "false")
          boolean fetchOnlyBundleVariants) {
    log.info("Get item basic deatils by productSku : {} ", productSku);
    Page<ItemL5ListingResponse> itemL5ListingResponses =
        productService.getItemL5Details(productSku, page, size, cncActivated, fetchOnlyBundleVariants);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        itemL5ListingResponses.getContent(), new Metadata(Objects.isNull(page) ? 0 : page,
        Objects.isNull(size) ? (int) itemL5ListingResponses.getTotalElements() : itemL5ListingResponses.getSize(),
        itemL5ListingResponses.getTotalElements()));
  }

  @Operation(summary = "API to get warehouse stock details for given item skus")
  @PostMapping(value = ProductApiPath.GET_WAREHOUSE_STOCK_BY_ITEM_SKUS, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<InventoryWarehouseStockWebResponse> getWarehouseStockByItemSkus(
      @RequestParam String warehouseCode, @RequestBody List<String> itemSkus) {
    log.info("Get warehouse stock info for itemSkus : {} ", itemSkus);
    try {
      List<InventoryWarehouseStockWebResponse> responses =
          productService.getWarehouseStockByItemSkusAndWarehouseCode(warehouseCode, itemSkus);
      return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(), responses,
          new Metadata(0, responses.size(), (long) responses.size()));
    } catch (Exception e) {
      log.error("error in fetching warehouse stock details ", e);
      return new ListBaseResponse<>(null, null, false, mandatoryParameterHelper.getRequestId(), null,
          new Metadata(0, 0, (long) 0));
    }
  }

  @Operation(summary = "API for downloading templates for assembly-request, disassembly-request, transfer-request")
  @GetMapping(value = ProductApiPath.DOWNLOAD_WORK_ORDER_TEMPLATES_BASED_ON_TYPE)
  public void downloadTemplatesForWorkLoad(HttpServletResponse response, @PathVariable("type") String type)
      throws Exception {
    log.info("Method: API to download template for type : {} with request id : {} ", type,
        mandatoryParameterHelper.getRequestId());
    productService.downloadWorkOrderTemplates(type, response);
  }

  @GetMapping(value = ProductApiPath.GET_APPEAL_PRODUCT_ELIGIBILITY, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch appeal product config", description = "Fetch appeal product config")
  public GdnRestSingleResponse<AppealProductConfigResponse> fetchAppealProductConfig(
    @RequestParam String storeId, @RequestParam String requestId,
    @PathVariable("businessPartnerCode") String businessPartnerCode) {
    AppealProductConfigResponse appealProductConfig = new AppealProductConfigResponse();
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY);
      log.info("Fetching appeal product config for bp-code {}", businessPartnerCode);
      appealProductConfig =
        productService.fetchAppealProductConfig(storeId, requestId, businessPartnerCode);
      return new GdnRestSingleResponse<>(null, null, true, appealProductConfig, requestId);
    } catch (Exception e) {
      log.error("Exception caught while getting appeal product config bp: {} ", businessPartnerCode,
        e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, appealProductConfig,
        requestId);
    }
  }

  @PostMapping(value = ProductApiPath.SUBMIT_EVIDENCE, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Submit evidence for IPR", description = "Submit evidence for IPR")
  public BaseResponse submitEvidence(@RequestParam String requestId,
      @RequestBody SubmitEvidenceIPRWebRequest submitEvidenceIPRWebRequest) {
      GdnPreconditions.checkArgument(
          StringUtils.isNotBlank(submitEvidenceIPRWebRequest.getProductSku()),
          ErrorMessages.ERROR_PRODUCT_SKU_NOT_EMPTY);
    GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(submitEvidenceIPRWebRequest.getEvidenceUrl())
            || StringUtils.isNotBlank(submitEvidenceIPRWebRequest.getEvidenceFilePath()),
        ErrorMessages.ERROR_FILE_PATH_OR_URL_SHOULD_NOT_BE_EMPTY);
      productService.submitEvidenceForIPR(
          ConverterUtil.convertToSubmitEvidenceIPRRequest(submitEvidenceIPRWebRequest));
      return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary = "api to fetch whether a set of upc code exists for a merchant")
  @PostMapping(value = ProductApiPath.GET_UPC_CODE_STATUS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<UpcStatusWebResponse> getUpcStatus(
      @RequestBody UpcStatusWebRequest request) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Get upcCode status request with requestId : {}, upcCodes : {} for merchantCode : {}",
        requestId, request.getUpcCodes(), request.getMerchantCode());
    List<UpcStatusWebResponse> responses =
        productService.getUpcStatus(request);
    return new ListBaseResponse<>(null, null, true, requestId, responses,
        new Metadata(0,  responses.size(), (long) responses.size()));
  }

  @Operation(summary = "Api to fetch product List to map to reels based on the filters provided")
  @PostMapping(value = ProductApiPath.PRODUCTS_LIST_FOR_REELS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ReelProductDetailWebResponse> getProductsListForReels(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
      @RequestBody ReelProductListingWebRequest reelProductListingWebRequest) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : fetch product List to map to reels : {} and request : {}",
        businessPartnerCode, reelProductListingWebRequest);
    validateReelListingWebRequest(reelProductListingWebRequest, businessPartnerCode);
    Page<ReelProductDetailWebResponse> response =
        productService.getProductListForReels(reelProductListingWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  private void validateReelListingWebRequest(
      ReelProductListingWebRequest reelProductListingWebRequest, String businessPartnerCode) {
    if (StringUtils.isNotBlank(reelProductListingWebRequest.getMerchantCode())
        && !StringUtils.equals(reelProductListingWebRequest.getMerchantCode(),
        businessPartnerCode)) {
      throw new ValidationException(ErrorMessages.INVALID_GDN_SKU);
    }
    List<String> accessibilities = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilities.contains(Accessibilty.STORE_SELLER_VIDEO)) {
      throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);

    }
  }

  @PostMapping(value = ProductApiPath.PRODUCT_BASIC_DETAILS_BY_PRODUCT_SKUS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "API to fetch basic product details by product sku",
      description = "Fetches basic product details like product name, state by product sku")
  GdnRestListResponse<ProductBasicWebResponse> getProductBasicDetailsByProductSkus(
      @RequestBody SimpleListStringRequest productBasicRequest) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching basic product details for productSku : {}, requestId : {}",
        productBasicRequest.getValue(), requestId);
    List<ProductBasicWebResponse> productBasicResponses =
        productService.getProductBasicDetailsByProductSkus(productBasicRequest.getValue());
    return new GdnRestListResponse<>(null, null, Boolean.TRUE, productBasicResponses, null,
        requestId);
  }
}
