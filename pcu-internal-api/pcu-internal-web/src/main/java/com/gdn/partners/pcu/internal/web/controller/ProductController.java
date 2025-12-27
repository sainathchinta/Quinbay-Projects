package com.gdn.partners.pcu.internal.web.controller;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import jakarta.validation.Valid;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductHistoryWebResponse;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.ProductApiPath;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductServiceWrapper;
import com.gdn.partners.pcu.internal.service.impl.util.ProductRevisionReasonsUtil;
import com.gdn.partners.pcu.internal.validaton.annotation.DraftProductAttributeValid;
import com.gdn.partners.pcu.internal.validaton.annotation.SuspensionProductBulkActionsWebRequestValid;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.controller.util.ExcelTemplateUtil;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductReturnForCorrectionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SelectedMasterProductDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeCheckResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductReviewerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductRevisionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.TemplateDownloadFilePathWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 10/01/2019 AD.
 */

@Slf4j
@Tag(name = "Product API")
@RestController
@RequestMapping(value = ProductApiPath.BASE_PATH)
@Validated
public class ProductController {

  private final String SUSPEND = "SUSPEND";
  private final String REACTIVATE = "REACTIVATE";

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Value("${product.name.trim}")
  private boolean productNameTrim;

  @Value("${webp.conversion.enabled}")
  private boolean webpConversionEnabled;

  @Operation(summary = "Get Revision reasons")
  @GetMapping(value = ProductApiPath.REVISION_REASONS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Map<String, String>> getProductRevisionReasons(@RequestParam String type) {
    log.info("Get product revision reasons for type :{}", type);
    Map<String, String> response = ProductRevisionReasonsUtil.getProductRevisionReasons(type);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Product Return for Correction")
  @PutMapping(value = ProductApiPath.RETURN_FOR_CORRECTION, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse returnForCorrection(@RequestBody ProductReturnForCorrectionWebRequest request) {
    log.info("Product returned for correction with reasons :{} ", request);
    this.productService.returnForCorrection(ConverterUtil.toProductReturnForCorrectionRequest(request));
    return new GdnBaseRestResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Merge Product to master product")
  @PostMapping(value = ProductApiPath.MERGE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse mergeProduct(@RequestParam String masterProductId, @RequestParam String duplicateProductId,
      @RequestParam(required = false) boolean isForceMerge) {
    log.info("Product merge with id:{} to masterId:{}", duplicateProductId, masterProductId);
    this.productService.mergeProduct(masterProductId, duplicateProductId, isForceMerge);
    return new GdnBaseRestResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Update Product")
  @PutMapping(value = ProductApiPath.UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<String> updateProduct(@RequestBody ProductWebRequest request,
      @RequestParam(required = false) boolean internalFlow3AddProduct, @RequestParam(required = false) boolean isActive)
      throws Exception {
    log.info("Update product with productRequest :{request}, internalFlow3AddProduct:{}", request,
        internalFlow3AddProduct);
    String requestId = clientParameterHelper.getRequestId();
    String internalActivationInterval =
        this.productServiceWrapper.updateProduct(requestId, clientParameterHelper.getUserType(),
            ConverterUtil.toProductRequest(request, clientParameterHelper, productNameTrim, webpConversionEnabled),
          internalFlow3AddProduct, isActive);
    return new SingleBaseResponse<>(null, null, true, requestId, internalActivationInterval);
  }

  @Operation(summary = "Update product assignment status")
  @PutMapping(value = ProductApiPath.UPDATE_PRODUCT_ASSIGNMENT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateProductAssignment(
      @RequestParam @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode,
      @RequestParam(defaultValue = "NA") @Valid @NotBlank(message = ErrorMessages.ERR_ASSIGNED_TO_EMPTY)
          String assignedTo,
      @RequestParam @Valid @NotBlank(message = ErrorMessages.ERR_ASSIGNED_BY_EMPTY) String assignedBy) {
    productService.updateProductAssignmentStatus(productCode, assignedTo, assignedBy);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "Product Detail API")
  @GetMapping(value = ProductApiPath.DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductDetailWebResponse> getProductDetail(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode,
      @RequestParam(required = false) boolean inAllProducts,
      @RequestParam(required = false) boolean concatenateValueWithValueType) {
    String requestId = clientParameterHelper.getRequestId();
    String clientId = clientParameterHelper.getClientId();
    log.info("Method : Get product info by product code : {}  requestId : {} clientId : {}", productCode, requestId,
        clientId);
    ProductDetailWebResponse productDetailWebResponse =
        productService.getProductDetail(productCode, inAllProducts, clientId, concatenateValueWithValueType);
    return new SingleBaseResponse<>(null, null, true, requestId, productDetailWebResponse);
  }

  @Operation(summary = "Count filter API")
  @GetMapping(value = ProductApiPath.FILTER_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<FilterCountWebResponse> getFilterCounts(@RequestParam boolean activated,
      @RequestParam boolean viewable) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Invoking controller method to get filter counts by activated : {}, viewable : {}, requestId : {}",
        activated, viewable, requestId);
    FilterCountWebResponse filterCountWebResponse = productService.getFilterCounts(activated, viewable);
    return new SingleBaseResponse<>(null, null, true, requestId, filterCountWebResponse);
  }

  @Operation(summary = "Get review products API")
  @PostMapping(value = ProductApiPath.REVIEW_PRODUCTS_FILTER, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ReviewProductWebResponse> getReviewProducts(@RequestParam boolean activated,
      @RequestParam boolean viewable, @RequestBody ReviewProductsFilterRequest request,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "100") int size) {
    log.info("invoking controller method to get review products by activated : {}, viewable : {},"
        + "and filter request : {}", activated, viewable, request);
    Page<ReviewProductWebResponse> responsePage =
        productService.getReviewProductsByFilterRequest(request, activated, viewable, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Approve draft product from screening")
  @PostMapping(value = ProductApiPath.APPROVE_DRAFT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse approveDraft(
      @RequestBody @Valid @DraftProductAttributeValid(message = "ProductAttribute Validation failed")
          ProductWebRequest request) throws Exception {
    log.info("Api to approve draft product with productCode : {}", request.getProductCode());
    productService.approveDraft(ConverterUtil.toProductRequest(request, clientParameterHelper, productNameTrim,
      false));
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get Product history")
  @GetMapping(value = ProductApiPath.PRODUCT_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductHistoryWebResponse> getProductHistory(
      @PathVariable("productId") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_ID_EMPTY) String productId,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Invoking product history summary for ProductId : {}", productId);
    Page<ProductHistoryWebResponse> responsePage = productService.getProductHistory(productId, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Bulk review product actions")
  @PostMapping(value = ProductApiPath.BULK_SCREENING_PRODUCT_ACTIONS,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkScreeningProductActions(@PathVariable("action") String bulkAction,
      @RequestBody ScreeningProductBulkActionsWebRequest request) {
    log.info("API to do bulk actions with productCodes : {}", request.getProductCodes());
    productService.doScreeningProductsBulkActions(bulkAction, request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get screening product suggestion")
  @PostMapping(value = ProductApiPath.SCREENING_SUGGESTION, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductSuggestionWebResponse> getScreeningSuggestion(
      @RequestParam("productName") String productName, @RequestParam("upcCode") String upcCode,
      @RequestParam("productCode") String productCode, @RequestParam("categoryId") String categoryId,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestBody List<ProductSuggestionWebRequest> productSuggestionWebRequests) {
    List<ProductSuggestionWebResponse> response = productService
        .getScreeningSuggestion(productCode, productName, upcCode, categoryId, productSuggestionWebRequests,
            PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response, new Metadata(page, size, (long) response.size()));
  }

  @Operation(summary = "Get screening product search")
  @GetMapping(value = ProductApiPath.SCREENING_PRODUCT_SEARCH, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductSuggestionWebResponse> filterProductsBySearchKeyword(
      @RequestParam("keyword") @Valid @NotBlank(message = ErrorMessages.KEYWORD_EMPTY) String keyword,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Invoking product screening search for keyword : {}", keyword);
    List<ProductSuggestionWebResponse> response = productService.filterProductsBySearchKeyword(keyword, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response, new Metadata(page, size, (long) response.size()));
  }

  @Operation(summary = "Get reviewers for product")
  @GetMapping(value = ProductApiPath.GET_REVIEWER_FOR_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductReviewerWebResponse> getProductReviewer(
      @RequestParam("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    log.info("Invoking get reviewer for product : {}", productCode);
    ProductReviewerWebResponse response = productService
        .getProductReviewerList(Constants.PREFIX_PRODUCT_REVIEWING.concat(productCode),
            clientParameterHelper.getUsername());
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Delete reviewer for a  product")
  @DeleteMapping(value = ProductApiPath.DELETE_REVIEWER_FOR_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse deleteProductReviewer(
      @RequestParam("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    log.info("Invoking delete reviewer : {} for product : {}",
        clientParameterHelper.getUsername(), productCode);
    productService.deleteProductReviewer(Constants.PREFIX_PRODUCT_REVIEWING.concat(productCode),
        clientParameterHelper.getUsername());
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get product revision history")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_REVISION_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductRevisionHistoryWebResponse> getProductRevisionHistory(
      @PathVariable("productCode") String productCode) {
    log.debug("Invoking get product revision history for product : {}", productCode);
    List<ProductRevisionHistoryWebResponse> response = productService.getProductRevisionHistory(productCode);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response, new Metadata());
  }

  @Operation(summary = "Check category change")
  @GetMapping(value = ProductApiPath.CATEGORY_CHANGE_CHECK, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<CategoryChangeCheckResponse> checkCategoryChange(
      @RequestParam("presentCategoryId") @Valid @NotBlank(message = ErrorMessages.ERR_CATEGORY_ID_EMPTY)
          String presentCategoryId,
      @RequestParam("targetCategoryId") @Valid @NotBlank(message = ErrorMessages.ERR_CATEGORY_ID_EMPTY)
          String targetCategoryId, @PathVariable("productCode") String productCode,
      @RequestParam(required = false, defaultValue = "false") boolean isActive) {
    log.debug("Checking if category change is possible for presentCategoryId: {} and targetCategoryId : {}",
        presentCategoryId, targetCategoryId);
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(presentCategoryId, targetCategoryId, productCode, isActive);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "API to suspend and reactivate products")
  @PostMapping(value = ProductApiPath.PRODUCTS_SUSPENSION, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse doSuspensionAction(@RequestBody @Valid
  @SuspensionProductBulkActionsWebRequestValid(message = "Suspension product request validation failed")
      SuspensionProductBulkActionsWebRequest request) {
    log.info("invoking controller method to suspend/activate products for request : {}", request);
    productService.doSuspensionAction(request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get all active and suspended products")
  @PostMapping(value = ProductApiPath.GET_ALL_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductSuspensionWebResponse> getAllProducts(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestBody ProductSuspensionFilterRequest productSuspensionFilterRequest) throws Exception {
    log.info("invoking controller method to get all products for request : {}", productSuspensionFilterRequest);
    Page<ProductSuspensionWebResponse> response =
        productService.getAllProducts(productSuspensionFilterRequest, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "Get suspension history for productSku")
  @GetMapping(value = ProductApiPath.SUSPENSION_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductSuspensionHistoryWebResponse> getSuspensionHistory(
      @RequestParam("productSku") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_SKU_EMPTY) String productSku,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.debug("Invoking get product suspension history for productSku : {}", productSku);
    Page<ProductSuspensionHistoryWebResponse> responsePage =
        productService.getSuspensionHistory(productSku, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Bulk product suspension")
  @PostMapping(value = ProductApiPath.BULK_PRODUCT_SUSPENSION, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkProductSuspension(
      @RequestParam("type") @Valid @NotBlank(message = ErrorMessages.SUSPENSION_TYPE_EMPTY) String type,
      @RequestParam MultipartFile request) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    GdnPreconditions
        .checkArgument(SUSPEND.equals(type) || REACTIVATE.equals(type), ErrorMessages.INVALID_SUSPENSION_TYPE);
    log.info("Bulk product suspension for type {} and requestId {}", type, requestId);
    try {
      productService.saveProductSuspensionFile(request, type, requestId, clientParameterHelper.getStoreId(),
          clientParameterHelper.getUsername());
    } catch (IOException e) {
      return new BaseResponse("Error when transferring file " + request.getOriginalFilename() + e.getMessage(), null,
          false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Update product category")
  @PostMapping(value = ProductApiPath.UPDATE_PRODUCT_CATEGORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateProductCategory(
      @PathVariable @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode,
      @RequestParam @Valid @NotBlank(message = ErrorMessages.EMPTY_CATEGORY_CODE) String categoryCode) {
    return new SingleBaseResponse<>(null, null, productService.updateProductCategory(productCode, categoryCode),
        clientParameterHelper.getRequestId(), null);
  }

  @Operation(summary = "API for active page listing")
  @PostMapping(value = ProductApiPath.GET_ACTIVE_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductCollectionWebResponse> getActiveProducts(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestBody SummaryFilterWebRequest summaryFilterWebRequest) throws Exception {
    log.info("invoking controller method to get active products for request : {}", summaryFilterWebRequest);
    Page<ProductCollectionWebResponse> response = productService
        .findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(page, size));
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "Bulk update of master product data")
  @PostMapping(value = ProductApiPath.BULK_UPDATE_MASTER_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkUpdateMasterProductData(@RequestParam MultipartFile request)
      throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Invoking bulk update master product in controller. Request Id: {}", requestId);
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    GdnPreconditions.checkArgument(ExcelTemplateUtil.isInExcelMimeType(request), ErrorMessages.EXCEL_FILE_TYPE_INVALID);
    productService
        .bulkUpdateMasterProductData(request, requestId, clientParameterHelper.getStoreId(),
            this.clientParameterHelper.getUsername());
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Fetch filter counts by source")
  @PutMapping(value = ProductApiPath.FILTER_COUNTS_BY_SOURCE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MapWebResponse> getFilterCountsBySource(@RequestBody CountWebRequest countWebRequest)
      throws Exception {
    log.debug("Get filter count for countWebRequest :{}", countWebRequest);
    MapWebResponse response = productService.getFilterCountsBySource(countWebRequest);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Download selected master products")
  @PostMapping(value = ProductApiPath.DOWNLOAD_SELECTED_MASTER_PRODUCTS,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadBulkSelectedMasterProductsForInternal(
      @RequestBody SelectedMasterProductDownloadWebRequest selectedMasterProductDownloadWebRequest) {
    log.debug("Invoking api to download selected master products : {}", selectedMasterProductDownloadWebRequest);
    this.productService
        .downloadBulkSelectedMasterProductsForInternal(clientParameterHelper.getUsername(),
            selectedMasterProductDownloadWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Download all master products")
  @PostMapping(value = ProductApiPath.DOWNLOAD_MASTER_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadAllMasterProducts(@RequestBody SummaryFilterWebRequest summaryFilterWebRequest) {
    log.debug("Invoking all product download at controller. requestId: {}", summaryFilterWebRequest);
    this.productService.downloadBulKMasterProducts(clientParameterHelper.getUsername(),
        summaryFilterWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get product history")
  @GetMapping(value = ProductApiPath.GET_PRODUCT_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<HistoryWebResponse> getProductHistory(
      @RequestParam("source") @Valid @NotBlank(message = ErrorMessages.ERR_SOURCE_EMPTY) String source,
      @RequestParam(required = false) String productCode, @RequestParam(required = false) String productId,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "25") Integer size)
      throws Exception {
    Page<HistoryWebResponse> responsePage =
        this.productService.findProductHistory(source, productCode, productId, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Retry product publish to PDT")
  @PostMapping(value = ProductApiPath.RETRY_PRODUCT_PUBLISH_TO_PDT, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse retryProductPublishToPDT(@RequestParam String productCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode), ErrorMessages.ERR_PRODUCT_CODE_EMPTY);
    log.info("Retry product publish to PDT for product-code {}", productCode);
    boolean value = this.productService.retryProductPublishToPDT(productCode);
    return new SingleBaseResponse(null, null, true, clientParameterHelper.getRequestId(),
        value);
  }

  @Operation(summary = "Reindex productSku")
  @GetMapping(value = ProductApiPath.REINDEX_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse reindexByProductSku(@PathVariable(value = "productSku") String productSku,
      @RequestParam(required = false) String storeId) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    this.productService.reindexByProductSku(storeId, productSku);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Retry product need revision publish to PBP")
  @GetMapping(value = ProductApiPath.RETRY_PRODUCT_NEED_REVISION_TO_PBP, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse retryProductNeedRevisionToPBP(@RequestParam String productCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode), ErrorMessages.ERR_PRODUCT_CODE_EMPTY);
    log.info("Retry product need revision publish to PBP for product-code {}", productCode);
    boolean value = this.productService
        .retryProductNeedRevisionToPBP(clientParameterHelper.getStoreId(), productCode);
    return new SingleBaseResponse(null, null, true, clientParameterHelper.getRequestId(),
        value);
  }

  @Operation(summary = "Check category change")
  @GetMapping(value = ProductApiPath.CHECK_CATEGORY_CHANGE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<CategoryChangeWebResponse> checkProductCategoryChange(
      @PathVariable(value = "productCode") String productCode,
      @RequestParam @Valid @NotBlank String oldCategoryId,
      @RequestParam @Valid @NotBlank String newCategoryId,
      @RequestParam(required = false, defaultValue = "false") boolean isActive,
      @RequestParam(required = false) String productType) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    return new SingleBaseResponse<>(null, null, true, requestId,
        this.productService.getProductCategoryChangeCheckResponse(productCode, oldCategoryId,
            newCategoryId, isActive, productType));
  }

  @Operation(summary = "Reindex product code")
  @PostMapping(value = ProductApiPath.REINDEX_BY_PRODUCT_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse reindexByProductCode(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Reindex  product. requestId: {}, productCode: {}", requestId, productCode);
    this.productService.reindexByProductCode(productCode);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "API to get all all internal template download paths")
  @GetMapping(value = ProductApiPath.GET_ALL_INTERNAL_TEMPLATE_DOWNLOAD_PATHS)
  public SingleBaseResponse<TemplateDownloadFilePathWebResponse> getInternalDownloadTemplateFilePaths() {
    log.info("Fetching all all internal template download paths");
    return new SingleBaseResponse<>(null, null, true, this.clientParameterHelper.getRequestId(),
        this.productService.getInternalDownloadTemplateFilePaths());
  }

  @Operation(summary = "Get halal product details by productSku")
  @GetMapping(value = ProductApiPath.GET_HAHAL_PRODUCT_DETAILS_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<HalalProductWebResponse> getHalalProductDetailsByProductSku(
      @PathVariable(value = "productSku") String productSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.ERR_PRODUCT_SKU_EMPTY);
    log.debug("getting halal product details for productSku : {} ", productSku);
    HalalProductWebResponse halalProductResponse = this.productService.getHalalProductDetailsByProductSku(productSku);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(), halalProductResponse);
  }

  @Operation(summary = "Get Halal Certification Details")
  @GetMapping(value = ProductApiPath.GET_HALAL_CERTIFICATION_DETAILS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<HalalCertificationWebDetailsResponse> getHalalCertificationDetails(
      @PathVariable("certificationNumber")
      @Valid @NotBlank(message = ErrorMessages.EMPTY_CERTIFICATION_NUMBER) String certificationNumber,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching Halal Certification Details For Certification No. : {} ", certificationNumber);
    Page<HalalCertificationWebDetailsResponse> responsePage =
        productService.getHalalCertificationDetails(certificationNumber, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Get hala product history by productSku")
  @GetMapping(value = ProductApiPath.GET_HALAL_PRODUCT_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<HalalProductHistoryWebResponse> getHalaProductHistory(
      @PathVariable(value = "productSku") String productSku, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    String requestId = this.clientParameterHelper.getRequestId();
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.ERR_PRODUCT_SKU_EMPTY);
    log.info("Getting halal product history. requestId : {} , productSku : {} ", requestId, productSku);
    Page<HalalProductHistoryWebResponse> responses = productService.getHalaProductHistory(productSku, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, responses.getContent(),
        new Metadata(page, size, responses.getTotalElements()));
  }


  @Operation(summary = "Fetch all halal dashboard products")
  @PostMapping(value = ProductApiPath.GET_HALAL_DASHBOARD_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<HalalDashboardProductsWebResponse> getHalalDashboardProducts(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size,
      @RequestBody HalalProductsFilterWebRequest halalProductsFilterWebRequest) {
    log.info("Fetching Halal dashboard products with filter request {} ", halalProductsFilterWebRequest);
    String requestId = this.clientParameterHelper.getRequestId();
    Page<HalalDashboardProductsWebResponse> halalDashboardProductsWebResponses =
        productService.getHalalDashboardProductsResponses(page, size, halalProductsFilterWebRequest);
    return new ListBaseResponse<>(null, null, true, requestId, halalDashboardProductsWebResponses.getContent(),
        new Metadata(page, size, halalDashboardProductsWebResponses.getTotalElements()));
  }

  @Operation(summary = "update halal config of product")
  @PutMapping(value = ProductApiPath.UPDATE_PRODUCT_HALAL_CONFIG, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateHalalConfigOfProduct(@PathVariable(value = "productSku") String productSku,
      @RequestParam(required = true) String curationStatus) {
    log.info("update halal config for product productSku: {}, requestId: {} ", productSku,
        clientParameterHelper.getRequestId());
    this.productService.updateHalalConfigOfProduct(clientParameterHelper.getStoreId(),
        clientParameterHelper.getRequestId(), clientParameterHelper.getUsername(), productSku, curationStatus);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }
}
