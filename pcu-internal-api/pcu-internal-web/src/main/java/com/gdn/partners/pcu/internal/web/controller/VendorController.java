package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.VendorApiPath;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.request.BulkDeleteProductRequest;
import com.gdn.partners.pcu.internal.web.model.request.PrimaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkUpdatePendingWebResposne;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFaultyTypeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.NeedRevisionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImageQcWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductScreeningWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RejectProductResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import java.io.IOException;
import java.util.List;

@Slf4j
@Tag(name = "Vendor API")
@RestController
@RequestMapping(value = VendorApiPath.BASE_PATH)
@Validated
public class VendorController {

  private static final int PAGE = 0;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private VendorService vendorService;

  private static final String BULK_APPROVAL = "BULK_APPROVAL";
  private static final String BULK_REJECTION = "BULK_REJECTION";

  @Operation(summary = "Retrieve business partners according to primary filters ")
  @PostMapping(value = VendorApiPath.BUSINESS_PARTNERS_LIST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductBusinessPartnerMapperWebResponse> getBusinessPartnerList(
      @RequestParam(defaultValue = "0", required = false) int page,
      @RequestParam(defaultValue = "10", required = false) int size,
      @RequestBody PrimaryFilterWebRequest request) {
    log.debug("Fetching business partners for selected primary filters : {}", request);
    try{
      request.setVendorCode(clientParameterHelper.getVendorCode());
      List<ProductBusinessPartnerMapperWebResponse> response =
          this.vendorService.getBusinessPartnerList(page, size, request);
        return new ListBaseResponse<>(null, null, true,
          clientParameterHelper.getRequestId(),
          response, new Metadata(page, size, (long) response.size()));
    } catch (Exception e) {
      return new ListBaseResponse<>(e.getMessage(), null, false,
          clientParameterHelper.getRequestId(), null, null);
    }
  }

  @Operation(summary = "Retrieve assignee list according to primary filters ")
  @PostMapping(value = VendorApiPath.ASSIGNEE_LIST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<AssigneeWebResponse> getAssigneeList(
      @RequestParam(defaultValue = "0", required = false) int page,
      @RequestParam(defaultValue = "10", required = false) int size,
      @RequestBody PrimaryFilterWebRequest request) {
    log.debug("Fetching assignee list for selected primary filters : {}", request);
    try{
      request.setVendorCode(clientParameterHelper.getVendorCode());
      List<AssigneeWebResponse> response = this.vendorService.getAssigneeList(page, size, request);
      return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
          response, new Metadata(page, response.size(), (long) response.size()));
    } catch (Exception e) {
      return new ListBaseResponse<>(e.getMessage(), null, false,
          clientParameterHelper.getRequestId(), null, null);
    }
  }

  @Operation(summary = "Get Product history for vendor")
  @GetMapping(value = VendorApiPath.PRODUCT_HISTORY_FOR_VENDOR, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ProductHistoryWebResponse> getProductHistory(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Invoking product history summary for ProductCode : {}", productCode);
    Page<ProductHistoryWebResponse> responsePage = vendorService.getProductHistory(productCode, page, size);
    return new ListBaseResponse<>(null, null, true,  clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Retrieve the content details for a product")
  @GetMapping(value = VendorApiPath.GET_PRODUCT_DETAILS, produces = {MediaType.APPLICATION_JSON_VALUE})
  public SingleBaseResponse<ProductDetailWebResponse> getProductDetailsByProductCode(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode) {
    String requestId = clientParameterHelper.getRequestId();
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse = null;
    try {
      log.info("Api to get product details for product by vendor. requestId: {}, productCode: "
              + "{}", requestId, productCode);
      responseSingleBaseResponse =
              this.vendorService.getProductDetailsByProductCode(requestId,
                  clientParameterHelper.getUsername(), productCode);
    } catch (Exception e) {
      log.error("Error listing product details for product by vendor. requestId: {}, productCode: {}",
              requestId, productCode, e);
    }
    return responseSingleBaseResponse;
  }

  @Operation(summary = "Republish edited product")
  @GetMapping(value = VendorApiPath.REPUBLISH_EDITED_PRODUCT, produces = {MediaType.APPLICATION_JSON_VALUE})
  public BaseResponse republishEditedProduct(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Republish edited product. requestId: {}, productCode: " + "{}", requestId, productCode);
    this.vendorService.republishEditedProduct(productCode);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Product need correction action")
  @PostMapping(value = VendorApiPath.PRODUCT_CORRECTION_ACTION,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<NeedRevisionWebResponse> productCorrectionAction(@RequestBody ScreeningProductBulkActionsWebRequest request) {
    log.info("API to do correction action on productCodes : {}", request.getProductCodes());
    String vendorCode = clientParameterHelper.getVendorCode();
    NeedRevisionWebResponse needRevisionWebResponse = vendorService.doProductNeedCorrection(vendorCode, request);
    return new SingleBaseResponse<>(null, null, needRevisionWebResponse.isSuccess(),
        clientParameterHelper.getRequestId(), needRevisionWebResponse);
  }

  @Operation(summary = "Update Product for Vendor")
  @PostMapping(value = VendorApiPath.VENDOR_PRODUCT_UPDATE, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateVendorProduct(
      @PathVariable("type") @Pattern(regexp = Constants.VENDOR_UPDATE_TYPE_PATTERN,
          message = ErrorMessages.INCORRECT_UPDATE_TYPE) String updateType,
      @RequestBody ProductWebRequest productWebRequest) {
    String vendorCode = clientParameterHelper.getVendorCode();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(vendorCode),
        ErrorMessages.EMPTY_VENDOR_CODE_ERROR);
    String requestId = clientParameterHelper.getRequestId();
    log.info("Update vendor product , update-type : {}", updateType);
    vendorService.updateProduct(updateType,
        vendorCode,
      ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequest,
        clientParameterHelper));
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get list of products for vendor review")
  @PostMapping(value = VendorApiPath.VENDOR_PRODUCT_LIST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<DistributionProductWebResponse> getVendorProductList(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size,
      @RequestBody VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Fetching product list for vendor");
    Page<DistributionProductWebResponse> response = this.vendorService
        .getVendorProductList(page, size, vendorSummaryFilterWebRequest,
            clientParameterHelper.getVendorCode());
    return new ListBaseResponse<>(null, null, true, requestId,
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "Bulk Download vendor products")
  @PostMapping(value = VendorApiPath.DOWNLOAD_FILTERED_PRODUCT_VENDOR, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkDownloadFilteredVendorProducts(
      @RequestBody VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest) {
    log.debug("Invoking api to bulk download products in vendor by filter : {}", vendorSummaryFilterWebRequest);
    this.vendorService.bulkDownloadFilteredVendorProducts(clientParameterHelper.getUsername(),
        clientParameterHelper.getVendorCode(), vendorSummaryFilterWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Assign and unassign assignees to product")
  @PostMapping(value = VendorApiPath.VENDOR_PRODUCT_ASSIGN_UNASSIGN, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse vendorProductActions(@PathVariable("action") String action,
      @RequestBody ScreeningProductBulkActionsWebRequest request) {
    log.info("API to do bulk actions with productCodes : {}", request.getProductCodes());
    vendorService.vendorProductActions(action, request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "To detect edit by merchant while vendor review")
  @GetMapping(value = VendorApiPath.DETECT_EDIT_BY_MERCHANT, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Boolean> detectEditByMerchant(@PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode,
      @RequestParam("version") int version) {
    log.info("API to detect edit by merchant with productCode : {},version : {}");
    boolean response = vendorService.getEditedByMerchant(productCode, version);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Get Primary filters value for product in review")
  @GetMapping(value = VendorApiPath.PRIMARY_FILTER, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MapResponse> getProductFilterCounts(
    @RequestParam("vendorCode") @Valid @NotBlank(message = ErrorMessages.ERR_VENDOR_CODE_EMPTY) String vendorCode,
    @RequestParam(required = false) Boolean postLive,
    @RequestParam(required = false) Boolean edited,
    @RequestParam(required = false) Boolean revised) {
    log.info("Invoking product filter counts for vendor: {}", vendorCode);
    MapResponse response = vendorService.getFilterCounts(vendorCode, postLive, edited,
      revised);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Get product counts by review config in review")
  @GetMapping(value = VendorApiPath.PRODUCT_REVIEW_CONFIG_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MapResponse> getProductReviewConfigCounts(
      @RequestParam("vendorCode") @Valid @NotBlank(message = ErrorMessages.ERR_VENDOR_CODE_EMPTY) String vendorCode) {
    log.info("Invoking review config counts for vendor: {}", vendorCode);
    MapResponse response = vendorService.getReviewConfigProductCounts(vendorCode);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Approve product for vendor")
  @PostMapping(value = VendorApiPath.VENDOR_PRODUCT_APPROVE, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse approveVendorProduct(@RequestBody ProductWebRequest productWebRequest) {
    String vendorCode = clientParameterHelper.getVendorCode();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(vendorCode),
        ErrorMessages.EMPTY_VENDOR_CODE_ERROR);
      GdnPreconditions.checkArgument(
          BrandApprovalStatus.APPROVED.name().equalsIgnoreCase(productWebRequest.getBrandApprovalStatus()),
          ErrorMessages.BRAND_APPROVAL_STATUS_ERROR);
    String requestId = clientParameterHelper.getRequestId();
    vendorService.approveVendorProduct(vendorCode,
        ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequest,
          clientParameterHelper));
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "API to reject the products")
  @PostMapping(value = VendorApiPath.VENDOR_BULK_REJECT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RejectProductResponse> vendorRejectProducts(
      @RequestParam("vendorCode") @Valid @NotBlank(message = ErrorMessages.ERR_VENDOR_CODE_EMPTY) String vendorCode,
      @RequestBody BulkDeleteProductRequest request) throws Exception {
    log.info("invoking reject product api by Vendor. vendorCode: {}, " + "bulkDeleteProductRequest: {}", vendorCode,
        request);
    GdnPreconditions.checkArgument(!request.getCodes().isEmpty(),
        ErrorMessages.PRODUCT_CODE_LIST_EMPTY);
    List<RejectProductResponse> rejectProductResponses =
        this.vendorService.rejectProductByVendor(vendorCode, request);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        rejectProductResponses, null);
  }

  @Operation(summary = "Get Product Screening Notes")
  @GetMapping(value = VendorApiPath.PRODUCT_SCREENING_NOTES, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductScreeningWebResponse> getScreeningNotes(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    log.info("Invoking product Screening notes for ProductCode : {}", productCode);
    String response = vendorService.getProductScreeningNotes(productCode);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        new ProductScreeningWebResponse(response));
  }

  @Operation(summary = "Send product back to vendor")
  @GetMapping(value = VendorApiPath.SEND_PRODUCT_BACK_TO_VENDOR, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse sendProductBackToVendor(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    log.info("Sending product back to vendor productCode: {}", productCode);
    vendorService.sendProductBackToVendor(productCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Bulk vendor product assignment")
  @PostMapping(value = VendorApiPath.BULK_PRODUCT_ASSIGN, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse vendorProductsBulkAssign(@RequestParam MultipartFile request) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    String username = clientParameterHelper.getUsername();
    log.info("Vendor product bulk assign  requestId {}", requestId);
    boolean response;
    try {
      vendorService.saveBulkAssignFile(clientParameterHelper.getStoreId(),
          clientParameterHelper.getVendorCode(), username, requestId, request);
    } catch (IOException e) {
      return new BaseResponse("Error when transferring file " + request.getOriginalFilename() + e.getMessage(), null,
          false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "API to get the vendor list")
  @GetMapping(value = VendorApiPath.VENDORS_LIST, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<VendorDetailWebResponse> getVendorList() {
    log.info("In controller API to fetch the vendor list");
    List<VendorDetailWebResponse> vendorDetailWebResponses = vendorService.getVendorList();
    return new ListBaseResponse(null, null, true, clientParameterHelper.getRequestId(),
        vendorDetailWebResponses, null);
  }

  @Operation(summary = "Retrieve the product image feedback")
  @GetMapping(value = VendorApiPath.GET_IMAGE_FEEDBACK, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductImageQcWebResponse> getProductImageFeedback(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode)
      throws IOException {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Api to get product image feedback details. requestId: {}, productCode: , {}", requestId, productCode);
    return new SingleBaseResponse<>(null, null, true, requestId, vendorService.getProductImageQcFeedback(productCode));
  }

  @Operation(summary = "update product image feedback")
  @PutMapping(value = VendorApiPath.UPDATE_IMAGE_FEEDBACK, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateProductImageFeedback(@RequestBody ProductImageQcWebRequest productImageQcWebRequest)
      throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    try {
      log.info("Api to update product image feedback details. requestId: {}, productCode: , {}", requestId,
          productImageQcWebRequest.getProductCode());
      vendorService.updateProductImageQcFeedback(productImageQcWebRequest);
    } catch (Exception e) {
      log.error("Error while updating product image qc feedback for productCode : {} ",
          productImageQcWebRequest.getProductCode(), e);
      return new BaseResponse(e.getMessage(), null, false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get different faulty type from PBP")
  @GetMapping(value = VendorApiPath.GET_IMAGE_FAULTY_TYPE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ImageFaultyTypeWebResponse> getDifferentFaultyType() {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Api to get different faulty type. requestId: {}", requestId);
    return new ListBaseResponse<>(null, null, true, requestId, vendorService.getDifferentFaultyType(), new Metadata());
  }

  @Operation(summary = "Reindex product by product code")
  @GetMapping(value = VendorApiPath.REINDEX_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse reindexProductToSolr(
      @PathVariable("productCode") @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY)
          String productCode) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Reindex product : {}. requestId: {}", productCode, requestId);
    return new BaseResponse(null, null, vendorService.reindexProductToSolr(productCode), requestId);
  }

  @Operation(summary = "Find pending upload for user")
  @GetMapping(value = VendorApiPath.PENDING_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BulkUpdatePendingWebResposne> checkPendingUpload(@RequestParam String bulkProcessType,
      @RequestParam String status) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Count Pending Uploads. requestId: {}, bulkProcessType: {}, status: {}", requestId, bulkProcessType,
        status);
    return new SingleBaseResponse<>(null, null, true, requestId,
        vendorService.checkCountOfUploads(bulkProcessType, status));
  }

  @Operation(summary = "Get count of newAdded/edited products for review config")
  @GetMapping(value = VendorApiPath.GET_REVIEW_CONFIG_COUNT, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MapResponse> getReviewConfigCount(
      @RequestParam("vendorCode") @Valid @NotBlank(message = ErrorMessages.ERR_VENDOR_CODE_EMPTY) String vendorCode,
      @RequestParam("postLive") boolean postLive) {
    log.info("Invoking review config counts for vendor: {} and postLive : {}", vendorCode, postLive);
    MapResponse response = vendorService.getReviewProductCountsForConfig(vendorCode, postLive);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Get vendor detail")
  @GetMapping(value = VendorApiPath.VENDOR_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<VendorDetailResponse> getVendorDetail(
      @PathVariable("vendorCode") @Valid @NotBlank(message = ErrorMessages.ERR_VENDOR_CODE_EMPTY) String vendorCode) {
    log.info("Invoking vendor detail for vendor code: {} ", vendorCode);
    VendorDetailResponse response = vendorService.getVendorDetail(vendorCode);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Vendor product quick approval")
  @PostMapping(value = VendorApiPath.QUICK_PRODUCT_APPROVAL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<VendorQuickApprovalResponse> vendorProductQuickApproval(
      @RequestBody VendorQuickApprovalRequest vendorQuickApprovalRequest) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Vendor product quick approval VendorQuickApprovalRequest {} and requestId {}", vendorQuickApprovalRequest,
        requestId);
    VendorQuickApprovalResponse response = vendorService.vendorProductQuickApproval(vendorQuickApprovalRequest);
    if (CollectionUtils.isEmpty(response.getErrorCodes())) {
      return new SingleBaseResponse<>(null, null, true, requestId, response);
    } else {
      return new SingleBaseResponse<>(null, null, false, requestId, response);
    }
  }

  @Operation(summary = "Get vendor reviewer list")
  @GetMapping(value = VendorApiPath.PRODUCT_REVIEWERS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<List<String>> getProductReviewers() throws Exception {
    log.info("Invoking vendor reviewer list");
    List<String> response = vendorService.getProductReviewers();
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }
  @Operation(summary = "Vendor auto assignment")
  @PostMapping(value = VendorApiPath.AUTO_ASSIGN_PRODUCTS, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse autoAssignProducts(@RequestBody VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest) {
    String requestId = this.clientParameterHelper.getRequestId();
    String username = this.clientParameterHelper.getUsername();
    String storeId = this.clientParameterHelper.getStoreId();
    String vendorCode = this.clientParameterHelper.getVendorCode();
    try {
      log.info("Api call to perform vendor auto assignment for username : {} and request : {} ", username,
          vendorAutoConsignmentWebRequest);
      vendorService.autoAssignProducts(vendorAutoConsignmentWebRequest, storeId, vendorCode);
    } catch (Exception e) {
      log.error("Error while updating default setting for auto assignment for username : {} ", username, e);
      return new BaseResponse(ErrorMessages.ERR_AUTO_ASSIGNMENT, null, false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get default setting using vendor email ")
  @GetMapping(value = VendorApiPath.GET_DEFAULT_SETTING, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<VendorDefaultFilterResponse> getDefaultSetting(
       @RequestParam("vendorEmail") @Valid @NotBlank(message = ErrorMessages.ERR_VENDOR_EMAIL_EMPTY) String vendorEmail) {
    VendorDefaultFilterResponse vendorDefaultFilterResponse = new VendorDefaultFilterResponse();
    String requestId = this.clientParameterHelper.getRequestId();
    try {
      log.info("Get default setting for vendor: {} ", vendorEmail);
      vendorDefaultFilterResponse = vendorService.getDefaultSetting(vendorEmail);
    } catch (Exception e) {
      log.error("Error while fetching default setting for vendor : {} ", vendorEmail, e);
      return new SingleBaseResponse<>(ErrorMessages.ERR_VENDOR_EMAIL_EMPTY, null, false, requestId, vendorDefaultFilterResponse);
    }
    return new SingleBaseResponse<>(null, null, true, requestId,
            vendorDefaultFilterResponse);
  }

  @Operation(summary = "Check pending assignments ")
  @GetMapping(value = VendorApiPath.CHECK_PENDING_ASSIGNMENTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<InternalProcessPendingFilesResponse> checkPendingAssignments(
     @RequestParam("processType") @Valid @NotBlank(message = ErrorMessages.ERR_PROCESS_TYPE) String processType) {
    String requestId = this.clientParameterHelper.getRequestId();
    String username = this.clientParameterHelper.getUsername();
    String storeId = this.clientParameterHelper.getStoreId();
    InternalProcessPendingFilesResponse internalProcessPendingFilesResponse = new InternalProcessPendingFilesResponse();
    try {
      log.info("Check pending assignments: {} ", username);
      internalProcessPendingFilesResponse = vendorService.checkPendingAssignments(storeId, username, processType);
    } catch (Exception e) {
      log.error("Error while fetching status of assignment : {} ", username, e);
      return new SingleBaseResponse<>(ErrorMessages.ERR_PROCESS_TYPE, null, false, requestId, internalProcessPendingFilesResponse);
    }
    return new SingleBaseResponse<>(null, null, true, requestId,
            internalProcessPendingFilesResponse);
  }


  @Operation(summary = "Bulk Review upload")
  @PostMapping(value = VendorApiPath.BULK_REVIEW_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkReviewUpload(
    @PathVariable("processType") @Valid @NotBlank(message = ErrorMessages.BULK_REVIEW_UPLOAD_EMPTY) String processType,
    @RequestParam MultipartFile request) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    GdnPreconditions.checkArgument(
      BULK_APPROVAL.equals(processType) || BULK_REJECTION.equals(processType),
      ErrorMessages.INVALID_BULK_REVIEW_TYPE);
    log.info("Bulk brand authorization upload for type {} and requestId {}", processType,
      requestId);
    try {
      vendorService.saveBulkReviewFile(request, processType, requestId,
        clientParameterHelper.getStoreId(), clientParameterHelper.getUsername());
    } catch (IOException e) {
      log.error("Error while uploading the Bulk Review file", e);
      return new BaseResponse(
        "Error when transferring file " + request.getOriginalFilename() + e.getMessage(), null,
        false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }
}
