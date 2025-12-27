package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedUserFeedbackResponse;
import com.gdn.partners.pcu.internal.model.AutoApprovedApiPath;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.AutoApprovedService;
import com.gdn.partners.pcu.internal.service.AutoApprovedServiceWrapper;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsDownloadWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

@Slf4j
@RestController
@RequestMapping(AutoApprovedApiPath.BASE_PATH)
public class AutoApprovedController {

  @Autowired
  private AutoApprovedService autoApprovedService;

  @Autowired
  private AutoApprovedServiceWrapper autoApprovedServiceWrapper;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Get auto approved products list")
  @PostMapping(value = AutoApprovedApiPath.AUTO_APPROVED_PRODUCTS_PATH,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<AutoApprovedListWebResponse> getAutoApprovedProductsList(
    @RequestParam(value = "page", defaultValue = "0") int page,
    @RequestParam(value = "size", defaultValue = "10") int size,
    @RequestBody AutoApprovedWebRequest autoApprovedWebRequest) throws Exception {
    log.info("invoking controller method to get auto approved products for request : {} ",
      autoApprovedWebRequest);
    GdnRestListResponse<AutoApprovedListWebResponse> response;
    response = autoApprovedService.getAutoApprovedProductsList(page, size, autoApprovedWebRequest);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "perform action on auto-approved-product")
  @PostMapping(value = AutoApprovedApiPath.PERFORM_ACTION_ON_AUTO_APPROVED_PRODUCTS,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse performActionOnAutoApprovedProduct(
      @PathVariable("productCode") String productCode,
      @RequestBody AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest)
      throws Exception {
    log.info("Perform action : {}  on auto-approved product with  product-code : {} ", productCode,
        autoApprovedProductsActionWebRequest.getAction());
    autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(productCode,
        autoApprovedProductsActionWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Auto Approved Product Detail API")
  @GetMapping(value = AutoApprovedApiPath.AUTO_APPROVED_PRODUCT_DETAILS_API_PATH,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductDetailWebResponse> getProductDetail(
      @PathVariable("productCode") @Valid @NotBlank(
          message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode) {
    String requestId = clientParameterHelper.getRequestId();
    String clientId = clientParameterHelper.getClientId();
    log.info("Method : Get auto approved product detail info by product code : {}  requestId : {} "
        + "clientId : {}", productCode, requestId, clientId);
    ProductDetailWebResponse productDetailWebResponse =
        autoApprovedServiceWrapper.fetchAutoApprovedProductDetail(productCode, clientId);
    return new SingleBaseResponse<>(null, null, true, requestId, productDetailWebResponse);
  }

  @Operation(summary = "Update assignee for auto approved product")
  @PostMapping(value = AutoApprovedApiPath.UPDATE_ASSIGNEE, consumes = MediaType.APPLICATION_JSON_VALUE,
    produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ProductAssigneeChangeResponse> updateChangedAssignee(
    @RequestBody AutoApprovedAssigneeRequest assigneeRequest) throws Exception {
    log.info("Perform action for change assignee for request = {} ", assigneeRequest);
    GdnRestListResponse<ProductAssigneeChangeResponse> response =
      autoApprovedService.updateAssignee(assigneeRequest);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Update user feedback for auto approved product")
  @PostMapping(value = AutoApprovedApiPath.UPDATE_USER_FEEDBACK, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateUserFeedback(@PathVariable("productCode") String productCode,
      @RequestBody AutoApprovedUserFeedbackRequest autoApprovedUserFeedbackRequest) {
    log.info("Update user feedback for productCode = {} ", productCode);
    return autoApprovedService.updateUserFeedback(productCode, autoApprovedUserFeedbackRequest);
  }

  @Operation(summary = "Fetch user feedback for auto approved product")
  @GetMapping(value = AutoApprovedApiPath.FETCH_USER_FEEDBACK, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> fetchUserFeedback(
      @PathVariable("productCode") String productCode) {
    log.info("Fetch user feedback for productCode = {} ", productCode);
    return autoApprovedService.fetchUserFeedback(productCode);
  }

  @Operation(summary = "Download items for auto approved product list")
  @PostMapping(value = AutoApprovedApiPath.AUTO_APPROVED_PRODUCT_DOWNLOAD, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadItemsForAutoApprovedProducts(
      @RequestBody AutoApprovedProductsDownloadWebRequest request) {
    log.info("Auto approved products download having request = {} ", request);
        this.autoApprovedServiceWrapper.downloadItemsForAutoApprovedProducts(
        clientParameterHelper.getUsername(), request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Bulk Upload Auto Approved File")
  @PostMapping(value = AutoApprovedApiPath.AUTO_APPROVED_PRODUCT_UPLOAD_ASSIGNEE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkUploadAssignee(@RequestParam MultipartFile file) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Bulk upload file for requestId {}", requestId);
    try {
      autoApprovedServiceWrapper.uploadBulkAssignFile(file, requestId,
          clientParameterHelper.getStoreId(), clientParameterHelper.getUsername(),
          clientParameterHelper.getVendorCode());
    } catch (IOException e) {
      log.error("Error while uploading the Bulk Assignee file", e);
      return new BaseResponse(
          "Error when transferring file " + file.getOriginalFilename() + e.getMessage(), null,
          false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }
}
