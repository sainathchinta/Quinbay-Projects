package com.gdn.x.mta.distributiontask.controller;

import static com.gdn.x.mta.distributiontask.controller.util.WorkflowStateUtil.VENDOR_PRODUCT_APPROVED_STATES;
import static com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil.ACTION_VALUE_INCORRECT;
import static com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil.PRODUCT_STATE_INVALID;

import java.util.Objects;

import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.model.Constants;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.controller.util.ModelConverterUtil;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkScreeningProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.ScheduledJobService;
import com.gdn.x.mta.distributiontask.service.impl.PDTExceptions.PDTGeneralException;
import com.gdn.x.productcategorybase.ErrorMessage;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by virajjasani on 21/09/16.
 */
@Controller
@RequestMapping(value = VendorActivityController.BASE_PATH)
@Tag(name = "Vendor Activity Controller", description = "Vendor Activity Controller")
@Slf4j
public class VendorActivityController {

  public static final String BASE_PATH = "/vendor-activity";
  private static final String ROOT = "/";
  private static final String UPDATE_PRODUCT_CONTENT = ROOT + "update-product-content";
  private static final String UPDATE_PRODUCT_IMAGE_INFO = ROOT + "update-product-image";
  private static final String REJECT_PRODUCT = ROOT + "reject-product";
  private static final String NOT_APPLICABLE  = "NA";
  public static final String VENDOR_PRODUCT_ACTIONS = ROOT + "vendorProductActions/{action}/action";
  public static final String BULK_VENDOR_PRODUCT_ACTIONS = ROOT + "bulkVendorProductActions";
  public static final String VENDOR_APPROVAL_NEW = ROOT + "approval-new";
  public static final String VENDOR_PRODUCT_DELETE = ROOT + "delete-from-DB";
  public static final String UPDATE_PRODUCT_IMAGE_FEEDBACK = "/update/image-feedback";
  public static final String VENDOR_QUICK_APPROVAL = ROOT + "quick-approval";

  private static final String ASSIGN = "assign";
  private static final String UNASSIGN = "unassign";

  @Autowired
  private ModelConverterUtil modelConverterUtil;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ScheduledJobService scheduledJobService;

  @Autowired
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @RequestMapping(value = UPDATE_PRODUCT_CONTENT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to update product content", description = "Api to update product content "
      + "by given vendor")
  @ResponseBody
  public GdnBaseRestResponse updateProductContent(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String vendorCode, @RequestBody DistributionProductDetailRequest request)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
      MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
      log.info(
          "update Product Content at Controller. MandatoryRequestParam: {}, vendorCode: {}, "
              + "productCode: {}",
          mandatoryRequestParam, vendorCode, request.getProductCode());
      ValidationUtil.validateUpdateProductDetails(request);
      Product existingProduct = this.productService.getAllProductDetailsByCode(request.getProductCode());
      ValidationUtil.validateExistingProductDetails(vendorCode, existingProduct);
      if (VENDOR_PRODUCT_APPROVED_STATES.contains(existingProduct.getState())) {
        throw new IllegalStateException(PRODUCT_STATE_INVALID);
      }
      Product newProduct =
          this.modelConverterUtil.convertProductDetailRequestToProductEntity(request, NOT_APPLICABLE);
      newProduct.setImageViolations(existingProduct.getImageViolations());
      newProduct.setTextViolations(existingProduct.getTextViolations());
      this.productWrapperService.updateProductDetails(existingProduct, newProduct);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException | PDTGeneralException e) {
      log.error(
          "error while updating Product Content at Controller. MandatoryRequestParam: {}, vendorCode: {}, productCode: {}, error - ",
          mandatoryRequestParam, vendorCode, request.getProductCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId);
    } catch (Exception e) {
      log.error(
          "error while updating Product Content at Controller. MandatoryRequestParam: {}, "
              + "vendorCode: {}, productCode: {}",
          mandatoryRequestParam, vendorCode, request.getProductCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = REJECT_PRODUCT, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to reject product", description = "Api call to reject any product by Vendor")
  @ResponseBody
  public GdnBaseRestResponse rejectProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String vendorCode, @RequestBody
      RejectProductVendorRequest rejectProductVendorRequest) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    String errorCode = null;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "Api to reject product details. mandatoryRequestParam: {}, vendorCode: {}, "
              + "rejectProductVendorRequest: {}",
          mandatoryRequestParam, vendorCode, rejectProductVendorRequest);
      ValidationUtil.validateRejectProductParams(vendorCode, rejectProductVendorRequest);
      this.productWrapperService
          .rejectProductByVendorAndDeleteFromSolr(ModelConverterUtil.toRejectProductDTO(rejectProductVendorRequest),
              vendorCode);
      isSuccess = true;
    } catch (ValidationException e){
      errorCode = e.getErrorCode();
      errorMessage = e.getErrorMessage();
      log.error(
          "Error while rejecting product details. mandatoryRequestParam: {}, vendorCode: {}, "
              + "rejectProductVendorRequest: {}, error: ",
          mandatoryRequestParam, vendorCode, rejectProductVendorRequest, e);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "Error while rejecting product details. mandatoryRequestParam: {}, vendorCode: {}, "
              + "rejectProductVendorRequest: {}, error: ",
          mandatoryRequestParam, vendorCode, rejectProductVendorRequest, e);
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, isSuccess, requestId);
  }
  
  @RequestMapping(value = VENDOR_APPROVAL_NEW, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to approve content", description = "Api to approve content of product")
  @ResponseBody
  public GdnBaseRestResponse approveProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String vendorCode, @RequestBody DistributionProductDetailRequest request) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      GdnPreconditions.checkArgument(request != null, "Vendor Approval Request cannot be null");
      log.info(
          "Api to approve product by Vendor. MandatoryRequestParam: {}, vendorCode: {}, " + "productCode: {}",
          mandatoryRequestParam, vendorCode, request.getProductCode());
      Product newProduct = this.modelConverterUtil.convertProductDetailRequestToProductEntity(request, NOT_APPLICABLE);
      this.productWrapperService.updateAndApproveProduct(newProduct, vendorCode, request.getNotes());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("error approving product by Vendor. MandatoryRequestParam: {} , vendorCode: {} , "
          + "productCode: {} , error - ", mandatoryRequestParam, request.getVendorCode(), request.getProductCode(), e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error("error approving product by Vendor. MandatoryRequestParam: {}, vendorCode: {}, "
          + "productCode: {} , error - ", mandatoryRequestParam, request.getVendorCode(), request.getProductCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = UPDATE_PRODUCT_IMAGE_INFO, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to update product image info", description = "Api to update product image "
      + "related info by given vendor")
  @ResponseBody
  public GdnBaseRestResponse updateProductImageInfo(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String vendorCode, @RequestBody DistributionProductDetailRequest request)
      throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "update Product image related info at Controller. MandatoryRequestParam: {}, "
              + "vendorCode: {}, productCode: {}",
          mandatoryRequestParam, vendorCode, request.getProductCode());
      ValidationUtil.validateUpdateProductImageDetails(request);
      Product existingProduct = this.productService.getAllProductDetailsByCode(request.getProductCode());
      ValidationUtil.validateExistingProductDetails(vendorCode, existingProduct);
      if (VENDOR_PRODUCT_APPROVED_STATES.contains(existingProduct.getState())) {
        throw new IllegalStateException(PRODUCT_STATE_INVALID);
      }
      Product newProduct =
          this.modelConverterUtil.convertProductDetailRequestToProductEntity(request, NOT_APPLICABLE);
      this.productService.updateProductImageDetails(existingProduct, newProduct, Boolean.FALSE);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "error while updating Product image related info at Controller. MandatoryRequestParam: "
              + "{}, vendorCode: {}, productCode: {}, ",
          mandatoryRequestParam, vendorCode, request.getProductCode(), e);
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = VENDOR_PRODUCT_ACTIONS, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to assign/unassign the reviewer", description = "Api to assign/unassign the reviewer")
  @ResponseBody
  public GdnBaseRestResponse doVendorProductActions(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("action") String action, @RequestBody BulkScreeningProductActionsRequest request) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    try {
      GdnPreconditions.checkArgument((ASSIGN.equals(action) || UNASSIGN.equals(action)), ACTION_VALUE_INCORRECT);
      if (StringUtils.isEmpty(request.getAssignedBy())) {
        request.setAssignedBy(username);
      }
      log.info("updating the image and content reviewer at Controller for productCode: {}", request.getProductCodes());
      productWrapperService
          .updateAssigneeDetails(storeId, request.getProductCodes(), request.getAssignedBy(), action, request.getAssignTo());
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error while updating the image and content reviewer at Controller for productCode: {}",
          request.getProductCodes(), e);
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = BULK_VENDOR_PRODUCT_ACTIONS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to assign/unassign the product reviewer", description = "Api to assign/unassign the product reviewer")
  @ResponseBody
  public GdnRestSingleResponse<BulkVendorProductActionsResponse> bulkVendorProductActions(@RequestParam String storeId,
      @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkVendorProductActionsRequest request) throws Exception {
    BulkVendorProductActionsResponse response = new BulkVendorProductActionsResponse();
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    try {
      GdnPreconditions.checkArgument((ASSIGN.equals(request.getActionType())), ACTION_VALUE_INCORRECT);
      log.info("updating the product reviewer at Controller");
      response = productWrapperService.bulkUpdateProductAssignee(storeId,
          modelConverterUtil.toBulkVendorProductActionsDTO(request));
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error while updating the image and content reviewer at Controller ", e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess,response, requestId);
  }

  @RequestMapping(value = {VENDOR_PRODUCT_DELETE}, method = {RequestMethod.DELETE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to delete the values from PDT", description = "Api to delete the values from PDT")
  @ResponseBody
  public GdnBaseRestResponse doDeleteForProducts(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam int days,
      @RequestParam int batchSize, @RequestParam int maxBatchSize,
      @RequestParam(required = false, defaultValue = "false") boolean eventDeleteFlow) throws Exception {
    GdnPreconditions.checkArgument(days > 0 && batchSize > 0, ErrorMessage.NOT_VALID_PARAMETERS.getMessage());
    log.info("deleting the values from PDT older than days : {} and batch size : {}", days, batchSize);
    scheduledJobService.deleteProducts(storeId, days, batchSize, maxBatchSize, eventDeleteFlow);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = UPDATE_PRODUCT_IMAGE_FEEDBACK, method = RequestMethod.PUT,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to update product image feedback", description = "Api to update product image feedback")
  @ResponseBody
  public GdnBaseRestResponse updateProductImageFeedback(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductImageQcFeedbackRequest productImageQcFeedbackRequest) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    try {
      productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, false);
      return new GdnBaseRestResponse(StringUtils.EMPTY, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while updating image feedback for productCode : {} , error - ",
          productImageQcFeedbackRequest.getProductCode(), e);
      return new GdnBaseRestResponse(e.getErrorMessage(), null, false, requestId);
    } catch (Exception e) {
      log.error("Error while updating image feedback for productCode : {} , error - ",
          productImageQcFeedbackRequest.getProductCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = VENDOR_QUICK_APPROVAL, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to approve content", description = "Api to approve content of product")
  @ResponseBody
  public GdnRestSingleResponse<VendorQuickApprovalResponse> quickApproveProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody VendorQuickApprovalRequest request) {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    VendorQuickApprovalResponse response = new VendorQuickApprovalResponse();
    try {
      mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      GdnPreconditions.checkArgument(Objects.nonNull(request), "Vendor Quick Approval Request cannot be null");
      log.info("Api to Quick approve by Vendor. MandatoryRequestParam: {}, vendorCode: {}, productCode: {}",
          mandatoryRequestParam, request.getVendorCode(), request.getProductCode());
      response = this.productWrapperService.quickApproveProduct(request);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error to Quick approve by Vendor. MandatoryRequestParam: {}, vendorCode: {}, productCode: {} ",
          mandatoryRequestParam, request.getVendorCode(), request.getProductCode(), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, response, requestId);
  }
}
