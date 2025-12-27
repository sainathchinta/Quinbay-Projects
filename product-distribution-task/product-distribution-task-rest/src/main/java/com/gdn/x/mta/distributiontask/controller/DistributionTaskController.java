package com.gdn.x.mta.distributiontask.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Controller
@Slf4j
@RequestMapping(value = DistributionTaskController.BASE_PATH)
@Tag(name = "Distribution task Controller", description = "Distribution task Controller")
public class DistributionTaskController {

  public static final String BASE_PATH = "/distirbutionTask";
  private static final String PRODUCT_VENDOR_MAPPING = "/product-vendor-mapping";
  private static final String PRODUCT_REMOVE = "/remove-product";
  private static final String PRODUCT_CORRECTION = "/product-need-correction";

  @Autowired
  private DistributionTaskService distributionTaskService;

  @Autowired
  private ProductWrapperService productWrapperService;

  @RequestMapping(value = PRODUCT_VENDOR_MAPPING, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "To Map the product and Vendor ", description = "product and Vendor Mapping")
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  @ResponseBody public GdnBaseRestResponse saveProductDistributionTask(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username,
      @RequestBody ProductDistributionTaskRequest productDistributionTaskRequest) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "Api call to Map the Product And  vendor. MandatoryRequestParam: {} , Vendor: {} , ProductCodes: {}",
          mandatoryRequestParam, productDistributionTaskRequest.getVendorCode(),
          productDistributionTaskRequest.getProductCodes());
      this.distributionTaskService.assignee(productDistributionTaskRequest.getVendorCode(),
          productDistributionTaskRequest.getProductCodes());
      return new GdnBaseRestResponse(null, null, true, null);
    } catch (Exception e) {
      log.error("Error To Map the product and Vendor : {} , ProductCodes: {}",
          productDistributionTaskRequest.getVendorCode(),
          productDistributionTaskRequest.getProductCodes(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = PRODUCT_REMOVE, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Remove Product from PDT", description = "Remove Product from PDT")
  @ResponseBody
  public GdnBaseRestResponse removeProductDetailsFromPDT(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody RemoveProductRequest request) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    String productCode = StringUtils.EMPTY;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      GdnPreconditions.checkArgument(request != null, "RemoveProductRequest is null");
      productCode = request.getProductCode();
      GdnPreconditions
          .checkArgument(productCode != null, "Product Code is null for RemoveProductRequest");
      log.info(
          "Api to remove product by setting markForDelete to true. mandatoryRequestParam: {}, "
              + "productCode: {}", mandatoryRequestParam, productCode);
      productWrapperService.removeProductAndDeleteOriginalImages(request);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "error while removing product by setting markForDelete to true. mandatoryRequestParam: "
              + "{}, productCode: {}", mandatoryRequestParam, productCode, e);
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = PRODUCT_CORRECTION, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Need Correction for Product", description = "Need Correction for Product")
  @ResponseBody
  public GdnRestSingleResponse<NeedRevisionResponse> doProductNeedCorrection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String vendorCode, @RequestBody NeedRevisionRequest request) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    NeedRevisionResponse needRevisionResponse = new NeedRevisionResponse();
    MandatoryRequestParam mandatoryRequestParam = null;
    List<String> productCodeList = new ArrayList<>();
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      GdnPreconditions
          .checkArgument(Objects.nonNull(request), "ScreeningProductBulkActionsRequest is null");
      productCodeList = request.getProductCodes();
      GdnPreconditions
          .checkArgument(!CollectionUtils.isEmpty(productCodeList), "ScreeningProductBulkActionsRequest is null");
      log.info(
          "Api to remove products by setting markForDelete to true for need correction in bulk and to update in pbp"
              + ". mandatoryRequestParam: {}, productCodesList: {}", mandatoryRequestParam, productCodeList);
      needRevisionResponse = productWrapperService
          .doProductNeedForCorrectionAndReindexSolr(storeId, requestId, username, vendorCode, request);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "error while updating markForDelete to true for need correction in bulk and to update in pbp."
              + " mandatoryRequestParam: {}, productCode: {}", mandatoryRequestParam, productCodeList, e);
    }
    if (needRevisionResponse.getFailedToUpdateNeedCorrectionProductsCount() != 0) {
      errorMessage = needRevisionResponse.getFailedToUpdateNeedCorrectionProductsCount()
          + " products failed to send to need correction";
    }
    return new GdnRestSingleResponse<>(errorMessage, null, needRevisionResponse.isSuccess(), needRevisionResponse,
        requestId);
  }
}
