package com.gdn.partners.pbp.controller.workflow.product;

import java.util.Map.Entry;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.service.ProductWorkflowServiceWrapper;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3AttributeWipRequest;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3ItemWipRequest;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3WipRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3AttributeWip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3ItemWip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.web.model.ProductWorkflowControllerPath;
import com.gdn.partners.pbp.workflow.product.ProductWfService;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductWorkflowControllerPath.BASE_PATH)
@Tag(name = "Product Workflow", description = "Product Workflow Service API")
public class ProductWorkflowController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductWorkflowController.class);

  @Autowired
  private ProductWfService productWorkflowService;

  @Autowired
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @RequestMapping(value = ProductWorkflowControllerPath.STATUS, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "status", description = "status")
  @ResponseBody
  public GdnRestSingleResponse<ProductWorkflowStatusResponse> status(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode) throws Exception {
    ProductWorkflowStatusResponse productWorkflowStatusResponse = this.status(productCode);
    return new GdnRestSingleResponse<>(productWorkflowStatusResponse, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductWorkflowControllerPath.RETURN_FOR_CORRECTION,
      method = RequestMethod.POST, produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "return screening for correction",
      description = "return screening for correction")
  @ResponseBody
  public GdnBaseRestResponse returnForCorrection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductReturnForCorrectionRequest request)
      throws Exception {
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "returnForCorrection",
        null, username, requestId, storeId, channelId, clientId,
        LoggerAspect.PRODUCT_RETURN_FOR_CORRECTION, request.getProductCode(), request.toString());
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductWorkflowController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getNotes()),
        ProductControllerErrorMessage.NOTES_MUST_NOT_BE_BLANK);
    try {
      this.productWorkflowServiceWrapper.returnForCorrection(storeId, request.getProductCode(), request.getNotes());
      return new GdnBaseRestResponse(requestId);
    } catch (Exception e) {
      LOGGER.error("Error in sending product for need correction, productCode:{}", request.getProductCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductWorkflowControllerPath.RESUBMIT, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "resubmit product to draft", description = "resubmit product to draft")
  public GdnBaseRestResponse resubmit(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductResubmitRequest request) throws Exception {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "resubmit", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_RESUBMIT, request.getProductRequest().getProductCode(), request.toString());
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    UpdateProductLevel3Wip updateProductLevel3Wip =
        this.generateUpdateProductLevel3Wip(request.getProductLevel3WipRequest());
    try {
      this.productWorkflowServiceWrapper.resubmit(storeId, request, updateProductLevel3Wip);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER
          .error("Error in resubmitting product to draft, productCode:{}", request.getProductRequest().getProductCode(),
              e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  private ProductWorkflowStatusResponse generateProductWorkflowStatusResponse(
      ProductWorkflowStatus productWorkflowStatus) throws Exception {
    ProductWorkflowStatusResponse productWorkflowStatusResponse = null;
    if (productWorkflowStatus != null) {
      productWorkflowStatusResponse = new ProductWorkflowStatusResponse();
      BeanUtils.copyProperties(productWorkflowStatus, productWorkflowStatusResponse, "states",
          "status");
      for (String state : productWorkflowStatus.getStates()) {
        productWorkflowStatusResponse.getStates().add(state);
      }
      for (Entry<String, Boolean> entry : productWorkflowStatus.getStatus().entrySet()) {
        productWorkflowStatusResponse.getStatus().put(entry.getKey(), entry.getValue());
      }
    }
    return productWorkflowStatusResponse;
  }

  private ProductWorkflowStatusResponse status(String productCode) throws Exception {
    ProductWorkflowStatus productWorkflowStatus = this.productWorkflowService.status(productCode);
    return this.generateProductWorkflowStatusResponse(productWorkflowStatus);
  }

  private UpdateProductLevel3Wip generateUpdateProductLevel3Wip(UpdateProductLevel3WipRequest request) throws Exception {
    UpdateProductLevel3Wip productLevel3Wip = new UpdateProductLevel3Wip();
    BeanUtils.copyProperties(request, productLevel3Wip, "items", "attributes");
    if (CollectionUtils.isNotEmpty(request.getItems())) {
      for (UpdateProductLevel3ItemWipRequest itemWipRequest : request.getItems()) {
        UpdateProductLevel3ItemWip productLevel3ItemWip = new UpdateProductLevel3ItemWip();
        BeanUtils.copyProperties(itemWipRequest, productLevel3ItemWip);
        productLevel3Wip.getItems().add(productLevel3ItemWip);
      }
    }
    if (CollectionUtils.isNotEmpty(request.getAttributes())) {
      for (UpdateProductLevel3AttributeWipRequest attributeWipRequest : request.getAttributes()) {
        UpdateProductLevel3AttributeWip attributeWip = new UpdateProductLevel3AttributeWip();
        BeanUtils.copyProperties(attributeWipRequest, attributeWip);
        productLevel3Wip.getAttributes().add(attributeWip);
      }
    }
    return productLevel3Wip;
  }

}
