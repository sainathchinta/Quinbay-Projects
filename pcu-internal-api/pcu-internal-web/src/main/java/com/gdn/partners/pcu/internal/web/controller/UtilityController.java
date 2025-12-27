package com.gdn.partners.pcu.internal.web.controller;

import java.util.List;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import jakarta.validation.Valid;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.web.model.request.BigQueryFetchWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import org.apache.commons.lang.StringUtils;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.UtilityApiPath;
import com.gdn.partners.pcu.internal.service.UtilityService;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovalEligibilityWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemViewConfigWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PDTStateUpdateWebRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@Tag(name = "Utility Controller")
@RequestMapping(value = UtilityApiPath.BASE_PATH)
public class UtilityController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private UtilityService utilityService;

  @Operation(summary = "Publish add edited event to PDT")
  @GetMapping(value = UtilityApiPath.PUBLISH_ADD_EDITED_EVENT_TO_PDT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse publishAddEditedEventToPDT(@PathVariable("productCode") String productCode,
      @RequestParam String reviewType) {
    log.info("Publish add edited event to PDT username : {} , productCode : {} , reviewType : {} ",
        clientParameterHelper.getUsername(), productCode, reviewType);
    utilityService.publishAddEditedEventToPDT(productCode, reviewType);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Publish add revised event to PDT")
  @GetMapping(value = UtilityApiPath.PUBLISH_ADD_REVISED_EVENT_TO_PDT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse publishAddRevisedEventToPDT(@PathVariable("productCode") String productCode) {
    log.info("Publish add revised event to PDT username : {} , productCode : {} ",
        clientParameterHelper.getUsername(), productCode);
    utilityService.publishAddRevisedEventToPDT(productCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Retry product image resize")
  @GetMapping(value = UtilityApiPath.RETRY_PRODUCT_IMAGE_RESIZE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse retryProductImageResize(@PathVariable("productCode") String productCode) {
    log.info("Retry product image resize username : {} , productCode : {} ",
        clientParameterHelper.getUsername(), productCode);
    utilityService.retryProductImageResize(productCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Retry edited product image resize")
  @GetMapping(value = UtilityApiPath.RETRY_EDITED_PRODUCT_IMAGE_RESIZE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse retryEditedProductImageResize(@PathVariable("productCode") String productCode) {
    log.info("Retry edited product image resize username : {} , productCode : {} ",
        clientParameterHelper.getUsername(), productCode);
    utilityService.retryEditedProductImageResize(productCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Update pbp product workflow")
  @GetMapping(value = UtilityApiPath.UPDATE_PBP_PRODUCT_WORKFLOW, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updatePBPProductWorkflow(@PathVariable("productCode") String productCode,
      @RequestParam String state) {
    log.info("Update pbp product workflow username : {} , productCode : {} ",
        clientParameterHelper.getUsername(), productCode);
    utilityService.updatePBPProductWorkflow(productCode, state);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Update pbp review pending")
  @GetMapping(value = UtilityApiPath.UPDATE_PBP_REVIEW_PENDING, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updatePBPReviewPending(@PathVariable("productCode") String productCode,
      @RequestParam boolean reviewPending) {
    log.info("Update pbp product review pending username : {} , productCode : {} reviewPending : {} ",
        clientParameterHelper.getUsername(), productCode, reviewPending);
    utilityService.updatePBPReviewPending(productCode, reviewPending);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Update pbp review pending")
  @GetMapping(value = UtilityApiPath.UPDATE_PBP_ACTIVATED_AND_VIEWABLE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updatePBPActivatedAndViewable(@PathVariable("productCode") String productCode,
      @RequestParam boolean activated, @RequestParam boolean viewable) {
    log.info("Update pbp product review pending username : {} , productCode : {} activated : {} viewable : {}",
        clientParameterHelper.getUsername(), productCode, activated, viewable);
    utilityService.updatePBPActivatedAndViewable(productCode, activated, viewable);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary= "Check product auto approval eligibility")
  @PostMapping(value = UtilityApiPath.CHECK_AUTO_APPROVAL_ELIGIBILITY, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<String> checkProductAutoApprovalEligibility(@PathVariable("productCode") String productCode,
      @RequestBody AutoApprovalEligibilityWebRequest request) {
    log.info("Checking auto approval eligibility for productCode : {} username : {} and request : {} ", productCode,
        clientParameterHelper.getUsername(), request);
    String value = utilityService.checkProductAutoApprovalEligibility(productCode, request);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        value);
  }

  @Operation(summary = "Republish pcb product publish event")
  @GetMapping(value = UtilityApiPath.REPUBLISH_PCB_PRODUCT_PUBLISH_EVENT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse republishPCBProductPublishEvent(@PathVariable("productCode") String productCode,
      @RequestParam String operationType) {
    log.info("Republish pcb product publish event username : {} , productCode : {} type : {}",
        clientParameterHelper.getUsername(), productCode, operationType);
    utilityService.republishPCBProductPublishEvent(productCode, operationType);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Clear pcb product cache")
  @GetMapping(value = UtilityApiPath.CLEAR_PCB_PRODUCT_CACHE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse clearPCBProductCache(@PathVariable("productCode") String productCode,
      @RequestParam String productId) {
    log.info("Clear pcb product cache username : {} , productCode : {} productId : {}",
        clientParameterHelper.getUsername(), productCode, productId);
    utilityService.clearPCBProductCache(productCode, productId);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "update pcb product viewable")
  @GetMapping(value = UtilityApiPath.UPDATE_PCB_PRODUCT_VIEWABLE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updatePCBProductViewable(@PathVariable("productCode") String productCode,
      @RequestParam boolean viewable) {
    log.info("update pcb product viewable username : {} , productCode : {} viewable : {}",
        clientParameterHelper.getUsername(), productCode, viewable);
    utilityService.updatePCBProductViewable(productCode, viewable);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "update pcb product review pending")
  @GetMapping(value = UtilityApiPath.UPDATE_PCB_PRODUCT_REVIEW_PENDING, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updatePCBProductReviewPending(@PathVariable("productCode") String productCode,
      @RequestParam boolean reviewPending) {
    log.info("update pcb product viewable username : {} , productCode : {} viewable : {}",
        clientParameterHelper.getUsername(), productCode, reviewPending);
    utilityService.updatePCBProductReviewPending(productCode, reviewPending);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Update PDT product state")
  @PostMapping(value = UtilityApiPath.UPDATE_PDT_STATE_UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updatePDTState(@PathVariable("productCode") String productCode,
      @RequestBody PDTStateUpdateWebRequest pdtStateUpdateWebRequest) {
    log.info("Update PDT product state for productCode : {} username : {} and request : {} ", productCode,
        clientParameterHelper.getUsername(), pdtStateUpdateWebRequest);
    utilityService.updatePDTState(productCode, pdtStateUpdateWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Check product auto approval eligibility")
  @PostMapping(value = UtilityApiPath.TAKE_DOWN_OR_REACTIVE_PRODUCT_IN_X_PRODUCT, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse takeDownOrReactivateProduct(@PathVariable("productSku") String productSku,
      @RequestParam boolean forceReview, @RequestBody List<ItemViewConfigWebRequest> request) {
    log.info("Checking auto approval eligibility for productCode : {} username : {} and request : {} ", productSku,
        clientParameterHelper.getUsername(), request);
    utilityService.takeDownOrReactivateProduct(productSku, forceReview, request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Abort bulk process by id")
  @PutMapping(value = UtilityApiPath.ABORT_PENDING_BULK_PROCESS_DATA_BY_ID, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse abortPendingBulkProcess(@RequestParam String id) {
    log.info("Aborting bulk process for data id : {} and username : {} ", id,
      clientParameterHelper.getUsername());
    utilityService.abortPendingBulkProcessById(id);
    return new BaseResponse(null, null, true,
      clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Abort pending bulk downloads by entity and status")
  @PutMapping(value = UtilityApiPath.ABORT_PENDING_DOWNLOADS_BY_ENTITY, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse abortPendingDownloads(@RequestParam String entityType,
    @RequestParam String status) {
    log.info("Aborting bulk download for entity : {} and username : {} ", entityType,
      clientParameterHelper.getUsername());
    utilityService.abortPendingDownloadsByEntity(entityType, status);
    return new BaseResponse(null, null, true,
      clientParameterHelper.getRequestId());
  }

  @Operation(summary = "CAUTION : Delete product by productCode from PCB, PBP and PDT")
  @PostMapping(value = UtilityApiPath.DELETE_PRODUCT_BY_PRODUCT_CODE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse deleteProductCollection(
    @RequestParam(required = false, defaultValue = "false") boolean needEmailNotification,
    @RequestBody DeleteProductWebRequest deleteProductWebRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(deleteProductWebRequest.getProductCode()),
      ErrorMessages.ERR_PRODUCT_CODE_EMPTY);
    log.info("Delete product collection for productCode : {} username : {} and request : {} ",
      deleteProductWebRequest.getProductCode(), clientParameterHelper.getUsername(),
      deleteProductWebRequest);
    utilityService.deleteProductCollection(clientParameterHelper.getUsername(),
      clientParameterHelper.getRequestId(), clientParameterHelper.getStoreId(),
      clientParameterHelper.getClientId(), clientParameterHelper.getChannelId(),
      needEmailNotification, deleteProductWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Reindex active product by product code")
  @PostMapping(value = UtilityApiPath.REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse reindexActiveProductByProductCode(@PathVariable("productCode")
  @Valid @NotBlank(message = ErrorMessages.ERR_PRODUCT_CODE_EMPTY) String productCode) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Reindex active product. requestId: {}, productCode: {}", requestId, productCode);
    this.utilityService.reindexActiveProductByProductCode(productCode);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Fetch Data from Big Query for master sku")
  @PutMapping(value = UtilityApiPath.FETCH_DATA_FROM_BIG_QUERY_FOR_MASTER_SKU_REVIEW, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse fetchBQDataForMasterSku(@RequestParam String processName,
      @RequestBody BigQueryFetchWebRequest bigQueryFetchWebRequest) {
    log.info(
        "Fetching Data from Big Query for master sku review with processName : {} , requestId : {}",
        processName, clientParameterHelper.getRequestId());
    utilityService.fetchDataFromBigQueryForMasterSkuReview(processName, bigQueryFetchWebRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @PostMapping(value = UtilityApiPath.MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE,
    consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Utility to update l3 and L5 details", description = "Utility to update l3 and L5 details")
  public GdnBaseRestResponse migrateProductAndL5DetailByProductSku(@RequestParam String storeId,
    @RequestParam String username,
    @RequestBody ProductAndL5MigrationRequest productAndL5MigrationRequest) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    String clientId = clientParameterHelper.getClientId();
    String channelId = clientParameterHelper.getChannelId();
    log.info("Invoking Product and L5 migration API for product SKU : {} ",
      productAndL5MigrationRequest.getProductSku());
    utilityService.migrateProductAndL5DetailsByProductSku(storeId, requestId, clientId, channelId,
      productAndL5MigrationRequest, username);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PutMapping(value = UtilityApiPath.UPDATE_SYSTEM_PARAMETER_IN_PCB, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Utility to update system param in PCB",
    description = "Utility to update system param in PCB")
  public GdnBaseRestResponse updateSystemParameter(@RequestParam String storeId,
    @RequestBody SystemParameterRequest systemParameterRequest) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    String clientId = clientParameterHelper.getClientId();
    String channelId = clientParameterHelper.getChannelId();
    log.info("Invoking system param update for request : {} ", systemParameterRequest);
    utilityService.updateSystemParameterInPCB(storeId, requestId, channelId, clientId,
      systemParameterRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = UtilityApiPath.GENERATE_PRODUCT_SCORE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Generate product score by productSku or productCode")
  public BaseResponse generateProductScoreByProductSkuOrProductCode(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam boolean updateCategory,
    @RequestParam String productSku, @RequestParam String productCode) {
    log.info("Generate product score by productSku or productCode: productSku={}, productCode={}",
      productSku, productCode);

    utilityService.generateProductScoreByProductSkuOrProductCode(storeId, channelId, clientId,
      requestId, username, updateCategory, productSku, productCode);

    return new BaseResponse(null, null, true, requestId);
  }

  @PostMapping(value = UtilityApiPath.PUBLISH_PRODUCT_ATTRIBUTE_EXTRACTIONS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Publish product attribute extractions")
  public BaseResponse publishProductAttributeExtractions(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId,
      @RequestBody ProductAttributeExtractionsRequest productAttributeExtractionsRequest) {
    log.info("Publish product attribute extractions for product skus: {}",
        productAttributeExtractionsRequest.getProductSkuList());
    utilityService.publishProductAttributeExtractions(storeId, productAttributeExtractionsRequest);
    return new BaseResponse(null, null, true, requestId);
  }
}
