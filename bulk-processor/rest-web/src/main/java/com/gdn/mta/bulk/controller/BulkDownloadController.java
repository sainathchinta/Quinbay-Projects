package com.gdn.mta.bulk.controller;

import java.util.Collections;
import java.util.UUID;

import com.gdn.mta.bulk.dto.PickupPointCodesRequestDTO;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.UnifiedBulkDownloadException;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentDTO;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentResponse;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkDownloadProductDTO;
import com.gdn.mta.bulk.dto.BulkDownloadProductRequest;
import com.gdn.mta.bulk.dto.BulkDownloadProductResponse;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.CampaignBulkDownloadRequest;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkDownloadService;
import com.gdn.mta.bulk.service.BulkDownloadServiceWrapper;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;


/**
 * Created by virajjasani on 02/09/16.
 */
@RestController
@RequestMapping(value = BulkProcessController.BASE_PATH)
@Tag(name = "BulkDownloadController", description = "Bulk Process Service API")
public class BulkDownloadController {

  private static final Logger LOG = LoggerFactory.getLogger(BulkDownloadController.class);

  private static final String ROOT = "/";
  private static final String DOWNLOAD_PRODUCT_BULK = ROOT + "download-bulk-product";
  private static final String BULK_DOWNLOAD_PRODUCT_INFO = ROOT + "bulk-download-product-info";
  private static final String DOWNLOAD_PRODUCT_FILE_CONTENT = ROOT + "download-bulk-file-detail";
  private static final String DOWNLOAD_BULK_FILE = ROOT +"download-bulk-file";
  private static final String DOWNLOAD_BULK_PROCESS_PRODUCT_FILE = ROOT + "download-bulk-process-product-file";
  private static final String DOWNLOAD_PRODUCT_UNIFIED_TEMPLATE = ROOT + "download-unified-product";
  private static final String DOWNLOAD_PRODUCT_UNIFIED_TEMPLATE_V2 = ROOT + "download-unified-product-v2";
  private static final String DOWNLOAD_VENDOR_PRODUCT_EXCEL = ROOT + "download-vendor-product";
  public static final String GET_INPROGRESS_COUNT = ROOT + "in-progress-requests";
  public static final String CLEAR_IN_PROGRESS_DOWNLOADS = ROOT + "clear-in-progress-downloads";
  public static final String CHECK_CAMPAIGN_PENDING_BULK_DOWNLOAD_REQUESTS =
      ROOT + "check-campaign-pending-bulk-download-requests";
  public static final String PROCESS_ALL_DOWNLOAD = ROOT + "process-all-download";

  private static final int PRIVILEGED_MAP_SIZE = 6;
  private static final String PRIVILEGED_MAP_SIZE_ERROR =
      "privileged map for business partner should have size: " + PRIVILEGED_MAP_SIZE;
  private static final String STORE_ID_MUST_NOT_BE_BLANK = "store Id must not be blank";
  private static final String REQUEST_ID_MUST_NOT_BE_BLANK = "request Id must not be blank";
  private static final String PRODUCT_SIZE_INVALID = "product size should be greater than 0";
  protected static final String FILE_PROCESSING_EXCEPTION = "bulk download file processing exception";
  protected static final String USER_NOT_AUTHORIZED_TO_DOWNLOAD_FILE_BUSINESS_PARTNER =
      "User not authorized to download file for businessPartnerCode: ";
  public static final String CAMPAIGN_CODE_MUST_NOT_BE_BLANK =
      "campaign code must not be blank";
  public static final String DOWNLOAD_TAGGED_PRODUCTS = ROOT + "download-tagged-products";
  public static final String BR_EMAIL_ADDRESS_MUST_NOT_BE_BLANK =
      "br email address must not be blank";

  @Autowired
  private BulkDownloadService bulkDownloadService;

  @Autowired
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Autowired
  private BulkDownloadServiceWrapper bulkDownloadServiceWrapper;

  @RequestMapping(value = DOWNLOAD_PRODUCT_BULK, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Download Process", description = "Api for downloading active products in "
      + "bulk")
  public GdnBaseRestResponse downloadBulkProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BulkDownloadProductRequest bulkRequest) {

    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "downloadBulkProducts", bulkRequest.getBusinessPartnerCode(),
        username, requestId, storeId, channelId, clientId, bulkRequest.getRequest().getGdnSku(), null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    try {
      bulkDownloadValidation(storeId, requestId, bulkRequest);
      BulkDownloadMailRecipient bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
      BeanUtils.copyProperties(bulkRequest, bulkDownloadMailRecipient, "businessPartnerCode",
          "privilegedMap", "productSize");
      bulkDownloadService.preProcess(requestId, bulkRequest.getPrivilegedMap(),
          bulkRequest.getBusinessPartnerCode(), bulkRequest.getProductSize(), bulkRequest.getRequest(), username,
          bulkDownloadMailRecipient);
      isSuccess = true;
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
     }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = DOWNLOAD_PRODUCT_FILE_CONTENT, method = RequestMethod.GET, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Download File Data", description = "api to get bulk download file content")
  public GdnRestSingleResponse<BulkDownloadFileContentResponse> getFileContents(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String fileId) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "getFileContents", null, 
        username, requestId, storeId, channelId, clientId, fileId, null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    BulkDownloadFileContentResponse response = new BulkDownloadFileContentResponse();
    try {
      BulkDownloadFileContentDTO bulkDownloadProductDTO =
          bulkDownloadService.getFileContents(fileId);
      GdnPreconditions.checkArgument(bulkDownloadProductDTO.isFileAvailable(),
          FILE_PROCESSING_EXCEPTION);
      BeanUtils.copyProperties(bulkDownloadProductDTO, response);
      isSuccess = true;
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, response, requestId);
  }

  @RequestMapping(value = BULK_DOWNLOAD_PRODUCT_INFO, method = RequestMethod.GET, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Download product info", description = "api to get bulk download product "
      + "related info")
  public GdnRestSingleResponse<BulkDownloadProductResponse> getBulkDownloadProductInfo(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String fileId) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "getBulkDownloadProductInfo", null, 
        username, requestId, storeId, channelId, clientId, fileId, null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    BulkDownloadProductResponse bulkDownloadProductResponse = new BulkDownloadProductResponse();
    try {
      BulkDownloadProductDTO bulkDownloadProductDTO =
          bulkDownloadService.getBulkDownloadProductByRequestId(fileId);
      BeanUtils.copyProperties(bulkDownloadProductDTO, bulkDownloadProductResponse);
      bulkDownloadProductResponse.setCreatedBy(bulkDownloadProductDTO.getCreatedBy());
      isSuccess = true;
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, bulkDownloadProductResponse,
        requestId);
  }

  @RequestMapping(value = DOWNLOAD_BULK_FILE, method = RequestMethod.GET, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Download File Data", description = "api to get bulk download file content")
  public GdnRestSingleResponse<BulkDownloadFileContentResponse> getBulkDownloadFile(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String fileId,
      @RequestParam String businessPartnerCode) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "getBulkDownloadFile", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, fileId, null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    BulkDownloadFileContentResponse response = new BulkDownloadFileContentResponse();
    try {
      BulkDownloadProductDTO bulkDownloadProductDTO = bulkDownloadService
          .getBulkDownloadProduct(fileId, BulkDownloadEntityStatus.STATUS_SUCCESS.getStatusValue());
      if (!StringUtils.isEmpty(bulkDownloadProductDTO.getBusinessPartnerCode())) {
        GdnPreconditions.checkArgument(
            businessPartnerCode.equals(bulkDownloadProductDTO.getBusinessPartnerCode()),
            USER_NOT_AUTHORIZED_TO_DOWNLOAD_FILE_BUSINESS_PARTNER + bulkDownloadProductDTO
                .getBusinessPartnerCode());
      }
      BulkDownloadFileContentDTO fileContent =
          bulkDownloadService.getFileContents(bulkDownloadProductDTO);
      GdnPreconditions.checkArgument(fileContent.isFileAvailable(), FILE_PROCESSING_EXCEPTION);
      BeanUtils.copyProperties(fileContent, response);
      response.setEntityType(bulkDownloadProductDTO.getEntityType());
      response.setFileType(ProcessorUtils.getFileType(bulkDownloadProductDTO.getFileName()).name());
      isSuccess = true;
    } catch (BulkDownloadException bulkException) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, bulkException), bulkException);
      errorMessage = bulkException.getErrorMessage();
      errorCode = bulkException.getErrorCode();
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, errorCode, isSuccess, response, requestId);
  }

  private void bulkDownloadValidation(String storeId, String requestId,
      BulkDownloadProductRequest bulkRequest) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(requestId), REQUEST_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions
        .checkArgument(!isPrivilegedMapSizeInvalid(bulkRequest), PRIVILEGED_MAP_SIZE_ERROR);
    GdnPreconditions.checkArgument(!isProductSizeInvalid(bulkRequest), PRODUCT_SIZE_INVALID);
  }

  private boolean isPrivilegedMapSizeInvalid(BulkDownloadProductRequest bulkRequest) {
    return bulkRequest.getPrivilegedMap() == null
        || bulkRequest.getPrivilegedMap().size() != PRIVILEGED_MAP_SIZE;
  }

  private boolean isProductSizeInvalid(BulkDownloadProductRequest bulkRequest) {
    return bulkRequest.getProductSize() == null || bulkRequest.getProductSize() <= 0;
  }

  @RequestMapping(value = DOWNLOAD_BULK_PROCESS_PRODUCT_FILE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Download Content of Bulk Process Product File Data",
      description = "api to get Bulk process Product File Content")
  public GdnRestSingleResponse<BulkDownloadFileContentResponse> getBulkProcessProductFile(
          @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
          @RequestParam String requestId, @RequestParam String username, @RequestParam String bulkProcessCode) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "getBulkProcessProductFile", null, 
        username, requestId, storeId, channelId, clientId, bulkProcessCode, null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    BulkDownloadFileContentResponse response = new BulkDownloadFileContentResponse();
    try {
      BulkDownloadFileContentDTO bulkDownloadFileContentDTO =
              bulkDownloadService.getBulkProcessProductFile(storeId, bulkProcessCode);
      BeanUtils.copyProperties(bulkDownloadFileContentDTO, response);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = "Error while invoking api to get file contents of Bulk Process Product file." + e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, response, requestId);
  }

  @Operation(summary = "API to download product unified template", description = "API to download product unified template")
  @RequestMapping(value = DOWNLOAD_PRODUCT_UNIFIED_TEMPLATE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<UnifiedBulkDownloadResponse> downloadProductUnifiedTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode) {
    LOG.info("Start: API to download unified template for business partner : {}", businessPartnerCode);
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "downloadProductUnifiedTemplate", null, username, requestId, storeId, channelId,
            clientId, businessPartnerCode, null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    UnifiedBulkDownloadResponse unifiedBulkDownloadResponse = new UnifiedBulkDownloadResponse();
    setMdcParameters(storeId, channelId, clientId, requestId, username);
    try {
      UnifiedBulkDownloadDTO unifiedBulkDownloadDTO =
          bulkDownloadService.downloadProductUnifiedTemplate(storeId, requestId, businessPartnerCode,
              Collections.emptySet());
      BeanUtils.copyProperties(unifiedBulkDownloadDTO, unifiedBulkDownloadResponse);
      isSuccess = true;
    } catch (UnifiedBulkDownloadException unifiedBulkDownloadException) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, unifiedBulkDownloadException),
          unifiedBulkDownloadException);
      errorMessage = unifiedBulkDownloadException.getErrorMessage();
      errorCode = unifiedBulkDownloadException.getErrorCode();
    } catch (Exception e) {
      errorMessage = "Error while downloading unified product template." + e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    LOG.info("Success: API to download unified template for business partner : {}", businessPartnerCode);
    return new GdnRestSimpleResponse<>(errorMessage, errorCode, isSuccess, requestId, unifiedBulkDownloadResponse);
  }

  @Operation(summary = "API v2 to download product unified template",
             description = "API v2 to download product unified template")
  @RequestMapping(value = DOWNLOAD_PRODUCT_UNIFIED_TEMPLATE_V2, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<UnifiedBulkDownloadResponse> downloadProductUnifiedTemplateV2(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestBody PickupPointCodesRequestDTO request) {
    LOG.info("Start: API to download unified template V2 for business partner : {}, request {}",
        businessPartnerCode, request);
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "downloadProductUnifiedTemplateV2", null, username, requestId, storeId, channelId,
            clientId, businessPartnerCode, null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    UnifiedBulkDownloadResponse unifiedBulkDownloadResponse = new UnifiedBulkDownloadResponse();
    setMdcParameters(storeId, channelId, clientId, requestId, username);
    try {
      UnifiedBulkDownloadDTO unifiedBulkDownloadDTO =
          bulkDownloadService.downloadProductUnifiedTemplate(storeId, requestId, businessPartnerCode,
              request.getPickupPointCodes());
      BeanUtils.copyProperties(unifiedBulkDownloadDTO, unifiedBulkDownloadResponse);
      isSuccess = true;
    } catch (UnifiedBulkDownloadException unifiedBulkDownloadException) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, unifiedBulkDownloadException),
          unifiedBulkDownloadException);
      errorMessage = unifiedBulkDownloadException.getErrorMessage();
      errorCode = unifiedBulkDownloadException.getErrorCode();
    } catch (Exception e) {
      errorMessage = "Error while downloading unified product template V2." + e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    LOG.info("Success: API to download unified template V2 for business partner : {}", businessPartnerCode);
    return new GdnRestSimpleResponse<>(errorMessage, errorCode, isSuccess, requestId, unifiedBulkDownloadResponse);
  }

  private static void setMdcParameters(String storeId, String channelId, String clientId,
        String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
  }

  @RequestMapping(value = DOWNLOAD_VENDOR_PRODUCT_EXCEL, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk Download Vendor Products",
      description = "Api for generating excel with vendor products")
  public GdnBaseRestResponse downloadBulkVendorProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody FilterSummaryRequest filterSummaryRequest) {
    LOG.info("Generating email with excel for vendor products with request ID : {}", requestId);
    try {
      requestId = UUID.randomUUID().toString();
      bulkDownloadServiceWrapper
          .downloadAndSendVendorProductMail(storeId, filterSummaryRequest, requestId, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Error generating excel and sending mail for vendor products, Error : ", e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = CHECK_CAMPAIGN_PENDING_BULK_DOWNLOAD_REQUESTS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check campaign bulk downloads", description = "Check campaign bulk downloads")
  public GdnRestSimpleResponse<Long> checkCampaignBulkDownloadRequest(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode,
      @RequestBody CampaignBulkDownloadRequest request) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "checkCampaignBulkDownloadRequest", businessPartnerCode, username, requestId,
            storeId, channelId, clientId, request.getCampaignCode(), String.valueOf(request.getPage()));
    LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    try {
      GdnPreconditions
          .checkArgument(StringUtils.isNotBlank(request.getCampaignCode()), CAMPAIGN_CODE_MUST_NOT_BE_BLANK);
      Long count = bulkDownloadService.countNumberOfCampaignDownloads(storeId, businessPartnerCode, request);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, count);
    } catch (Exception ex) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, ex), ex);
      return new GdnRestSimpleResponse<>(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId, null);
    }
  }

  @RequestMapping(value = GET_INPROGRESS_COUNT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get in progress download list", description = "api to get inprogress download list")
  public GdnRestSingleResponse<BulkInternalPendingRequestResponse> getPendingBulkRequests(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode, @RequestParam String downloadType)
      throws Exception {
    BulkInternalPendingRequestResponse pendingCount =
        bulkDownloadService.countPendingRequestsByUsernameAndDownloadType(businessPartnerCode, username, downloadType);
    return new GdnRestSingleResponse<>(null, null, true, pendingCount, requestId);
  }

  @RequestMapping(value = CLEAR_IN_PROGRESS_DOWNLOADS, method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Clear in progress downloads", description = "Clear in progrress downloads")
  public GdnBaseRestResponse clearInProgressDownloads(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String entityType, @RequestParam String status) {
    try {
      bulkDownloadService.clearInProgressDownloads(storeId, entityType, status);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      LOG.error("Error while aborting in progress downloads", ex);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = PROCESS_ALL_DOWNLOAD, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to process downloads in batch", description = "Api to process downloads in batch")
  public GdnBaseRestResponse processBulkDownloads(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) throws Exception {
    bulkProcessDownloadService.processDownload(storeId);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = DOWNLOAD_TAGGED_PRODUCTS, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Api to make entry in bulk download table for tagged products", description = "Api to make entry in bulk download table for tagged products")
  public GdnBaseRestResponse processBulkDownloadTaggedProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody TaggedProductFilterRequest taggedProductFilterRequest) {
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(taggedProductFilterRequest.getEmailAddress()),
          BR_EMAIL_ADDRESS_MUST_NOT_BE_BLANK);
      bulkProcessDownloadService.processDownloadTaggedProducts(taggedProductFilterRequest);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      LOG.error("Error while creating entry in bulk for tagged products", ex);
      return new GdnBaseRestResponse(false);
    }
  }
}
