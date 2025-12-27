package com.gdn.mta.bulk.controller;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.service.EANProductLevel4BulkUpdateService;
import com.gdn.mta.bulk.service.ExternalProductCreationService;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessAddCampaignProductRequest;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessPublishRequest;
import com.gdn.mta.bulk.dto.BulkProcessRequest;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkProcessSubjectToVatRequest;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import com.gdn.mta.bulk.service.BulkDeleteService;
import com.gdn.mta.bulk.service.BulkProcessDataService;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.BulkProcessServiceWrapper;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.mta.bulk.service.BulkUpsertService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.mta.bulk.service.util.BeanUtils;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.ConverterUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping(value = BulkProcessController.BASE_PATH)
@Tag(name = "BulkProcessController", description = "Bulk Process Service API")
@Slf4j
public class BulkProcessController {

  private static final Logger LOG = LoggerFactory.getLogger(BulkProcessController.class);
  public static final String ROOT = "/";
  private static final String DETAIL = "{id}";
  private static final String DETAIL2 = "{id2}";
  public static final String BASE_PATH = ROOT + "api/bulk-process";
  public static final String UPLOAD = ROOT + "upload";
  public static final String EXTERNAL_UPLOAD = ROOT + "external-upload";
  public static final String DELETE_BULK_PROCESS_CODE = ROOT + "delete/bulk-process-code";
  public static final String FILTER_BUSINESS_PARTNER_CODE =
      ROOT + "filter/business-partner-code" + ROOT;
  public static final String DELETE_BULK_PROCESS_RECORDS = ROOT + "delete/bulk-process-records";
  public static final String FILTER_BULK_PROCESS_CODE = ROOT + "filter/bulk-process-code" + ROOT;
  public static final String FILTER_BULK_PROCESS_CODE_WITH_PROMO =
      FILTER_BULK_PROCESS_CODE + "{bulkProcessCode}/promoNotes";
  public static final String FILTER_BULK_PROCESS_CODE_WITH_WHOLESALE_CONFIG =
      FILTER_BULK_PROCESS_CODE + "{bulkProcessCode}/wholesaleConfig";
  public static final String UPLOAD_BULK_UPDATE = ROOT + "upload-bulk-update";
  public static final String UPLOAD_BULK_UPDATE_EAN = ROOT + "upload-bulk-update-ean";
  public static final String UPLOAD_BASIC_INFO_BULK_UPDATE = ROOT + "upload-bulk-basic-info-file";
  public static final String UPLOAD_BULK_ADD_CAMPAIGN_PRODUCT = ROOT + "upload-bulk-add-campaign-product";
  public static final String UPLOAD_BULK_UPSERT_OFFLINE_ITEMS = ROOT + "upload-bulk-upsert-offline-items";
  public static final String UPLOAD_BULK_DELETE_OFFLINE_ITEMS = ROOT + "upload-bulk-delete-offline-items";
  public static final String UPLOAD_BULK_RECAT = ROOT + "upload-bulk-recat";
  public static final String SCHEDULE_EXCEL_FILE_DELETE = ROOT + "delete-bulk-upload-files";
  public static final String ABORT_PENDING_TASK = ROOT + "abort-pending-tasks";
  public static final String ABORT_PENDING_DOWNLOAD_TASK = ROOT + "abort-pending-download-task";
  public static final String CHECK_STUCK_PROCESS_STATUS = ROOT + "check-stuck-process-status";
  public static final String BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK =
      "bulkProcessType must not be blank";
  public static final String BULK_PROCESS_CODE_MUST_NOT_BE_BLANK =
      "bulkProcessCode must not be blank";
  public static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK =
      "businessPartnerCode must not be blank";
  public static final String FILES_MUST_NOT_BE_BLANK = "files must not be blank";
  public static final String SERVICE_BEAN_NAME = "BulkUpdateService";
  private static final int PRIVILEGED_MAP_SIZE = 7;
  public static final String CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE = ROOT + "check-pending-bulk-requests";
  public static final String CHECK_QR_PROCESS_ALLOWED = ROOT + "check-qr-process-allowed";
  public static final String CHECK_PENDING_BULK_CNC_REQUESTS_BY_BP_CODE = ROOT + "check-pending-bulk-cnc-requests";
  public static final String BULK_ARCHIVE_ITEM = ROOT + "bulk-archive-items";
  public static final String BULK_ARCHIVE_PRODUCTS = ROOT + "bulk-archive-productSkus";
  public static final String BULK_UPDATE_OFF2ON = ROOT + "bulk-update-off2on";
  public static final String REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE = ROOT + "regenerateCategoryAttributeMapping";
  public static final String REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE_BY_FILE_TYPE =
      ROOT + "regenerateCategoryAttributeMappingByFileType";
  public static final String REGENERATE_BRAND_VALUES_IN_GENERIC_TEMPLATE = ROOT + "regenerateBrandValues";
  private static final String PRODUCT_LEVEL_3 = "ProductLevel3";
  public static final String REGENERATE_MASTER_BRAND_VALUES_IN_GENERIC_TEMPLATE = ROOT + "regenerateMasterBrandValues";
  public static final String REGENERATE_BRAND_VALUES_IN_CATEGORY_TEMPLATE = ROOT + "regenerateBrandValuesCategoryTemplate";
  public static final String COUNT_NUMBER_OF_UPLOADS = ROOT + "countNumberOfUploads";
  public static final String GET_PENDING_BULK_REQUESTS_MAIL = ROOT + "get-pending-bulk-request-mail";
  public static final String UPLOAD_SUBJECT_TO_VAT = ROOT + "upload-subject-to-vat";
  public static final String DELETE_DATA_FROM_DB = ROOT + "delete-data-from-Db";
  public static final String PUBLISH_BULK_CREATE_EVENT = ROOT + "publish_bulk_create_event";
  public static final String PUBLISH_EVENTS_BY_PROCESS_TYPE = ROOT + "{bulkProcessType}/publishEvents";
  public static final String SEND_NOTIFICATION_BY_PROCESS_TYPE = ROOT + "{bulkProcessType}/sendNotification";
  public static final String CHECK_BULK_PROCESS_STATUS = ROOT + "{bulkProcessType}/check-bulk-process-status";
  public static final String ABORT_BULK_PROCESS = ROOT + "abort-bulk-process";
  public static final String BULK_PROCESS_STATUS_LISTING = ROOT + "bulk-process-status-listing";
  public static final String EVALUATE_BULK_PROCESS_ESTIMATION = ROOT + "evaluateBulkProcessEstimation";

  public static final String UPLOAD_EXCEL_FOR_QR = ROOT + "upload-qr-excel";
  public static final String BULK_WORK_ORDER = ROOT + "bulk-work-order-creation";
  public static final String ABORT_STRUCK_PROCESSES_BY_PROCESS_TYPE = ROOT + "abort-struck-processes-by-process-type";

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkProcessServiceWrapper bulkProcessServiceWrapper;

  @Autowired
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private ExternalProductCreationService externalProductCreationService;

  @Autowired
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  @Value("${bulk.creation.avoid.redundant.download}")
  private boolean avoidRedundantDownloadInBulkCreation;

  @RequestMapping(value = DELETE_BULK_PROCESS_CODE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete bulk process by bulkProcessCode", description = "delete bulk process by"
      + " bulkProcessCode")
  public GdnBaseRestResponse deleteByBulkProcessCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BulkProcessRequest request) throws ApplicationException {
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "deleteByBulkProcessCode", request.getBusinessPartnerCode(),
        username, requestId, storeId, channelId, clientId, request.getBulkProcessCode() + ":" + request.getBulkProcessType(),
        request.toString());
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBulkProcessCode()),
          BULK_PROCESS_CODE_MUST_NOT_BE_BLANK);
      this.bulkProcessService.deleteByBulkProcessCode(storeId, request.getBulkProcessCode());
      return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if merchant has pending ", description = "Check if merchant has pending ")
  public GdnRestSingleResponse<BulkPendingRequestsResponse> checkForPendingBulkProcessByMerchantCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) String businessPartnerCode,
      @RequestParam(required = false, defaultValue = StringUtils.EMPTY) String bulkProcessType,
      @RequestParam(required = false, defaultValue = Constant.BULK_UPLOAD_TYPE) String type) {
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "checkForPendingBulkProcess",
        businessPartnerCode, username, requestId, storeId, channelId, clientId, null, null);
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
          BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
      BulkPendingRequestsResponse bulkPendingRequestsResponse =
          this.bulkProcessService.checkForPendingBulkProcess(storeId, username, type,
              businessPartnerCode, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, bulkProcessType);
      return new GdnRestSingleResponse<>(null, null, true, bulkPendingRequestsResponse, requestId);
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnRestSingleResponse<>(null, null, false, null, requestId);
    }
  }

  @RequestMapping(value = CHECK_PENDING_BULK_CNC_REQUESTS_BY_BP_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if cnc merchant has pending bulk process (?)",
      description = "Check if cnc merchant has pending bulk process (?)")
  public GdnRestSingleResponse<BulkPendingRequestsResponse> checkForPendingBulkCncProcessByMerchantCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "checkForPendingBulkCncProcess", businessPartnerCode,
            username, requestId, storeId, channelId, clientId, null, null);
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
          BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
      BulkPendingRequestsResponse bulkPendingRequestsResponse =
          this.bulkProcessService.checkForPendingBulkProcess(storeId, username, Constant.BULK_UPLOAD_TYPE,
              businessPartnerCode, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE,
              Constant.INSTANT_PICKUP_PRODUCT);
      return new GdnRestSingleResponse<>(null, null, true, bulkPendingRequestsResponse, requestId);
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnRestSingleResponse<>(null, null, false, null, requestId);
    }
  }

  @RequestMapping(value = FILTER_BULK_PROCESS_CODE
      + DETAIL, method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk process filter by bulkProcessCode", description = "bulk process filter by"
      + " bulkProcessCode")
  public GdnRestSingleResponse<BulkProcessResponse> filterByBulkProcessCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("id") String bulkProcessCode) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "filterByBulkProcessCode", null, 
        username, requestId, storeId, channelId, clientId, bulkProcessCode, null);

      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      BulkProcess bulkProcess = this.bulkProcessService.findByBulkProcessCode(storeId, bulkProcessCode);
      BulkProcessResponse bulkProcessResponse = null;
      if (bulkProcess != null) {
        bulkProcessResponse = new BulkProcessResponse();
        BeanUtils.copyProperties(bulkProcess, bulkProcessResponse, new String[] {"bulkProcessNotes"});
        for (BulkProcessNotes bulkProcessNotes : bulkProcess.getBulkProcessNotes()) {
          if(!bulkProcessNotes.isPromoNote()) {
            BulkProcessNotesResponse bulkProcessNotesResponse = new BulkProcessNotesResponse();
            BeanUtils.copyProperties(bulkProcessNotes, bulkProcessNotesResponse);
            bulkProcessResponse.getBulkProcessNotes().add(bulkProcessNotesResponse);
          }
        }
      }
      return new GdnRestSingleResponse<BulkProcessResponse>(null, null, true, bulkProcessResponse,
          requestId);
  }

  @RequestMapping(value = FILTER_BULK_PROCESS_CODE_WITH_PROMO
      + DETAIL, method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk process promo notes filter by bulkProcessCode", description =
      "bulk process promo notes filter by bulkProcessCode")
  public GdnRestListResponse<BulkProcessNotesResponse> filterPromoBulkProcessNotesByBulkProcessCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("bulkProcessCode") String bulkProcessCode) {

    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "filterByBulkProcessCode", null,
        username, requestId, storeId, channelId, clientId, bulkProcessCode, null);
    try{
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      List<BulkProcessNotesResponse> bulkProcessNotesResponseList =
          this.bulkProcessService.filterPromoBulkProcessNotes(storeId, bulkProcessCode);
      return new GdnRestListResponse<>(null, null, true, bulkProcessNotesResponseList, new PageMetaData(), requestId);
    } catch(Exception e){
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = FILTER_BULK_PROCESS_CODE_WITH_WHOLESALE_CONFIG
      + DETAIL, method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk process promo notes filter by bulkProcessCode", description = "bulk process promo notes filter by bulkProcessCode")
  public GdnRestSingleResponse<WholeSaleCountResponse> filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("bulkProcessCode") String bulkProcessCode) {

    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "filterByBulkProcessCode", null, username, requestId, storeId, channelId,
            clientId, bulkProcessCode, null);
    try {
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      WholeSaleCountResponse wholeSaleCountResponse =
          this.bulkProcessService.filterWholeSaleConfigBulkProcessNotes(storeId, bulkProcessCode);
      return new GdnRestSingleResponse<>(null, null, true, wholeSaleCountResponse, requestId);
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = FILTER_BUSINESS_PARTNER_CODE + DETAIL + ROOT + "bulk-process-type" + ROOT
      + DETAIL2, method = RequestMethod.GET)
  @Operation(summary = "bulk process summary filter by businessPartnerCode and bulkProcessType "
      + "and createdBy", description = "bulk process summary filter by businessPartnerCode and "
      + "bulkProcessType and createdBy")
  public GdnRestListResponse<BulkProcessResponse>
  filterByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @PathVariable("id") String businessPartnerCode, @PathVariable("id2") String bulkProcessType) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this,
        "filterByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy", businessPartnerCode,
        username, requestId, storeId, channelId, clientId, bulkProcessType, null);

      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      
      Pageable pageable = PageRequest.of(page, size);
      Page<BulkProcess> bulkProcess = this.bulkProcessService
          .findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(storeId, businessPartnerCode,
              bulkProcessType, pageable);
      List<BulkProcessResponse> bulkProcessResponses = new ArrayList<>();
      for (BulkProcess bulkProcessItem : bulkProcess) {
        BulkProcessResponse bulkProcessResponse = new BulkProcessResponse();
        BeanUtils
            .copyProperties(bulkProcessItem, bulkProcessResponse, new String[] {"bulkProcessNotes"});
        bulkProcessResponses.add(bulkProcessResponse);
      }
      return new GdnRestListResponse<>(null, null, true, bulkProcessResponses,
          new PageMetaData(size, page, bulkProcess.getTotalElements()), requestId);
  }

  @RequestMapping(value = DELETE_BULK_PROCESS_RECORDS, method = RequestMethod.DELETE)
  @Operation(summary = "delete old Bulk Process Records", description = "delete old bulk Process "
      + "Records")
  public GdnBaseRestResponse deleteOldBulkProcessRecords(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String date) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "deleteOldBulkProcessRecords", null, 
        username, requestId, storeId, channelId, clientId, date, null);

      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      
      this.bulkProcessService.updateBulkProcessRecordAsMarkForDeleteTrue(date);
      return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = FILTER_BUSINESS_PARTNER_CODE
      + DETAIL, method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk process summary filter by businessPartnerCode and createdBy", description = "bulk process summary filter by businessPartnerCode and createdBy")
  public GdnRestListResponse<BulkProcessResponse> filterByBusinessPartnerCodeAndCreatedBy(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @PathVariable("id") String businessPartnerCode) {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "filterByBusinessPartnerCodeAndCreatedBy", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, null, null);

      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      Pageable pageable = PageRequest.of(page, size);

      Page<BulkProcess> bulkProcess = this.bulkProcessService
          .findByBusinessPartnerCodeAndCreatedBy(storeId, businessPartnerCode, username, pageable);
      List<BulkProcessResponse> bulkProcessResponses = new ArrayList<>();
      for (BulkProcess bulkProcessItem : bulkProcess) {
        BulkProcessResponse bulkProcessResponse = new BulkProcessResponse();
        BeanUtils
            .copyProperties(bulkProcessItem, bulkProcessResponse, new String[] {"bulkProcessNotes"});
        bulkProcessResponses.add(bulkProcessResponse);
      }
      return new GdnRestListResponse<>(null, null, true, bulkProcessResponses,
          new PageMetaData(size, page, bulkProcess.getTotalElements()), requestId);
  }

  @RequestMapping(value = UPLOAD, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk upload process", description = "bulk upload process")
  public GdnBaseRestResponse bulkUpload(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessUploadRequest request) throws Exception {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "upload", request.getBusinessPartnerCode(), 
        username, requestId, storeId, channelId, clientId, request.getBulkProcessType(), null);
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBulkProcessType()),
          BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
    if (!avoidRedundantDownloadInBulkCreation) {
      GdnPreconditions.checkArgument(!request.getFiles().isEmpty(), FILES_MUST_NOT_BE_BLANK);
    }
    try {
      ProcessorService processorService =
          (ProcessorService) this.autowireCapableBeanFactory.getBean(request.getBulkProcessType() + "ProcessorService");
      request.getArgs().put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      processorService.preProcess(storeId, requestId, request.getBulkProcessType(), request.getBusinessPartnerCode(),
          request.getFiles(), request.getArgs(), GdnUUIDHelper.generateUUID(), username);
    } catch (ApiIncorrectInputDataException exception) {
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, exception.getErrorCode()),
          exception);
      return new GdnBaseRestResponse(exception.getErrorMessage(), exception.getErrorCode(), false, requestId);
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e));
      return new GdnBaseRestResponse(e.getMessage(), StringUtils.EMPTY, false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = EXTERNAL_UPLOAD, method = RequestMethod.POST, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk upload process", description = "bulk upload process")
  public GdnBaseRestResponse externalBulkUpload(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestBody BulkProcessExternalUploadRequest request) {
    log.info("Received externalBulkUpload request | storeId={} | requestId={} | username={} | "
        + " channelId={} | clientId={} | requestBody={} ", storeId, requestId, username, channelId,
      clientId, request);
    try {
      externalProductCreationService.preProcess(storeId, requestId, username, request);
    } catch (ApplicationRuntimeException exception) {
      log.error("ApplicationRuntimeException in externalBulkUpload | requestId={} | message={} ",
        requestId, exception.getErrorMessage(), exception);
      return new GdnBaseRestResponse(exception.getErrorMessage(),
        exception.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error("System error in externalBulkUpload | requestId={} ", requestId, e);
      return new GdnBaseRestResponse(Constant.SYSTEM_ERROR, StringUtils.EMPTY, false, requestId);
    }
    log.info("Completed externalBulkUpload successfully | requestId={} | bulkProcessCode={} ",
      requestId, request.getBulkProcessCode());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = UPLOAD_BULK_UPDATE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Update process", description = "Api for bulk update of products")
  public GdnBaseRestResponse uploadForBulkUpdate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BulkProcessV2Request bulkRequest)
      throws Exception {
    
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "uploadForBulkUpdate", bulkRequest.getBusinessPartnerCode(), 
        username, requestId, storeId, channelId, clientId, bulkRequest.getBulkProcessType(), null);
    
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);

    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
      BulkUpdateServiceUtil.bulkProcessUpdateRequestValidation(bulkRequest);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(bulkRequest);
      BulkUpdateService bulkUpdateService = (BulkUpdateService) this.autowireCapableBeanFactory
          .getBean(bulkRequest.getBulkProcessType() + SERVICE_BEAN_NAME);
      bulkUpdateService.preProcessBulkUpdate(storeId, requestId, bulkUpdateProcessDTO,
          bulkRequest.getAccessiblePickupPoints());
      isSuccess = true;
    } catch (ApplicationRuntimeException exception) {
      errorMessage = exception.getMessage();
      errorCode = exception.getErrorCodes().getCode();
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, errorCode),
          exception);
      this.trackerService
              .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED,
                      username);

    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      this.trackerService
              .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED,
                      username);
    } finally {
      return new GdnBaseRestResponse(errorMessage, errorCode, isSuccess, requestId);
    }
  }

  @RequestMapping(value = UPLOAD_BULK_UPDATE_EAN, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Update-EAN process", description = "Api for bulk update ean of products")
  public GdnBaseRestResponse uploadForBulkUpdateEAN(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessV2Request bulkRequest) throws Exception {

    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "uploadForBulkUpdateEAN", bulkRequest.getBusinessPartnerCode(), username,
            requestId, storeId, channelId, clientId, bulkRequest.getBulkProcessType(), null);

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);

    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
      BulkUpdateServiceUtil.bulkProcessUpdateRequestValidation(bulkRequest);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(bulkRequest);
      eanProductLevel4BulkUpdateService.preProcessBulkUpdateEAN(storeId, requestId, bulkUpdateProcessDTO);
      isSuccess = true;
    } catch (ApplicationRuntimeException exception) {
      errorMessage = exception.getMessage();
      errorCode = exception.getErrorCodes().getCode();
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, errorCode), exception);
      this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED, username);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED, username);
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, isSuccess, requestId);
  }


  @PostMapping(value = UPLOAD_BASIC_INFO_BULK_UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk basic info Update process", description = "Api for bulk basic info update of products")
  public GdnBaseRestResponse uploadBulkBasicInfoFile(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkBasicInfoRequest bulkBasicInfoRequest) throws Exception {
    log.info("Processing bulk basic info update for request : {} and file : {} ", bulkBasicInfoRequest,
        bulkBasicInfoRequest.getFileName());
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    boolean success = false;
    try {
      BulkUpdateServiceUtil.bulkProcessBasicInfoValidation(bulkBasicInfoRequest);
      bulkBasicInfoRequest.setStoreId(storeId);
      bulkBasicInfoUpdateService.preProcessBulkBasicInfoUpdate(storeId, requestId, bulkBasicInfoRequest);
      success = true;
    } catch (Exception e) {
      String errorMessage = e.getMessage();
      log.error("Error occurred while processing bulk info update sheet for request : {} ", bulkBasicInfoRequest, e);
      return new GdnBaseRestResponse(errorMessage, null, success, requestId);
    }
    return new GdnBaseRestResponse(null, null, success, requestId);
  }

  @RequestMapping(value = UPLOAD_BULK_ADD_CAMPAIGN_PRODUCT, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Add process", description = "Api for bulk add of campaign products")
  public GdnBaseRestResponse uploadForBulkAddCampaignProduct(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username,
    @RequestParam(defaultValue = "false", required = false) boolean filterOutInactiveCn,
      @RequestBody BulkProcessAddCampaignProductRequest bulkProcessAddCampaignProductRequest)
      throws Exception {

    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "uploadForBulkUpdate",
        bulkProcessAddCampaignProductRequest.getBusinessPartnerCode(), username, requestId, storeId,
        channelId, clientId, bulkProcessAddCampaignProductRequest.getBulkProcessType(), null);

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);

    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
      BulkUpdateServiceUtil
          .bulkProcessUpdateRequestValidation(bulkProcessAddCampaignProductRequest);

      BulkAddCampaignProductDTO bulkAddCampaignProductDTO = ConverterUtil.
          convertToBulkAddCampaignProductDTO(bulkProcessAddCampaignProductRequest);
      bulkAddCampaignProductDTO.setFilterOutInactiveCn(filterOutInactiveCn);
      BulkUpdateService bulkUpdateService = (BulkUpdateService) this.autowireCapableBeanFactory
          .getBean(PRODUCT_LEVEL_3 + SERVICE_BEAN_NAME);

      bulkUpdateService.preProcessCampaignProductBulkUpdate(storeId, requestId,
          bulkAddCampaignProductDTO);
      isSuccess = true;
    } catch (ApplicationRuntimeException exception) {
      errorMessage = exception.getMessage();
      errorCode = exception.getErrorCodes().getCode();
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, errorCode),
          exception);

    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e));
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, isSuccess, requestId);
  }

  @RequestMapping(value = UPLOAD_SUBJECT_TO_VAT, method = RequestMethod.POST,
                  consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-Add process", description = "Api for bulk add of campaign products")
  public GdnBaseRestResponse uploadSubjectToVat(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username,
      @RequestBody BulkProcessSubjectToVatRequest bulkProcessSubjectToVatRequest)
      throws Exception {

    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "uploadForBulkUpdate",
        bulkProcessSubjectToVatRequest.getBusinessPartnerCode(), username, requestId, storeId,
        channelId, clientId, BulkProcessType.SUBJECT_TO_VAT.getValue(), null);

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);

    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(bulkProcessSubjectToVatRequest.getFileName()),
          GenericErrorMessages.FILE_NAME_MUST_NOT_BE_BLANK);
      bulkProcessService.preProcessSubjectToVatUploadEvent(storeId, requestId, username,
          bulkProcessSubjectToVatRequest.getBusinessPartnerCode(), bulkProcessSubjectToVatRequest.getBulkProcessCode(),
          bulkProcessSubjectToVatRequest.getFileName());
      isSuccess = true;
    } catch (ApplicationRuntimeException exception) {
      errorMessage = exception.getMessage();
      errorCode = exception.getErrorCodes().getCode();
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, errorCode),
          exception);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e));
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, isSuccess, requestId);
  }

  public static BulkUpdateProcessDTO convertBulkProcessDTO(BulkProcessUpdateRequest bulkRequest) {
    BulkUpdateProcessDTO bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    BeanUtils.copyProperties(bulkRequest, bulkUpdateProcessDTO);
    return bulkUpdateProcessDTO;
  }

  @RequestMapping(value = UPLOAD_BULK_RECAT, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk upload recat process", description = "bulk upload react process")
  public GdnBaseRestResponse uploadBulkRecat(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessUploadRequest request) throws Exception {

    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "uploadBulkRecat", request
            .getBusinessPartnerCode(), username, requestId, storeId, channelId, clientId, request.getBulkProcessType(),
            null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getBulkProcessType()),
            BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!request.getFiles().isEmpty(), FILES_MUST_NOT_BE_BLANK);

    ProcessorService processorService = (ProcessorService) this.autowireCapableBeanFactory
            .getBean(request.getBulkProcessType() + Constant.PROCESSOR_SERVICE);
    request.getArgs().put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    processorService.preProcess(storeId, requestId, request.getBulkProcessType(),
        request.getBusinessPartnerCode(), request.getFiles(), request.getArgs(),
        UUID.randomUUID().toString(), username);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = SCHEDULE_EXCEL_FILE_DELETE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Deletes files older than a certain number of days present in the system from bulk upload")
  public GdnBaseRestResponse scheduledDeletionBulkUploadFile(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    try {
      LOG.info("Starting scheduled deletion for : {}", requestId);
      this.bulkProcessService.findAndDeleteBulkProcessCodesBeforeXDays(storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Error while scheduled deletion for request Id: {} ", requestId , e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = REGENERATE_BRAND_VALUES_IN_GENERIC_TEMPLATE, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Regenerate brand values in generic template")
  public GdnBaseRestResponse regenerateBrandValuesInGenericBulkTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false, defaultValue = "DEFAULT_FILE") String fileType) {
    setMdcParameters(storeId, channelId, clientId, requestId, username);
    LOG.info("Starting scheduled regeneration of brand values for : {}", requestId);
    this.bulkProcessService.regenerateBrandValuesInGenericBulkTemplate(fileType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  private static void setMdcParameters(String storeId, String channelId, String clientId,
      String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
  }

  @RequestMapping(value = BULK_ARCHIVE_ITEM, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk Archive Item Skus", description = "Bulk Archive Item Skus")
  public GdnBaseRestResponse bulkArchiveItems (@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessUpdateRequest request) {
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "upload", request.getBusinessPartnerCode(),
        username, requestId, storeId, channelId, clientId, request.getBulkProcessType(), null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getBulkProcessType()),
          BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(request);
      BulkUpdateService bulkUpdateService = (BulkUpdateService) this.autowireCapableBeanFactory.getBean(
          bulkUpdateProcessDTO.getBulkProcessType() + SERVICE_BEAN_NAME);
      bulkUpdateService.preProcessBulkArchiveItems(storeId, requestId, bulkUpdateProcessDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception ex) {
      LOG.error("Error while bulk archiving items ", ex);
      return new GdnBaseRestResponse(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(value = BULK_ARCHIVE_PRODUCTS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk Archive Product Skus", description = "Bulk Archive Product Skus")
  public GdnBaseRestResponse bulkArchiveProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessUpdateRequest request) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "upload", request.getBusinessPartnerCode(), username, requestId, storeId,
            channelId, clientId, request.getBulkProcessType(), null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    try {
      GdnPreconditions
          .checkArgument(StringUtils.isNotBlank(request.getBulkProcessType()), BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(request);
      BulkUpdateService bulkUpdateService = (BulkUpdateService) this.autowireCapableBeanFactory
          .getBean(bulkUpdateProcessDTO.getBulkProcessType() + SERVICE_BEAN_NAME);
      bulkUpdateService.preProcessBulkArchiveProducts(storeId, requestId, bulkUpdateProcessDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception ex) {
      LOG.error("Error while bulk archiving products ", ex);
      return new GdnBaseRestResponse(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = BULK_WORK_ORDER, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "API to create workOrders", description = "API to create workOrders")
  public GdnBaseRestResponse createWorkOrder(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessUpdateRequest bulkWorkOrderRequest) {
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(bulkWorkOrderRequest.getBulkProcessType()),
          BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
      LOG.info("Creating {} work order with request {} ", bulkWorkOrderRequest.getBulkProcessType(),
          bulkWorkOrderRequest);
      BulkUpdateProcessDTO bulkWorkOrderDTO = convertBulkProcessDTO(bulkWorkOrderRequest);
      bulkProcessService.preProcessWorkOrder(storeId, requestId, bulkWorkOrderDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Error while creating bulk work-order and Error - {} ", e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = BULK_UPDATE_OFF2ON, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk update off2on", description = "Bulk update off2on")
  public GdnBaseRestResponse bulkUpdateOff2On(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody BulkProcessUpdateRequest request) {
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "upload", request.getBusinessPartnerCode(),
        username, requestId, storeId, channelId, clientId, request.getBulkProcessType(), null);
    LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getBulkProcessType()),
          BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(request);
      BulkUpdateService bulkUpdateService =
          (BulkUpdateService) this.autowireCapableBeanFactory.getBean(PRODUCT_LEVEL_3 + SERVICE_BEAN_NAME);
      bulkUpdateService.preProcessBulkUpdateOff2On(storeId, requestId, bulkUpdateProcessDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception ex) {
      LOG.error("Error while bulk updating o2o for products ", ex);
      return new GdnBaseRestResponse(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(value = ABORT_PENDING_TASK, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Abort pending tasks", description = "Abort Pending Tasks")
  public GdnBaseRestResponse abortPendingBulkProcess(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username,
    @RequestParam(required=false) String id) {
    try {
      bulkProcessService.abortPendingBulkProcessBefore(storeId);
      bulkProcessDataService.abortPendingBulkProcessDataBeforeOrById(storeId, id);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      LOG.error("Error while aborting pending tasks", ex);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = UPLOAD_BULK_UPSERT_OFFLINE_ITEMS, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Upload Bulk Upsert Offline Items",
      description = "API for upload bulk upsert offline items")
  public GdnBaseRestResponse uploadForBulkUpsertOfflineItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username,
      @RequestBody BulkProcessUpsertOfflineItemRequest bulkProcessUpsertOfflineItemRequest)
      throws Exception {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "uploadForBulkUpsertOfflineItems",
            bulkProcessUpsertOfflineItemRequest.getBusinessPartnerCode(), username, requestId, storeId,
            channelId, clientId, bulkProcessUpsertOfflineItemRequest.getBulkProcessType(), null);

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);

    boolean isSuccess = false;
    String errorMessage = "";
    String errorCode = "";
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
      BulkUpdateServiceUtil.bulkProcessUpdateRequestValidation(bulkProcessUpsertOfflineItemRequest);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(bulkProcessUpsertOfflineItemRequest);

      BulkUpsertService bulkUpsertService = (BulkUpsertService) this.autowireCapableBeanFactory
          .getBean(bulkProcessUpsertOfflineItemRequest.getBulkProcessType() + "BulkUpsertService");

      bulkUpsertService.preProcessInstantPickupProductBulkUpsert(storeId, requestId,
          bulkUpdateProcessDTO, bulkProcessUpsertOfflineItemRequest.getAccessiblePickupPoints());
      isSuccess = true;
    } catch (ApplicationRuntimeException exception) {
      errorMessage = exception.getMessage();
      errorCode = exception.getErrorCodes().getCode();
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, errorCode),
          exception);
      this.trackerService.sendTracker(TrackerConstants.INSTANT_PICKUP_UPDATE,
              TrackerConstants.INSTANT_PICKUP_UPDATE_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, username);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      this.trackerService.sendTracker(TrackerConstants.INSTANT_PICKUP_UPDATE,
              TrackerConstants.INSTANT_PICKUP_UPDATE_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, username);
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, isSuccess, requestId);
  }

  @RequestMapping(value = REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE, method = RequestMethod.GET, produces =
     MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Regenerate category and attribute mapping for generic template")
  public GdnBaseRestResponse regenerateCategoryAttributeInGenericBulkTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    try {
      LOG.info("Regenerate category and attribute mapping for generic template for requestId: {}, username: {}",
          requestId, username);
      this.bulkProcessService
          .regenerateCategoryAttributeMappingInGenericBulkTemplate();
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Regenerate category and attribute mapping for generic template failed ", e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE_BY_FILE_TYPE, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Regenerate category and attribute mapping for generic template")
  public GdnBaseRestResponse regenerateCategoryAttributeInGenericBulkTemplateByFileType(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String fileType) {
    try {
      LOG.info(
          "Regenerate category and attribute mapping for generic template for requestId: {}, username: {}, fileType: {} ",
          requestId, username, fileType);
      GenericTemplateFileType genericTemplateFileType =
          CommonUtils.validateAndGetGenericTemplateFileType(fileType, requestId);
      this.bulkProcessService.regenerateTemplateByFileType(storeId, genericTemplateFileType, requestId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Regenerate category and attribute mapping for generic template failed ", e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = ABORT_PENDING_DOWNLOAD_TASK, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Abort pending Download tasks", description = "Abort Pending Download Tasks")
  public GdnBaseRestResponse abortPendingDownloadTasks(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    try {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      unifiedBulkDownloadService.abortPendingDownloadProcesses(storeId);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      LOG.error("Error while aborting pending download tasks", ex);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = REGENERATE_MASTER_BRAND_VALUES_IN_GENERIC_TEMPLATE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Regenerate Master brand values in generic template")
  @ResponseBody
  public GdnBaseRestResponse regenerateMasterBrandValuesInGenericBulkTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false, defaultValue = "DEFAULT_FILE") String fileType) {
    LOG.info("Starting scheduled regeneration of brand values for : {}, username : {}", requestId, username);
    this.bulkProcessService.regenerateMasterBrandValuesInGenericBulkTemplate(fileType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = UPLOAD_BULK_DELETE_OFFLINE_ITEMS, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Upload Bulk Delete Offline Items",
      description = "API for upload bulk delete offline items")
  public GdnBaseRestResponse uploadForBulkDeleteOfflineItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BulkProcessDeleteOfflineItemRequest request)
      throws Exception {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "uploadForBulkUpsertOfflineItems",
            request.getBusinessPartnerCode(), username, requestId, storeId, channelId, clientId,
            request.getBulkProcessType(), null);
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    String errorMessage = "";
    String errorCode = "";

    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
      BulkUpdateServiceUtil.bulkProcessUpdateRequestValidation(request);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = convertBulkProcessDTO(request);
      bulkUpdateProcessDTO.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());

      BulkDeleteService bulkDeleteService = (BulkDeleteService) this.autowireCapableBeanFactory
          .getBean(request.getBulkProcessType() + "BulkDeleteService");

      bulkDeleteService.preProcessInstantPickupProductBulkDelete(storeId, requestId, bulkUpdateProcessDTO,
          request.getAccessiblePickupPoints());
    } catch (ApplicationRuntimeException exception) {
      errorMessage = exception.getMessage();
      errorCode = exception.getErrorCodes().getCode();
      LOG.error(LoggerStandard.getLogErrorTemplateWithErrorCode(loggerModel, exception, errorCode),
          exception);
      this.trackerService.sendTracker(TrackerConstants.INSTANT_PICKUP_DEL,
              TrackerConstants.INSTANT_PICKUP_DEL_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, username);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      this.trackerService.sendTracker(TrackerConstants.INSTANT_PICKUP_DEL,
              TrackerConstants.INSTANT_PICKUP_DEL_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, username);
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, Boolean.TRUE, requestId);
  }

  @RequestMapping(value = REGENERATE_BRAND_VALUES_IN_CATEGORY_TEMPLATE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to regenerate active brand values in category template")
  public GdnBaseRestResponse regenerateBrandValuesInCategoryTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    LOG.info("Starting scheduled regeneration of brand values in category template for : {}, username : {}", requestId,
        username);
    this.bulkProcessService.regenerateBrandValuesInCategoryTemplate();
    return new GdnBaseRestResponse(null, null, true, requestId);

  }

  @RequestMapping(value = COUNT_NUMBER_OF_UPLOADS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Return count of number of uploads for a user")
  public GdnRestSingleResponse<UploadProcessCount> countNumberOfUploads(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String bulkProcessType, @RequestParam String status) {
    LOG.info("Starting scheduled regeneration of brand values in category template for : {}, username : {}", requestId,
        username);
    long count = this.bulkProcessService.countNumberOfUploadsByUser(storeId, bulkProcessType, username, status);
    return new GdnRestSingleResponse(null, null, true, new UploadProcessCount(count), requestId);
  }

  @RequestMapping(value = GET_PENDING_BULK_REQUESTS_MAIL, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get pending bulk request mail")
  public GdnBaseRestResponse getPendingBulkRequestMail(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    try {
      LOG.info("API to get the mail of pending bulk requests if they are more than a certain threshold");
      this.bulkProcessService.sendMailIfPendingRequestMoreThanThreshold(storeId);
    } catch (Exception e) {
      LOG.error("Error while sending penging bulk email ", e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CHECK_BULK_PROCESS_STATUS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check bulk process status")
  public GdnBaseRestResponse checkBulkProcessStatus(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("bulkProcessType") String bulkProcessType) throws Exception {
    LOG.info("API to check bulk process status");
    this.bulkProcessService.checkBulkProcessStatus(storeId, bulkProcessType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = DELETE_DATA_FROM_DB, method = RequestMethod.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to delete the values from data and image table", description = "Api to delete the values from data and image table")
  public GdnBaseRestResponse deleteFromDb(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    LOG.info("Starting scheduler to delete from data and image tables : {}, username : {}", requestId, username);
    bulkProcessService.deleteFromDb(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = PUBLISH_EVENTS_BY_PROCESS_TYPE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to publish events", description = "Api to process ready to process and processed data")
  public GdnBaseRestResponse publishEventsByProcessType(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("bulkProcessType") String bulkProcessType) throws Exception {
    LOG.info("Starting scheduler to process ready to process and processed data : {}, username : {}", requestId,
        username);
    bulkProcessServiceWrapper.processReadyToProcessData(storeId, bulkProcessType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = SEND_NOTIFICATION_BY_PROCESS_TYPE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to send notifications", description = "Api to process ready to process and processed data")
  public GdnBaseRestResponse sendNotificationByProcessType(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("bulkProcessType") String bulkProcessType) throws Exception {
    LOG.info("Starting scheduler to process ready to process and processed data : {}, username : {}", requestId,
        username);
    bulkProcessServiceWrapper.processProcessedData(storeId, bulkProcessType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = PUBLISH_BULK_CREATE_EVENT, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to publish create event", description = "Api to publish create event")
  public GdnBaseRestResponse publishCreateEvent(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody BulkProcessPublishRequest bulkProcessPublishRequest) throws Exception {
    LOG.info("Starting scheduler to process ready to process and processed data : {}, username : {}", requestId,
        username);
    BulkProcess bulkProcess = new BulkProcess();
    BeanUtils.copyProperties(bulkProcessPublishRequest, bulkProcess);
    bulkProcessService.publishBulkProductCreationEvent(storeId, bulkProcess,
      bulkProcess.getBulkProcessType());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CHECK_STUCK_PROCESS_STATUS, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check stuck process status", description = "Check stuck process status")
  public GdnBaseRestResponse checkStuckProcessStatus(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) String bulkProcessCode) {
    bulkProcessService.checkStuckProcessStatus(bulkProcessCode);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ABORT_BULK_PROCESS, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "abort pending in-progress bulk process", description = "abort pending in-progress bulk process")
  public GdnBaseRestResponse abortBulkProcess(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String bulkProcessCode) {
    try {
      bulkProcessService.abortPendingInprogressBulkProcess(storeId, bulkProcessCode);
      return new GdnBaseRestResponse(true);
    } catch (Exception e) {
      LOG.error("Error while abort bulk process : {} ", bulkProcessCode, e);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = BULK_PROCESS_STATUS_LISTING, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to retrieve the bulk process status for a merchant", description = "Api to retrieve the bulk process status for a merchant")
  public GdnRestListResponse<BulkProcessStatusListingResponse> fetchBulkProcessStatusListing(
    @RequestParam(required = false) String storeId,
    @RequestParam(required = false) String channelId,
    @RequestParam(required = false) String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @RequestParam(value = "bulkProcessType") String bulkProcessType,
    @RequestParam(value = "businessPartnerCode") String businessPartnerCode,
    @RequestParam(required = false) Optional<List<String>> bulkProcessCodes,
    @RequestParam(required = false) Optional<String> primaryIdentifier,
    @RequestParam(required = false) boolean estimationsNeeded,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size)
    throws Exception {
    LoggerAttributeModel loggerModel =
      new LoggerAttributeModel(this, "fetchProcessListingResponse", businessPartnerCode, username,
        requestId, storeId, channelId, clientId, bulkProcessType, null);
    try {
      LOG.info("Invoking fetchProcessListingResponse for merchant : {} for process-type : {} ",
        businessPartnerCode, bulkProcessType);
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(bulkProcessType),
        BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(businessPartnerCode),
        BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
      Pageable pageable = PageRequest.of(page, size);
        Page<BulkProcessStatusListingResponse> listingResponse =
          bulkProcessService.fetchProcessListingResponse(storeId, bulkProcessType, requestId,
            businessPartnerCode, bulkProcessCodes, primaryIdentifier, estimationsNeeded, pageable);
        return new GdnRestListResponse<>(null, null, true, listingResponse.getContent(),
          new PageMetaData(size, page, listingResponse.getTotalElements()), requestId);
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(),
        false, null, null, requestId);
    }
  }

  @RequestMapping(value = CHECK_QR_PROCESS_ALLOWED, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if merchant has pending ", description = "Check if QR upload allowed")
  public GdnBaseRestResponse checkForQrProcessAllowed(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "checkForQrProcessAllowed", businessPartnerCode, username,
            requestId, storeId, channelId, clientId, null, null);
    try {
      LOG.debug(LoggerStandard.getLogInfoTemplate(loggerModel));

      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
          BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
      return new GdnBaseRestResponse(
          this.bulkProcessService.checkForQrProcessAllowed(storeId, businessPartnerCode));
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = UPLOAD_EXCEL_FOR_QR, method = RequestMethod.POST, produces =
    MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Upload qr generation excel request", description = "Upload qr generation excel request")
  public GdnBaseRestResponse uploadQrGenerationExcel(@RequestParam String storeId,
    @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestBody QrExcelUploadRequest qrExcelUploadRequest) {
    LOG.info("Excel upload for request : {}, by user : {}",
      qrExcelUploadRequest.getDownloadQRCodeRequest(), username);
    try {
      bulkProcessService.insertQrExcelRequest(storeId, qrExcelUploadRequest, requestId, username);
      return new GdnBaseRestResponse(true);
    } catch (Exception e) {
      LOG.error("Error while uploading QR Excel request : {}, filename :{} ",
        qrExcelUploadRequest.getDownloadQRCodeRequest(), qrExcelUploadRequest.getFileName(), e);
      return new GdnBaseRestResponse(false);
    }
  }

  @PostMapping(value = EVALUATE_BULK_PROCESS_ESTIMATION, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to populate the estimates for process and records completion", description = "Populates blp_process_time_estimation")
  public GdnBaseRestResponse evaluateBulkProcessEstimation(@RequestParam String storeId,
    @RequestParam String username) throws JsonProcessingException {
    LOG.info("Invoking evaluateBulkProcessEstimation for process time estimation at : {} ",
      LocalDateTime.now());
    bulkProcessService.evaluateBulkProcessEstimation(storeId, username);
    return new GdnBaseRestResponse(true);
  }

  @PutMapping(value = ABORT_STRUCK_PROCESSES_BY_PROCESS_TYPE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to Abort Struck process in bulk main table", description = "Abort struck process scheduler")
  public GdnBaseRestResponse abortStruckProcessesByProcessType(@RequestParam String storeId,
    @RequestBody List<String> bulkProcessTypes) throws JsonProcessingException {
    try {
      LOG.info("Abort the struck process types  ran at : {} to abort : {} ", LocalDateTime.now(),
        bulkProcessTypes);
      bulkProcessServiceWrapper.abortStruckProcessesByProcessTypes(storeId, bulkProcessTypes);
      return new GdnBaseRestResponse(true);
    } catch (Exception ex) {
      LOG.error("Error while aborting pending tasks", ex);
      return new GdnBaseRestResponse(false);
    }
  }
}
