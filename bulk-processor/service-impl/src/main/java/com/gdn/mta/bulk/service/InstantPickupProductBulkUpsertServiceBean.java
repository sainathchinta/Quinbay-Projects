package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.BulkProcessType.DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_UPDATE_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.PreOrderRequest;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.UpsertValidationDTO;
import com.gdn.mta.bulk.service.util.BeanUtils;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.FastExcelUtils;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.util.Strings;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.dhatim.fastexcel.reader.ReadableWorkbook;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.CnExcelHeaderNames;
import com.gdn.mta.bulk.dto.BulkCncUpsertErrorDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service("InstantPickupProductBulkUpsertService")
public class InstantPickupProductBulkUpsertServiceBean implements BulkUpsertService {

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Value("${bulk.process.upsert.batch.size:100}")
  private int bulkProcessUpsertBatchSize;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${static.baseUrl}")
  private String staticBaseUrl;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${bulk.update.item.pickup.point.list.fetch.size}")
  private int itemPickupPointListFetchSize;

  @Value("${validate.bulk.max.number.of.rows}")
  private boolean validateBulkMaxNumberOfRows;

  @Value("${bulk.max.number.of.rows}")
  private int bulkMaxNumberOfRows;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCncRestrictionEnabled;

  @Value("${system.parameter.bopis.unsupported.merchant.types}")
  private String bopisCategoryValidationForSellerTypes;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Value("${bulk.upsert.using.fast.excel.enabled}")
  private boolean bulkUpsertUsingFastExcelEnabled;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  public static final String DOWNLOAD_LINK_HREF = "<a href=\"";
  public static final String DOWNLOAD_LINK_DOWNLOAD = "\">Download</a>";
  public static final String FAILED_PRODUCTS_FILE =
    "File produk gagal multi-pickup-point-template.xlsx";
  public static final String FILE_UPLOAD_MTA = "/filestore/upload/mta/";
  public static final String BULK_PROCESS_DATA_NULL_ERROR = "Bulk process data null error";

  @Override
  public void preProcessInstantPickupProductBulkUpsert(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO, Set<String> accessiblePickupPoints) {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();
    bulkUpdateProcessDTO.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    log.info(
      "Invoking pre process for bulk upsert instant pickup product with storeId = {}, bulkProcessCode: {}, "
          + "and businessPartnerCode: {} and bulkUpdateProcessDTO {}, accessiblePickupPoints: {} ",
        storeId, bulkProcessCode, businessPartnerCode, bulkUpdateProcessDTO.toString(),
        accessiblePickupPoints);

    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      this.saveProductInstantPickupFileToDirectory(bulkProcessCode, fileName, bulkUpdateProcessDTO);

      BulkProcess bulkProcess =
        bulkUpdateServiceUtil.getBulkProcess(storeId, requestId, bulkProcessCode,
          bulkUpdateProcessDTO, 0, 0, false, false);
      ProfileResponse businessPartner =
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(storeId, businessPartnerCode);
      bulkProcess.setInternationalMerchant(businessPartner.getCompany().isInternationalFlag());
      bulkProcess.setDescription(fileName + BulkUpdateServiceUtil.END_SYMBOL + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
      bulkProcess.setUploadedFile(Paths.get(fileName).getFileName().toString());
      if (CollectionUtils.isNotEmpty(accessiblePickupPoints)) {
        AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
        auditTrailInfo.setAccessiblePickupPointCodes(accessiblePickupPoints);
        bulkProcess.setNotes(this.objectMapper.writeValueAsString(auditTrailInfo));
      }
      bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess);

      bulkUpdateProcessDTO.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
      BulkUpdateQueue bulkUpdateQueue =
        bulkUpdateServiceUtil.getBulkUpdateQueue(storeId, requestId, bulkProcessCode,
          bulkUpdateProcessDTO);

      kafkaProducer.send(kafkaTopicProperties.getBulkUploadInstantPickupProductEvent(), bulkUpdateQueue);

      log.info(
        "Pre processing done. Sent object for Queue processing. storeId: {}, bulkProcessCode: " + "{}, businessPartnerCode: {}",
        storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      log.error(
        "Error occurred while pre processing bulk upsert instant pickup product. storeId: {}, bulkProcessCode: {}, "
          + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode, e);
      this.trackerService.sendTracker(INSTANT_PICKUP_UPDATE, INSTANT_PICKUP_UPDATE_TYPE, HYPHEN,
        FAILED, MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  @Override
  public void processInstantPickupProductBulkUpsert(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    log.info(
      "#ProductLevel3BulkUpdateServiceBean-processInstantPickupProductBulkUpsert - bulkUpdateQueue: {}",
      bulkUpdateQueue);
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();

    BulkProcess bulkProcess = bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode,
        BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      log.warn(
        "ProductLevel3BulkUpdateServiceBean-processInstantPickupProductBulkUpsert for bulkProcessCode : {} is "
          + "already processed or being processed", bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
        "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode() + " is already processed or being processed");
    }
    GdnPreconditions.checkArgument(bulkProcess != null, BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    final String updatedBy = bulkUpdateQueue.getUpdatedBy();
    final String fileName = bulkUpdateQueue.getFileName();
    AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
    if (StringUtils.isNotBlank(bulkProcess.getNotes())) {
      auditTrailInfo = this.objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    }
    auditTrailInfo.setBusinessPartnerCode(bulkUpdateQueue.getBusinessPartnerCode());
    auditTrailInfo.setRequestId(bulkUpdateQueue.getRequestId());
    auditTrailInfo.setRemoteAddress(bulkUpdateQueue.getClientHost());
    auditTrailInfo.setUsername(updatedBy);
    auditTrailInfo.setFileName(fileName);
    bulkProcess.setNotes(objectMapper.writeValueAsString(auditTrailInfo));
    bulkProcess = bulkProcessService.saveOperation(bulkProcess);

    List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList = new ArrayList<>();
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    int upsertedInstantPickupProduct = 0;
    int totalInstantPickupProduct = 0;
    try {
      Map<Integer, String> updatedHeaders = new HashMap<>();
      List<Map<String, String>> instantPickupProductRowDataFromExcel = new ArrayList<>();
      Sheet excelSheetData = null;
      if (bulkUpsertUsingFastExcelEnabled) {
        try (InputStream is = fileStorageServiceBean.openGcsInputStream(bulkUpdateQueue, bulkProcess);
            ReadableWorkbook workbook = new ReadableWorkbook(is)) {
          Map<Integer, String> headers = FastExcelUtils.readHeaders(workbook, 0, 0, bulkProcess.getBulkProcessCode());
          if (headers.isEmpty()) {
            bulkUpdateServiceUtil.saveBulkProcessAsAborted(bulkProcess, bulkUpdateQueue, bulkUpdateErrorCounter);
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                BulkUpdateServiceUtil.HEADER_MISMATCH_PRODUCT);
          }
          GdnPreconditions.checkArgument(
              bulkUpdateServiceUtil.validateForHeaderMismatchAndGetUpdatedHeaders(updatedHeaders, headers),
              BulkUpdateServiceUtil.HEADER_MISMATCH_PRODUCT);
          instantPickupProductRowDataFromExcel =
              FastExcelUtils.readDataRows(workbook, updatedHeaders, 1, bulkMaxNumberOfRows, validateBulkMaxNumberOfRows,
                  bulkProcessCode);
        }
      } else {
        excelSheetData =
            validateBulkInstantPickupProductUpsertRequest(bulkUpdateQueue, bulkProcess, bulkUpdateErrorCounter,
                updatedHeaders);
        POIUtil.validateNumberOfRows(excelSheetData, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows,
            validateBulkMaxNumberOfRows);
        instantPickupProductRowDataFromExcel =
            POIUtil.readFromExcelForBulkUpdate(excelSheetData, 1, 0, 0, updatedHeaders);
      }
      saveBulkProcessData(bulkProcess, instantPickupProductRowDataFromExcel);
      return;
    } catch (Exception e) {
      log.error("Error while processing for bulk upsert offline items - bulkUpdateQueue: {}", bulkUpdateQueue, e);
      this.trackerService.sendTracker(PRODUCT_UPDATE_EVENT, PRODUCT_UPDATE_ATTRI_TYPE, HYPHEN, FAILED, updatedBy);
      BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, e.getMessage(),
          bulkUpdateErrorCounter);
    }
    List<String> descriptions = Lists.newArrayList(BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_PROCESSED,
      BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_SUCCESSFULLY_PROCESSED, BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_FAILED_PROCESSED);
    bulkUpdateServiceUtil.updateBulkCncProductFinalStatus(bulkProcess, bulkUpdateQueue,
      upsertedInstantPickupProduct, totalInstantPickupProduct, bulkCncUpsertErrorDTOList,
      bulkUpdateErrorCounter, descriptions);
    this.bulkProcessService.saveOperation(bulkProcess);
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, StringUtils.EMPTY, false, false);
  }

  @Override
  public void processEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    log.info("Bulk process in progress, bulk process code : {}, rowNumbers : {}",
      bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList = bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
      bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
      bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      log.warn("No rows found in pending state for the bulk process code : {}, row numbers : {}",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      return;
    }
    AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
    if (StringUtils.isNotBlank(bulkProcess.getNotes())) {
      auditTrailInfo = this.objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    }

    Map<String, Map<String, String>> excelDataMap = new HashMap<>();
    Map<String, BulkProcessData> updatedBulkDataMap = new HashMap<>();
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, ItemPickupPointListingL3Response> offlineItemIdResponseMap = new HashMap<>();
    Map<String, ItemPickupPointListingL3Response> itemSkuResponseMap = new HashMap<>();
    UpsertValidationDTO upsertValidationDto = new UpsertValidationDTO();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      Map<String, String> row = setBlpInitialData(bulkProcessData);
      bulkProcessData.setStartDate(new Date());
      excelDataMap.put(getSkuValueFromRowMap(row) + Constant.HYPHEN + getPickupPointCodeFromRowMap(row), row);
      updatedBulkDataMap.put(getSkuValueFromRowMap(row) + Constant.HYPHEN + getPickupPointCodeFromRowMap(row),
        bulkProcessDataService.saveOperation(bulkProcessData));
    }
    log.info("Upserting the offline items for item skus : {} for bulk process code : {}",
      updatedBulkDataMap.keySet(), bulkUpdateEventModel.getBulkProcessCode());
    List<UpsertOfflineItemFailedResponse> validationFailedResponse = new ArrayList<>();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    try {
      ProfileResponse profileResponse =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateEventModel.getStoreId(),
          bulkProcess.getBusinessPartnerCode());
      List<UpsertOfflineItemRequest> requests = constructUpsertOfflineItemRequestsWithMpp(new ArrayList<>(excelDataMap.values()),
          validationFailedResponse, profileResponse, upsertValidationDto,
          auditTrailInfo.getAccessiblePickupPointCodes());
      List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(requests)) {
        PreOrderDTO preOrderDTO = null;
        if (preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse)) {
          String productSku = requests.getFirst().getItemSku()
              .substring(0, StringUtils.ordinalIndexOf(requests.getFirst().getItemSku(), HYPHEN, 3));
          preOrderDTO =
              xProductOutboundService.getBasicProductInfo(bulkUpdateEventModel.getStoreId(), productSku).getPreOrder();
        }
        fetchDataFromExistingAndValidate(bulkProcess.getBusinessPartnerCode(), requests,
          productVariantUpdateRequest, offlineItemIdResponseMap, itemSkuResponseMap);
        formL5UpdateRequest(productVariantUpdateRequest, requests, offlineItemIdResponseMap, itemSkuResponseMap,
            upsertValidationDto, validationFailedResponse, profileResponse, bulkProcess, preOrderDTO);
        if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductItems())
          || CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints())) {
          itemsPriceStockImagesUpdateResponse =
            pbpOutboundService.updateSummaryL5(bulkProcess.getCreatedBy(),
              productVariantUpdateRequest, Constant.CLIENT_ID);
        }
      }
      if (CollectionUtils.isNotEmpty(itemsPriceStockImagesUpdateResponse.getVariantsErrorList())) {
        populateErrorMessage(itemsPriceStockImagesUpdateResponse.getVariantsErrorList(), responses);
        validationFailedResponse.addAll(responses);
      }

      List<BulkProcessData> failedBulkData = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(validationFailedResponse)) {
        for (UpsertOfflineItemFailedResponse response : validationFailedResponse) {
          log.error("Error while upsert offline item for bulk process code : {}, response : {}",
            bulkUpdateEventModel.getBulkProcessCode(), response);
          String errorMessage;
          String key = response.getItemSku() + Constant.HYPHEN + response.getPickupPointCode();
          BulkProcessData bulkProcessData = updatedBulkDataMap.get(key);
          if (Objects.isNull(bulkProcessData)) {
            continue;
          }
          if (StringUtils.equals(response.getErrorCode(), BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.name())) {
            errorMessage = BulkUpdateServiceUtil.constructErrorMessage(response.getItemSku(),
              BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
          } else {
            errorMessage = BulkUpdateServiceUtil.constructErrorMessage(response.getItemSku(),
              response.getErrorCode());
          }
          bulkProcessData.setEndDate(new Date());
          bulkProcessData.setSystemErrorCount(1);
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new BulkCncUpsertErrorDTO(response.getItemSku(), response.getPickupPointCode(),
              errorMessage)));
          failedBulkData.add(bulkProcessData);
          updatedBulkDataMap.remove(key);
        }
      }

      List<BulkProcessData> successData = new ArrayList<>();
      if (MapUtils.isNotEmpty(updatedBulkDataMap)) {
        for (BulkProcessData bulkProcessData : updatedBulkDataMap.values()) {
          bulkProcessData.setEndDate(new Date());
          bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
          successData.add(bulkProcessData);
        }
      }

      if (CollectionUtils.isNotEmpty(successData)) {
        bulkUpdateServiceUtil.setScheduleRemovalForBulkProcessUpdateAndUpsert(successData,
          itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
          profileResponse.getCompany().isInternationalFlag(),
          productVariantUpdateRequest.getProductSku(), productVariantUpdateRequest.getProductName());
        bulkProcessDataService.saveBulkProcessData(successData);
      }

      if (CollectionUtils.isNotEmpty(failedBulkData)) {
        bulkProcessDataService.saveBulkProcessData(failedBulkData);
      }
    } catch (Exception e) {
      log.error("Error while processing for bulk upsert offline items = {}", bulkUpdateEventModel, e);
      for (Map.Entry<String, BulkProcessData> entry : updatedBulkDataMap.entrySet()) {
        BulkProcessData bulkProcessData = entry.getValue();
        String itemSku = entry.getKey();
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        bulkProcessData.setEndDate(new Date());
        Map<String, String> row = excelDataMap.get(itemSku);
        BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = new BulkCncUpsertErrorDTO();
        bulkCncUpsertErrorDTO.setItemSku(getSkuValueFromRowMap(row));
        bulkCncUpsertErrorDTO.setPickupPointCode(getPickupPointCodeFromRowMap(row));
        bulkCncUpsertErrorDTO.setReason(
          BulkUpdateServiceUtil.constructErrorMessage(row.get(ExcelHeaderNames.BLIBLI_SKU_EN),
            e.getMessage()));
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkCncUpsertErrorDTO));
        bulkProcessDataService.saveOperation(bulkProcessData);
      }
    }
  }

  /**
   * Construct offline item request
   *
   * @param validationPassedData must not be null
   * @param profileResponse      must not be blank
   * @return List of Upsert Offline Item Request
   */
  private List<UpsertOfflineItemRequest> constructUpsertOfflineItemRequestsWithMpp(
      List<Map<String, String>> validationPassedData,
      List<UpsertOfflineItemFailedResponse> validationFailedResponseList, ProfileResponse profileResponse,
      UpsertValidationDTO upsertValidationDto, Set<String> accessiblePickupPointCodes) throws ApplicationException {
    List<UpsertOfflineItemRequest> requests = new ArrayList<>();
    if (CollectionUtils.isEmpty(validationPassedData)) {
      return requests;
    }
    for (Map<String, String> data : validationPassedData) {
      upsertValidationDto.setResult(true);
      UpsertOfflineItemRequest request = new UpsertOfflineItemRequest();
      request.setItemSku(data.get(ExcelHeaderNames.BLIBLI_SKU_EN));
      UpsertOfflineItemFailedResponse validationFailedResponse =
        new UpsertOfflineItemFailedResponse();
      validationFailedResponse.setItemSku(data.get(ExcelHeaderNames.BLIBLI_SKU_EN));
      validationFailedResponse.setPickupPointCode(data.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN));
      validateMerchantType(profileResponse);
      if (!BulkUpdateServiceUtil.validateItemSkuByBusinessPartnerCode(data.get(ExcelHeaderNames.BLIBLI_SKU_EN),
          profileResponse.getBusinessPartnerCode())) {
        upsertValidationDto.setResult(false);
        validationFailedResponse.setErrorCode(BulkProcessValidationErrorMessages.INVALID_OR_BLANK_ITEM_SKU);
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      BulkUpdateServiceUtil.validateAndSetStock(data, validationFailedResponse, request,
        upsertValidationDto);
      if (!upsertValidationDto.isResult()) {
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      BulkUpdateServiceUtil.validateAndSetCncStatus(data, validationFailedResponse, request,
        upsertValidationDto);
      if (!upsertValidationDto.isResult()) {
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      validateAndSetPickupPointCode(data, validationFailedResponse, request, profileResponse,
          upsertValidationDto, accessiblePickupPointCodes);
      if (!upsertValidationDto.isResult()) {
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      validateAndSetOfferPrice(data, validationFailedResponse, request,
        upsertValidationDto);
      if (!upsertValidationDto.isResult()) {
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      validateAndSetListPrice(data, validationFailedResponse, request,
        upsertValidationDto.getOfferPrice(), upsertValidationDto);
      if (!upsertValidationDto.isResult()) {
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      //validate offer price should not be greater than list price
      if(request.getOfferPrice() > request.getListPrice()) {
        validationFailedResponse.setErrorCode(
            BulkProcessValidationErrorMessages.MAXIMUM_SALE_PRICE_VALUE_INVALID);
        upsertValidationDto.setResult(false);
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      BulkUpdateServiceUtil.validateAndSetOnlineStatus(data, validationFailedResponse, request,
          upsertValidationDto, cncForWarehouseFeatureSwitch);
      if (!upsertValidationDto.isResult()) {
        validationFailedResponseList.add(validationFailedResponse);
        continue;
      }
      requests.add(request);
    }
    return requests;
  }

  /**
   * Save excel file, containing upsert action of instant pickup products to directory
   *
   * @param bulkProcessCode      must not be blank
   * @param fileName             must not be blank
   * @param bulkUpdateProcessDTO must not be null
   */
  private void saveProductInstantPickupFileToDirectory(String bulkProcessCode, String fileName,
    BulkUpdateProcessDTO bulkUpdateProcessDTO) throws Exception {
    fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, fileName);
  }

  /**
   * Validate header of excel file for bulk upsert instant pickup product
   *
   * @param bulkUpdateQueue
   * @param bulkProcess
   * @param counter
   * @param updatedHeaders
   * @return
   * @throws Exception
   */
  private Sheet validateBulkInstantPickupProductUpsertRequest(BulkUpdateQueue bulkUpdateQueue,
    BulkProcess bulkProcess, BulkUpdateErrorCounter counter, Map<Integer, String> updatedHeaders)
    throws Exception {
    Sheet excelSheetData = fileStorageService.getFileData(bulkUpdateQueue, bulkProcess);
    GdnPreconditions.checkArgument(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(excelSheetData,
      bulkProcess, bulkUpdateQueue, counter, updatedHeaders), BulkUpdateServiceUtil.HEADER_MISMATCH_PRODUCT);
    return excelSheetData;
  }

  private void saveBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> dataMaps)
      throws Exception {
    log.info("Inserting data rows for bulk process code = {} ", bulkProcess.getBulkProcessCode());
    int rowNumber = 1;
    Map<String, List<BulkProcessData>> distinctParentMap = new HashMap<>();
    for (Map<String, String> dataMap : dataMaps) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      String parent = getSkuValueFromRowMap(dataMap);
      if (StringUtils.isBlank(parent)) {
        parent = bulkProcess.getBulkProcessCode() + rowNumber;
      }
      BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO =
        BulkUpdateServiceUtil.validateDataForPickupUpsert(dataMap, bulkProcess.getInternationalMerchant());
      if (Objects.nonNull(bulkCncUpsertErrorDTO)) {
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkCncUpsertErrorDTO));
        bulkProcessData.setInputErrorCount(1);
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
      }
      String pickupPointCodeParam = ExcelHeaderNames.PICKUP_POINT_CODE_EN;
      parent = parent + Constant.HYPHEN + dataMap.get(pickupPointCodeParam);
      bulkProcessData.setParentProduct(parent);
      bulkProcessData.setRowNumber(rowNumber);
      rowNumber = rowNumber + 1;
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(dataMap));
      BulkUpdateServiceUtil.addToMap(parent, bulkProcessData, distinctParentMap);
    }
    List<BulkProcessData> requestData = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(distinctParentMap, requestData, false, false);
    errorCount += bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(bulkProcess.getBusinessPartnerCode(),
      requestData, true, null);
    bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setTotalCount(rowNumber - 1);
    if (errorCount == bulkProcess.getTotalCount()) {
      bulkProcess.setStatus(BulkProcess.STATUS_PROCESSED);
    }
    bulkProcessService.saveOperation(bulkProcess);
  }

  private Map<String, String> setBlpInitialData(BulkProcessData bulkProcessData) throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    LinkedHashMap<String, String> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(),
        new TypeReference<LinkedHashMap<String, String>>() {
        });
    return rowDataJson;
  }

  @Override
  @Transactional(readOnly = false)
  public void setFinalStatusAndNotificationOnInstantPickupUpsert(BulkProcess bulkProcess, String storeId) throws Exception {
    log.info("Sending offline item upsert notification for bulk process code : {} and store Id: {}",
      bulkProcess.getBulkProcessCode(), storeId);
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    AuditTrailInfo auditTrailInfo = objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    int inputErrorCount = 0;
    int errorCount = 0;
    List<BulkProcessData> failedBulkProcessDataList = new ArrayList<>();
    List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList = new ArrayList<>();
    for (BulkProcessData bulkProcessData : rowDataList) {
      if (StringUtils.equals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus())) {
        failedBulkProcessDataList.add(bulkProcessData);
        if (Objects.nonNull(bulkProcessData.getInputErrorCount())) {
          inputErrorCount++;
        }
        BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = null;
        if (Constant.SYSTEM_ERROR.equals(bulkProcessData.getErrorMessage())) {
          bulkCncUpsertErrorDTO = new BulkCncUpsertErrorDTO();
          bulkCncUpsertErrorDTO.setReason(bulkProcessData.getErrorMessage());
        } else {
          bulkCncUpsertErrorDTO = objectMapper.readValue(bulkProcessData.getErrorMessage(), BulkCncUpsertErrorDTO.class);
        }
        bulkCncUpsertErrorDTOList.add(bulkCncUpsertErrorDTO);
        errorCount++;
      }
    }

    if (CollectionUtils.isEmpty(bulkCncUpsertErrorDTOList)) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      String status = BulkProcess.STATUS_ABORTED;
      if (bulkProcess.getTotalCount() != errorCount) {
        status = BulkProcess.STATUS_PARTIALLY_DONE;
      }
      bulkProcess.setStatus(status);
    }
    bulkProcess.setErrorCount(bulkCncUpsertErrorDTOList.size());
    bulkProcess.setInputErrorCount(inputErrorCount);
    bulkProcess.setSystemErrorCount(bulkCncUpsertErrorDTOList.size() - inputErrorCount);
    bulkProcess.setSuccessCount(bulkProcess.getTotalCount() - errorCount);
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    bulkUpdateErrorCounter.setSystemErrorCounter(bulkProcess.getSystemErrorCount());
    bulkUpdateErrorCounter.setInputErrorCounter(bulkProcess.getInputErrorCount());
    for (int i = 0; i < bulkProcess.getInputErrorCount(); i++) {
      this.trackerService.sendTracker(INSTANT_PICKUP_UPDATE, INSTANT_PICKUP_UPDATE_TYPE, HYPHEN,
        FAILED, auditTrailInfo.getUsername());
    }
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkUpdateQueue.setStoreId(bulkProcess.getStoreId());
    bulkUpdateQueue.setFileName(auditTrailInfo.getFileName());

    String url = StringUtils.EMPTY;
    if (BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus()) || BulkProcess.STATUS_ABORTED.equals(bulkProcess.getStatus())) {
      List<LinkedHashMap<String, String>> bulkProcessDataFailedList = new ArrayList<>();
      for (BulkProcessData bulkProcessData : failedBulkProcessDataList) {
        LinkedHashMap<String, String> bulkProcessDataFailedMap =
          objectMapper.readValue(bulkProcessData.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
        if (Objects.nonNull(bulkProcessDataFailedMap)) {
          bulkProcessDataFailedMap.put(BulkConfigurationUpdateParameters.FAILURE_REASON,
            bulkProcessData.getErrorMessage());
        }
        bulkProcessDataFailedList.add(bulkProcessDataFailedMap);
      }
      String filePath =
        ProcessorUtils.DOWNLOAD_FAILED_MPP_PRODUCT_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
          + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.getFileFormat(bulkUpdateQueue.getFileName());
      url = generateFailedProductsFile(bulkProcessDataFailedList, bulkProcess, filePath,
        bulkUpdateQueue.getFileName());
    }

    List<String> descriptions = Lists.newArrayList(BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_PROCESSED,
        BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_SUCCESSFULLY_PROCESSED,
        BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_FAILED_PROCESSED);
    bulkUpdateServiceUtil.updateBulkCncProductFinalStatus(bulkProcess, bulkUpdateQueue, bulkProcess.getSuccessCount(),
      bulkProcess.getTotalCount(), bulkCncUpsertErrorDTOList, bulkUpdateErrorCounter, descriptions);
    if (BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus()) || BulkProcess.STATUS_ABORTED.equals(bulkProcess.getStatus())) {
      String fileName =
        bulkUpdateQueue.getFileName().replaceFirst(FILE_UPLOAD_MTA, StringUtils.EMPTY);
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
        FAILED_PRODUCTS_FILE + StringUtils.SPACE + url, false, false);
    } else {
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, StringUtils.EMPTY, false, false);
    }
  }

  private String generateFailedProductsFile(List<LinkedHashMap<String, String>> bulkProcessDataFailedList,
    BulkProcess bulkProcess, String filePath, String fileName) throws Exception {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkConfigurationUpdateParameters.DATA);
    Row row = dataSheet.createRow((short) 0);
    List<String> headers = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(bulkProcessDataFailedList)) {
      if (Objects.nonNull(bulkProcessDataFailedList.get(0)) && bulkProcessDataFailedList.get(0).containsKey(ExcelHeaderNames.BLIBLI_SKU_IN)) {
        headers = generateHeadersIN();
      } else {
        headers = generateHeadersEN();
      }
    }
    int cellIndex = 0;
    for (String header : headers) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(header);
    }
    int rowid = 1;
    for (LinkedHashMap<String, String> bulkProcessData : bulkProcessDataFailedList) {
      GdnPreconditions.checkArgument(Objects.nonNull(bulkProcessData), BULK_PROCESS_DATA_NULL_ERROR);
      row = dataSheet.createRow(rowid++);
      int cellid = 0;
      for (String cellValue : headers) {
        Cell cell = row.createCell(cellid);
        if (cellid == 0) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.BLIBLI_SKU_EN));
        } else if (cellid == 1) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN));
        } else if (cellid == 2) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.LIST_PRICE_EN));
        } else if (cellid == 3) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.OFFER_PRICE_EN));
        } else if (cellid == 4) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.STOCK_EN));
        } else if (cellid == 5) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.DELIVERY_STATUS_EN));
        } else if (cellid == 6) {
          cell.setCellValue(bulkProcessData.get(ExcelHeaderNames.CNC_STATUS_EN));
        } else if (cellid == 7) {
          cell.setCellValue(bulkProcessData.get(BulkConfigurationUpdateParameters.FAILURE_REASON));
        }
        cellid++;
      }
    }
    byte[] bytes = POIUtil.getByteContentFromExcel(dataSheet);

    BulkUpdateProcessDTO bulkUpdateProcessDTO = BulkUpdateProcessDTO.builder().fileContent(bytes)
      .bulkProcessType(DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT.getValue()).build();
    fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkProcess.getBulkProcessCode(),
      fileName);
    return fileStorageService.getDownloadLink(filePath, DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT.getValue(),
      bulkProcess.getBulkProcessCode(), fileName);
  }

  public String getDownloadLinkHtml(String url) {
    return new StringBuilder().append(DOWNLOAD_LINK_HREF).append(url).append(DOWNLOAD_LINK_DOWNLOAD)
      .toString();
  }

  private static List<String> generateHeadersEN() {
    List<String> headers =
      Arrays.asList(ExcelHeaderNames.BLIBLI_SKU_EN, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
        CnExcelHeaderNames.PRICE, ExcelHeaderNames.SELLING_PRICE, ExcelHeaderNames.STOCK_EN,
        ExcelHeaderNames.DELIVERY_STATUS_EN, ExcelHeaderNames.CNC_STATUS_EN,
        BulkConfigurationUpdateParameters.FAILURE_REASON);
    return headers;
  }

  private static List<String> generateHeadersIN() {
    List<String> headers = Arrays.asList(ExcelHeaderNames.BLIBLI_SKU_IN, ExcelHeaderNames.PICKUP_POINT_CODE_IN,
        ExcelHeaderNames.LIST_PRICE_IN, ExcelHeaderNames.OFFER_PRICE_IN, BulkParameters.STOCK_HEADER,
      ExcelHeaderNames.DELIVERY_STATUS_IN, ExcelHeaderNames.CNC_STATUS_IN, BulkUpdateServiceUtil.FAILURE_REASON);
    return headers;
  }

  private String getSkuValueFromRowMap(Map<String, String> rowMap) {
    if (rowMap.containsKey(ExcelHeaderNames.BLIBLI_SKU_IN)) {
      return rowMap.get(ExcelHeaderNames.BLIBLI_SKU_IN);
    } else if (rowMap.containsKey(ExcelHeaderNames.BLIBLI_SKU_EN)) {
      return rowMap.get(ExcelHeaderNames.BLIBLI_SKU_EN);
    } else if (rowMap.containsKey(BulkParameters.BLIBLI_SKU)) {
      return rowMap.get(BulkParameters.BLIBLI_SKU);
    } else {
      return rowMap.get(BulkParameters.BLIBLI_SKU_WITH_EXAMPLE);
    }
  }

  private String getPickupPointCodeFromRowMap(Map<String, String> rowMap) {
    if (rowMap.containsKey(ExcelHeaderNames.PICKUP_POINT_CODE_IN)) {
      return rowMap.get(ExcelHeaderNames.PICKUP_POINT_CODE_IN);
    } else if (rowMap.containsKey(ExcelHeaderNames.PICKUP_POINT_CODE_EN)) {
      return rowMap.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN);
    } else {
      return rowMap.get(BulkParameters.PICKUP_POINT_CODE);
    }
  }

  private void validateMerchantType(ProfileResponse profileResponse) {
    if (!ConverterUtil.checkIfMPPIsAllowed(mppAllowedSellers, profileResponse)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        BulkProcessValidationErrorMessages.MPP_NOT_ALLOWED_FOR_SELLER);
    }
  }

  private void validateAndSetPickupPointCode(Map<String, String> data,
    UpsertOfflineItemFailedResponse validationFailedResponse, UpsertOfflineItemRequest request,
      ProfileResponse profileResponse, UpsertValidationDTO upsertValidationDto,
      Set<String> accessiblePickupPointCodes) throws ApplicationException {
    List<String> fbbPickupPointList = new ArrayList<>();
    if (Strings.isNotEmpty(data.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN))) {
      String pickupPointCode = data.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN);
      request.setPickupPointCode(pickupPointCode);
      List<PickupPointResponse> pickupPointResponseList =
        pickupPointService.getPickupPointSummaryFilter(0, PickupPointFilterRequest.builder()
          .businessPartnerCode(profileResponse.getBusinessPartnerCode())
          .codes(new HashSet<>(Collections.singletonList(pickupPointCode))).build());
      fbbPickupPointList =
        pickupPointResponseList.stream().filter(PickupPointResponse::isFbbActivated)
          .map(PickupPointResponse::getCode).collect(Collectors.toList());
      Set<String> cncActivatedPickupPointCodes =
        pickupPointResponseList.stream().filter(PickupPointResponse::isCncActivated).map(PickupPointResponse::getCode).collect(Collectors.toSet());
      if (CollectionUtils.isEmpty(pickupPointResponseList)) {
        validationFailedResponse.setErrorCode(
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID);
        upsertValidationDto.setResult(false);
      } else if (request.isCncActive() && !cncActivatedPickupPointCodes.contains(
        request.getPickupPointCode())) {
        validationFailedResponse.setErrorCode(
          BulkProcessValidationErrorMessages.PICKUP_POINT_NOT_ELIGIBLE_FOR_CNC);
        upsertValidationDto.setResult(false);
      } else if (CollectionUtils.isNotEmpty(accessiblePickupPointCodes)
          && !accessiblePickupPointCodes.contains(pickupPointCode)) {
        validationFailedResponse.setErrorCode(
            BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS);
        upsertValidationDto.setResult(false);
      }
    } else {
      validationFailedResponse.setErrorCode(
        BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      upsertValidationDto.setResult(false);
    }
    List<String> existingFbbPpCode = upsertValidationDto.getFbbPickupPointList();
    existingFbbPpCode.addAll(fbbPickupPointList);
    upsertValidationDto.setFbbPickupPointList(existingFbbPpCode);
  }

  private void validateAndSetOfferPrice(Map<String, String> data,
    UpsertOfflineItemFailedResponse validationFailedResponse, UpsertOfflineItemRequest request,
    UpsertValidationDTO upsertValidationDto) {
    double offerPrice = 0;
    if (StringUtils.isNotEmpty(data.get(ExcelHeaderNames.OFFER_PRICE_EN))) {
      try {
        offerPrice = Double.parseDouble(data.get(ExcelHeaderNames.OFFER_PRICE_EN));
        if (Double.compare(offerPrice, Double.NaN) == 0) {
          throw new ApplicationRuntimeException();
        }
        validateWithMinimumPrice(offerPrice, Constant.STORE_ID, validationFailedResponse,
          upsertValidationDto);
        upsertValidationDto.setOfferPrice(offerPrice);
        request.setOfferPrice(offerPrice);
      } catch (Exception ex) {
        validationFailedResponse.setErrorCode(
          BulkProcessValidationErrorMessages.OFFER_PRICE_MUST_BE_DECIMAL);
        upsertValidationDto.setResult(false);
      }
    } else {
      validationFailedResponse.setErrorCode(BulkProcessValidationErrorMessages.SALE_PRICE_MUST_NOT_BE_BLANK);
      upsertValidationDto.setResult(false);
    }
  }

  private void validateAndSetListPrice(Map<String, String> data,
    UpsertOfflineItemFailedResponse validationFailedResponse,
    UpsertOfflineItemRequest request, double offerPrice, UpsertValidationDTO upsertValidationDto) {
    double listPrice;
    if (StringUtils.isNotEmpty(data.get(ExcelHeaderNames.LIST_PRICE_EN))) {
      try {
        listPrice = Double.parseDouble(data.get(ExcelHeaderNames.LIST_PRICE_EN));
        if (Double.compare(listPrice, Double.NaN) == 0) {
          throw new ApplicationRuntimeException();
        }
        validateWithMinimumPrice(listPrice, Constant.STORE_ID, validationFailedResponse,
          upsertValidationDto);
        request.setListPrice(listPrice);
      } catch (Exception ex) {
        validationFailedResponse.setErrorCode(
          BulkProcessValidationErrorMessages.REGULAR_PRICE_MUST_BE_DECIMAL);
        upsertValidationDto.setResult(false);
      }
    } else {
      listPrice = offerPrice;
      request.setListPrice(listPrice);
      request.setOfferPrice(offerPrice);
    }
  }

  private void fetchDataFromExistingAndValidate(String businessPartnerCode,
    List<UpsertOfflineItemRequest> requests,
    ProductVariantUpdateRequest productVariantUpdateRequest,
    Map<String, ItemPickupPointListingL3Response> offlineItemIdResponseMap,
    Map<String, ItemPickupPointListingL3Response> itemSkuResponseMap) throws Exception {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    for (UpsertOfflineItemRequest request : requests) {
      String productSku = request.getItemSku()
        .substring(0, StringUtils.ordinalIndexOf(request.getItemSku(), HYPHEN, 3));
      itemPickupPointListingL3Request.setItemSku(request.getItemSku());
      itemPickupPointListingL3Request.setProductSku(productSku);
      itemPickupPointListingL3Request.setBusinessPartnerCode(businessPartnerCode);
      itemPickupPointListingL3Request.setPickupPointCodes(Set.of(request.getPickupPointCode()));
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>(
          getItemPickupPointListingL3ResponseList(itemPickupPointListFetchSize, itemPickupPointListingL3Request));
      itemPickupPointListingL3Request.setPickupPointCodes(new HashSet<>());
      itemPickupPointListingL3Request.setFbbSortRequired(true);
      List<ItemPickupPointListingL3Response> fbbTrueItemPickupPointList =
          pbpOutboundService.getItemPickupPointListingL3Response(0, 1, itemPickupPointListingL3Request).getContent();
      fbbTrueItemPickupPointList.stream().findFirst().ifPresent(itemPickupPointListingL3Response -> {
        offlineItemIdResponseMap.put(itemPickupPointListingL3Response.getItemSku().concat(Constant.HYPHEN)
            .concat(itemPickupPointListingL3Response.getPickupPointCode()), itemPickupPointListingL3Response);
        itemSkuResponseMap.put(itemPickupPointListingL3Response.getItemSku(), itemPickupPointListingL3Response);
      });
      for (ItemPickupPointListingL3Response itemPickupPointL3Response : itemPickupPointListingL3ResponseList) {
        offlineItemIdResponseMap.put(
          itemPickupPointL3Response.getItemSku().concat(Constant.HYPHEN)
            .concat(itemPickupPointL3Response.getPickupPointCode()), itemPickupPointL3Response);
        itemSkuResponseMap.put(itemPickupPointL3Response.getItemSku(), itemPickupPointL3Response);
      }
      productVariantUpdateRequest.setProductSku(productSku);
      productVariantUpdateRequest.setProductName(null);
      productVariantUpdateRequest.setBusinessPartnerCode(businessPartnerCode);
    }
  }

  private List<ItemPickupPointListingL3Response> getItemPickupPointListingL3ResponseList(int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception {
    Page<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponsePage = Page.empty();
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    int page = 0;
    do {
      itemPickupPointListingL3ResponsePage =
          pbpOutboundService.getItemPickupPointListingL3Response(page, size, itemPickupPointListingL3Request);
      itemPickupPointListingL3ResponseList.addAll(itemPickupPointListingL3ResponsePage.getContent());
      page++;
    } while (page * itemPickupPointListFetchSize < itemPickupPointListingL3ResponsePage.getTotalElements());
    return itemPickupPointListingL3ResponseList;
  }

  private void formL5UpdateRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
      List<UpsertOfflineItemRequest> offlineItemRequestList,
      Map<String, ItemPickupPointListingL3Response> offlineItemIdResponseMap,
      Map<String, ItemPickupPointListingL3Response> itemSkuResponseMap, UpsertValidationDTO upsertValidationDto,
      List<UpsertOfflineItemFailedResponse> validationFailedResponse, ProfileResponse profileResponse,
      BulkProcess bulkProcess, PreOrderDTO preOrderDTO) {
    boolean isFbbPpCode = false;
    Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap = new HashMap<>();
    boolean validateFreeSample;
    for (UpsertOfflineItemRequest offlineItemRequest : offlineItemRequestList) {
      String offlineItemId = BulkUpdateServiceUtil.constructOfflineItemId(offlineItemRequest);
      ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        offlineItemIdResponseMap.get(offlineItemId);
      ItemPickupPointListingL3Response itemLevelResponse =
        itemSkuResponseMap.get(offlineItemRequest.getItemSku());
      if (Objects.isNull(itemPickupPointListingL3Response)) {
        if (upsertValidationDto.getFbbPickupPointList()
          .contains(offlineItemRequest.getPickupPointCode()) && !mppForWhEnabled) {
          List<PickupPointDeleteRequest> pickupPointDeleteRequests =
            offlineItemIdResponseMap.values().stream()
              .filter(ItemPickupPointListingL3Response::isFbbActivated).map(
                response -> new PickupPointDeleteRequest(response.getPickupPointCode(),
                  response.getItemSku())).collect(Collectors.toList());
          isFbbPpCode = true;
          if (CollectionUtils.isNotEmpty(pickupPointDeleteRequests)) {
            productVariantUpdateRequest.setDeletePickupPoints(pickupPointDeleteRequests);
          }
        }
        if (Objects.isNull(itemLevelResponse)) {
          log.error("Invalid itemSku passed for request : {} ", bulkProcess.getBulkProcessCode());
          validationFailedResponse.add(new UpsertOfflineItemFailedResponse(offlineItemRequest.getItemSku(),
              offlineItemRequest.getPickupPointCode(), BulkProcessValidationErrorMessages.INVALID_ITEM_SKU_ERROR));
        } else {
          validateFreeSample = validateProduct(itemLevelResponse.isFreeSample(), offlineItemRequest.isCncActive(),
              validationFailedResponse, offlineItemRequest, profileResponse, itemLevelResponse);
          if (validateFreeSample) {
            setProductItemInRequest(offlineItemRequest.getItemSku(), l4ProductItemMap, itemLevelResponse);
            productVariantUpdateRequest.getAddPickupPoints()
                .add(addPickupPointCodeRequest(offlineItemRequest, isFbbPpCode, profileResponse, preOrderDTO));
          }
        }
      } else {
        validateFreeSample =
            validateProduct(itemPickupPointListingL3Response.isFreeSample(), offlineItemRequest.isCncActive(),
                validationFailedResponse, offlineItemRequest, profileResponse,
              itemPickupPointListingL3Response);
        if (validateFreeSample) {
          setModifiedItemInEditRequest(l4ProductItemMap, offlineItemRequest, itemPickupPointListingL3Response,
              profileResponse, preOrderDTO);
        }
      }
    }
    if (preOrderQuotaFeatureSwitch && Objects.nonNull(preOrderDTO)) {
      PreOrderRequest preOrderRequest = new PreOrderRequest();
      BeanUtils.copyProperties(preOrderDTO, preOrderRequest);
      productVariantUpdateRequest.setPreOrder(preOrderRequest);
    }
    productVariantUpdateRequest.setProductItems(new ArrayList<>(l4ProductItemMap.values()));
  }

  public void setModifiedItemInEditRequest(Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap,
      UpsertOfflineItemRequest offlineItemRequest, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ProfileResponse profileResponse, PreOrderDTO preOrderDTO) {
    String itemSku = offlineItemRequest.getItemSku();
    if (l4ProductItemMap.containsKey(itemSku)) {
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        l4ProductItemMap.get(itemSku);
      List<ItemPickupPointRequest> requests =
        new ArrayList<>(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints());
      requests.add(
          setModifiedItemPickupPointRequest(offlineItemRequest, itemPickupPointListingL3Response,
              cncForWarehouseFeatureSwitch, profileResponse, preOrderDTO));
      productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(requests);
      l4ProductItemMap.put(offlineItemRequest.getItemSku(),
        productVariantPriceStockAndImagesRequest);
    } else {
      ProductVariantPriceStockAndImagesRequest priceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
      priceStockAndImagesRequest.setSkuCode(itemPickupPointListingL3Response.getSkuCode());
      priceStockAndImagesRequest.setItemSku(itemSku);
      priceStockAndImagesRequest.setProductSku(itemPickupPointListingL3Response.getProductSku());
      priceStockAndImagesRequest.setProductCode(itemPickupPointListingL3Response.getProductCode());

      priceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(
          setModifiedItemPickupPointRequest(offlineItemRequest, itemPickupPointListingL3Response,
              cncForWarehouseFeatureSwitch, profileResponse, preOrderDTO)));

      l4ProductItemMap.put(itemSku, priceStockAndImagesRequest);
    }
  }

  public void setProductItemInRequest(String itemSku,
    Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap,
    ItemPickupPointListingL3Response itemLevelResponse) {
    if (!l4ProductItemMap.containsKey(itemSku)) {
      ProductVariantPriceStockAndImagesRequest priceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
      priceStockAndImagesRequest.setSkuCode(itemLevelResponse.getSkuCode());
      priceStockAndImagesRequest.setItemSku(itemSku);
      priceStockAndImagesRequest.setProductSku(itemLevelResponse.getProductSku());
      priceStockAndImagesRequest.setProductCode(itemLevelResponse.getProductCode());
      l4ProductItemMap.put(itemSku, priceStockAndImagesRequest);
    }
  }

  public boolean validateProduct(boolean isFreeSample, boolean cncActive,
      List<UpsertOfflineItemFailedResponse> validationFailedResponse, UpsertOfflineItemRequest offlineItemRequest,
      ProfileResponse profileResponse, ItemPickupPointListingL3Response itemLevelResponse) {
    if (isFreeSample) {
      return !buyableOrCncActiveFreeSampleProduct(cncActive, validationFailedResponse,
        offlineItemRequest);
    } else if (BulkUpdateServiceUtil.isDimensionMissing(bopisCategoryRestrictionEnabled, offlineItemRequest.isBuyable(),
        profileResponse, CommonUtils.getDimensionMissingFromMissingFields(itemLevelResponse),
        BulkUpdateServiceUtil.getSellerTypes(bopisCategoryValidationForSellerTypes))) {
      addValidationFailure(validationFailedResponse, offlineItemRequest, BulkProcessValidationErrorMessages.PRODUCT_CANT_BE_SET_ONLINE);
      return false;
    } else if (BulkUpdateServiceUtil.isBopisProductMadeCnc(bopisCncRestrictionEnabled, offlineItemRequest.isCncActive(),
        itemLevelResponse.getProductType())) {
      addValidationFailure(validationFailedResponse, offlineItemRequest, BulkProcessValidationErrorMessages.PRODUCT_CANT_BE_SET_CNC);
      return false;
    } else if (instoreNewFlowEnabled && (cncActive || offlineItemRequest.isBuyable())) {
      Set<String> requiredMissingFields =
        Set.of(Constant.DIMENSIONS_MISSING, Constant.DESCRIPTION_MISSING);
      boolean isFieldMissing =
        requiredMissingFields.stream().anyMatch(itemLevelResponse.getMissingFields()::contains);
      if (isFieldMissing) {
        //pure InStore product cannot be made online or CNC Active without updating mandatory fields
        addValidationFailure(validationFailedResponse, offlineItemRequest,
          BulkProcessValidationErrorMessages.PURE_INSTORE_MISSING_FIELDS_ERROR);
        return false;
      }
    }
    return true;
  }

  private static boolean buyableOrCncActiveFreeSampleProduct(boolean cncActive,
    List<UpsertOfflineItemFailedResponse> validationFailedResponse,
    UpsertOfflineItemRequest offlineItemRequest) {
    if (offlineItemRequest.isBuyable()) {
      addValidationFailure(validationFailedResponse, offlineItemRequest,
        BulkProcessValidationErrorMessages.FREE_SAMPLE_CANNOT_BE_ONLINE);
      return true;
    } else if (cncActive) {
      addValidationFailure(validationFailedResponse, offlineItemRequest,
        BulkProcessValidationErrorMessages.FREE_SAMPLE_CANNOT_BE_CNC);
      return true;
    }
    return false;
  }

  private static void addValidationFailure(
    List<UpsertOfflineItemFailedResponse> validationFailedResponse,
    UpsertOfflineItemRequest offlineItemRequest, String errorMessage) {
    validationFailedResponse.add(
      new UpsertOfflineItemFailedResponse(offlineItemRequest.getItemSku(),
        offlineItemRequest.getPickupPointCode(), errorMessage));
  }

  private ItemPickupPointRequest addPickupPointCodeRequest(UpsertOfflineItemRequest offlineItemRequest,
      boolean isFbbPpCodeAdded, ProfileResponse profileResponse, PreOrderDTO preOrderDTO) {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(offlineItemRequest.getItemSku());
    itemPickupPointRequest.setPickupPointId(offlineItemRequest.getPickupPointCode());
    if ((isFbbPpCodeAdded && CommonUtils.isNotFaasEligible(faasFeatureSwitch, profileResponse)) || (
        preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse) && CommonUtils.isPreOrderActive(
            preOrderDTO))) {
      itemPickupPointRequest.setStock(0);
    } else {
      itemPickupPointRequest.setStock(offlineItemRequest.getStock());
    }
    itemPickupPointRequest.setSalePrice(offlineItemRequest.getOfferPrice());
    itemPickupPointRequest.setPrice(offlineItemRequest.getListPrice());
    itemPickupPointRequest.setBuyable(offlineItemRequest.isBuyable());
    itemPickupPointRequest.setDisplay(offlineItemRequest.isDiscoverable());
    if (cncForWarehouseFeatureSwitch) {
      itemPickupPointRequest.setCncDisplay(offlineItemRequest.isCncActive());
      itemPickupPointRequest.setCncBuyable(offlineItemRequest.isCncActive());
    } else {
      itemPickupPointRequest.setCncActive(offlineItemRequest.isCncActive());
    }
    setSellerSku(itemPickupPointRequest, offlineItemRequest.getItemSku());
    return itemPickupPointRequest;
  }

  private void setSellerSku(ItemPickupPointRequest itemPickupPointRequest, String itemSku) {
    List<ItemBasicDetailV2Response> responses = xProductOutboundService.getItemBasicDetailsByItemSku(itemSku);
    itemPickupPointRequest.setSellerSku(responses.stream().findFirst().get().getMerchantSku());
  }

  private ItemPickupPointRequest setModifiedItemPickupPointRequest(UpsertOfflineItemRequest offlineItemRequest,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response, boolean cncForWarehouseFeatureSwitch,
      ProfileResponse profileResponse, PreOrderDTO preOrderDTO) {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(offlineItemRequest.getItemSku());
    itemPickupPointRequest.setPickupPointId(offlineItemRequest.getPickupPointCode());
    if ((Boolean.TRUE.equals(itemPickupPointListingL3Response.isWebSyncStock()) && CommonUtils.isNotFaasEligible(
        faasFeatureSwitch, profileResponse)) || (
        preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse) && CommonUtils.isPreOrderActive(
            preOrderDTO))) {
      itemPickupPointRequest.setStock(0);
    } else {
      itemPickupPointRequest.setStock(
        offlineItemRequest.getStock() - itemPickupPointListingL3Response.getAvailableStockLevel2());
    }
    itemPickupPointRequest.setSalePrice(offlineItemRequest.getOfferPrice());
    itemPickupPointRequest.setPrice(offlineItemRequest.getListPrice());
    itemPickupPointRequest.setBuyable(offlineItemRequest.isBuyable());
    itemPickupPointRequest.setDisplay(offlineItemRequest.isDiscoverable());
    if (cncForWarehouseFeatureSwitch) {
      itemPickupPointRequest.setCncBuyable(offlineItemRequest.isCncActive());
      itemPickupPointRequest.setCncDisplay(offlineItemRequest.isCncActive());
    } else {
      itemPickupPointRequest.setCncActive(offlineItemRequest.isCncActive());
    }
    return itemPickupPointRequest;
  }

  public ProductVariantPriceStockAndImagesRequest getProductVariantPriceStockAndImagesRequest(
    UpsertOfflineItemRequest offlineItemRequest,
    ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemSku(offlineItemRequest.getItemSku());
    productVariantPriceStockAndImagesRequest.setSkuCode(itemPickupPointListingL3Response.getSkuCode());
    productVariantPriceStockAndImagesRequest.setMerchantSku(null);
    productVariantPriceStockAndImagesRequest.setItemName(itemPickupPointListingL3Response.getItemName());
    productVariantPriceStockAndImagesRequest.setUpcCode(itemPickupPointListingL3Response.getUpcCode());
    productVariantPriceStockAndImagesRequest.setFreeSample(itemPickupPointListingL3Response.isFreeSample());
    return productVariantPriceStockAndImagesRequest;
  }

  private void populateErrorMessage(List<VariantsErrorListResponse> errorListResponses,
    List<UpsertOfflineItemFailedResponse> itemFailedResponses) {
    errorListResponses.forEach(errorResponse -> itemFailedResponses.add(
      new UpsertOfflineItemFailedResponse(errorResponse.getItemSku(),
        errorResponse.getPickupPointCode(), errorResponse.getMessage())));

  }

  public void validateWithMinimumPrice(double price, String storeId,
    UpsertOfflineItemFailedResponse validationFailedResponse,
    UpsertValidationDTO upsertValidationDto) {
    SystemParameterConfig minimumPrice =
      systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.MINIMUM_PRICE);
    int minPrice = Integer.parseInt(minimumPrice.getValue());
    if (minPrice > price) {
      upsertValidationDto.setResult(false);
      validationFailedResponse.setErrorCode(
        BulkProcessValidationErrorMessages.PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE);
    }
  }
}
