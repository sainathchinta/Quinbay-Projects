package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.BulkProcessType.PICKUP_POINT_DELETE_ERROR;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_DEL;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_DEL_TYPE;
import static com.gdn.mta.bulk.util.BulkUpdateServiceUtil.FILE_BLANK_ERROR;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkCncUpsertErrorDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service("InstantPickupProductBulkDeleteService")
public class InstantPickupProductBulkDeleteServiceBean implements BulkDeleteService {

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private InventoryOutboundServiceBean inventoryOutboundServiceBean;

  @Value("${bulk.process.upsert.batch.size:100}")
  private int batchSize;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${error.file.name.size}")
  private int errorFileNameSize;

  @Value("${validate.warehouse.variant.deletion.enabled}")
  private boolean validateWarehouseVariantDeletionEnabled;

  @Value("${supported.merchant.for.warehouse.stock.validation}")
  private String supportedMerchantsForWarehouseStockValidation;


  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  @Override
  public void preProcessInstantPickupProductBulkDelete(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO, Set<String> accessiblePickupPointCodes) throws Exception {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();

    log.info(
        "Invoking pre process for bulk delete instant pickup product with "
            + "storeId = {}, bulkProcessCode: {}, and businessPartnerCode: {}",
        storeId, bulkProcessCode, businessPartnerCode);

    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      this.saveProductInstantPickupFileToDirectory(bulkProcessCode, fileName, bulkUpdateProcessDTO);

      BulkProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(storeId, requestId, bulkProcessCode,
          bulkUpdateProcessDTO, 0, 0, false, false);
      bulkProcess.setDescription(fileName + BulkUpdateServiceUtil.END_SYMBOL
          + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
      AuditTrailInfo auditTrailInfo =
          AuditTrailInfo.builder().accessiblePickupPointCodes(accessiblePickupPointCodes).build();
      bulkProcess.setNotes(objectMapper.writeValueAsString(auditTrailInfo));
      bulkProcess.setUploadedFile(fileName);

      bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess);
      bulkUpdateProcessDTO.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
      BulkUpdateQueue bulkUpdateQueue = bulkUpdateServiceUtil.getBulkUpdateQueue(storeId, requestId,
          bulkProcessCode, bulkUpdateProcessDTO);

      kafkaProducer.send(
          kafkaTopicProperties.getBulkUploadDeleteInstantPickupProductEvent(), bulkUpdateQueue);

      log.info(
          "Pre processing done. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
              + "{}, businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      log.error(
          "Error occurred while pre processing bulk delete instant pickup product. "
              + "storeId: {}, bulkProcessCode: {}, businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode, e);
      this.trackerService.sendTracker(INSTANT_PICKUP_DEL, INSTANT_PICKUP_DEL_TYPE, HYPHEN, FAILED,
          MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  @Override
  public void processInstantPickupProductBulkDelete(BulkUpdateQueue bulkUpdateQueue)
      throws Exception {
    log.info(
        "#InstantPickupProductBulkDeleteServiceBean-processInstantPickupProductBulkDelete - "
            + "bulkUpdateQueue: {}", bulkUpdateQueue);

    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();

    BulkProcess bulkProcess =
        bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode, BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      log.error(
          "#InstantPickupProductBulkDeleteServiceBean-processInstantPickupProductBulkDelete for bulkProcessCode : {} "
              + "is already processed or being processed", bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
              + " is already processed  or being processed");
    }
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    final String updatedBy = bulkUpdateQueue.getUpdatedBy();
    AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
    if (StringUtils.isNotBlank(bulkProcess.getNotes())) {
      auditTrailInfo = objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    }
    auditTrailInfo.setBusinessPartnerCode(bulkUpdateQueue.getBusinessPartnerCode());
    auditTrailInfo.setRequestId(bulkUpdateQueue.getRequestId());
    auditTrailInfo.setRemoteAddress(bulkUpdateQueue.getClientHost());
    auditTrailInfo.setUsername(updatedBy);
    auditTrailInfo.setFileName(bulkUpdateQueue.getFileName());
    bulkProcess.setNotes(objectMapper.writeValueAsString(auditTrailInfo));
    bulkProcess = bulkProcessService.saveOperation(bulkProcess);
    GdnPreconditions.checkArgument(bulkProcess != null,
        BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);

    List<BulkCncUpsertErrorDTO> bulkCncDeleteErrorDTOList = new ArrayList<>();
    int deletedInstantPickupProduct = 0;
    int totalInstantPickupProduct = 0;

    BulkUpdateErrorCounter bulkUpdateErrorCounter = null;
    try {
      bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
      Sheet excelSheetData = validateBulkInstantPickupProductDeleteRequest(bulkUpdateQueue,
          bulkProcess, bulkUpdateErrorCounter);

      List<Map<String, String>> instantPickupProductRowDataFromExcel =
          POIUtil.readFromExcelForBulkUpdate(excelSheetData, 1, 0, 0, new HashMap<>());
      totalInstantPickupProduct = instantPickupProductRowDataFromExcel.size();
      if (totalInstantPickupProduct == Constant.ZERO) {
        log.info("Aborting Process with bulkProcessCode {} since no data is present ",
            bulkProcess.getBulkProcessCode());
        bulkProcess.setDescription(FILE_BLANK_ERROR);
        bulkProcess.setEndDate(new Date());
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
        bulkProcessService.saveOperation(bulkProcess);
        return;
      }
      saveBulkProcessData(bulkProcess, instantPickupProductRowDataFromExcel);
      return;
    } catch (Exception e) {
      log.error("Error while processing for bulk delete offline items - bulkUpdateQueue: {}",
          bulkUpdateQueue, e);
      this.trackerService.sendTracker(INSTANT_PICKUP_DEL, INSTANT_PICKUP_DEL_TYPE, HYPHEN, FAILED, updatedBy);
      BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode,
          e.getMessage(), bulkUpdateErrorCounter);
    }
  }

  @Override
  public void processEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    log.info("Bulk process in progress, bulk process code : {}, rowNumbers : {}",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      log.warn("No rows found in pending state for the bulk process code : {}, row numbers : {}",
          bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      return;
    }

    Map<String, Map<String, String>> excelDataMap = new HashMap<>();
    Map<String, BulkProcessData> updatedBulkDataMap = new HashMap<>();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      Map<String, String> row = setBlpInitialData(bulkProcessData);
      bulkProcessData.setStartDate(new Date());
      excelDataMap.put(row.get(BulkParameters.BLIBLI_SKU) + Constant.HYPHEN + row.get(BulkParameters.PICKUP_POINT_CODE),
          row);
      updatedBulkDataMap.put(
          row.get(BulkParameters.BLIBLI_SKU) + Constant.HYPHEN + row.get(BulkParameters.PICKUP_POINT_CODE),
          bulkProcessDataService.saveOperation(bulkProcessData));
    }
    log.info("Deleting the offline items for item skus : {} for bulk process code : {}", updatedBulkDataMap.keySet(),
        bulkUpdateEventModel.getBulkProcessCode());
    try {
      AuditTrailInfo auditTrailInfo = objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
      List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
      List<DeleteOfflineItemRequest> requests = constructDeleteOfflineItemRequests(new ArrayList<>(excelDataMap.values()),
          responses);
      validateWarehouseProductForStockBeforeDeletion(requests, responses, bulkProcess.getStoreId(),
        bulkProcess.getBusinessPartnerCode());
      RequestHelper.removeL5sWithStockToPreventDeletion(requests, responses);
      if (CollectionUtils.isNotEmpty(requests)) {
        responses.addAll(
          productBusinessPartnerRepository.deleteOfflineItems(Constant.STORE_ID, requests,
            auditTrailInfo));
      }
      List<BulkProcessData> failedBulkData = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(responses)) {
        for (DeleteOfflineItemDetailResponse response : responses) {
          log.error("Error while deleting the instant pickup for bulk process code : {} response : {}",
              bulkUpdateEventModel.getBulkProcessCode(), response);
          String key = response.getItemSku() + Constant.HYPHEN + response.getPickupPointCode();
          BulkProcessData bulkProcessData = updatedBulkDataMap.get(key);
          bulkProcessData.setEndDate(new Date());
          bulkProcessData.setSystemErrorCount(1);
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          String errorMessage = new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(BulkUpdateServiceUtil.IS)
              .append(response.getItemSku()).append(BulkUpdateServiceUtil.AND_SYMBOL)
              .append(BulkParameters.PICKUP_POINT_CODE).append(BulkUpdateServiceUtil.IS)
              .append(response.getPickupPointCode()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.FAILURE_REASON).append(BulkUpdateServiceUtil.IS)
              .append(response.getErrorMessage()).toString();
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
              new BulkCncUpsertErrorDTO(response.getItemSku(), response.getPickupPointCode(), errorMessage)));
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
        bulkProcessDataService.saveBulkProcessData(successData);
      }

      if (CollectionUtils.isNotEmpty(failedBulkData)) {
        bulkProcessDataService.saveBulkProcessData(failedBulkData);
      }
    } catch (Exception e) {
      log.error("Error while post processing for campaign product bulk add with bulkUpdateEventModel = {}",
          bulkUpdateEventModel, e);
      for (Map.Entry<String, BulkProcessData> entry : updatedBulkDataMap.entrySet()) {
        BulkProcessData bulkProcessData = entry.getValue();
        String itemSku = entry.getKey();
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        bulkProcessData.setEndDate(new Date());
        Map<String, String> row = excelDataMap.get(itemSku);
        BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = new BulkCncUpsertErrorDTO();
        bulkCncUpsertErrorDTO.setItemSku(row.get(BulkParameters.BLIBLI_SKU));
        bulkCncUpsertErrorDTO.setPickupPointCode(row.get(BulkParameters.PICKUP_POINT_CODE));
        bulkCncUpsertErrorDTO.setReason(BulkUpdateServiceUtil.constructErrorMessage(row.get(BulkParameters.BLIBLI_SKU),
            BulkUpdateServiceUtil.INTERNAL_ERROR));
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkCncUpsertErrorDTO));
        bulkProcessDataService.saveOperation(bulkProcessData);
      }
    }
  }


  private void validateWarehouseProductForStockBeforeDeletion(
    List<DeleteOfflineItemRequest> requests, List<DeleteOfflineItemDetailResponse> responses,
    String storeId, String merchantCode) throws Exception {
    boolean shouldValidate = validateWarehouseVariantDeletionEnabled && CollectionUtils.isNotEmpty(
      BulkUpdateServiceUtil.getSupportedMerchants(supportedMerchantsForWarehouseStockValidation));
    if (shouldValidate) {
      ProfileResponse merchantProfile = getMerchantProfile(storeId, merchantCode);
      boolean isMerchantEligible =
        BulkUpdateServiceUtil.isMerchantEligibleForValidation(merchantProfile,
          supportedMerchantsForWarehouseStockValidation);
      if (isMerchantEligible) {
        Map<String, InventoryDetailInfoResponseDTO> inventoryMap = getInventoryDetails(requests,
          merchantCode);
        validateInventoryForRequests(requests, responses, inventoryMap);
      }
    }
  }


  private ProfileResponse getMerchantProfile(String storeId, String merchantCode) throws Exception {
    return businessPartnerRepository.filterByBusinessPartnerCodeV2(storeId, merchantCode);
  }


  private Map<String, InventoryDetailInfoResponseDTO> getInventoryDetails(
    List<DeleteOfflineItemRequest> requests, String merchantCode) {
    List<InventoryDetailInfoRequestDTO> inventoryRequests = requests.stream()
      .peek(request -> request.setMerchantSku(merchantCode))
      .map(BulkUpdateServiceUtil::toInventoryDetailInfoRequestDTO)
      .collect(Collectors.toList());
    return Optional.ofNullable(inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(
          new ListRequestDTO<>(inventoryRequests))).orElse(new ArrayList<>()).stream()
      .filter(response -> Objects.nonNull(response.getWebInventoryResponse()))
      .collect(Collectors.toMap(this::getInventoryKey, Function.identity()));
  }

  private String getInventoryKey(InventoryDetailInfoResponseDTO response) {
    return RequestHelper.toL5Id(response.getWebInventoryResponse().getWebItemSku(),
      response.getWebInventoryResponse().getPickupPointCode());
  }

  private void validateInventoryForRequests(List<DeleteOfflineItemRequest> requests,
    List<DeleteOfflineItemDetailResponse> responses,
    Map<String, InventoryDetailInfoResponseDTO> inventoryMap) {
    Map<String, DeleteOfflineItemRequest> requestMap = requests.stream().collect(Collectors.toMap(
      request -> RequestHelper.toL5Id(request.getItemSku(), request.getPickupPointCode()),
      Function.identity()));
    requestMap.forEach((l5Id, request) -> {
      if (inventoryMap.containsKey(l5Id)) {
        validateSingleRequest(inventoryMap.get(l5Id), request, responses);
      }
    });
  }

  private void validateSingleRequest(InventoryDetailInfoResponseDTO inventory,
    DeleteOfflineItemRequest request, List<DeleteOfflineItemDetailResponse> responses) {
    BulkUpdateServiceUtil.validateWareHouseStockAvailabilityForPPDeletion(inventory, request, responses);
  }

  /**
   * Delete instant pickup product in batch size
   *
   * @param bulkCncDeleteErrorDTOList must not be null
   * @param validationPassedData must not be null
   * @param bulkUpdateErrorCounter must not be null
   * @param auditTrailInfo must not be null
   * @return number of successfully deleted instant pickup product, add failed data if the number
   *         of request mismatch with number of deleted instant pickup products
   */
  private int processBulkDeleteInstantPickupProduct(String storeId,
      List<BulkCncUpsertErrorDTO> bulkCncDeleteErrorDTOList,
      List<Map<String, String>> validationPassedData, BulkUpdateErrorCounter bulkUpdateErrorCounter,
      AuditTrailInfo auditTrailInfo) {
    List<Map<String, String>> failedInstantPickupProducts = new ArrayList<>();
    List<DeleteOfflineItemRequest> requests = constructDeleteOfflineItemRequests(validationPassedData, new ArrayList<>());
    bulkUpdateErrorCounter.setInputErrorCounter(validationPassedData.size() - requests.size());

    int deletedInstantPickupProduct =
        bulkDeleteInstantPickupProduct(storeId, requests, failedInstantPickupProducts, auditTrailInfo);

    if (deletedInstantPickupProduct != requests.size()) {
      BulkUpdateServiceUtil.addErrorCounterAndConstructErrorMessageForCncBulkDelete(
          failedInstantPickupProducts, bulkCncDeleteErrorDTOList, bulkUpdateErrorCounter);
    }
    return deletedInstantPickupProduct;
  }

  /**
   * Construct delete offline item request
   *
   * @param validationPassedData must not be null
   * @param responses
   * @return List of Delete Offline Item Request
   */
  private List<DeleteOfflineItemRequest> constructDeleteOfflineItemRequests(
      List<Map<String, String>> validationPassedData, List<DeleteOfflineItemDetailResponse> responses) {
    List<DeleteOfflineItemRequest> requests = new ArrayList<>();
    if (CollectionUtils.isEmpty(validationPassedData)) {
      return requests;
    }
    for (Map<String, String> data : validationPassedData) {

      DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
      request.setItemSku(data.get(BulkParameters.BLIBLI_SKU));
      request.setPickupPointCode(data.get(BulkParameters.PICKUP_POINT_CODE));
      requests.add(request);
    }
    return requests;
  }

  /**
   * Bulk delete instant pickup product
   *
   * @param requests must not be null
   * @param failedInstantPickupProducts must not be null
   * @param auditTrailInfo must not be null
   * @return number of successfully deleted instant pickup product, if failure happens construct to
   *         failed data
   */
  private int bulkDeleteInstantPickupProduct(String storeId, List<DeleteOfflineItemRequest> requests,
      List<Map<String, String>> failedInstantPickupProducts, AuditTrailInfo auditTrailInfo) {
    int deletedInstantPickupProductCount = 0;
    List<List<DeleteOfflineItemRequest>> requestsPartition = Lists.partition(requests, batchSize);
    for (List<DeleteOfflineItemRequest> deleteOfflineItemRequests : requestsPartition) {
      try {
        List<DeleteOfflineItemDetailResponse> responses = productBusinessPartnerRepository
            .deleteOfflineItems(storeId, deleteOfflineItemRequests, auditTrailInfo);
        if (CollectionUtils.isNotEmpty(responses)) {
          responses.forEach(response -> {
            Map<String, String> failedData = new HashMap<>();
            failedData.put(BulkParameters.BLIBLI_SKU, response.getItemSku());
            failedData.put(BulkParameters.PICKUP_POINT_CODE, response.getPickupPointCode());
            failedData.put(BulkParameters.ERROR_CODE, response.getErrorMessage());
            failedInstantPickupProducts.add(failedData);
          });
        }
      } catch (Exception e) {
        constructErrorFailedInstantPickupData(deleteOfflineItemRequests, failedInstantPickupProducts);
      }
    }
    deletedInstantPickupProductCount = requests.size() - failedInstantPickupProducts.size();
    return deletedInstantPickupProductCount;
  }

  /**
   * construct error failed instant pickup product data to be put in bulk process notes
   *
   * @param deleteOfflineItemRequests must not be null
   * @param failedInstantPickupProducts must not be null
   */
  private void constructErrorFailedInstantPickupData(
      List<DeleteOfflineItemRequest> deleteOfflineItemRequests,
      List<Map<String, String>> failedInstantPickupProducts) {
    deleteOfflineItemRequests.forEach(request -> {
      Map<String, String> failedData = new HashMap<>();
      failedData.put(BulkParameters.BLIBLI_SKU, request.getItemSku());
      failedData.put(BulkParameters.PICKUP_POINT_CODE, request.getPickupPointCode());
      failedInstantPickupProducts.add(failedData);
    });
  }

  /**
   * Save excel file, containing delete action of instant pickup products to directory
   *
   * @param bulkProcessCode must not be blank
   * @param fileName must not be blank
   * @param bulkUpdateProcessDTO must not be null
   */
  private void saveProductInstantPickupFileToDirectory(String bulkProcessCode, String fileName,
      BulkUpdateProcessDTO bulkUpdateProcessDTO) throws Exception {
    fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, fileName);
  }

  /**
   * Validate header of excel file for bulk delete instant pickup product
   *
   * @param bulkUpdateQueue must not be null
   * @param bulkProcess must not be null
   * @param counter must not be null
   */
  private Sheet validateBulkInstantPickupProductDeleteRequest(BulkUpdateQueue bulkUpdateQueue,
      BulkProcess bulkProcess, BulkUpdateErrorCounter counter) throws Exception {
    Sheet excelSheetData  = fileStorageService.getFileData(bulkUpdateQueue,bulkProcess);
    GdnPreconditions.checkArgument(
        bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(
            excelSheetData, bulkProcess, bulkUpdateQueue, counter),
        BulkUpdateServiceUtil.HEADER_MISMATCH_PRODUCT);
    return excelSheetData;
  }

  private void createErrorFileForPartialSuccess(String filePath, Sheet excelSheetData,
      List<BulkCncUpsertErrorDTO> bulkCncDeleteErrorDTOList, BulkProcess bulkProcess) throws Exception {
    Row headerRow = excelSheetData.getRow(0);
    Cell errorCell = headerRow.createCell(2); // adding error column
    errorCell.setCellValue(BulkUpdateServiceUtil.FAILURE_REASON);
    for (int i = 1; i <= excelSheetData.getPhysicalNumberOfRows(); i++) {
      Row row = excelSheetData.getRow(i);
      if (row != null) {
        // clear cells
        row.getCell(0).setCellValue(StringUtils.EMPTY);
        row.getCell(1).setCellValue(StringUtils.EMPTY);
        row.createCell(2); // create cell for error messages
      }
    }
    int writeRowIndex = 1;
    for (BulkCncUpsertErrorDTO bulkCncDeleteErrorDTO : bulkCncDeleteErrorDTOList) {
      // fill with failed products
      Row writeRow = excelSheetData.getRow(writeRowIndex);
      writeRow.getCell(0).setCellValue(bulkCncDeleteErrorDTO.getItemSku());
      writeRow.getCell(1).setCellValue(bulkCncDeleteErrorDTO.getPickupPointCode());
      writeRow.getCell(2).setCellValue(bulkCncDeleteErrorDTO.getReason());
      writeRowIndex++;
    }
    byte[] bytes = POIUtil.getByteContentFromExcel(excelSheetData);
    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_UPDATE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_UPDATE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
            bulkProcess.getBulkProcessCode() + ProcessorUtils.getFileFormat(filePath), bytes);
  }

  private void saveBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> dataMaps) throws Exception {
    log.info("Inserting data rows for bulk process code = {} ", bulkProcess.getBulkProcessCode());
    int rowNumber = 1;
    Map<String, List<BulkProcessData>> distinctParentMap = new HashMap<>();
    AuditTrailInfo auditTrailInfo=objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    for (Map<String, String> dataMap : dataMaps) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      String parent = dataMap.get(BulkParameters.BLIBLI_SKU);
      String pickupPointCode=dataMap.get(BulkParameters.PICKUP_POINT_CODE);
      if (StringUtils.isBlank(parent)) {
        StringBuilder tempErrorMessage =
            new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(BulkUpdateServiceUtil.IS).append(parent)
                .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON)
                .append(BulkUpdateServiceUtil.IS).append(BulkUpdateServiceUtil.BLIBLI_SKU_BLANK)
                .append(BulkUpdateServiceUtil.END_SYMBOL);
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setInputErrorCount(1);
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new BulkCncUpsertErrorDTO(dataMap.get(BulkParameters.BLIBLI_SKU),
                dataMap.get(BulkParameters.PICKUP_POINT_CODE), tempErrorMessage.toString())));
        parent = bulkProcess.getBulkProcessCode() + rowNumber;
      } else if (!BulkUpdateServiceUtil.validateItemSku(parent)) {
        StringBuilder tempErrorMessage =
            new StringBuilder().append(BulkParameters.BLIBLI_SKU).append(BulkUpdateServiceUtil.IS).append(parent)
                .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON)
                .append(BulkUpdateServiceUtil.IS).append(Constant.ERROR_IN_ITEM_SKU)
                .append(BulkUpdateServiceUtil.END_SYMBOL);
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setInputErrorCount(1);
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new BulkCncUpsertErrorDTO(dataMap.get(BulkParameters.BLIBLI_SKU),
                dataMap.get(BulkParameters.PICKUP_POINT_CODE), tempErrorMessage.toString())));
        parent = bulkProcess.getBulkProcessCode() + rowNumber;
      }
      if (CollectionUtils.isNotEmpty(auditTrailInfo.getAccessiblePickupPointCodes())
          && !auditTrailInfo.getAccessiblePickupPointCodes().contains(pickupPointCode)) {
        StringBuilder tempErrorMessage =
            new StringBuilder().append(BulkParameters.PICKUP_POINT_CODE)
                .append(BulkUpdateServiceUtil.IS).append(pickupPointCode)
                .append(BulkUpdateServiceUtil.END_SYMBOL)
                .append(BulkUpdateServiceUtil.FAILURE_REASON).append(BulkUpdateServiceUtil.IS)
                .append(BulkUpdateServiceUtil.PICKUP_POINT_NOT_ACCESSIBLE)
                .append(BulkUpdateServiceUtil.END_SYMBOL);
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setInputErrorCount(1);
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new BulkCncUpsertErrorDTO(dataMap.get(BulkParameters.BLIBLI_SKU), pickupPointCode,
                tempErrorMessage.toString())));
        parent = bulkProcess.getBulkProcessCode() + rowNumber;
      }
      parent = parent + Constant.HYPHEN + dataMap.get(BulkParameters.PICKUP_POINT_CODE);
      bulkProcessData.setParentProduct(parent);
      bulkProcessData.setRowNumber(rowNumber);
      rowNumber = rowNumber + 1;
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(dataMap));
      BulkUpdateServiceUtil.addToMap(parent, bulkProcessData, distinctParentMap);
    }

    List<BulkProcessData> requestData = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(distinctParentMap, requestData, false, true);
    bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setTotalCount(rowNumber - 1);
    if (errorCount == bulkProcess.getTotalCount()) {
      bulkProcess.setStatus(BulkProcess.STATUS_PROCESSED);
    }
    bulkProcessService.saveOperation(bulkProcess);
  }

  private Map<String, String> setBlpInitialData(BulkProcessData bulkProcessData)
      throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    LinkedHashMap<String, String> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
    });
    return rowDataJson;
  }

  @Override
  @Transactional(readOnly = false)
  public void setFinalStatusAndNotificationOnInstantPickupDelete(BulkProcess bulkProcess, String storeId) throws Exception {
    String errorFilePath = StringUtils.EMPTY;
    log.info("Sending offline item delete notification for bulk process code : {} and store Id: {}",
        bulkProcess.getBulkProcessCode(), storeId);
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    AuditTrailInfo auditTrailInfo = objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    int inputErrorCount = 0;
    int errorCount = 0;
    List<BulkCncUpsertErrorDTO> bulkCncDeleteErrorDTOList = new ArrayList<>();
    for (BulkProcessData bulkProcessData : rowDataList) {
      if (StringUtils.equals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus())) {
        if (Objects.nonNull(bulkProcessData.getInputErrorCount())) {
          inputErrorCount++;
        }
        BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = null;
        if (Constant.SYSTEM_ERROR.equals(bulkProcessData.getErrorMessage())) {
          bulkCncUpsertErrorDTO = new BulkCncUpsertErrorDTO();
          bulkCncUpsertErrorDTO.setReason(bulkProcessData.getErrorMessage());
        } else {
          bulkCncUpsertErrorDTO =
              objectMapper.readValue(bulkProcessData.getErrorMessage(), BulkCncUpsertErrorDTO.class);
        }
        bulkCncDeleteErrorDTOList.add(bulkCncUpsertErrorDTO);
        errorCount++;
      }
    }

    if (CollectionUtils.isEmpty(bulkCncDeleteErrorDTOList)) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      String status = BulkProcess.STATUS_ABORTED;
      if (bulkProcess.getTotalCount() != errorCount) {
        status = BulkProcess.STATUS_PARTIALLY_DONE;
      }
      bulkProcess.setStatus(status);
    }
    bulkProcess.setErrorCount(bulkCncDeleteErrorDTOList.size());
    bulkProcess.setInputErrorCount(inputErrorCount);
    bulkProcess.setSystemErrorCount(bulkCncDeleteErrorDTOList.size() - inputErrorCount);
    bulkProcess.setSuccessCount(bulkProcess.getTotalCount() - errorCount);
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    bulkUpdateErrorCounter.setSystemErrorCounter(bulkProcess.getSystemErrorCount());
    bulkUpdateErrorCounter.setInputErrorCounter(bulkProcess.getInputErrorCount());
    for (int i = 0; i < bulkProcess.getInputErrorCount(); i++) {
      this.trackerService.sendTracker(INSTANT_PICKUP_DEL, INSTANT_PICKUP_DEL_TYPE, HYPHEN, FAILED,
          auditTrailInfo.getUsername());
    }
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkUpdateQueue.setStoreId(bulkProcess.getStoreId());
    bulkUpdateQueue.setFileName(auditTrailInfo.getFileName());
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      errorFilePath = generatePartialSuccessFile(bulkCncDeleteErrorDTOList, bulkProcess,
        bulkUpdateQueue.getFileName());
    }
    List<String> descriptions = Lists.newArrayList(BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_PROCESSED,
        BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_SUCCESSFULLY_PROCESSED,
        BulkUpdateServiceUtil.PRODUCTS_INSTANT_PICKUP_FAILED_PROCESSED);
    bulkUpdateServiceUtil.updateBulkDeleteCncProductFinalStatus(bulkProcess, bulkUpdateQueue,
        bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(), bulkCncDeleteErrorDTOList, bulkUpdateErrorCounter,
        descriptions);
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
      bulkProcess.getDescription().concat(errorFilePath), false, false);
  }

  private String generatePartialSuccessFile(List<BulkCncUpsertErrorDTO> failureData,
    BulkProcess bulkProcess, String fileName) throws Exception {
    try (SXSSFWorkbook workbook = new SXSSFWorkbook()) {
      Sheet dataSheet = workbook.createSheet(BulkConfigurationUpdateParameters.DATA);
      Row row = dataSheet.createRow((short) 0);
      List<String> headers =
        Arrays.asList(BulkParameters.BLIBLI_SKU, BulkParameters.PICKUP_POINT_CODE,
          BulkUpdateServiceUtil.FAILURE_REASON);
      int cellIndex = 0;
      for (String header : headers) {
        Cell cell = row.createCell((short) cellIndex);
        cellIndex++;
        cell.setCellType(Cell.CELL_TYPE_STRING);
        cell.setCellValue(header);
      }
      int rowid = 1;
      for (BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO : failureData) {
        row = dataSheet.createRow(rowid++);
        int cellid = 0;
        for (String cellValue : headers) {
          Cell cell = row.createCell(cellid);
          if (cellid == 0) {
            cell.setCellValue(bulkCncUpsertErrorDTO.getItemSku());
          }
          if (cellid == 1) {
            cell.setCellValue(bulkCncUpsertErrorDTO.getPickupPointCode());
          }
          if (cellid == 2) {
            cell.setCellValue(bulkCncUpsertErrorDTO.getReason());
          }
          cellid++;
        }
      }
      byte[] bytes = POIUtil.getByteContentFromExcel(dataSheet);
      BulkUpdateProcessDTO bulkUpdateProcessDTO = BulkUpdateProcessDTO.builder().fileContent(bytes)
        .bulkProcessType(PICKUP_POINT_DELETE_ERROR.getValue()).build();
      fileStorageService.createBulkFile(bulkUpdateProcessDTO,
        StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize), fileName);
      return fileStorageService.getDownloadLink(StringUtils.EMPTY,
        PICKUP_POINT_DELETE_ERROR.getValue(),
        StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
        ProcessorUtils.FILETYPE_XLSX_EXCEL);
    }
  }
}
