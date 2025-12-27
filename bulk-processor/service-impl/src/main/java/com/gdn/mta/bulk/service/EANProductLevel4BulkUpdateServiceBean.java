package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.FastExcelUtils;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dhatim.fastexcel.reader.ReadableWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static com.gdn.mta.bulk.BulkProcessValidationErrorMessages.HEADER_VALIDATION_ERROR;
import static com.gdn.mta.bulk.util.BulkUpdateServiceUtil.INTERNAL_ERROR;

@Service
@Slf4j
public class EANProductLevel4BulkUpdateServiceBean implements EANProductLevel4BulkUpdateService {
  private static final Logger LOGGER = LoggerFactory.getLogger(EANProductLevel4BulkUpdateServiceBean.class);
  private static final int BULK_UPDATE_FIRST_ROW_INDEX = 4;

  @Value("${validate.bulk.max.number.of.rows}")
  private boolean validateBulkMaxNumberOfRows;

  @Value("${bulk.max.number.of.rows}")
  private int bulkMaxNumberOfRows;

  @Value("${header.validation.check}")
  private boolean headerValidationCheck;

  @Value("${update.product.download.link}")
  private String updateProductDownloadLink;

  @Value("${ean.upc.valid.length}")
  private String eanUpcValidLengths;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private FileStorageService fileStoreService;

  @Autowired
  private BulkDownloadService bulkDownloadService;

  @Autowired
  private UpdateProductHistoryService updateProductHistoryService;


  public BulkProcessRepository getBulkProcessRepository() {
    return bulkProcessRepository;
  }

  public void setBulkProcessRepository(BulkProcessRepository bulkProcessRepository) {
    this.bulkProcessRepository = bulkProcessRepository;
  }

  public BulkUpdateServiceUtil getBulkUpdateServiceUtil() {
    return bulkUpdateServiceUtil;
  }

  public void setBulkUpdateServiceUtil(BulkUpdateServiceUtil bulkUpdateServiceUtil) {
    this.bulkUpdateServiceUtil = bulkUpdateServiceUtil;
  }

  @Override
  public void preProcessBulkUpdateEAN(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO)
      throws Exception {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();
    LOGGER.info(
        "invoking EAN preProcess for bulk update. storeId: {}, bulkProcessCode: {}, " + "businessPartnerCode: {}",
        storeId, bulkProcessCode, businessPartnerCode);
    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      BulkUpdateQueue bulkUpdateQueue =
          createFileAndGetBulkUpdateQueue(storeId, requestId, bulkUpdateProcessDTO, bulkProcessCode, fileName);
      kafkaProducer.send(kafkaTopicProperties.getBulkUploadEANEvent(), bulkProcessCode, bulkUpdateQueue);
      LOGGER.info("EAN preProcessing done. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
          + "{}, businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      LOGGER.error("Error occurred while EAN preProcessing bulk update. storeId: {}, bulkProcessCode: {}, "
          + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode, e);
      trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED,
          MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  @Override
  public void processBulkEANUpdate(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    LOGGER.info("invoking postProcessing for bulk EAN update. bulkUpdateQueue: {}", bulkUpdateQueue);
    BulkProcess bulkProcess =
        getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId,
            bulkProcessCode, BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      LOGGER.warn("Bulk update EAN for bulkProcessCode : {} is already processed or being processed",
          bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
              + " is already processed or being processed");
    }
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    int totalProduct;
    int headerRowIndex = 0;
    int offsetCellsFromLast = 0;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    try (InputStream is = fileStorageServiceBean.openGcsInputStream(bulkUpdateQueue, bulkProcess);
        ReadableWorkbook workbook = new ReadableWorkbook(is)) {
      Map<Integer, String> headers =
          FastExcelUtils.readHeaders(workbook, headerRowIndex, offsetCellsFromLast, bulkProcess.getBulkProcessCode());
      GdnPreconditions.checkArgument(
          getBulkUpdateServiceUtil().authorizeUploadBulkUpdateEAN(bulkProcess, counter, bulkProcess.getStoreId(),
              headers, bulkProcessCode, bulkProcess.getBusinessPartnerCode()),
          BulkErrorCategory.EXCEL_HEADER_ERROR + " - " + BulkUpdateServiceUtil.HEADER_MISMATCH);

      List<Map<String, String>> productDataFromExcel =
          FastExcelUtils.readDataRows(workbook, headers, BULK_UPDATE_FIRST_ROW_INDEX, bulkMaxNumberOfRows,
              validateBulkMaxNumberOfRows, bulkProcess.getBulkProcessCode());
      totalProduct = productDataFromExcel.size();
      bulkProcess.setTotalCount(totalProduct);
      saveBulkProcessDataForEANUpdate(bulkProcess, productDataFromExcel);
    } catch (Exception e) {
      LOGGER.error("Error while postProcessing for bulk update EAN. bulkUpdateQueue: {}", bulkUpdateQueue, e);
      if (headerValidationCheck) {
        if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
            .contains(BulkUpdateServiceUtil.HEADER_MISMATCH)) {
          bulkProcess.setDescription(HEADER_VALIDATION_ERROR);
          notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(),
              fileStorageServiceBean.getDownloadLinkHtml(updateProductDownloadLink));
        } else if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
            .contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) || Optional.ofNullable(e.getMessage())
            .orElse(StringUtils.EMPTY).contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN)) {
          bulkProcess.setDescription(e.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY));
          notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(),
              StringUtils.EMPTY);
        }
      }
      BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, e.getMessage(), counter);
      bulkProcessService.saveOperation(bulkProcess);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveBulkProcessDataForEANUpdate(BulkProcess bulkProcess, List<Map<String, String>> productDataFromExcel)
      throws Exception {
    log.info("Inserting EAN data rows for bulk process code = {} ", bulkProcess.getBulkProcessCode());
    int rowNumber = 1;
    List<BulkProcessData> requestData = new ArrayList<>(productDataFromExcel.size());
    for (Map<String, String> userData : productDataFromExcel) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setRowNumber(rowNumber);
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(userData));
      rowNumber += 1;
      requestData.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setTotalCount(rowNumber - 1);
    bulkProcessService.saveOperation(bulkProcess);
  }

  @Override
  public void processBulkEANUpdateItem(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    try {
      LOGGER.info("Start processing for rows : {} for blpCode {}", bulkUpdateEventModel.getRowNumbers(),
          bulkProcess.getBulkProcessCode());
      List<Map<String, String>> productDataFromExcel = new ArrayList<>(bulkProcessDataList.size());
      for (BulkProcessData bulkProcessData : bulkProcessDataList) {
        productDataFromExcel.add(objectMapper.readValue(bulkProcessData.getBulkRequestData(),
            new TypeReference<LinkedHashMap<String, String>>() {
            }));
        bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
        bulkProcessData.setStartDate(new Date());
      }
      bulkProcessDataList = bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
      LOGGER.info("Bulk process code {} for row {} saved as in-progress", bulkProcess.getBulkProcessCode(),
          bulkUpdateEventModel.getRowNumbers());
      List<Map<String, String>> validationPassedData = new ArrayList<>();
      List<Map<String, String>> validationFailedData = new ArrayList<>();
      BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO =
          getBulkUpdateServiceUtil().validateExcelDataBulkUpdateEANProduct(productDataFromExcel, validationPassedData,
              validationFailedData, counter, bulkProcess.getBusinessPartnerCode());
      counter.setInputErrorCounter(validationFailedData.size());
      validationFailedData = new ArrayList<>();
      List<BulkUpdateSuccessDTO> savedProduct = new ArrayList<>();
      List<EditUpcRequest> editUpcRequests =
          getBulkEANUpdateSuccessDTOList(validationPassedData, validationFailedData, listBulkUpdateErrorDTO);
      counter.setInputErrorCounter(counter.getInputErrorCounter() + validationFailedData.size());

      updateEanCode(editUpcRequests, listBulkUpdateErrorDTO, savedProduct, bulkProcess);

      getBulkUpdateServiceUtil().setEanUpdateFinalDataStatus(bulkProcessDataList, listBulkUpdateErrorDTO, savedProduct);
      bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
    } catch (Exception e) {
      getBulkUpdateServiceUtil().setFinalStatusForSystemFailure(bulkProcessDataList, bulkProcess);
      bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
      LOGGER.error("Error while postProcessing for bulk update. bulkUpdateQueue: {}", bulkUpdateEventModel, e);
      trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED, bulkProcess.getCreatedBy());
    }
  }

  private void updateEanCode(List<EditUpcRequest> editUpcRequests, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      List<BulkUpdateSuccessDTO> savedProduct, BulkProcess bulkProcess) {
    log.info("[EAN Update] Start processing update of upcs");
    editUpcRequests.forEach((EditUpcRequest editRequest) -> {
      // call pcb to update the ean
      log.info("[EAN Update] Request to call pcb for ean update : {}, {}", editRequest.productCode,
          editRequest.productItemUpcCodeUpdateRequest);
      boolean editResponse =
          pcbOutboundService.editItemUpcCode(editRequest.productCode, editRequest.productItemUpcCodeUpdateRequest);
      mapPrdProductToPrdItemAndSaveHistory(listBulkUpdateErrorDTO, savedProduct, editRequest, editResponse,
          bulkProcess);
    });
  }

  private List<UpdatedProductHistory> buildAuditRequest(Map<String, String> excelData, BulkProcess bulkProcess) {
    UpdatedProductHistory request = new UpdatedProductHistory();
    request.setBusinessPartnerCode(bulkProcess.getBusinessPartnerCode());
    request.setActivity(UpdateProductActivity.UPC_CODE.name());
    request.setGdnSku(excelData.get(BulkParameters.BLIBLI_SKU));
    request.setProductSku(excelData.get(BulkParameters.BLIBLI_PRODUCT_SKU));
    request.setGdnName(excelData.get(BulkParameters.NAMA_PRODUK));
    request.setOldValues(excelData.get(BulkParameters.OLD_EAN_UPC_CODE));
    request.setNewValues(excelData.get(BulkParameters.EAN_OR_UPC));
    request.setStatus(Boolean.TRUE);
    request.setChangedBy(bulkProcess.getCreatedBy());
    request.setRequestId(bulkProcess.getRequestId());
    request.setClientHost(Constant.CLIENT_ID);
    return List.of(request);
  }

  private void mapPrdProductToPrdItemAndSaveHistory(List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      List<BulkUpdateSuccessDTO> savedProduct, EditUpcRequest editRequest, boolean editResponse,
      BulkProcess bulkProcess) {
    if (editResponse) {
      // generate history
      List<UpdatedProductHistory> auditTrailUpdatedProductBulkRequest =
          buildAuditRequest(editRequest.excelData, bulkProcess);
      updateProductHistoryService.updateProductHistoryDetailList(auditTrailUpdatedProductBulkRequest,
          UpdateProductActivity.UPC_CODE);

      // call x-product to sync prd_product to prd_item
      boolean syncResponse = xProductOutboundService.mapProductToItem(editRequest.productCode);
      if (syncResponse) {
        savedProduct.add(BulkUpdateSuccessDTO.builder()
            .productName(editRequest.excelData.getOrDefault(BulkParameters.NAMA_PRODUK, StringUtils.EMPTY))
            .productSku(editRequest.excelData.get(BulkParameters.BLIBLI_SKU)).build());
      } else {
        listBulkUpdateErrorDTO.add(addEanErrorDto(editRequest.excelData, INTERNAL_ERROR));
      }
    } else {
      listBulkUpdateErrorDTO.add(addEanErrorDto(editRequest.excelData, INTERNAL_ERROR));
    }
  }

  public List<EditUpcRequest> getBulkEANUpdateSuccessDTOList(List<Map<String, String>> validationPassedData,
      List<Map<String, String>> validationFailedData, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO) {
    List<EditUpcRequest> requests = new ArrayList<>();
    Set<String> eanItems = new HashSet<>();
    for (Map<String, String> validatedMap : validationPassedData) {
      try {
        //    fetch only items for which upc code is not null
        if (validatedMap.containsKey(BulkParameters.EAN_OR_UPC) && (
            StringUtils.isEmpty(validatedMap.get(BulkParameters.EAN_OR_UPC)) || "-".equals(
                validatedMap.get(BulkParameters.EAN_OR_UPC)))) {
          listBulkUpdateErrorDTO.add(
              addEanErrorDto(validatedMap, BulkProcessValidationErrorMessages.BULK_UPDATE_EAN_INPUT_ERROR));
          validationFailedData.add(validatedMap);
        } else if (validatedMap.containsKey(BulkParameters.EAN_OR_UPC) && validateEanLength(
            validatedMap.get(BulkParameters.EAN_OR_UPC))) {
          listBulkUpdateErrorDTO.add(
              addEanErrorDto(validatedMap, BulkProcessValidationErrorMessages.BULK_UPDATE_INVALID_EAN_LENGTH));
          validationFailedData.add(validatedMap);
        } else if (validatedMap.containsKey(BulkParameters.EAN_OR_UPC) && validateDuplicateEan(
            validatedMap.get(BulkParameters.EAN_OR_UPC), eanItems)) {
          listBulkUpdateErrorDTO.add(
              addEanErrorDto(validatedMap, BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID));
          validationFailedData.add(validatedMap);
        } else {
          //    Get product code and sku code from x-product
          List<ItemBasicDetailV2Response> responses =
              xProductOutboundService.getItemBasicDetailsByItemSku(validatedMap.get(BulkParameters.BLIBLI_SKU));
          log.info("[EAN Update] Item Basic Response from x-product : {}", responses);
          // Using Sku code get the current upc and build modified upc code requests
          List<ProductItemResponse> pcbResponse = new ArrayList<>();
          if (CollectionUtils.isNotEmpty(responses) && StringUtils.isNotEmpty(responses.getFirst().getItemCode())) {
            log.info("[EAN Update] Get Current product details from pcb for item code: {}",
                responses.getFirst().getItemCode());
            pcbResponse = pcbOutboundService.getProductItemBySkuCodes(
                new SkuCodesRequest(Collections.singletonList(responses.getFirst().getItemCode())));
          }
          log.info("[EAN Update] Current product details from pcb for item code: {}", pcbResponse);
          if (validatedMap.containsKey(BulkParameters.EAN_OR_UPC) && CollectionUtils.isNotEmpty(pcbResponse)
              && !validatedMap.get(BulkParameters.EAN_OR_UPC).equals(pcbResponse.getFirst().getUpcCode())) {
            log.info("[EAN Update] Uploaded Ean is different from current so starting update");
            ItemBasicDetailV2Response itemResponse = responses.getFirst();
            requests.add(
                new EditUpcRequest(BulkUpdateServiceUtil.getProductCodeFromItemCode(itemResponse.getItemCode()),
                    Collections.singletonList(
                        ProductItemUpcCodeUpdateRequest.builder().skuCode(itemResponse.getItemCode())
                            .upcCode(validatedMap.get(BulkParameters.EAN_OR_UPC)).build()), validatedMap));
            validatedMap.put(BulkParameters.OLD_EAN_UPC_CODE, pcbResponse.getFirst().getUpcCode());
          } else {
            listBulkUpdateErrorDTO.add(
                addEanErrorDto(validatedMap, BulkProcessValidationErrorMessages.NO_CHANGE_IN_EAN_UPC));
            validationFailedData.add(validatedMap);
          }
        }
      } catch (Exception e) {
        log.error("Exception during ean / upc update for blibli sku : {}", validatedMap.get(BulkParameters.BLIBLI_SKU));
        if (e instanceof ApplicationRuntimeException) {
          listBulkUpdateErrorDTO.add(addEanErrorDto(validatedMap, ((ApplicationRuntimeException) e).getErrorMessage()));
          validationFailedData.add(validatedMap);
        } else {
          listBulkUpdateErrorDTO.add(
              addEanErrorDto(validatedMap, BulkProcessValidationErrorMessages.BULK_UPDATE_EAN_INPUT_ERROR));
          validationFailedData.add(validatedMap);
        }
      }
    }
    validationPassedData.removeIf(validationFailedData::contains);
    log.info("[EAN Update] Valid bulk ean update requests : {}", requests);
    return requests;
  }

  private static BulkUpdateErrorDTO addEanErrorDto(Map<String, String> validatedMap, String errorMsg) {
    return new BulkUpdateErrorDTO(validatedMap.getOrDefault(BulkParameters.NAMA_PRODUK, StringUtils.EMPTY),
        validatedMap.get(BulkParameters.BLIBLI_SKU), errorMsg);
  }

  private boolean validateDuplicateEan(String eanCode, Set<String> eanItems) {
    boolean valid = true;
    if (eanItems.contains(eanCode)) {
      valid = false;
    } else {
      eanItems.add(eanCode);
    }
    return !valid;
  }

  private boolean validateEanLength(String eanCode) {
    boolean valid = true;
    try {
      Long.parseLong(eanCode);
    } catch (NumberFormatException exception) {
      log.info("Invalid ean value {}", eanCode);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          BulkErrorCategory.EAN_MUST_BE_NUMBER.getDescription());
    }
    int length = eanCode.length();
    List<Integer> attributeValueLength =
        Arrays.stream(eanUpcValidLengths.split("\\s*,\\s*")).map(Integer::parseInt).toList();
    if (!attributeValueLength.contains(length)) {
      valid = false;
    }
    return !valid;
  }

  private BulkUpdateQueue createFileAndGetBulkUpdateQueue(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO, String bulkProcessCode, String fileName) throws Exception {
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, fileName);
    BulkProcess bulkProcess =
        getBulkUpdateServiceUtil().getBulkProcess(storeId, requestId, bulkProcessCode, bulkUpdateProcessDTO, 0, 0,
            false, false);
    bulkProcess.setDescription(
        fileName + BulkUpdateServiceUtil.END_SYMBOL + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
    bulkProcess.setUploadedFile(fileName);
    savePreProcessBulkData(bulkProcess);
    return getBulkUpdateServiceUtil().getBulkUpdateQueue(storeId, requestId, bulkProcessCode, bulkUpdateProcessDTO);
  }

  @Transactional
  public void savePreProcessBulkData(BulkProcess bulkProcess) {
    getBulkProcessRepository().save(bulkProcess);
  }

  public record EditUpcRequest(String productCode,
                               List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequest,
                               Map<String, String> excelData) {
  }

  @Override
  public void setFinalStatusAndNotificationOnEANUpdate(String storeId, BulkProcess bulkProcess) throws Exception {
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    getBulkUpdateServiceUtil().getFailedSuccessDto(bulkUpdateSuccessDTOList, bulkProcess, bulkUpdateErrorDTOList,
        rowDataList);
    String downloadLink = createEANErrorWorkbook(bulkProcess.getBulkProcessCode(), bulkUpdateErrorDTOList);
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, bulkProcess.getDescription() + downloadLink,
        false, false);
  }

  private String createEANErrorWorkbook(String bulkProcessCode, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO)
      throws Exception {
    List<List<String>> xlData = new ArrayList<>();
    for (BulkUpdateErrorDTO bulkUpdateErrorDTO : listBulkUpdateErrorDTO) {
      List<String> cellValue = new ArrayList<>();
      cellValue.add(bulkUpdateErrorDTO.getProductName());
      cellValue.add(bulkUpdateErrorDTO.getProductSku());
      cellValue.add(bulkUpdateErrorDTO.getReason());
      xlData.add(cellValue);
    }
    if (CollectionUtils.isEmpty(xlData)) {
      return StringUtils.EMPTY;
    }
    return bulkDownloadService.generateEanErrorWorkBookBulkDownload(bulkProcessCode, xlData);
  }
}

