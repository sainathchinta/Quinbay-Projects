package com.gdn.mta.bulk.service;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.models.BulkArchiveRequest;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.StreamUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

import lombok.extern.slf4j.Slf4j;

import static com.gdn.mta.bulk.dto.BulkProcessType.ARCHIVE_ERROR;

@Service
@Slf4j
public class BulkArchiveServiceImpl implements BulkArchiveService {

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${row.access.size:100}")
  private int rowAccessSize;

  @Value("${error.file.name.size}")
  private int errorFileNameSize;

  @Override
  public void processArchiveEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
      bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      log.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      return;
    }
    bulkProcessDataList = setBulkDataInProgressAndSave(bulkProcessDataList);
    List<String> validProductSkus = new ArrayList<>();
    Map<String, BulkProcessData> productSkuBulkProcessDataMap = new HashMap<>();
    validateInput(bulkProcessDataList, bulkProcess.getBusinessPartnerCode(), validProductSkus,
      productSkuBulkProcessDataMap);
    List<String> failedSkus;
    Map<String, String> productSkuErrorMessageMap = new HashMap<>();
    try {
      failedSkus =
        this.productBusinessPartnerRepository.bulkArchiveProductSkus(bulkProcess.getCreatedBy(),
          bulkProcess.getRequestId(), true, new SimpleListStringRequest(validProductSkus),
          productSkuErrorMessageMap);
    } catch (Exception e) {
      log.error("Error on updating archive flag with input : {}, error - ", validProductSkus, e);
      failedSkus = validProductSkus;
    }
    updateStatusForFailedArchival(failedSkus, validProductSkus, productSkuBulkProcessDataMap, productSkuErrorMessageMap);
    this.bulkProcessDataService.saveBulkProcessData(
      new ArrayList<>(productSkuBulkProcessDataMap.values()));
  }

  public void updateStatusForFailedArchival(List<String> failedSkus, List<String> validProductSkus,
    Map<String, BulkProcessData> productSkuBulkProcessDataMap, Map<String, String> productSkuErrorMessageMap) {
    for (String productSku : validProductSkus) {
      if (failedSkus.contains(productSku)) {
        productSkuBulkProcessDataMap.get(productSku).setStatus(BulkProcessData.STATUS_FAIL);
        productSkuBulkProcessDataMap.get(productSku).setEndDate(new Date());
        productSkuBulkProcessDataMap.get(productSku)
          .setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
        productSkuBulkProcessDataMap.get(productSku).setSystemErrorCount(1);
      } else if (productSkuErrorMessageMap.containsKey(productSku)) {
        productSkuBulkProcessDataMap.get(productSku).setStatus(BulkProcessData.STATUS_FAIL);
        productSkuBulkProcessDataMap.get(productSku).setEndDate(new Date());
        productSkuBulkProcessDataMap.get(productSku)
          .setErrorMessage(productSkuErrorMessageMap.get(productSku));
        productSkuBulkProcessDataMap.get(productSku).setSystemErrorCount(1);
      } else {
        productSkuBulkProcessDataMap.get(productSku).setStatus(BulkProcessData.STATUS_SUCCESS);
        productSkuBulkProcessDataMap.get(productSku).setEndDate(new Date());
      }
    }
  }

  private void validateInput(List<BulkProcessData> bulkProcessDataList, String businessPartnerCode,
    List<String> validProductSkus, Map<String, BulkProcessData> productSkuBulkProcessDataMap) throws Exception{
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      BulkArchiveRequest bulkArchiveRequest =
        this.objectMapper.readValue(bulkProcessData.getBulkRequestData(), BulkArchiveRequest.class);
      if (BulkUpdateServiceUtil.validateProductSkuAndSellerCodeWhenUpload(
        bulkArchiveRequest.getProductSku(), businessPartnerCode)) {
        validProductSkus.add(bulkArchiveRequest.getProductSku());
      } else {
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setErrorMessage(Constant.INVALID_PRODUCT_SKU);
        bulkProcessData.setEndDate(new Date());
        bulkProcessData.setInputErrorCount(1);
      }
      productSkuBulkProcessDataMap.put(bulkArchiveRequest.getProductSku(), bulkProcessData);
    }
  }

  private List<BulkProcessData> setBulkDataInProgressAndSave(List<BulkProcessData> bulkProcessDataList) {
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
      bulkProcessData.setStartDate(new Date());
    }
    return bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Override
  public void addProcessDataAndUpdateProcess(String storeId, Sheet bulkArchiveSheet, BulkProcess bulkProcess) {
    List<BulkArchiveRequest> bulkArchiveRequestList = getBulkArchiveRequestList(bulkArchiveSheet);
    List<BulkArchiveRequest> filteredBulkArchiveRequestList = bulkArchiveRequestList.stream()
      .filter(StreamUtils.distinctByKey(BulkArchiveRequest::getProductSku))
      .collect(Collectors.toList());
    addBulkArchiveDataAndUpdateStatus(storeId, bulkProcess, filteredBulkArchiveRequestList);
  }

  private List<BulkArchiveRequest> getBulkArchiveRequestList(Sheet bulkArchiveSheet) {
    return POIUtil.readFromExcelForBulkUpdate(bulkArchiveSheet, 1, 0, 0, new HashMap<>()).parallelStream()
      .filter(excelRow -> StringUtils.isNotEmpty(excelRow.get(BulkParameters.BLIBLI_PRODUCT_SKU)))
      .map(excelRow -> toBulkArchiveRequest(excelRow)).collect(Collectors.toList());
  }

  private BulkArchiveRequest toBulkArchiveRequest(Map<String, String> excelRow) {
    return BulkArchiveRequest.builder().productSku(excelRow.get(BulkParameters.BLIBLI_PRODUCT_SKU))
      .productName(excelRow.getOrDefault(BulkParameters.PARENT_PRODUCT_NAME, StringUtils.EMPTY))
      .build();
  }

  private void addBulkArchiveDataAndUpdateStatus(String storeId, BulkProcess bulkProcess,
    List<BulkArchiveRequest> bulkArchiveRequestList) {
    int rowNumber = 0;
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    for (BulkArchiveRequest bulkArchiveRequest : bulkArchiveRequestList) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setStoreId(storeId);
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setRowNumber(rowNumber++);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setParentProduct(bulkArchiveRequest.getProductSku());
      try {
        bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(bulkArchiveRequest));
      } catch (Exception e) {
        log.error("Failed on object mapper for input : {}, error - ", bulkArchiveRequest, e);
        continue;
      }
      bulkProcessDataList.add(bulkProcessData);
    }
    if (CollectionUtils.isEmpty(bulkArchiveRequestList)) {
      bulkProcess.setTotalCount(0);
      bulkProcess.setSuccessCount(0);
      bulkProcess.setErrorCount(0);
      bulkProcess.setStatus(BulkProcess.STATUS_PROCESSED);
    } else {
      bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
      bulkProcess.setTotalCount(bulkProcessDataList.size());
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    }
    bulkProcessService.saveOperation(bulkProcess);
  }

  @Override
  public void setFinalStatusAndSendNotificationOnBulkArchive(BulkProcess bulkProcess,
    String storeId) {
    log.info("Setting final status and sending notification for bulk process : {}",
      bulkProcess.getBulkProcessCode());
    String errorFileUrl = StringUtils.EMPTY;
    try {
      List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
      List<BulkProcessData> failedDataList = rowDataList.stream()
        .filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus()))
        .collect(Collectors.toList());
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList =
        failedDataList.stream().map(this::generateBulkUpdateErrorDTO)
          .collect(Collectors.toList());
      updateBulkProcess(bulkProcess, rowDataList, failedDataList);
      bulkUpdateServiceUtil.updateBulkProcessNotes(bulkUpdateErrorDTOList, storeId, bulkProcess.getBulkProcessCode(),
        bulkProcess, false, null);
      if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
        errorFileUrl = createErrorFile(bulkProcess, bulkUpdateErrorDTOList);
      }
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
        bulkProcess.getDescription().concat(errorFileUrl), false, false);
    } catch (Exception e) {
      log.error("Error updating final status for bulk process : {}, error - ", bulkProcess, e);
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    }
  }

  private BulkUpdateErrorDTO generateBulkUpdateErrorDTO(BulkProcessData bulkProcessData) {
    BulkArchiveRequest bulkArchiveRequest;
    try {
      bulkArchiveRequest =
        this.objectMapper.readValue(bulkProcessData.getBulkRequestData(), BulkArchiveRequest.class);
    } catch(Exception e) {
      log.error("Error parsing input string : {} to class BulkArchiveRequest.class, error - ",
        bulkProcessData.getBulkRequestData(), e);
      return new BulkUpdateErrorDTO();
    }
    return BulkUpdateErrorDTO.builder().productName(bulkArchiveRequest.getProductName())
      .productSku(bulkArchiveRequest.getProductSku()).reason(bulkProcessData.getErrorMessage())
      .build();
  }

  private String createErrorFile(BulkProcess bulkProcess,
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList) throws Exception {
    SXSSFWorkbook workBook = new SXSSFWorkbook(null, rowAccessSize, true, false);
    Sheet excel = workBook.createSheet();
    Row headerRow = excel.createRow(0);
    createHeaderRows(headerRow);
    CellStyle cellStyle = workBook.createCellStyle();
    generateHeaderCellStyle(cellStyle);
    headerRow.getCell(0).setCellStyle(cellStyle);
    headerRow.getCell(1).setCellStyle(cellStyle);
    headerRow.getCell(2).setCellStyle(cellStyle);
    int rowNumber = 0;
    for (BulkUpdateErrorDTO bulkUpdateErrorDTO : bulkUpdateErrorDTOList) {
      rowNumber++;
      Row dataRow = excel.createRow(rowNumber);
      Cell productSkuCell = dataRow.createCell(0);
      productSkuCell.setCellType(Cell.CELL_TYPE_STRING);
      productSkuCell.setCellValue(bulkUpdateErrorDTO.getProductSku());
      Cell productNameCell = dataRow.createCell(1);
      productNameCell.setCellType(Cell.CELL_TYPE_STRING);
      productNameCell.setCellValue(bulkUpdateErrorDTO.getProductName());
      Cell errorCell = dataRow.createCell(2);
      errorCell.setCellType(Cell.CELL_TYPE_STRING);
      errorCell.setCellValue(bulkUpdateErrorDTO.getReason());
    }
    try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
      workBook.write(bos);
      BulkUpdateProcessDTO processDTO =
        BulkUpdateProcessDTO.builder().bulkProcessType(ARCHIVE_ERROR.getValue())
          .fileContent(bos.toByteArray()).build();
      fileStorageService.createBulkFile(processDTO,
        StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
        ProcessorUtils.FILETYPE_XLSX_EXCEL);
    }
    return fileStorageService.getDownloadLink(StringUtils.EMPTY, ARCHIVE_ERROR.getValue(),
      StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
      ProcessorUtils.FILETYPE_XLSX_EXCEL);
  }

  private void generateHeaderCellStyle(CellStyle cellStyle) {
    setThinBlackBorderCellStyle(cellStyle);
    setLightGreenCellBackground(cellStyle);
  }

  private static void setThinBlackBorderCellStyle(CellStyle cellStyle) {
    cellStyle.setBorderBottom(BorderStyle.THIN);
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderTop(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setTopBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
  }

  private static void setLightGreenCellBackground(CellStyle cellStyle) {
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    cellStyle.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex());
  }

  private void createHeaderRows(Row headerRow) {
    Cell productSkuCell = headerRow.createCell(0);
    Cell productNameCell = headerRow.createCell(1);
    Cell errorMessageCell = headerRow.createCell(2);
    productSkuCell.setCellValue(BulkParameters.BLIBLI_PRODUCT_SKU);
    productNameCell.setCellType(Cell.CELL_TYPE_STRING);
    productNameCell.setCellValue(BulkParameters.PARENT_PRODUCT_NAME);
    productNameCell.setCellType(Cell.CELL_TYPE_STRING);
    errorMessageCell.setCellValue(BulkParameters.ERROR_HEADER);
    errorMessageCell.setCellType(Cell.CELL_TYPE_STRING);
  }

  private void updateBulkProcess(BulkProcess bulkProcess, List<BulkProcessData> rowDataList,
    List<BulkProcessData> failedDataList) throws Exception {
    bulkProcess.setSuccessCount(rowDataList.size() - failedDataList.size());
    if (CollectionUtils.isEmpty(failedDataList)) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      bulkProcess.setErrorCount(failedDataList.size());
      if (failedDataList.size() == rowDataList.size()) {
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      } else {
        bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      }
      bulkProcess.setSystemErrorCount((int) failedDataList.stream()
        .filter(bulkProcessData -> Objects.nonNull(bulkProcessData.getSystemErrorCount()))
        .filter(bulkProcessData -> bulkProcessData.getSystemErrorCount() > 0).count());
      bulkProcess.setInputErrorCount((int) failedDataList.stream()
        .filter(bulkProcessData -> Objects.nonNull(bulkProcessData.getInputErrorCount()))
        .filter(bulkProcessData -> bulkProcessData.getInputErrorCount() > 0).count());
    }
    ProfileResponse profileResponse =
      businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        bulkProcess.getBusinessPartnerCode());
    generateDescription(bulkProcess, rowDataList, failedDataList,
      profileResponse.getCompany().isInternationalFlag());
  }

  private void generateDescription(BulkProcess bulkProcess, List<BulkProcessData> rowDataList,
    List<BulkProcessData> failedDataList, boolean isInternationalMerchant) {
    String failedMessage = getFailedMessageString(bulkProcess, isInternationalMerchant);
    if (isInternationalMerchant) {
      bulkProcess.setDescription(
        new StringBuilder(bulkProcess.getDescription()).append(BulkUpdateServiceUtil.END_SYMBOL)
          .append(BulkUpdateServiceUtil.SUCCESSFULLY_ARCHIVED_EN).append(BulkUpdateServiceUtil.IS)
          .append(rowDataList.size() - failedDataList.size())
          .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILED_ARCHIVED_EN)
          .append(BulkUpdateServiceUtil.IS).append(failedDataList.size())
          .append(BulkUpdateServiceUtil.END_SYMBOL).append(failedMessage).toString());
    } else {
      bulkProcess.setDescription(
        new StringBuilder(bulkProcess.getDescription()).append(BulkUpdateServiceUtil.END_SYMBOL)
          .append(BulkUpdateServiceUtil.SUCCESSFULLY_ARCHIVED).append(BulkUpdateServiceUtil.IS)
          .append(rowDataList.size() - failedDataList.size())
          .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILED_ARCHIVED)
          .append(BulkUpdateServiceUtil.IS).append(failedDataList.size())
          .append(BulkUpdateServiceUtil.END_SYMBOL).append(failedMessage).toString());
    }
  }

  private String getFailedMessageString(BulkProcess bulkProcess, boolean isInternationalMerchant) {
    bulkProcess.setEndDate(new Date());
    String failed_message = StringUtils.EMPTY;
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      if (isInternationalMerchant)
        failed_message = BulkUpdateServiceUtil.FAILED_MESSAGE_EN;
      else
        failed_message = BulkUpdateServiceUtil.FAILED_MESSAGE;
    }
    return failed_message;
  }
}
