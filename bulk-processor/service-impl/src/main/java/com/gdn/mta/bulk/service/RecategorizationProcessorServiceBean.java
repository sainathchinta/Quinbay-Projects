package com.gdn.mta.bulk.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.BulkDataForRecategorizationRequest;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gda.mta.product.dto.CategoryUserMappingRequest;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.util.ExcelUtils;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.Lists;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 22/05/18.
 */
@Service("RecategorizationProcessorService")
@Transactional(readOnly = true)
public class RecategorizationProcessorServiceBean implements ProcessorService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RecategorizationProcessorServiceBean.class);

  @Value("${bulk.process.batch.size:100}")
  private int batchSize;

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void preProcess(String storeId, String requestId, String bulkProcessType, String businessPartnerCode,
    Map<String, String> files, Map<String, String> argumentsForBulkProcess, String bulkProcessCode,
      String username) throws Exception {
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode);

    final String excelFileName = argumentsForBulkProcess.get(Constant.EXCEL_FILE_NAME);
    final byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    checkArgument(StringUtils.isNotBlank(excelFileName), Constant.EXCEL_FILE_NAME_MUST_NOT_BE_BLANK);
    checkArgument(excelFile.length > 0, Constant.EXCEL_FILE_MUST_NOT_BE_BLANK);
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + File.separator
        + bulkProcessCode + ProcessorUtils.FILETYPE_EXCEL, excelFile);

    BulkProcess bulkProcess = this.createBulkProcess(storeId, requestId, bulkProcessCode, bulkProcessType,
        businessPartnerCode, excelFileName);
    this.bulkProcessRepository.saveAndFlush(bulkProcess);
    argumentsForBulkProcess.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    BulkProcessQueue bulkProcessQueue =
        createBulkProcessQueue(storeId, bulkProcessCode, bulkProcessType, argumentsForBulkProcess);

    kafkaProducer.send(kafkaTopicProperties.getBulkCreateEvent(), bulkProcessQueue);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void process(BulkProcessQueue bulkProcessQueue) throws Exception {
    BulkProcess bulkProcess = this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
            bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode());
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    if (bulkProcess == null) {
      LOGGER.error("Error at bulk process, data not found for : {}", bulkProcessQueue);
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "at bulkProcess. Store Id : {"
          + bulkProcessQueue.getStoreId() + "}, Bulk Process Code : {"
          + bulkProcessQueue.getBulkProcessCode() + "}");
    }
    Map<String, String> arguments = bulkProcessQueue.getArgs();
    String excelFileName = arguments.get("excelFilename");
    String recatId = arguments.get("recatId");
    String requestId = arguments.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    String username = arguments.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    saveInProgressBulkProcess(bulkProcess, excelFileName);
    bulkProcess.setBulkProcessNotes(new ArrayList<>());

    File excelFile =
        new File(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator
            + bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL);

    try (
        FileInputStream fileInputStreamForUserMapping = new FileInputStream(excelFile);
        FileInputStream fileInputStreamForCodeMapping = new FileInputStream(excelFile);
        FileInputStream fileInputStreamForSkuMapping = new FileInputStream(excelFile);
        FileInputStream fileInputStreamForSalesCatalogMapping = new FileInputStream(excelFile)
    ) {
      LOGGER.info("bulk process file {}", excelFile.getName());

      Map<String, List<List<Object>>> sheetNameToValidRowsMap = new HashedMap();
      Map<String, List<List<Object>>> sheetNameToInvalidRowsMap = new HashedMap();
      parseExcelSheet(0, sheetNameToInvalidRowsMap, fileInputStreamForUserMapping, excelFileName,
          sheetNameToValidRowsMap);
      parseExcelSheet(1, sheetNameToInvalidRowsMap, fileInputStreamForCodeMapping, excelFileName,
          sheetNameToValidRowsMap);
      parseExcelSheet(2, sheetNameToInvalidRowsMap, fileInputStreamForSkuMapping, excelFileName,
          sheetNameToValidRowsMap);
      parseExcelSheet(3, sheetNameToInvalidRowsMap, fileInputStreamForSalesCatalogMapping, excelFileName,
          sheetNameToValidRowsMap);

      processBusinessPartnerToCategoryMapping(sheetNameToValidRowsMap.get(Constant.SHEET_1), recatId, bulkProcess
          .getStoreId(), requestId, username);
      processProductCodeToCategoryMapping(sheetNameToValidRowsMap.get(Constant.SHEET_2), recatId, bulkProcess
          .getStoreId(), requestId, username);
      processProductSkuToCategoryMapping(sheetNameToValidRowsMap.get(Constant.SHEET_3), recatId, bulkProcess
          .getStoreId(), requestId, username);
      processProductSkuToSalesCatalogMapping(sheetNameToValidRowsMap.get(Constant.SHEET_4), recatId, bulkProcess
          .getStoreId(), requestId, username);

      bulkProcess.setDescription(new StringBuilder(excelFileName).append(Constant.FINISH_UPLOADING).toString());

      // TODO: 31/05/18 consider invalid rows for bulkProcess status
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } catch (Exception e) {
      LOGGER.error("error invoking process at service. Bulk Process Code : {}, Bulk Process Type : {}",
          bulkProcess.getBulkProcessCode(), bulkProcess.getBulkProcessType(), e);
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setBulkProcess(bulkProcess);
      bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessNotes.setNotes(e.getMessage());
      bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      bulkProcess.setInputErrorCount(bulkUploadErrorCounter.getInputErrorCount());
      bulkProcess.setSystemErrorCount(bulkUploadErrorCounter.getSystemError());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, "error while processing excel data"
          + " for recategorization.");
    }
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode();
    try {
      if (BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus()) ) {
        ProcessorUtils.deleteFile(directoryPath);
      }
    } catch (Exception e) {
      LOGGER.error("error while deleting the directories for Bulk Upload. Directory Path: {} ",
          directoryPath, e);
    }
    bulkProcess.setEndDate(Calendar.getInstance().getTime());
    this.bulkProcessRepository.save(bulkProcess);
    notificationService.sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
        !StringUtils.equals(BulkProcess.STATUS_FINISHED, bulkProcess.getStatus()));
  }

  @Trace(dispatcher=true)
  @Async
  private void processBusinessPartnerToCategoryMapping(List<List<Object>> excelData, String recatId, String storeId,
      String requestId, String username) {
    if (null != excelData && !CollectionUtils.isEmpty(excelData)) {
      for (List<Object> row : excelData) {
        try {
          CategoryUserMappingRequest mappingRequest = new CategoryUserMappingRequest(String.valueOf(row.get(0)), String
              .valueOf(row.get(1)), String.valueOf(row.get(2)), recatId, null);
          mappingRequest.setRequestId(requestId);
          mappingRequest.setStoreId(storeId);
          kafkaProducer.send(kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent(),
              mappingRequest);
        } catch (Exception ex) {
          LOGGER.error("error occurred while publishing kafka {} for  batch : {}, error is : ",
              kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent(), row, ex);
        }
      }
    }
  }

  @Trace(dispatcher=true)
  @Async
  private void processProductSkuToCategoryMapping(List<List<Object>> excelData, String recatId, String storeId,
      String requestId, String username) {
    if (null != excelData && !CollectionUtils.isEmpty(excelData)) {
      for (List<Object> row : excelData) {
        try {
          CategoryProductSkuMappingRequest mappingRequest = new CategoryProductSkuMappingRequest(String.valueOf(row.get
              (0)), String.valueOf(row.get(1)), recatId, null);
          mappingRequest.setStoreId(storeId);
          mappingRequest.setRequestId(requestId);
          kafkaProducer.send(kafkaTopicProperties.getCategoryToProductSkuMappingEvent(),
              mappingRequest);
        } catch (Exception ex) {
          LOGGER.error("error occurred while publishing kafka {} for  batch : {}, error is : ",
              kafkaTopicProperties.getCategoryToProductSkuMappingEvent(), row, ex);
        }
      }
    }
  }

  @Trace(dispatcher=true)
  @Async
  private void processProductSkuToSalesCatalogMapping(List<List<Object>> excelData, String recatId, String storeId,
      String requestId, String username) {
    if (null != excelData && !CollectionUtils.isEmpty(excelData)) {
      for (List<Object> row : excelData) {
        try {
          ProductSkuToSalesCatalogMappingRequest mappingRequest = new ProductSkuToSalesCatalogMappingRequest(String
              .valueOf(row.get(1)), String.valueOf(row.get(2)), String.valueOf(row.get(0)), recatId, null);
          mappingRequest.setStoreId(storeId);
          mappingRequest.setRequestId(requestId);
          kafkaProducer.send(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent(), mappingRequest);
        } catch (Exception ex) {
          LOGGER.error("error occurred while publishing kafka {} for  batch : {}, error is : ",
              kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent(), row, ex);
        }
      }
    }
  }

  @Trace(dispatcher=true)
  @Async
  private void processProductCodeToCategoryMapping(List<List<Object>> excelData, String recatId, String storeId,
      String requestId, String username) {
    if (null != excelData && !CollectionUtils.isEmpty(excelData)) {
      List<List<List<Object>>> batches = Lists.partition(excelData, batchSize);
      for (List<List<Object>> batch : batches) {
        try {
          BulkDataForRecategorizationRequest batchData = new BulkDataForRecategorizationRequest(batch, recatId,
              requestId, username, storeId);
          kafkaProducer.send(kafkaTopicProperties.getCategoryToProductCodeMappingEvent(),
              batchData);
        } catch (Exception ex) {
          LOGGER.error("error occurred while publishing kafka {} for  batch : {}, error is : ",
              kafkaTopicProperties.getCategoryToProductCodeMappingEvent(), batch, ex);
        }
      }
    }
  }

  private BulkProcess createBulkProcess(String storeId, String requestId, String bulkProcessCode,
      String bulkProcessType, String businessPartnerCode, String excelFilename) {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(storeId);
    bulkProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setBulkProcessType(bulkProcessType);
    bulkProcess.setBusinessPartnerCode(businessPartnerCode);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(requestId);
    bulkProcess.setDescription(excelFilename + ". " + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
    bulkProcess.setErrorCount(0);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setTotalCount(0);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    bulkProcess.setCreatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    bulkProcess.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    bulkProcess.setUploadedFile(excelFilename);
    return bulkProcess;
  }

  private BulkProcessQueue createBulkProcessQueue(String storeId, String bulkProcessCode,
      String bulkProcessType, Map<String, String> args) {
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue();
    bulkProcessQueue.setStoreId(storeId);
    bulkProcessQueue.setBulkProcessCode(bulkProcessCode);
    bulkProcessQueue.setBulkProcessType(bulkProcessType);
    bulkProcessQueue.setArgs(args);
    return bulkProcessQueue;
  }

  private void saveInProgressBulkProcess(BulkProcess bulkProcess, String excelFileName) {
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setDescription(excelFileName + ". "
        + ProductLevel3ProcessorServiceBean.DESCRIPTION_VALIDATION_IN_PROGRESS);
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setUploadedFile(excelFileName);
    this.bulkProcessRepository.save(bulkProcess);
  }

  private void parseExcelSheet(int index, Map<String, List<List<Object>>> sheetNameToInvalidRowsMap,
      FileInputStream fileInputStreamRaw, String fileName, Map<String, List<List<Object>>> sheetNameToExcelDataMap)
      throws Exception {
    try {
      List<List<Object>> rows;
      try {
        rows = ExcelUtils.read(fileInputStreamRaw, index, 0);
        if (CollectionUtils.isEmpty(rows)) {
          LOGGER.error("Error no data found in excel File : {}", fileName);
          throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, Constant.EXCEL_FILE_MUST_NOT_BE_BLANK + " in"
              + " sheet : " + (index + 1));
        }
      } catch (Exception e) {
        LOGGER.error("Error occurred while reading rows of Excel File :{}, ", fileName, e);
        throw new ApplicationException(ErrorCategory.INVALID_FORMAT, "Invalid excel rows in sheet : " + (index + 1));
      } finally {
        try {
          fileInputStreamRaw.close();
        } catch (Exception ex) {
          LOGGER.error("Error occurred while closing file input stream :{}, ", fileInputStreamRaw, ex);
          throw new ApplicationException(ErrorCategory.INVALID_STATE, "error while closing fileInputStream : " +
              fileInputStreamRaw);
        }
      }
      List<List<Object>> invalidRows = new ArrayList<>();
      rows = validateAllRows(rows, invalidRows);
      sheetNameToInvalidRowsMap.put(Constant.SHEET + (index + 1), invalidRows);
      sheetNameToExcelDataMap.put(Constant.SHEET + (index + 1), rows);
    } catch (Exception ex) {
      LOGGER.error("error occurred while parsing sheet : {} of excel file : {}, error is : ", (index + 1), fileName, ex);
    }
  }

  private List<List<Object>> validateAllRows(List<List<Object>> rows, List<List<Object>> invalidRows) {
    List<List<Object>> validRows = new ArrayList<>();
    List<Object> excelHeaders = rows.get(0);
    boolean isInvlid;
    rows.remove(0);
    for (List<Object> row : rows) {
      isInvlid = false;
      for (int i = 0; i < excelHeaders.size(); i++) {
        if (StringUtils.isBlank(row.get(i).toString())) {
          invalidRows.add(row);
          isInvlid = true;
          break;
        }
      }
      if (!isInvlid) {
        validRows.add(row);
      }
    }
    return validRows;
  }
}
