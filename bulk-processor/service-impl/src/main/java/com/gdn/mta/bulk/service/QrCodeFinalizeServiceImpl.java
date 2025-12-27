package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.QRCodeProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.QrCodeErrorDTO;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.util.ExcelUtils;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@Slf4j
public class QrCodeFinalizeServiceImpl implements QrCodeFinalizeService {
  private static final String ERROR_EXCEL_FILE_NAME = "errors.xlsx";
  private static final String MERGED_PDF_FILE_NAME = "qr-codes.pdf";
  private static final String COMPRESSED_FILE_NAME = "qr-codes.zip";
  private static final List<String> ADD_TO_BAG_ERROR_CASE =
      Arrays.asList(BulkProcessData.STATUS_FAIL, BulkProcessData.STATUS_SUCCESS);

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private PdfUtilityService pdfUtilityService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private QRCodeProperties qrCodeProperties;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Override
  public void setFinalStatusAndSendNotificationOnQRGeneration(String storeId,
      BulkProcess bulkProcess) throws Exception {
    try {
      if ((!AllowedQRGenerationType.ADD_TO_BAG.getValue()
          .equalsIgnoreCase(bulkProcess.getDescription())) && BulkProcess.STATUS_ABORTED.equals(
          bulkProcess.getStatus())) {
        notificationService.sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(),
            bulkProcess.getDescription());
        return;
      }
      String localBaseDirectory = qrCodeProperties.getLocalTempDirectory()
          + bulkProcess.getBulkProcessType() + File.separator + bulkProcess.getBulkProcessCode();
      String gcsDirectory = fileStorageService.getBasePath(BulkProcessType.QR_GENERATION.getValue())
          + bulkProcess.getBulkProcessCode();
      ProcessorUtils.createDirectories(localBaseDirectory);

      // generate error file store locally
      List<BulkProcessData> failedEntries = new ArrayList<>();

      if (AllowedQRGenerationType.ADD_TO_BAG.getValue()
            .equalsIgnoreCase(bulkProcess.getDescription())) {
        List<BulkProcessData>
            fetchAllBulkProcessDataWithSuccessAndFailedState =
            bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(
                storeId, bulkProcess.getBulkProcessCode(), ADD_TO_BAG_ERROR_CASE);
        for (BulkProcessData bulkProcessData : fetchAllBulkProcessDataWithSuccessAndFailedState) {
          if (StringUtils.isNotBlank(bulkProcessData.getErrorMessage())) {
            failedEntries.add(bulkProcessData);
          }
        }
      } else {
        failedEntries = bulkProcessDataService.getFailedDataForProcessedFile(
            storeId, bulkProcess.getBulkProcessCode());
      }
      boolean errorsExist = CollectionUtils.isNotEmpty(failedEntries);
      if (errorsExist) {
        List<QrCodeErrorDTO> errorMessages = generateQRCodeErrorDTO(failedEntries);
        ExcelUtils.generateErrorFileForQrGeneration(
            localBaseDirectory + File.separator + ERROR_EXCEL_FILE_NAME, errorMessages);
      }

      // merge PDFs and store locally
      if (!(AllowedQRGenerationType.ADD_TO_BAG.getValue()
          .equalsIgnoreCase(bulkProcess.getDescription()) && BulkProcess.STATUS_ABORTED.equals(
          bulkProcess.getStatus()))) {
        pdfUtilityService.generateMergedPdfFileFromPdfFilesAtGcsDirectory(gcsDirectory,
            localBaseDirectory, MERGED_PDF_FILE_NAME);
      }

      // compress merged PDF + error excel
      Map<String, InputStream> filesToCompress = new HashMap<>(2);
      if (!(AllowedQRGenerationType.ADD_TO_BAG.getValue()
          .equalsIgnoreCase(bulkProcess.getDescription()) && BulkProcess.STATUS_ABORTED.equals(
          bulkProcess.getStatus()))){
        filesToCompress.put(MERGED_PDF_FILE_NAME,
            new FileInputStream(localBaseDirectory + File.separator + MERGED_PDF_FILE_NAME));
      }
      if (errorsExist) {
        filesToCompress.put(ERROR_EXCEL_FILE_NAME,
            new FileInputStream(localBaseDirectory + File.separator + ERROR_EXCEL_FILE_NAME));
      }
      ProcessorUtils.compressFiles(localBaseDirectory, COMPRESSED_FILE_NAME, filesToCompress);

      // upload compressed file and clear other contents of GCS directory
      String url = fileStorageService.replaceFilesAtGcsDirectory(
          gcsDirectory, COMPRESSED_FILE_NAME, Constant.MIME_TYPE_ZIP,
          new FileInputStream(localBaseDirectory + File.separator + COMPRESSED_FILE_NAME));
      if (StringUtils.isBlank(url)) {
        throw new RuntimeException(String.format("Failed to fetch uploaded file URL from %s",
            gcsDirectory));
      }

      // send notification to the seller and set final status
      notificationService.sendGenerateQrCodeNotification(bulkProcess, url);

      // clear all temp files created on storage
      ProcessorUtils.deleteDirectory(Path.of(localBaseDirectory));
    } catch (Exception e) {
      log.error("#setFinalStatusAndSendNotificationOnQRGeneration failed with bulkProcess {}, error: ",
          bulkProcess, e);
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      bulkProcess.setEndDate(new Date());
      bulkProcessService.saveBulkProcess(bulkProcess);
      notificationService.sendGenerateQrCodeFailedNotification(
          bulkProcess.getBusinessPartnerCode(), bulkProcess.getDescription());
    }
  }

  private List<QrCodeErrorDTO> generateQRCodeErrorDTO(
      List<BulkProcessData> failedBulkProcessDataEntryList) {
    return failedBulkProcessDataEntryList.stream().map(failedBulkProcessData -> {
      try {
        QrCodeRowInfo qrCodeRowInfo =
            objectMapper.readValue(failedBulkProcessData.getBulkRequestData(), QrCodeRowInfo.class);
          if (AllowedQRGenerationType.ADD_TO_BAG.getValue()
              .equalsIgnoreCase(qrCodeRowInfo.getQrGenerationType())) {
          return convertToQrCodeErrorDTOForAddToBagType(qrCodeRowInfo,
              failedBulkProcessData.getErrorMessage(),
              qrCodeRowInfo.getQrGenerationType());
        } else {
          return qrCodeRowInfo.getRowItems().stream().map(
                  qrCodeRowItemInfo -> convertToQrCodeErrorDTO(qrCodeRowItemInfo,
                      failedBulkProcessData.getErrorMessage(), qrCodeRowInfo.getQrGenerationType()))
              .collect(Collectors.toList());
        }
      } catch (JsonProcessingException e) {
        log.error("Error while parsing the QrCodeErrorDTO: {}, e- ",
            failedBulkProcessData.getErrorMessage(), e);
        return new ArrayList<QrCodeErrorDTO>();
      }
    }).flatMap(List::stream).collect(Collectors.toList());
  }

  private QrCodeErrorDTO convertToQrCodeErrorDTO(QrCodeRowItemInfo qrCodeRowItemInfo, String errorMessage,
      String qrGenerationType) {
    return QrCodeErrorDTO.builder().itemSku(qrCodeRowItemInfo.getItemSku())
        .productSku(qrCodeRowItemInfo.getProductSku())
        .pickupPointName(qrCodeRowItemInfo.getPickupPointName())
        .errorMessage(errorMessage)
        .qrGenerationType(qrGenerationType).build();
    }

  private List<QrCodeErrorDTO> convertToQrCodeErrorDTOForAddToBagType(QrCodeRowInfo qrCodeRowInfo,
      String errorMessage,
      String qrGenerationType) {
    List<QrCodeErrorDTO> qrCodeErrorDTOList = new ArrayList<>();
    try {
      Map<String, String> errorMap =
          objectMapper.readValue(errorMessage, new TypeReference<Map<String, String>>() {
          });
      if (MapUtils.isEmpty(errorMap)) {
        for (QrCodeRowItemInfo qrCodeRowItemInfo : qrCodeRowInfo.getRowItems()) {
          qrCodeErrorDTOList.add(constructQrCodeErrorDTO(qrCodeRowItemInfo.getItemSku(), qrCodeRowItemInfo.getPickupPointName(), qrGenerationType));
        }
      } else {
        errorMap.forEach((key, value) -> {
          qrCodeErrorDTOList.add(constructQrCodeErrorDTO(key, value, qrGenerationType));
        });
      }
    } catch (JsonProcessingException e) {
      log.error("#QrCodeFinalizeServiceImpl convertToQrCodeErrorDTOForAddToBagType throws : ", e);
      throw new RuntimeException(e);
    }
    return qrCodeErrorDTOList;

  }

  private QrCodeErrorDTO constructQrCodeErrorDTO(String itemSku, String pickupPointName, String qrGenerationType){
    return QrCodeErrorDTO.builder()
        .itemSku(itemSku)
        .pickupPointName(pickupPointName)
        .qrGenerationType(qrGenerationType)
        .errorMessage(String.format(BulkProcessValidationErrorMessages.INVALID_ITEM_SKU, itemSku))
        .build();
  }
}
