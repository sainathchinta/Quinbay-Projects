package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.PlatformConfig;
import com.gdn.mta.bulk.models.SheetConfig;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.FastExcelUtils;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipFile;


@Slf4j
@Service
public class ExternalProductCreationServiceBean implements ExternalProductCreationService {

  public static final String ZIP_FILE_NAME_MUST_NOT_BE_BLANK = "Zip file name must not be blank";
  public static final String BULK_PROCESS_CODE_MUST_NOT_BE_BLANK =
    "BulkProcessCode must not be blank";
  public static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK =
    "Business partner code must not be blank";
  public static final String FILE_NAMES_MUST_NOT_BE_BLANK = "File names must not be blank";
  private static final String DOT_AND_SPACE = ". ";
  private static final String SHOPEE = "shopee";
  private static final String TEMP_ZIP_PREFIX = "upload-";

  @Autowired
  private ProductLevel3ProcessorServiceBean productLevel3ProcessorServiceBean;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkProcessImageService bulkProcessImageService;

  @Autowired
  private BulkProcessImageQCService bulkProcessImageQCService;

  @Value("${external.upload.max.distinct.product.count.limit}")
  private int maxProductCount;

  @Value("${new.notification.for.header.validation.error}")
  private boolean newNotificationForHeaderValidationError;

  @Override
  public void preProcess(String storeId, String requestId, String username,
    BulkProcessExternalUploadRequest request) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getZipFileName()),
      ZIP_FILE_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getBulkProcessCode()),
      BULK_PROCESS_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(MapUtils.isNotEmpty(request.getFiles()),
      FILE_NAMES_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getBusinessPartnerCode()),
      BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    Map<String, String> args = new HashMap<>();
    args.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    args.put(BulkParameters.IS_ONLY_EXTERNAL_USER, request.getOnlyExternalUser());
    BulkProcess bulkProcess =
      productLevel3ProcessorServiceBean.createBulkProcess(storeId, requestId,
        request.getBulkProcessCode(), BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
        request.getBusinessPartnerCode(), request.getZipFileName(), args);
    this.bulkProcessService.saveBulkProcess(bulkProcess);
    CommonUtils.setProcessExternalUploadRequest(storeId, username, requestId,
      BulkProcessType.EXTERNAL_CREATION_UPLOAD, request);
    kafkaProducer.send(kafkaTopicProperties.getBulkExternalCreateEvent(),
      request.getBulkProcessCode(), request);
    log.info("Kafka event sent | topic={} | bulkProcessCode={} | requestId={} ",
      kafkaTopicProperties.getBulkExternalCreateEvent(), request.getBulkProcessCode(), requestId);
  }

  @Override
  public void process(BulkProcessExternalUploadRequest bulkProcessExternalUploadRequest)
    throws ApplicationException {
    log.info("Start processing external product creation: {} ", bulkProcessExternalUploadRequest);
    BulkProcess bulkProcess =
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        bulkProcessExternalUploadRequest.getStoreId(),
        bulkProcessExternalUploadRequest.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    if (bulkProcess == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
        "BulkProcess not found for StoreId: " + bulkProcessExternalUploadRequest.getStoreId()
          + ", BulkProcessCode: " + bulkProcessExternalUploadRequest.getBulkProcessCode());
    }
    String zipFileName = CommonUtils.generateExternalTemplateUploadPath(bulkProcess);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
      bulkProcessExternalUploadRequest.getUsername());
    try {
      ProfileResponse businessPartner = businessPartnerRepository.filterByBusinessPartnerCodeV2(
        bulkProcessExternalUploadRequest.getStoreId(), bulkProcess.getBusinessPartnerCode());
      CommonUtils.validateActiveBusinessPartner(businessPartner, bulkProcess, zipFileName);
      bulkProcess.setInternationalMerchant(false);
      bulkProcess.setNotes(Constant.GENERIC);
      bulkProcess = saveInProgressBulkProcess(bulkProcess, zipFileName);
      bulkProcess.setBulkProcessNotes(new ArrayList<>());
      bulkProcess.setUploadedFile(zipFileName);
      byte[] zipFileData = fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
      if (Objects.isNull(zipFileData) || zipFileData.length == 0) {
        throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          "Uploaded files are empty for bulkProcessCode: " + bulkProcess.getBulkProcessCode());
      }
      SystemParameterConfig systemParameterConfig =
        systemParameterConfigService.findValueByStoreIdAndVariable(
          bulkProcessExternalUploadRequest.getStoreId(),
          SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
      List<PlatformConfig> platforms = objectMapper.readValue(systemParameterConfig.getValue(),
        new TypeReference<List<PlatformConfig>>() {
        });
      PlatformConfig shopeeConfig =
        platforms.stream().filter(p -> SHOPEE.equalsIgnoreCase(p.getPlatform())).findFirst()
          .orElseThrow(() -> new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
            "Shopee config not found"));
      Map<String, SheetConfig> sheetConfigMap = shopeeConfig.getSheetConfigs().stream()
        .collect(Collectors.toMap(SheetConfig::getSheetType, sc -> sc));
      // Maps to hold sheet data
      Map<String, List<Map<String, String>>> productBasicInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> productMediaInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> productShippingInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> productSalesInfoMap = new HashMap<>();
      Map<String, String> sourceColumnToDestinationColumnMap = new HashMap<>();
      Map<String, Boolean> sourceColumnToMandatoryMap = new HashMap<>();
      Map<String, StringBuilder> productValidationErrorMap = new HashMap<>();
      // Map Excel file names to sheet types
      Map<String, String> excelFileNameToTypeMap = new HashMap<>();
      bulkProcessExternalUploadRequest.getFiles()
        .forEach((sheetType, fileName) -> excelFileNameToTypeMap.put(fileName, sheetType));
      // Write ZIP to temp file for ZipFile
      File tempZip = File.createTempFile(TEMP_ZIP_PREFIX, Constant.FILE_TYPE_ZIP);
      try (FileOutputStream fos = new FileOutputStream(tempZip)) {
        fos.write(zipFileData);
      }
      try (ZipFile zipFile = new ZipFile(tempZip)) {
        FastExcelUtils.processEachZipFileForExternalUpload(zipFileName, zipFile,
          excelFileNameToTypeMap, sheetConfigMap, shopeeConfig, sourceColumnToDestinationColumnMap,
          sourceColumnToMandatoryMap, productValidationErrorMap, productBasicInfoMap,
          productMediaInfoMap, productShippingInfoMap, productSalesInfoMap);
      } finally {
        Files.deleteIfExists(tempZip.toPath());
      }
      Set<String> baseProductIds = productBasicInfoMap.keySet();
      boolean productCountMatched =
        Stream.of(productMediaInfoMap, productShippingInfoMap, productSalesInfoMap)
          .allMatch(map -> map.keySet().equals(baseProductIds));
      if (!productCountMatched) {
        throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          "The number of unique l3 count does not match across all the sheets");
      }
      if (productBasicInfoMap.size() > maxProductCount) {
        throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          String.format(" Produk unik melebihi %d. Process - %s ", maxProductCount, zipFileName));
      }
      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap = new HashMap<>();
      Set<String> imageUrls = new HashSet<>();
      CommonUtils.readInfoSalesSheetAndCreateBulkProcessData(bulkProcessExternalUploadRequest,
        productSalesInfoMap, bulkProcess, productValidationErrorMap, businessPartner,
        productBasicInfoMap, shopeeConfig, sourceColumnToDestinationColumnMap, productMediaInfoMap,
        imageUrls, productShippingInfoMap, productIdToImageQCModelMap, bulkProcessDataList);
      bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
      bulkProcess.setBulkProcessType(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
      processBulkProcessImages(imageUrls, bulkProcess);
      bulkProcess.setTotalCount(productBasicInfoMap.size());
      bulkProcess.setStatus(
        BulkCreationCommonUtil.getImageDownloadStatusByPriority(bulkProcess.getBulkProcessType(),
          true));
      bulkProcessService.saveOperation(bulkProcess);
      List<BulkProcessImageQC> bulkProcessImageQCList = new ArrayList<>();
      processBulkProcessImageQcForUniqueProduct(productIdToImageQCModelMap, bulkProcess,
        bulkProcessImageQCList);
    } catch (ValidationException e) {
      handleExternalCreationUploadException(e, bulkProcess, zipFileName, true);
    } catch (Exception e) {
      handleExternalCreationUploadException(e, bulkProcess, zipFileName, false);
    }
  }

  public void processBulkProcessImages(Set<String> imageUrls, BulkProcess bulkProcess) {
    List<BulkProcessImage> bulkProcessImageList =
      CommonUtils.getBulkProcessImageList(imageUrls, bulkProcess);
    if (!CollectionUtils.isEmpty(bulkProcessImageList)) {
      bulkProcessImageService.saveBulkProcessImage(bulkProcessImageList);
      List<String> imageURLsList = new ArrayList<>(imageUrls);
      int imageDownloadBatchSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
          Constant.IMAGE_DOWNLOAD_BATCH_SIZE).getValue());
      List<List<String>> imageURLsSublist = Lists.partition(imageURLsList, imageDownloadBatchSize);
      for (List<String> subList : imageURLsSublist) {
        bulkProcessService.publishBulkExternalImageDownloadEventModel(
          bulkProcess.getBulkProcessCode(), bulkProcess.getBulkProcessType(), subList);
      }
      bulkProcess.setStatus(
        BulkCreationCommonUtil.getImageDownloadStatusByPriority(bulkProcess.getBulkProcessType(),
          true));
    }
  }

  public void processBulkProcessImageQcForUniqueProduct(
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap,
    BulkProcess bulkProcess, List<BulkProcessImageQC> bulkProcessImageQCList) {
    if (MapUtils.isNotEmpty(productIdToImageQCModelMap)) {
      for (Map.Entry<String, BrandAndCategoryPredictionRequest> brandAndCategoryPredictionEntry :
        productIdToImageQCModelMap.entrySet()) {
        BulkProcessImageQC bulkProcessImageQC =
          CommonUtils.getBulkProcessImageQC(brandAndCategoryPredictionEntry, bulkProcess);
        bulkProcessImageQCList.add(bulkProcessImageQC);
      }
      bulkProcessImageQCService.saveBulkProcessImageQc(bulkProcessImageQCList);
      bulkProcessService.publishBulkExternalImageQCDownloadEventModel(bulkProcess,
        productIdToImageQCModelMap);
    }
  }

  private void handleExternalCreationUploadException(Exception e, BulkProcess bulkProcess,
    String zipFileName, boolean isValidationError) {
    log.error("Error in process. Code: {}, Type: {} ", bulkProcess.getBulkProcessCode(),
      bulkProcess.getBulkProcessType(), e);
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setBulkProcess(bulkProcess);
    bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    bulkProcess.setErrorCount(0);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setUploadedFile(zipFileName);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(1);
    if (isValidationError) {
      bulkProcess.setInputErrorCount(1);
      bulkProcess.setSystemErrorCount(0);
      bulkProcess.setDescription(((ValidationException) e).getErrorMessage());
      bulkProcessNotes.setNotes(bulkProcess.getDescription());
      if (newNotificationForHeaderValidationError) {
        notificationService.sendBulkUploadedNotificationWithoutErrorSheet(bulkProcess,
          NotificationType.BULK_UPLOADED.getValue());
      } else {
        notificationService.sendBulkUploadedNotification(bulkProcess,
          NotificationType.BULK_UPLOADED.getValue(), StringUtils.EMPTY);
      }
    } else  {
      bulkProcessNotes.setNotes(BulkParameters.SYSTEM_ERROR);
    }
    bulkProcessService.saveOperation(bulkProcess);
  }

  private BulkProcess saveInProgressBulkProcess(BulkProcess bulkProcess, String excelFileName) {
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setDescription(excelFileName + DOT_AND_SPACE
      + ProductLevel3GenericProcessorServiceBean.DESCRIPTION_VALIDATION_IN_PROGRESS);
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setUploadedFile(excelFileName);
    return this.bulkProcessService.saveOperation(bulkProcess);
  }
}
