package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.CnExcelHeaderNames;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.ExcelUtils;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.google.common.collect.ImmutableMap;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@Service("ProductLevel3ProcessorService")
public class ProductLevel3ProcessorServiceBean implements ProcessorService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3ProcessorServiceBean.class);
  public static final String PROCESS_ABORTED = "Proses dibatalkan";
  public static final String EXCEL_FILE_MUST_NOT_BE_BLANK = "Excel File tidak boleh kosong";
  public static final String EXCEL_FILE_NOT_FOUND = "Excel File not found ";
  public static final String ZIP_FILE_MUST_NOT_BE_BLANK = "Zip File must not be blank";
  public static final String CATEGORY_CODE_MUST_NOT_BE_BLANK = "CategoryCode tidak boleh kosong";
  public static final String EXCEL_DATA_IS_BLANK = "File Excel Data Kosong. Silakan periksa file yang diupload";
  public static final String EXCEL_HEADER_INVALID =
      "Header File Excel tidak sesuai dengan template. Silakan periksa lagi dan upload";
  public static final String DESCRIPTION_PENDING = "menunggu untuk diproses";
  public static final String DESCRIPTION_VALIDATION_IN_PROGRESS = "proses validasi berlangsung";
  public static final String DESCRIPTION_SUCCESS = "Sukses: ";
  public static final String DESCRIPTION_UPLOADING_FAILED = "Proses upload gagal ";
  public static final String SPACE = " ";
  public static final String OUT_OF = "out of";
  public static final String DARI = "dari";
  public static final String EXCEL_ERROR =
      "Header File Excel tidak dapat dibaca. Silakan periksa file yang diupload.  Membatalkan "
          + "proses bulk upload karena alasan tersebut.";
  public static final String DEFAULT = "DEFAULT";
  private static final String EMPTY_CATEGORY_CODE = "Category code not present in the excel sheet.";
  private static final String INVALID_EXCEL_FILE = "Invalid excel file";
  public static final String ROW = "Baris : ";
  private static final String ACTIVE_MERCHANT = "ACTIVE";
  private static final String FILE_FROM_INACTIVE_SELLER = "File uploaded by inactive seller";
  private static final String INACTIVE_CATEGORY = "Category is not active currently";
  private static final Map<Boolean, String> CHILD_SKU =
      ImmutableMap.of(true, BulkCnCreationHeaderNames.CHILD_SKU_EN, false, BulkCnCreationHeaderNames.CHILD_SKU_ID);
  private static final Map<Boolean, String> QUANTITY =
      ImmutableMap.of(true, BulkCnCreationHeaderNames.QUANTITY_EN, false, BulkCnCreationHeaderNames.QUANTITY_ID);
  private static final Map<Boolean, String> VARIANT_IMAGE =
      ImmutableMap.of(true, BulkCnCreationHeaderNames.VARIANT_IMAGE_EN, false,
          BulkCnCreationHeaderNames.VARIANT_IMAGE_ID);

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private GeneratorRepository generatorRepository;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkCategoryProcessorService bulkCategoryProcessorService;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${download.link}")
  private String downloadLink;

  @Value("${validate.bulk.max.number.of.rows}")
  private boolean validateBulkMaxNumberOfRows;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Value("${bulk.max.number.of.rows}")
  private int bulkMaxNumberOfRows;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCncRestrictionEnabled;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${bulk.creation.avoid.redundant.download}")
  private boolean avoidRedundantDownloadInBulkCreation;

  public CategoryRepository getCategoryRepository() {
    return this.categoryRepository;
  }

  @Override
  public void preProcess(String storeId, String requestId, String bulkProcessType,
      String businessPartnerCode, Map<String, String> files, Map<String, String> args,
      String bulkProcessCode, String username) throws Exception {

    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode);
    String excelFileName = args.get(Constant.EXCEL_FILE_NAME);
    String excelFileType = null;
    if (excelFileName.endsWith(ProcessorUtils.FILETYPE_EXCEL)) {
      excelFileType = Constant.FILE_TYPE_XLS;
    } else if (excelFileName.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL)) {
      excelFileType = Constant.FILE_TYPE_XLSX;
    } else if (excelFileName.endsWith(ProcessorUtils.FILETYPE_XLSM_EXCEL)) {
      excelFileType = Constant.FILE_TYPE_XLSM;
    }
    List<String> zipFileNames = new ArrayList<>();
    if (avoidRedundantDownloadInBulkCreation) {
      String[] allFileNames = args.get(Constant.FILE_NAMES).split(Constant.UNICODE_DELIMITER);
      for (String fileName : allFileNames) {
        if (fileName.endsWith(ProcessorUtils.FILETYPE_EXCEL)) {
          excelFileType = Constant.FILE_TYPE_XLS;
          excelFileName = fileName;
        } else if (fileName.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL)) {
          excelFileType = Constant.FILE_TYPE_XLSX;
          excelFileName = fileName;
        } else if (fileName.endsWith(ProcessorUtils.FILETYPE_XLSM_EXCEL)) {
          excelFileType = Constant.FILE_TYPE_XLSM;
          excelFileName = fileName;
        } else {
          zipFileNames.add(fileName);
        }
      }
    }
    final byte[] excelFile = getExcelFile(files, excelFileType, excelFileName, username);
    GdnPreconditions
        .checkArgument(excelFile.length > 0, ProductLevel3ProcessorServiceBean.EXCEL_FILE_MUST_NOT_BE_BLANK);
    args.put(Constant.EXCEL_FILE_NAME, excelFileName);

    if (!Constant.FILE_TYPE_XLSM.equalsIgnoreCase(excelFileType)) {
      String categoryCode =
          ExcelUtils.getCategoryCodeFromExcel(new ByteArrayInputStream(excelFile), excelFileType);
      if (StringUtils.isBlank(categoryCode)) {
        LOGGER.error("Category code not present in the excel sheet : {} for bulkProcessCode : {}",
            excelFileName, bulkProcessCode);
        throw new ApiIncorrectInputDataException(INVALID_EXCEL_FILE,
            ErrorCategory.INVALID_FORMAT.getCode());
      }
    }
    String defaultBulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    boolean convertedProductCreationUpload = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    if (convertedProductCreationUpload) {
      defaultBulkProcessType = BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue();
    }

    BulkUpdateProcessDTO bulkUpdateProcessDTO = BulkUpdateProcessDTO.builder()
      .bulkProcessType(defaultBulkProcessType).fileContent(excelFile)
      .build();
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, excelFileName);

    processZipFileImages(files, bulkProcessCode, username , zipFileNames);

    boolean priorityQueueEnabled = Boolean.parseBoolean(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED).getValue());
    ProfileResponse profileResponse =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(storeId, businessPartnerCode);

    BulkProcess bulkProcess = this.createBulkProcess(storeId, requestId, bulkProcessCode,
        BulkCreationCommonUtil.getProductCreationBulkProcessTypeForPrioritySellers(priorityQueueEnabled,
            profileResponse), businessPartnerCode, excelFileName, args);
    if (convertedProductCreationUpload) {
      bulkProcess.setBulkProcessType(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
    }
    this.bulkProcessService.saveBulkProcess(bulkProcess);
    BulkProcessQueue bulkProcessQueue = createBulkProcessQueue(storeId, bulkProcessCode, bulkProcessType, args);

    if (Constant.FILE_TYPE_XLSM.equalsIgnoreCase(excelFileType)) {
      LOGGER.info("Creating bulk products for unified for seller : {}", businessPartnerCode);
      // Setting same bulkProcessType as PRODUCT_LEVEL_3_GENERIC to use same bean for ConvertedProductCreationUpload as well
      bulkProcessQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.getValue());
      if (convertedProductCreationUpload) {
        log.info("Overriding bulkProcessType in event payload for {} bulkProcessType ",
            BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
        String event = kafkaTopicProperties.getBulkConvertedProductCreationEvent();
        kafkaProducer.send(event, bulkProcessQueue);
        LOGGER.info("publishing kafka topic {} for bulk converted product create request : {} ", event, requestId);
      } else {
        kafkaProducer.send(getGenericProductCreationEventForPrioritySellers(priorityQueueEnabled, profileResponse),
            bulkProcessQueue);
        LOGGER.info("publishing kafka topic {} for bulk generic create request : {}",
            getGenericProductCreationEventForPrioritySellers(priorityQueueEnabled, profileResponse), requestId);
      }
    } else {
      String categoryCode =
        ExcelUtils.getCategoryCodeFromExcel(new ByteArrayInputStream(excelFile), excelFileType);
      if (StringUtils.isNotBlank(categoryCode)) {
        LOGGER.info("Creating bulk products for category : {}", categoryCode);
        bulkProcessQueue.getArgs().put(Constant.CATEGORY_CODE, categoryCode);
        kafkaProducer.send(getCatgeoryProductCreationEventForPrioritySellers(priorityQueueEnabled, profileResponse),
            bulkProcessQueue);
        LOGGER.info("publishing kafka topic {} for bulk create request : {}",
            getCatgeoryProductCreationEventForPrioritySellers(priorityQueueEnabled, profileResponse), requestId);
      } else {
        LOGGER.error("Category code not present in the excel sheet : {} for bulkProcessCode : {}", excelFileName,
            bulkProcessCode);
        throw new ApiIncorrectInputDataException(INVALID_EXCEL_FILE,ErrorCategory.INVALID_FORMAT.getCode());
      }
    }
  }

  private void processZipFileImages(Map<String, String> files, String bulkProcessCode,
      String username, List<String> zipFileNames) throws Exception {
    if (avoidRedundantDownloadInBulkCreation) {
      List<MultipartFile> filesList =
          fileStorageServiceBean.getListOfMultipartFile(username, zipFileNames);
      for (MultipartFile multipartFile : filesList) {
        byte[] zipImageFile = multipartFile.getBytes();
        GdnPreconditions.checkArgument(zipImageFile.length > 0,
            ProductLevel3ProcessorServiceBean.ZIP_FILE_MUST_NOT_BE_BLANK);
        fileStorageServiceBean.decompressFile(bulkProcessCode, zipImageFile);
      }
    } else {
      for (int idx = 1; idx < files.size(); idx++) {
        byte[] zipImageFile = Base64.decodeBase64(files.get("zip_" + idx));
        if (zipImageFile != null) {
          GdnPreconditions.checkArgument(zipImageFile.length > 0,
              ProductLevel3ProcessorServiceBean.ZIP_FILE_MUST_NOT_BE_BLANK);
          fileStorageServiceBean.decompressFile(bulkProcessCode, zipImageFile);
        }
      }
    }
  }

  private byte[] getExcelFile(Map<String, String> files, String excelFileType, String excelFileName,
      String username) throws Exception {
    if (avoidRedundantDownloadInBulkCreation) {
      List<MultipartFile> filesList =
          fileStorageServiceBean.getListOfMultipartFile(username, List.of(excelFileName));
      MultipartFile excelFile = filesList.stream().findFirst().orElseThrow(
          () -> new FileNotFoundException(
              ProductLevel3ProcessorServiceBean.EXCEL_FILE_NOT_FOUND + excelFileName));
      return excelFile.getBytes();
    } else {
      return Base64.decodeBase64(files.get(excelFileType));
    }
  }

  private String getCatgeoryProductCreationEventForPrioritySellers(boolean priorityQueueEnabled,
      ProfileResponse profileResponse) {
    if (priorityQueueEnabled && isTrustedSeller(profileResponse)) {
      return kafkaTopicProperties.getBulkCreatePriorityEvent();
    } else {
      return kafkaTopicProperties.getBulkCreateEvent();
    }
  }

  private String getGenericProductCreationEventForPrioritySellers(boolean priorityQueueEnabled,
      ProfileResponse profileResponse) {
    if (priorityQueueEnabled && isTrustedSeller(profileResponse)) {
      return kafkaTopicProperties.getBulkGenericCreatePriorityEvent();
    } else {
      return kafkaTopicProperties.getBulkGenericCreateEvent();
    }
  }

  private static boolean isTrustedSeller(ProfileResponse profileResponse) {
    return profileResponse.isTrustedSeller();
  }

  @Override
  public void process(BulkProcessQueue bulkProcessQueue) throws Exception {
    BulkProcess bulkProcess = this.bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    if (bulkProcess == null) {
      LOGGER.error("Error at bulk process, data not found for : {}", bulkProcessQueue);
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at bulkProcess. Store Id : {" + bulkProcessQueue.getStoreId() + "}, Bulk Process Code : {" + bulkProcessQueue
              .getBulkProcessCode() + "}");
    }
    String excelFileName = bulkProcessQueue.getArgs().get(Constant.EXCEL_FILE_NAME);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    bulkProcess = saveInProgressBulkProcess(bulkProcess, excelFileName);
    bulkProcess.setBulkProcessNotes(new ArrayList<>());
    ByteArrayInputStream fileInputStreamHeader = null;
    ByteArrayInputStream fileInputStreamRaw = null;
    try {
      String categoryCode = bulkProcessQueue.getArgs().get(Constant.CATEGORY_CODE);
      GdnPreconditions.checkArgument(!StringUtils.isBlank(categoryCode),
          ProductLevel3ProcessorServiceBean.CATEGORY_CODE_MUST_NOT_BE_BLANK);
      CategoryDetailResponse category = this.getCategoryRepository()
          .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
      CommonUtils.filterHideFromSellerAttributes(productSuitabilityFeatureEnabled, category);
      ProfileResponse businessPartner = this.businessPartnerRepository
          .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(),
              bulkProcess.getBusinessPartnerCode());
      bulkProcess.setInternationalMerchant(businessPartner.getCompany().isInternationalFlag());
      bulkProcess.setNotes(categoryCode);
      bulkProcess.setUploadedFile(excelFileName);
      bulkProcess = bulkProcessService.saveOperation(bulkProcess);
      String excelFileType = excelFileName.substring(excelFileName.lastIndexOf(Constant.DOT));
      byte[] fileByteData = fileStorageServiceBean.downloadFile(bulkProcess, excelFileType);
      List<List<Object>> excelHeadersWithGroupHeaders = null;
      List<Object> excelHeaders = null;
      List<List<Object>> rows = null;
      List<Integer> excludeColumns = new ArrayList<>();
      fileInputStreamHeader = new ByteArrayInputStream(fileByteData);
      fileInputStreamRaw = new ByteArrayInputStream(fileByteData);
      try {
        excelHeadersWithGroupHeaders = ExcelUtils
                .readByDataSheetName(fileInputStreamHeader, BulkParameters.DATA_SHEET, BulkParameters.HEADER_START_ROW,
                        BulkParameters.HEADER_END_ROW, excelFileType, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);
        excelHeaders = excelHeadersWithGroupHeaders.get(1);
        if(BulkParameters.ERROR_HEADER.equals(excelHeaders.get(excelHeaders.size() - 1))) {
          excludeColumns.add(excelHeaders.size() - 1);
          excelHeaders.remove(excelHeaders.size() - 1);
        }
        rows = ExcelUtils
            .readByDataSheetName(fileInputStreamRaw, BulkParameters.DATA_SHEET, BulkParameters.USER_DATA_START_ROW,
                excludeColumns, excelFileType);
      } catch (ApplicationRuntimeException ae) {
        bulkProcess
                .setDescription(excelFileName + ". " + ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN);
        throw new ApplicationException(ErrorCategory.VALIDATION, ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN);
      }
      if (CollectionUtils.isEmpty(rows)) {
        bulkProcess.setDescription(excelFileName + ". " + ProductLevel3ProcessorServiceBean.EXCEL_DATA_IS_BLANK);
        LOGGER.error("Error no data found in excel File : {} for process id: {}", excelFileName,
            bulkProcess.getBulkProcessCode());
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
            EXCEL_FILE_MUST_NOT_BE_BLANK + " " + PROCESS_ABORTED);
      }
      if (!ACTIVE_MERCHANT.equals(businessPartner.getMerchantStatus())) {
        bulkProcess.setDescription(excelFileName + ". " + ProductLevel3ProcessorServiceBean.FILE_FROM_INACTIVE_SELLER);
        LOGGER.error("Error for file, uploaded by inactive seller. File : {}, process id :{}, "
          + "seller code : {}, seller status : {}", excelFileName,
          bulkProcess.getBulkProcessCode(), bulkProcess.getBusinessPartnerCode(),
          businessPartner.getMerchantStatus());
        throw new ApplicationException(ErrorCategory.VALIDATION,
          FILE_FROM_INACTIVE_SELLER + " " + PROCESS_ABORTED);
      }
      if (!category.isActivated()) {
        bulkProcess.setDescription(excelFileName + ". " + ProductLevel3ProcessorServiceBean.INACTIVE_CATEGORY);
        LOGGER.error("Error for file, uploaded for inactive category. File : {}, process id :{}, "
            + "category code : {}, category active : {}", excelFileName,
          bulkProcess.getBulkProcessCode(), category.getCategoryCode(),
          category.isActivated());
        throw new ApplicationException(ErrorCategory.VALIDATION,
          INACTIVE_CATEGORY + " " + PROCESS_ABORTED);
      }
      List<Integer> ignoredIndex = new ArrayList<Integer>();
      MerchantStatusType merchantStatusType = BulkCreationCommonUtil.getMerchantType(businessPartner);
      String merchantType =
          Optional.ofNullable(businessPartner.getCompany()).map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
      boolean instoreEligible = CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, businessPartner);
      this.validateExcelHeaders(excelHeaders, category, ignoredIndex, bulkProcess.getInternationalMerchant(),
          merchantStatusType, merchantType, instoreEligible);

      //if switch is true, will follow the new workflow, else existing workflow.
      SystemParameterConfig bulkSwitchParameter =
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
      boolean bulkSwitch = false;
      if (Objects.nonNull(bulkSwitchParameter)) {
        bulkSwitch = Boolean.valueOf(bulkSwitchParameter.getValue());
      }

      if (bulkSwitch) {
        bulkCategoryProcessorService.generateBulkProcessDataAndImage(bulkProcess, rows, excelHeaders,
            bulkProcessQueue.getArgs().get(Constant.ACCESSIBLE_PICKUP_POINTS));
      }
    } catch (Exception e) {
      String notes = e.getMessage();
      ProductLevel3ProcessorServiceBean.LOGGER
          .error("error invoking process at service. Bulk Process Code : {}, Bulk Process Type : {}",
              bulkProcess.getBulkProcessCode(), bulkProcess.getBulkProcessType(), e);
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setBulkProcess(bulkProcess);
      bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      bulkProcess.setInputErrorCount(bulkUploadErrorCounter.getInputErrorCount());
      bulkProcess.setSystemErrorCount(bulkUploadErrorCounter.getSystemError());
      notes = setNotesAndSendNotificationForEmptyFileUploaded(bulkProcess, excelFileName, e.getMessage(), notes);
      bulkProcessNotes.setNotes(notes);
      bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      bulkProcessService.saveOperation(bulkProcess);
      this.trackerService
          .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
              "", TrackerConstants.FAILED,
              bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    } finally {
      if (Objects.nonNull(fileInputStreamHeader)) {
        fileInputStreamHeader.close();
      }
      if (Objects.nonNull(fileInputStreamRaw)) {
        fileInputStreamRaw.close();
      }
    }
  }

  private String setNotesAndSendNotificationForEmptyFileUploaded(BulkProcess bulkProcess, String excelFileName,
      String errorMessage, String notes) {
    if (Optional.ofNullable(errorMessage).orElse(StringUtils.EMPTY).contains(EXCEL_FILE_MUST_NOT_BE_BLANK)) {
      notes = BulkCreationCommonUtil.setNotesForEmptyFileUploaded(bulkProcess, excelFileName);
      notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
          fileStorageServiceBean.getDownloadLinkHtml(downloadLink));
    }
    return notes;
  }

  private BulkProcess saveInProgressBulkProcess(BulkProcess bulkProcess, String excelFileName) {
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess
        .setDescription(excelFileName + ". " + ProductLevel3ProcessorServiceBean.DESCRIPTION_VALIDATION_IN_PROGRESS);
    bulkProcess.setBulkUpdate(false);
    return this.bulkProcessService.saveOperation(bulkProcess);
  }

  private void validateExcelHeaders(List<Object> excelHeaders, CategoryDetailResponse category,
      List<Integer> ignoredIndexes, boolean isInternationalMerchant, MerchantStatusType merchantStatusType,
      String merchantType, boolean instoreEligible) throws Exception {
    Map<String, List<String>> groupHeaders = new LinkedHashMap<>();
    List<String> validExcelHeaders = new ArrayList<>();
    List<String> infoHeaders = getValidExcelHeaders(isInternationalMerchant);
    validExcelHeaders.addAll(infoHeaders);
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.PRODUCT_INFO, infoHeaders);
    String colourFamilyExcelHeader = null;
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && BulkCreationCommonUtil.isDescriptiveOrPredefinedAndNotVariantCreation(
          categoryAttribute.getAttribute())) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (Constant.COLOUR_FAMILY.equals(attributeName)) {
          colourFamilyExcelHeader = attributeName;
        }
      }
    }
    List<String> variantAttributes = new ArrayList<>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && BulkCreationCommonUtil.isDefiningOrVariantCreation(
          categoryAttribute.getAttribute())) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        validExcelHeaders.add(attributeName);
        variantAttributes.add(attributeName);
        if ((Constant.WARNA.equals(attributeName) || Constant.COLOR.equals(attributeName)) && Objects.nonNull(colourFamilyExcelHeader)) {
          validExcelHeaders.add(colourFamilyExcelHeader);
          variantAttributes.add(colourFamilyExcelHeader);
        }
      }
    }
    validExcelHeaders.add(CnExcelHeaderNames.PARENT);
    variantAttributes.add(CnExcelHeaderNames.PARENT);
    validExcelHeaders.add(VARIANT_IMAGE.get(isInternationalMerchant));
    variantAttributes.add(VARIANT_IMAGE.get(isInternationalMerchant));
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.VARIENT, variantAttributes);
    List<String> images = new ArrayList<>();
    for (int imageNumber = 0; imageNumber < GenericBulkParameters.NUMBER_OF_IMAGES; imageNumber++) {
      if(isInternationalMerchant) {
        validExcelHeaders.add(CnExcelHeaderNames.IMAGE_PREFIX + (imageNumber + 1));
        images.add(CnExcelHeaderNames.IMAGE_PREFIX + (imageNumber + 1));
      } else {
        validExcelHeaders.add(CnExcelHeaderNames.FOTO_PREFIX + (imageNumber + 1));
        images.add(CnExcelHeaderNames.FOTO_PREFIX + (imageNumber + 1));
      }
    }
    int index = 0;
    Iterator<Object> iterator = excelHeaders.iterator();
    while (iterator.hasNext()) {
      Object header = iterator.next();
      if (BulkParameters.WAREHOUSE_STOCK_HEADER.equalsIgnoreCase(String.valueOf(header))) {
        ignoredIndexes.add(index);
        iterator.remove();
      }
      index++;
    }
    // Add URL video after images
    validExcelHeaders.add(CnExcelHeaderNames.URL_VIDEO);
    images.add(CnExcelHeaderNames.URL_VIDEO);
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.IMAGES, images);
    validExcelHeaders.addAll(addShipmentInfo(isInternationalMerchant));
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.SHIPMENT_INFO, addShipmentInfo(isInternationalMerchant));
    validExcelHeaders.addAll(addPriceAndStock(isInternationalMerchant));
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.PRICE_AND_STOCK,
        addPriceAndStock(isInternationalMerchant));
    validExcelHeaders.addAll(addSkuVisibility(isInternationalMerchant, merchantStatusType.getType(), instoreEligible));
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.SKU_VISIBILITY,
        addSkuVisibility(isInternationalMerchant, merchantStatusType.getType(), instoreEligible));

    List<String> recommendedAttributes = new ArrayList<>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      List<String> attributes =  setRecommendedAttributes(categoryAttribute, isInternationalMerchant);
      recommendedAttributes.addAll(attributes);
    }
    validExcelHeaders.addAll(recommendedAttributes);
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.RECOMMENDED_ATTRIBUTES, recommendedAttributes);

    List<String> otherAttributes =  new ArrayList<>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      List<String> attributes  = setNonRecommendedAttributes(categoryAttribute, isInternationalMerchant);
      otherAttributes.addAll(attributes);
    }
    validExcelHeaders.addAll(otherAttributes);
    groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.OTHER_ATTRIBUTES, otherAttributes);

    if (Arrays.asList(productBundlingEligibleMerchantTypes.split(Constants.COMMA)).contains(merchantType)) {
      groupHeaders.put(com.gdn.partners.bulk.util.ExcelHeaderNames.BUNDLE_INFO, addBundleInfo(isInternationalMerchant));
      if (productBundlingEnabled) {
        validExcelHeaders.addAll(addBundleInfo(isInternationalMerchant));
      }
    }

    if (excelHeaders.size() != validExcelHeaders.size()) {
      LOGGER.error("Header Mismatch :uploaded  Excel Headers {} ", excelHeaders.toString());
      LOGGER.error("Header Mismatch :Expected  Excel Headers {} ", validExcelHeaders.toString());
      throw new Exception(EXCEL_ERROR);
    }

    List<String> uploadedExcelHeaders = new ArrayList<>();
    for(Object header : excelHeaders) {
      uploadedExcelHeaders.add(String.valueOf(header).replace("*", "").toLowerCase());
    }

    int startIndex = 0;
    for(Map.Entry<String, List<String>> entry : groupHeaders.entrySet()) {
      List<String> groupedHeaders = entry.getValue();
      List<String> uploadedHeaderSublist = uploadedExcelHeaders.subList(startIndex, startIndex + groupedHeaders.size());
      if (groupedHeaders.size() != uploadedHeaderSublist.size()) {
        LOGGER.error("Header Mismatch :uploaded  Excel Headers {} ", uploadedHeaderSublist.toString());
        LOGGER.error("Header Mismatch :Expected  Excel Headers {} ", groupedHeaders.toString());
        throw new Exception(EXCEL_ERROR);
      }

      List<String> formattedGroupedHeader = new ArrayList<>();
      for (int i = 0; i < groupedHeaders.size(); i++) {
        String validExcelHeader = groupedHeaders.get(i).replace("*", "").toLowerCase();
        formattedGroupedHeader.add(validExcelHeader);
        if (!uploadedHeaderSublist.contains(validExcelHeader)) {
          LOGGER.error("Header Mismatch - Expected Header - {}, Actual Header - {}.", groupedHeaders,
              uploadedHeaderSublist);
          throw new Exception("Header Mismatch - Expected Headers - " + groupedHeaders + " Actual Headers -" + uploadedHeaderSublist + " "
              + ProductLevel3ProcessorServiceBean.EXCEL_HEADER_INVALID);
        }
      }

      for (int i = 0; i < groupedHeaders.size(); i++) {
        if (!formattedGroupedHeader.contains(uploadedHeaderSublist.get(i))) {
          LOGGER.error("Header Mismatch - Expected Header - {}, Actual Header - {}.", groupedHeaders,
              uploadedHeaderSublist);
          throw new Exception("Expected Header - " + groupedHeaders + " Actual Header -" + uploadedHeaderSublist + " "
              + ProductLevel3ProcessorServiceBean.EXCEL_HEADER_INVALID);
        }
        startIndex++;
      }
    }
  }

  private List<String> addBundleInfo(boolean isInternationalMerchant) {
    List<String> bundleHeaders = new ArrayList<>();
    bundleHeaders.add(CHILD_SKU.get(isInternationalMerchant));
    bundleHeaders.add(QUANTITY.get(isInternationalMerchant));
    return bundleHeaders;
  }

  private List<String> setRecommendedAttributes(CategoryAttributeResponse categoryAttribute,
      boolean isInternationalMerchant) {
    List<String> recommendedAttributes = new ArrayList<>();
    String attributeName = categoryAttribute.getAttribute().getName();
    if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
      attributeName = categoryAttribute.getAttribute().getNameEnglish();
    }
    if ((!categoryAttribute.isMarkForDelete() && BulkCreationCommonUtil.isDescriptiveOrPredefinedAndNotVariantCreation(
        categoryAttribute.getAttribute()) && (categoryAttribute.getAttribute().isBasicView() || categoryAttribute
        .getAttribute().isSkuValue())) && (!BulkParameters.FAMILY_COLOUR.equals(attributeName)
        && !BulkParameters.BRAND.equals(attributeName))) {
      recommendedAttributes.add(attributeName);
    }
    return recommendedAttributes;
  }

  private List<String> setNonRecommendedAttributes(CategoryAttributeResponse categoryAttribute,
      boolean isInternationalMerchant) {
    List<String> otherAttributes = new ArrayList<>();
    String attributeName = categoryAttribute.getAttribute().getName();
    if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
      attributeName = categoryAttribute.getAttribute().getNameEnglish();
    }
    if (!categoryAttribute.isMarkForDelete() && BulkCreationCommonUtil.isDescriptiveOrPredefinedAndNotVariantCreation(
        categoryAttribute.getAttribute()) && !categoryAttribute.getAttribute().isBasicView()
        && !categoryAttribute.getAttribute().isSkuValue() && (!BulkParameters.FAMILY_COLOUR.equals(attributeName)
        && !BulkParameters.BRAND.equals(attributeName))) {
      otherAttributes.add(attributeName);
    }
    return otherAttributes;
  }

  private List<String> addPriceAndStock( boolean isInternationalMerchant) {
    List<String> priceAndStockHeader = new ArrayList<>();
    if (isInternationalMerchant) {
      priceAndStockHeader.add(CnExcelHeaderNames.PRICE);
      priceAndStockHeader.add(CnExcelHeaderNames.SELLING_PRICE);
      priceAndStockHeader.add(CnExcelHeaderNames.AVAILABLE_STOCK);
      priceAndStockHeader.add(CnExcelHeaderNames.MINIMUM_STOCK);
    } else {
      priceAndStockHeader.add(CnExcelHeaderNames.HARGA);
      priceAndStockHeader.add(CnExcelHeaderNames.HARGA_PENJULAN);
      priceAndStockHeader.add(CnExcelHeaderNames.STOK_TERSEDIA);
      priceAndStockHeader.add(CnExcelHeaderNames.STOK_MINIMUM);
    }
    return priceAndStockHeader;
  }

  private List<String> addShipmentInfo(boolean isInternationalMerchant) {
    List<String> shipmentHeaders =  new ArrayList<>();
    if (isInternationalMerchant) {
      shipmentHeaders.add(ExcelHeaderNames.HANDLING_TYPE);
      shipmentHeaders.add(ExcelHeaderNames.PICKUP_POINT_CODE);
      shipmentHeaders.add(ExcelHeaderNames.LENGTH);
      shipmentHeaders.add(ExcelHeaderNames.WIDTH);
      shipmentHeaders.add(ExcelHeaderNames.HEIGHT);
      shipmentHeaders.add(ExcelHeaderNames.WEIGHT);
    } else {
      shipmentHeaders.add(ExcelHeaderNames.TIPE_PENANGAN);
      shipmentHeaders.add(ExcelHeaderNames.KODE_PICKUP_POINT);
      shipmentHeaders.add(ExcelHeaderNames.PANJANG);
      shipmentHeaders.add(ExcelHeaderNames.LEBAR);
      shipmentHeaders.add(ExcelHeaderNames.TINGGI);
      shipmentHeaders.add(ExcelHeaderNames.BERAT);
    }
    return shipmentHeaders;
  }

  private List<String> addSkuVisibility(boolean isInternationalMerchant, int merchantStatusType,
      boolean instoreEligible) {
    List<String> skuVisibilityHeaders = new ArrayList<>();
    if (isInternationalMerchant) {
      skuVisibilityHeaders.add(ExcelHeaderNames.DELIVERY_STATUS);
    } else {
      skuVisibilityHeaders.add(ExcelHeaderNames.STATUS_PENGIRIMAN);
    }
    if (merchantStatusType >= MerchantStatusType.BFB.getType()) {
      if (isInternationalMerchant) {
        skuVisibilityHeaders.add(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN);
        skuVisibilityHeaders.add(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN);
        skuVisibilityHeaders.add(BulkCnCreationHeaderNames.BFB_STATUS_HEADER);
      } else {
        skuVisibilityHeaders.add(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_ID);
        skuVisibilityHeaders.add(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_ID);
        skuVisibilityHeaders.add(BulkCnCreationHeaderNames.BFB_STATUS_HEADER);
      }
    }
    if (merchantStatusType == MerchantStatusType.DELIVERY_AND_CNC.getType()
        || merchantStatusType == MerchantStatusType.BFB_AND_CNC.getType()) {
      skuVisibilityHeaders.add(ExcelHeaderNames.CNC);
    }
    if (instoreEligible) {
      skuVisibilityHeaders.add(ExcelHeaderNames.INSTORE_HEADER);
    }
    return skuVisibilityHeaders;
  }

  public Integer getMinimumPrice(String storeId) {
    SystemParameterConfig minimumPrice =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.MINIMUM_PRICE);
    if (Objects.nonNull(minimumPrice)) {
      return Integer.parseInt(minimumPrice.getValue());
    }
    return null;
  }

  private BulkProcessQueue createBulkProcessQueue(String storeId, String bulkProcessCode, String bulkProcessType,
      Map<String, String> args) {
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue();
    bulkProcessQueue.setStoreId(storeId);
    bulkProcessQueue.setBulkProcessCode(bulkProcessCode);
    bulkProcessQueue.setBulkProcessType(bulkProcessType);
    bulkProcessQueue.setArgs(args);
    return bulkProcessQueue;
  }

  public BulkProcess createBulkProcess(String storeId, String requestId, String bulkProcessCode,
      String bulkProcessType, String businessPartnerCode, String excelFilename, Map<String, String> args)
      throws JsonProcessingException {
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
    bulkProcess.setUploadedFile(excelFilename);
    if (bopisCncRestrictionEnabled) {
      Map<String, String> additionalInfo = new HashMap<>();
      additionalInfo.put(BulkParameters.IS_ONLY_EXTERNAL_USER,
          args.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(false)));
      bulkProcess.setPrimaryIdentifier(objectMapper.writeValueAsString(additionalInfo));
    }
    return bulkProcess;
  }

  private List<String> getValidExcelHeaders(boolean isInternationalMerchant) {
    List<String> validExcelHeaders = new ArrayList<String>();
    if (isInternationalMerchant) {
      validExcelHeaders.add(CnExcelHeaderNames.PRODUCT_NAME);
      validExcelHeaders.add(CnExcelHeaderNames.MODEL_EAN_UPC);
      validExcelHeaders.add(CnExcelHeaderNames.SELLER_SKU);
      validExcelHeaders.add(CnExcelHeaderNames.DESCRIPTION);
      validExcelHeaders.add(CnExcelHeaderNames.UNIQUE_SELLING_POINT);
      validExcelHeaders.add(CnExcelHeaderNames.BRAND);
    } else {
      validExcelHeaders.add(CnExcelHeaderNames.NAMA_PRODUCK);
      validExcelHeaders.add(CnExcelHeaderNames.MODEL_EAN_UPC);
      validExcelHeaders.add(CnExcelHeaderNames.SELLER_SKU);
      validExcelHeaders.add(CnExcelHeaderNames.DESKRIPSI);
      validExcelHeaders.add(CnExcelHeaderNames.KEUNGGULAN_PRODUK);
      validExcelHeaders.add(CnExcelHeaderNames.BRAND);
    }
    return validExcelHeaders;
  }
}