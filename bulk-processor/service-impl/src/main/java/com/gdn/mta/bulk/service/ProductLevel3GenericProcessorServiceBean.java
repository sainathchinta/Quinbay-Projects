package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.GenericTemplateDataReadDTO;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.notification.enumeration.NotificationType;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;

import static com.gdn.mta.bulk.BulkProcessValidationErrorMessages.HEADER_VALIDATION_ERROR;
import static com.gdn.partners.bulk.util.Constant.ACCESSIBLE_PICKUP_POINTS;

@Service("ProductLevel3GenericProcessorService")
public class ProductLevel3GenericProcessorServiceBean implements ProcessorService {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3GenericProcessorServiceBean.class);
  public static final String PROCESS_ABORTED = "Proses dibatalkan";
  public static final String EXCEL_FILE_MUST_NOT_BE_BLANK = "Excel File tidak boleh kosong";
  public static final String EXCEL_HEADER_INVALID =
      "Header File Excel tidak sesuai dengan template. Silakan periksa lagi dan upload";
  public static final String DESCRIPTION_VALIDATION_IN_PROGRESS = "proses validasi berlangsung";
  public static final String DESCRIPTION_SUCCESS = "Sukses: ";
  public static final String DESCRIPTION_UPLOADING_FAILED = "Proses upload gagal ";
  public static final String PERIOD = ". ";
  public static final String FEATURES = "features";
  public static final String VARIANT = "variant";
  public static final String IMAGES = "images";
  public static final String CATEGORY_CODE = "categoryCode";
  public static final String SPACE = " ";
  public static final String ROW = "Baris : ";
  public static final String DEFAULT = "DEFAULT";
  public static final String HYPHEN = "-";
  private static final String DOT_AND_SPACE = ". ";
  private static final String ACTIVE_MERCHANT = "ACTIVE";
  private static final String FILE_FROM_INACTIVE_SELLER = "File uploaded by inactive seller";
  public static final String EXCEL_FILE_MUST_NOT_BE_BLANK_EN = "Excel File must not be blank. Process Aborted";
  public static final String EXCEL_FILE_MUST_NOT_BE_BLANK_ID = "Excel File tidak boleh kosong. Proses dibatalkan";
  public static final String EXCEL_VERSION_OUTDATED_EN =
      "Please  make sure you download the latest template that available. Download newest version template";
  public static final String EXCEL_VERSION_OUTDATED_ID =
      "Pastikan Anda men-download template terbaru yang sudah tersedia. Download template versi terbaru";

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkGenericProcessorService bulkGenericProcessorService;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private NotificationService notificationService;

  @Value("${header.validation.check}")
  private boolean headerValidationCheck;

  @Value("${download.link}")
  private String downloadLink;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${generic.file.header.validation.en}")
  private boolean genericFileHeaderValidationEn;

  @Value("${bulk.generic.excel.version}")
  private String bulkGenericExcelVersion;

  @Value("${bulk.excel.versioning.switch.en}")
  private boolean bulkExcelVersioningEn;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${bulk.generic.instore.excel.version}")
  private String bulkGenericInstoreExcelVersion;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void preProcess(String storeId, String requestId, String bulkProcessType, String businessPartnerCode,
    Map<String, String> files, Map<String, String> args, String bulkProcessCode, String username) {
  }

  @Override
  public void process(BulkProcessQueue bulkProcessQueue) throws Exception {
    LOGGER.info("Start processing of Generic bulk : {}", bulkProcessQueue);
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    BulkProcess bulkProcess = getBulkProcess(bulkProcessQueue);
    String excelFileName = bulkProcessQueue.getArgs().get(Constant.EXCEL_FILE_NAME);
    String accessiblePickupPoints =
        Optional.ofNullable(bulkProcessQueue.getArgs().get(ACCESSIBLE_PICKUP_POINTS))
            .orElse(StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    int maxRowSize = 0;
    try {
      ProfileResponse businessPartner =
          this.businessPartnerRepository.filterByBusinessPartnerCodeV2(
              bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
      MerchantStatusType merchantStatusType = BulkCreationCommonUtil.getMerchantType(businessPartner);
      String merchantType =
          Optional.ofNullable(businessPartner.getCompany()).map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
      bulkProcess.setInternationalMerchant(businessPartner.getCompany().isInternationalFlag());
      bulkProcess.setNotes(Constant.GENERIC);
      bulkProcess = saveInProgressBulkProcess(bulkProcess, excelFileName);
      bulkProcess.setBulkProcessNotes(new ArrayList<>());
      bulkProcess.setUploadedFile(excelFileName);
      String excelFileType = Constant.DOT + getExcelFileType(excelFileName);
      byte[] fileByteData = fileStorageServiceBean.downloadFile(bulkProcess, excelFileType);
      LOGGER.info("Bulk process in progress, generic create for process id : {} ",
        bulkProcess.getBulkProcessCode());
      LOGGER.info("Start reading of file for bulk upload, code : {}", bulkProcess.getBulkProcessCode());
      GenericTemplateDataReadDTO genericTemplateDataReadDTO =
          RequestHelper.getGenericTemplateDataReadDTO(merchantStatusType, fileByteData, merchantType,
              bulkProcess);
      boolean instoreEligible = CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, businessPartner);
      genericTemplateDataReadDTO.setProductBundlingEligibleMerchantTypes(productBundlingEligibleMerchantTypes);
      genericTemplateDataReadDTO.setGenericFileHeaderValidationEn(genericFileHeaderValidationEn);
      genericTemplateDataReadDTO.setProductBundlingEnabled(productBundlingEnabled);
      genericTemplateDataReadDTO.setBulkGenericExcelVersion(bulkGenericExcelVersion);
      genericTemplateDataReadDTO.setBulkExcelVersioningEn(bulkExcelVersioningEn);
      genericTemplateDataReadDTO.setInstoreEligible(instoreEligible);
      genericTemplateDataReadDTO.setBulkGenericInstoreExcelVersion(bulkGenericInstoreExcelVersion);
      try {
        ExcelTemplateUtil.readGenericTemplateData(genericTemplateDataReadDTO);
      } catch (ApplicationRuntimeException exception) {
        LOGGER.error("Excel version outdated for file : {} , bulk process code : {} ", excelFileName,
            bulkProcess.getBulkProcessCode(), exception);
        String errorMessage = exception.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY);
        throw new ApplicationException(ErrorCategory.VALIDATION, errorMessage);
      } catch (Exception e) {
        LOGGER.error("Error occurred while reading the headers of Generic Excel File :{} bulk process code : {} ",
            excelFileName, bulkProcess.getBulkProcessCode(), e);
        bulkProcess.setDescription(
            excelFileName + DOT_AND_SPACE + ProductLevel3GenericProcessorServiceBean.EXCEL_FILE_MUST_NOT_BE_BLANK);
        throw new ApplicationException(ErrorCategory.INVALID_FORMAT, EXCEL_HEADER_INVALID);
      }
      LOGGER.debug("Complete reading of file for bulk upload, code : {}", bulkProcess.getBulkProcessCode());
      Runtime runtime = Runtime.getRuntime();
      long startMemory = runtime.totalMemory() - runtime.freeMemory();
      long endMemory = runtime.totalMemory() - runtime.freeMemory();
      LOGGER.debug("Upload sheet Memory in MB : {} , code :{}", ((endMemory - startMemory) / (1024 * 1024)),
          bulkProcess.getBulkProcessCode());
      List<List<Object>> userInputRows = genericTemplateDataReadDTO.getUserInputRows();
      if (CollectionUtils.isEmpty(userInputRows)) {
        bulkProcess.setDescription(
            excelFileName + ". " + ProductLevel3GenericProcessorServiceBean.EXCEL_FILE_MUST_NOT_BE_BLANK);
        LOGGER.error("Error no data found in excel File : {} for process id: {}", excelFileName,
            bulkProcess.getBulkProcessCode());
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
            EXCEL_FILE_MUST_NOT_BE_BLANK + " " + PROCESS_ABORTED);
      }
      CommonUtils.validateActiveBusinessPartner(businessPartner, bulkProcess, excelFileName);

      //if switch is true, will follow the new workflow, else existing workflow
      SystemParameterConfig bulkSwitchParameter =
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              SystemParameterConfigNames.BULK_SWITCH);
      boolean bulkSwitch = false;
      if (Objects.nonNull(bulkSwitchParameter)) {
        bulkSwitch = Boolean.valueOf(bulkSwitchParameter.getValue());
      }

      if (bulkSwitch) {
        bulkGenericProcessorService.generateBulkProcessDataAndImage(bulkProcess, userInputRows, merchantStatusType,
            merchantType, genericTemplateDataReadDTO.getExcelBahasaHeaderList(),
            genericTemplateDataReadDTO.getExcelEnglishHeaderList(), genericTemplateDataReadDTO.getFailedExcelRows(),
            accessiblePickupPoints, instoreEligible, bulkProcessQueue.getArgs());
      }
    } catch (Exception e) {
      String notes = e.getMessage();
      ProductLevel3GenericProcessorServiceBean.LOGGER.error(
          "error invoking process at service. Bulk Process Code : {}, Bulk Process Type : {}", bulkProcess.getBulkProcessCode(), bulkProcess.getBulkProcessType(), e);
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setBulkProcess(bulkProcess);
      bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      bulkProcess.setInputErrorCount(bulkUploadErrorCounter.getInputErrorCount());
      bulkProcess.setSystemErrorCount(bulkUploadErrorCounter.getSystemError());
      bulkProcess.setErrorCount(0);
      bulkProcess.setUploadedFile(excelFileName);
      if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY).contains(EXCEL_FILE_MUST_NOT_BE_BLANK)) {
        notes = BulkCreationCommonUtil.setNotesForEmptyFileUploaded(bulkProcess, excelFileName);
        notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
            fileStorageServiceBean.getDownloadLinkHtml(downloadLink));
      }
      if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
          .contains(ProductLevel3GenericProcessorServiceBean.EXCEL_VERSION_OUTDATED_EN) || Optional.ofNullable(
              e.getMessage()).orElse(StringUtils.EMPTY)
          .contains(ProductLevel3GenericProcessorServiceBean.EXCEL_VERSION_OUTDATED_ID)) {
        notes = e.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY);
        bulkProcess.setDescription(notes);
        notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
            fileStorageServiceBean.getDownloadLinkHtml(downloadLink));
      }
      bulkProcessNotes.setNotes(notes);
      bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      if (headerValidationCheck) {
        if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY).contains(EXCEL_HEADER_INVALID)) {
          bulkProcess.setDescription(HEADER_VALIDATION_ERROR);
          notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
              fileStorageServiceBean.getDownloadLinkHtml(downloadLink));
        } else if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY).contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) || Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
            .contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN)) {
          bulkProcess.setDescription(e.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY));
          notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), StringUtils.EMPTY);
        }
      }
      bulkProcessService.saveOperation(bulkProcess);
      this.trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
          StringUtils.SPACE, TrackerConstants.FAILED, bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  private String getExcelFileType(String excelFileName) {
    if ((ProcessorUtils.FILETYPE_XLSM_EXCEL).equalsIgnoreCase(excelFileName.substring(excelFileName.lastIndexOf(Constant.DOT)))) {
      return Constant.FILE_TYPE_XLSM;
    } else if ((ProcessorUtils.FILETYPE_XLSX_EXCEL).equalsIgnoreCase(excelFileName.substring(excelFileName.lastIndexOf(Constant.DOT)))) {
      return Constant.FILE_TYPE_XLSX;
    } else {
      return Constant.FILE_TYPE_XLS;
    }
  }

  private BulkProcess getBulkProcess(BulkProcessQueue bulkProcessQueue) throws Exception {
    BulkProcess bulkProcess = this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    if (bulkProcess == null) {
      LOGGER.error("Error at generic bulk process, data not found for : {}", bulkProcessQueue);
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at bulkProcess. Store Id : {" + bulkProcessQueue.getStoreId() + "}, Bulk Process Code : {" + bulkProcessQueue.getBulkProcessCode() + "}");
    }
    return bulkProcess;
  }

  private BulkProcess saveInProgressBulkProcess(BulkProcess bulkProcess, String excelFileName) {
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setDescription(
        excelFileName + DOT_AND_SPACE + ProductLevel3GenericProcessorServiceBean.DESCRIPTION_VALIDATION_IN_PROGRESS);
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setUploadedFile(excelFileName);
    return this.bulkProcessService.saveOperation(bulkProcess);
  }

}