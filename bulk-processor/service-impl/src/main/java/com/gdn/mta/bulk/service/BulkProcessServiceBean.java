package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.BulkProcessValidationErrorMessages.HEADER_VALIDATION_ERROR;
import static com.gdn.mta.bulk.dto.BulkProcessType.EXTERNAL_CREATION_UPLOAD;
import static com.gdn.mta.bulk.dto.BulkProcessType.QR_GENERATION;
import static com.gdn.mta.bulk.dto.BulkProcessType.getBulkProcessType;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_ABORTED;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_PROCESSING;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1;
import static com.gdn.mta.bulk.entity.BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2;
import static com.gdn.mta.bulk.models.EmailConstants.MAIL_SENDER;
import static com.gdn.mta.bulk.models.EmailConstants.PRODUCT_CENTER_DOWNLOAD_EN;
import static com.gdn.mta.bulk.models.EmailConstants.PRODUCT_CENTER_DOWNLOAD_ID;
import static com.gdn.mta.bulk.util.BulkCreationCommonUtil.ITEM_SKU_PATTERN;
import static com.gdn.mta.bulk.util.BulkUpdateServiceUtil.FILE_BLANK_ERROR;
import static com.gdn.partners.bulk.util.Constant.HEADER_ROW_ID;
import static com.gdn.partners.bulk.util.Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE;
import static com.gdn.partners.bulk.util.Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_1;
import static com.gdn.partners.bulk.util.Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_2;
import static com.gdn.partners.bulk.util.Constant.OFFSET;
import static com.gdn.partners.bulk.util.Constant.READ_START_FROM_INDEX;
import static com.gdn.partners.bulk.util.Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;

import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.time.DateUtils;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.product.BulkVideoDownloadRequestEventModel;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkPendingProductResponse;
import com.gdn.mta.bulk.dto.BulkPendingProductsDTO;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.QRCodeExcelQueue;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.RowNumberParentCodeDTO;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.Configuration;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.helper.CategoryExcelTemplateUtil;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.models.BulkDataDeletionModel;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.ChildSkuAndCogsMapping;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;
import com.gdn.mta.bulk.repository.BulkProcessCustomRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.QRCodeGenerationUtil;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.newrelic.api.agent.Trace;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

@Service
@Transactional(readOnly = true)
@Slf4j
public class BulkProcessServiceBean implements BulkProcessService {

  private static final Logger LOG = LoggerFactory.getLogger(BulkProcessServiceBean.class);
  private static final String CATEGORY_C1 = "C1";
  private static final String CATEGORY_CN = "Cn";
  private static final int CATEGORY_ATTRIBUTE_SHEET_ROW = 4;
  private static final int ATTRIBUTE_COLUMN1 = 13;
  private static final int ATTRIBUTE_COLUMN2 = 14;
  private static final int UKURAN_COLUMN1 = 20;
  private static final int UKURAN_COLUMN2 = 21;
  public static final String DESCRIPTION_PENDING = "menunggu untuk diproses";
  private static final String FILE_BYTE_DATA = "fileByteData";
  private static final String FILE_BYTE_DATA_EN = "fileByteDataEnglish";
  private static final String SKU_DELIMITER = "-";
  public static final List<String> BULK_PROCESS_COMPLETE =
      ImmutableList.<String>builder().add(BulkProcessData.STATUS_FAIL).add(BulkProcessData.STATUS_SUCCESS).build();
  private String FIELD_ITEM_SKU = "Blibli SKU";
  private static final String QR_CODE_CNC = "Click & Collect";
  private static final String FAILED_TO_FETCH_PRODUCT_DETAILS = "Failed to fetch product excel";
  private static final String FAILED_NO_PRODUCTS = "Failed as there are no records to process";

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Autowired
  private BulkProcessCustomRepository bulkProcessCustomRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ConfigurationService configurationService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private KafkaEventLogService kafkaEventLogService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private POIUtil poiUtil;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private BulkProcessImageService bulkProcessImageService;

  @Autowired
  private BulkProcessImageQCService bulkProcessImageQCService;

  @Autowired
  private BulkProcessVideoService bulkProcessVideoService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  @Lazy
  private PickupPointDeleteService pickupPointDeleteService;

  @Autowired
  private BulkProcessDataEstimationService bulkProcessDataEstimationService;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String PENDING = "PENDING";
  private static final String X_BULK = "x_bulk";
  private static final String ROOT = "/";

  @Value("${max.bulk.requests.per.merchant:1}")
  private int maxBulkRequests;

  @Value("${header.validation.check}")
  private boolean headerValidationCheck;

  @Value("#{${bundling.templates.download.link}}")
  private Map<String, String> bundlingTemplateDownloadLinks;

  @Value("${sysparam.directory.unified.base.file}")
  private String unifiedBaseTemplateFile;

  @Value("${blibli.mass.template.location}")
  private String blibliMassTemplateLocation;

  @Value("${sysparam.directory.unified.file}")
  private String unifiedUploadTemplateFile;

  @Value("${sysparam.directory.unified.file.en}")
  private String unifiedUploadTemplateFileEnglish;

  @Value("${sysparam.directory.template}")
  private String categoryTemplateLocation;

  @Value("${sysparam.directory.category.file}")
  private String categoryUploadTemplateFile;

  @Value("${sysparam.directory.category.file.en}")
  private String categoryUploadTemplateFileEnglish;

  @Value("${sysparam.directory.category.base.file}")
  private String categoryBaseTemplateFile;

  @Value("${sysparam.directory.category.base.file.en}")
  private String categoryBaseTemplateFileEnglish;

  @Value("${http.connection.timeout}")
  private int httpConnectionTimeout;

  @Value("${http.connection.readTimeout}")
  private int httpConnectionReadTimeout;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${max.fetch.days.listing}")
  private String  maxFetchDaysForListing;

  @Value("${estimation.fetch.listing.enabled}")
  private boolean estimationFetchEnabledForListing;

  @Value("${fetch.item.pickup.point.size}")
  private int fetchItemPickupPointSize;

  @Value("${pickup.point.name.delimiter}")
  private String wareHouseNameDelimeter;

  @Value("${restrict.duplicate.external.bulk.download.process}")
  private Set<String> restrictDuplicateExternalBulkDownloadProcess;

  @Value("${restrict.duplicate.internal.bulk.download.process}")
  private Set<String> restrictDuplicateInternalBulkDownloadProcess;

  @Value("${bulk.pending.external.download.status}")
  private Set<String> bulkPendingExternalDownloadStatus;

  @Value("${bulk.pending.internal.download.status}")
  private Set<String> bulkPendingInternalDownloadStatus;

  @Value("${supported.status.abort.job.main.table}")
  private String supportedStatusForAbortJobMainTable;

  @Value("${supported.status.abort.job.data.table}")
  private String supportedStatusForAbortJobDataTable;

  @Value("${validate.bulk.max.number.of.rows}")
  private boolean validateBulkMaxNumberOfRows;

  @Value("${bulk.max.number.of.rows}")
  private int bulkMaxNumberOfRows;

  @Value("${generic.template.file.types}")
  private String genericTemplateFileTypes;

  @Value("${abort.scheduler.process.status.list}")
  private String abortSchedulerProcessStatusList;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Value("${video.client.id}")
  private String videoClientId;

  @Value("${video.source}")
  private String videoSource;

  @Value("${brand.sheet.new.reading}")
  private boolean useNewBrandSheetReading;

  @Value("${suppress.error.for.total.count.listing.bulk.process.types}")
  private Set<String> suppressErrorForTotalCountListingBulkProcessTypes;

  @Value("${skip.override.parent.status.listing.bulk.process.types}")
  private Set<String> skipOverrideParentStatusBulkProcessTypes;

  @PostConstruct
  public void setUp() {
    RequestHelper.setFileStorageService(fileStorageService);
  }

  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public SystemParameterConfigService getSystemParameterConfigService() {
    return systemParameterConfigService;
  }

  public void setSystemParameterConfigService(SystemParameterConfigService systemParameterConfigService) {
    this.systemParameterConfigService = systemParameterConfigService;
  }

  public PCBOutboundService getPcbOutboundService() {
    return pcbOutboundService;
  }

  public void setPcbOutboundService(PCBOutboundService pcbOutboundService) {
    this.pcbOutboundService = pcbOutboundService;
  }

  public void setKafkaEventLogService(KafkaEventLogService kafkaEventLogService) {
    this.kafkaEventLogService = kafkaEventLogService;
  }

  public void setPbpOutboundService(PBPOutboundService pbpOutboundService) {
    this.pbpOutboundService = pbpOutboundService;
  }

  public void setXProductOutboundService(XProductOutboundService xProductOutboundService) {
    this.xProductOutboundService = xProductOutboundService;
  }

  public void setMailDeliveryService(MailDeliveryService mailDeliveryService) {
    this.mailDeliveryService = mailDeliveryService;
  }

  public void setPoiUtil(POIUtil poiUtil) {
    this.poiUtil = poiUtil;
  }

  public BulkProcessImageService getBulkProcessImageService() {
    return bulkProcessImageService;
  }

  public BulkProcessVideoService getBulkProcessVideoService() {
    return bulkProcessVideoService;
  }

  public void setBulkProcessImageService(BulkProcessImageService bulkProcessImageService) {
    this.bulkProcessImageService = bulkProcessImageService;
  }

  public BulkProcessDataService getBulkProcessDataService() {
    return bulkProcessDataService;
  }

  public void setBulkProcessDataService(BulkProcessDataService bulkProcessDataService) {
    this.bulkProcessDataService = bulkProcessDataService;
  }

  public void setBulkFailedProductFileService(BulkFailedProductFileService bulkFailedProductFileService) {
    this.bulkFailedProductFileService = bulkFailedProductFileService;
  }

  public void setBusinessPartnerRepository(BusinessPartnerRepository businessPartnerRepository) {
    this.businessPartnerRepository = businessPartnerRepository;
  }

  public void setBulkProcessCustomRepository(BulkProcessCustomRepository bulkProcessCustomRepository) {
    this.bulkProcessCustomRepository = bulkProcessCustomRepository;
  }

  public void setKafkaProducer(KafkaPublisher kafkaProducer) {
    this.kafkaProducer  = kafkaProducer;
  }

  public void setPickupPointDeleteService(PickupPointDeleteService pickupPointDeleteService) {
    this.pickupPointDeleteService = pickupPointDeleteService;
  }

  public void setFileStorageService(FileStorageService fileStorageService) {
    this.fileStorageService  = fileStorageService;
  }

  public void setBulkDownloadAuditRepository(BulkDownloadAuditRepository bulkDownloadAuditRepository) {
    this.bulkDownloadAuditRepository = bulkDownloadAuditRepository;
  }

  public void setBulkProcessDataEstimationService(BulkProcessDataEstimationService bulkProcessDataEstimationService) {
    this.bulkProcessDataEstimationService  = bulkProcessDataEstimationService;
  }

  public void setNotificationService(NotificationService notificationService) {
    this.notificationService = notificationService;
  }

  public void setKafkaTopicProperties(KafkaTopicProperties kafkaTopicProperties) {
    this.kafkaTopicProperties = kafkaTopicProperties;
  }

  public void setInternalProcessService(InternalProcessService internalProcessService) {
    this.internalProcessService = internalProcessService;
  }

  public void setBulkProcessVideoService(BulkProcessVideoService bulkProcessVideoService) {
    this.bulkProcessVideoService = bulkProcessVideoService;
  }

  public void setBulkProcessImageQCService( BulkProcessImageQCService bulkProcessImageQCService){
    this.bulkProcessImageQCService = bulkProcessImageQCService;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteByBulkProcessCode(String storeId, String bulkProcessCode) throws ApplicationException {
    BulkProcess bulkProcess =
        getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId, bulkProcessCode);
    if (bulkProcess == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at bulkProcess. Store Id : {" + storeId + "}, Bulk Process Code : {" + bulkProcessCode + "}");
    }
    bulkProcess.setMarkForDelete(true);
    for (BulkProcessNotes bulkProcessNotes : bulkProcess.getBulkProcessNotes()) {
      bulkProcessNotes.setMarkForDelete(true);
    }
    getBulkProcessRepository().save(bulkProcess);
  }

  @Override
  public BulkProcess findByBulkProcessCode(String storeId, String bulkProcessCode) {
    return getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId, bulkProcessCode);
  }

  @Override
  public BulkProcess findByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode, String status) {
    return getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId,
        bulkProcessCode, status);
  }

  @Override
  public List<BulkProcessNotesResponse> filterPromoBulkProcessNotes(String storeId, String bulkProcessCode)
      throws Exception {
    BulkProcess bulkProcess =
        getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId, bulkProcessCode);
    return filterBulkPromoNotes(bulkProcess);
  }

  @Override
  @Async
  @Trace(dispatcher = true)
  @Transactional(readOnly = false)
  public void regenerateCategoryAttributeMappingInGenericBulkTemplate() {
    boolean dataUpdated = false;
    List<KafkaEventLog> changeEvent = kafkaEventLogService.getKafkaEventLogs();
    List<GenericTemplateFileType> genericTemplateFileTypeList =
        Arrays.stream(genericTemplateFileTypes.split(Constants.COMMA)).map(String::trim)
            .map(GenericTemplateFileType::valueOf).collect(Collectors.toList());
    for (GenericTemplateFileType fileType : genericTemplateFileTypeList) {
      LOG.info("Category and Attribute scheduler started for type : {} !!", fileType);
      boolean categoryOrAttributeChanged =
          regenerateCategoryAttributeMappingInGenericBulkTemplateByFileType(fileType, changeEvent, false);
      dataUpdated = dataUpdated || categoryOrAttributeChanged;
    }
    if (dataUpdated) {
      systemParameterConfigService.update(Constant.STORE_ID, Constant.USER_NAME,
          getSystemParameterConfigRequestForCategoryAttributeRun());
      kafkaEventLogService.deleteKafkaEventLogsMarkForDelete(changeEvent);
    }
  }

  @Trace(dispatcher = true)
  @Async
  @Transactional(readOnly = false)
  private boolean regenerateCategoryAttributeMappingInGenericBulkTemplateByFileType(GenericTemplateFileType fileType,
      List<KafkaEventLog> changeEvent, boolean regenerateExplicitly) {
    try {
      String unifiedFileName = Arrays.asList(unifiedUploadTemplateFile.split(Constants.COMMA))
        .get(Integer.parseInt(GenericTemplateFileType.valueOf(fileType.name()).getValue()) - 1);
      boolean templateFileExist = fileStorageService.isMassTemplateFileExist(unifiedFileName);
      if (CommonUtils.skipCategoryRegeneration(templateFileExist, changeEvent, regenerateExplicitly)) {
        LOG.info("No category or attribute changed, Hence no regenerating category and attribute sheet for type : {} ",
            fileType.name());
        return false;
      }
      List<CategoryTreeResponse> genericTemplateCategories =
          pcbOutboundService.getGenericTemplateCategories(true, fileType.isIgnoreB2bExclusive());
      regenerateCategoryAttributeMappingGenericBulkTemplate(genericTemplateCategories, true, fileType.name());
      regenerateCategoryAttributeMappingGenericBulkTemplate(genericTemplateCategories, false, fileType.name());
      LOG.info("Category and Attribute scheduler ended for type : {} !!", fileType.name());
      return true;
    } catch (Exception e) {
      LOG.error("Exception caught while regenerating category and attribute sheet for type : {} ", fileType.name(), e);
      return false;
    }
  }

  private void regenerateCategoryAttributeMappingGenericBulkTemplate(
      List<CategoryTreeResponse> genericTemplateCategories, boolean isEnglishTemplate, String genericTemplateFileType) throws Exception {
    List<String> warnaEligibleCnList = new ArrayList<>();
    List<String> descriptiveAttributeEligibleCnList = new ArrayList<>();
    Map<String, List<String>> cnToAttributeNameMapping = new HashMap<>();
    Map<String, List<String>> ukuranValuesMappingWithCN = new HashMap<>();
    Map<String, AttributeResponse> attributeResponseMap = new HashMap<>();
    List<String> variasiEligibleCnList = new ArrayList<>();

    List<String> c1CategoryName = genericTemplateCategories.stream()
        .map(categoryTreeResponse -> getCategoryName(categoryTreeResponse, isEnglishTemplate))
        .collect(Collectors.toList());

    Map<String, List<CategoryTreeResponse>> categoryC2Mapping = genericTemplateCategories.stream()
        .filter(categoryTreeResponse -> CollectionUtils.isNotEmpty(categoryTreeResponse.getChildren())).collect(
            Collectors.toMap(categoryTreeResponse -> getCategoryName(categoryTreeResponse, isEnglishTemplate),
                CategoryTreeResponse::getChildren, (v1, v2) -> v2));

    Collections.sort(c1CategoryName, String.CASE_INSENSITIVE_ORDER);

    List<String> cnList = getCategoryCn(genericTemplateCategories, isEnglishTemplate);
    Map<String, List<String>> categoryC2ToCnMap = getCategoryC2ToCnMapping(cnList);


    Map<String, List<?>> data = new HashMap<>();
    data.put(CATEGORY_C1, c1CategoryName);
    data.put(CATEGORY_CN, cnList);
    String unifiedUploadTemplateFileName;
    if (isEnglishTemplate) {
      unifiedUploadTemplateFileName =
        Arrays.asList(unifiedUploadTemplateFileEnglish.split(Constants.COMMA)).get(
          Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    } else {
      unifiedUploadTemplateFileName =
        Arrays.asList(unifiedUploadTemplateFile.split(Constants.COMMA)).get(
          Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    }
    String unifiedBaseTemplateFileName = Arrays.asList(unifiedBaseTemplateFile.split(Constants.COMMA))
      .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    XSSFWorkbook workbook = fileStorageService.getUnifiedBaseTemplate(unifiedBaseTemplateFileName,
      unifiedUploadTemplateFileName,
      isEnglishTemplate);
    ExcelTemplateUtil.regenerateCategorySheet(data, categoryC2Mapping, categoryC2ToCnMap, workbook, isEnglishTemplate);
    regenerateCategoryAttributeMappings(workbook, genericTemplateCategories, warnaEligibleCnList,
        descriptiveAttributeEligibleCnList, cnToAttributeNameMapping, ukuranValuesMappingWithCN, attributeResponseMap,
        variasiEligibleCnList, isEnglishTemplate);

    fileStorageService.writeGenericTemplate(unifiedUploadTemplateFileName, workbook);
  }

  private Map<String, List<String>> getCategoryC2ToCnMapping(List<String> cnList) {
    Map<String, List<String>> mapC2ToCn = new HashMap<>();
    for (String category : cnList) {
      int first = category.indexOf(Constant.ARROW_DELIMITER);
      int second = category.indexOf(Constant.ARROW_DELIMITER, first + 1);
      if (first > 0 && second > 0) {
        String c2 = category.substring(0, second);
        String cn = category.substring(second + 2);
        if (mapC2ToCn.containsKey(c2)) {
          mapC2ToCn.get(c2).add(cn);
        } else {
          List<String> categoryList = new ArrayList<>();
          categoryList.add(cn);
          mapC2ToCn.put(c2, categoryList);
        }
      } else if (first > 0) {
        List<String> categoryList = new ArrayList<>();
        mapC2ToCn.put(category, categoryList);
      }
    }
    return mapC2ToCn;
  }

  private void regenerateCategoryAttributeMappings(XSSFWorkbook workbook,
      List<CategoryTreeResponse> genericTemplateCategories, List<String> warnaEligibleCnList,
      List<String> descriptiveAttributeEligibleCnList, Map<String, List<String>> cnToAttributeNameMapping,
      Map<String, List<String>> ukuranValuesMappingWithCN, Map<String, AttributeResponse> attributeResponseMap,
      List<String> variasiEligibleCnList, boolean isEnglishTemplate) {
    XSSFSheet sheet = workbook.getSheet(Constant.CATEGORY_ATTRIBUTE_SHEET);
    int start_row_index = CATEGORY_ATTRIBUTE_SHEET_ROW;
    Map<String, List<?>> data = new HashMap<>();
    StringBuilder categoryNameMapping = new StringBuilder();
    for (CategoryTreeResponse node : genericTemplateCategories) {
      categoryNameMapping.setLength(0);
      categoryNameMapping.append(getCategoryName(node, isEnglishTemplate));
      start_row_index = generateC1ToCnMappingsWithAttributeValues(node, categoryNameMapping, workbook, start_row_index,
          warnaEligibleCnList, descriptiveAttributeEligibleCnList, cnToAttributeNameMapping, ukuranValuesMappingWithCN,
          attributeResponseMap, variasiEligibleCnList, isEnglishTemplate);
    }
    data.put(BulkParameters.WARNA_CN_CATEGORY_MAPPING,
        getCategoryMappedWithNotApplicableLeafCategoryList(warnaEligibleCnList));
    data.put(BulkParameters.DESCRIPTIVE_ATTR_CN_CATEGORY_MAPPING,
        getCategoryMappedWithNotApplicableLeafCategoryList(descriptiveAttributeEligibleCnList));
    data.put(BulkParameters.VARIASI_CN_CATEGORY_MAPPING,
        getCategoryMappedWithNotApplicableLeafCategoryList(variasiEligibleCnList));
    if (isEnglishTemplate) {
      data.put(BulkParameters.SHIPPING_TYPE_LIST, BulkParameters.BULK_OPTION_UPLOAD_SUPPORT_EN);
    } else {
      data.put(BulkParameters.SHIPPING_TYPE_LIST, BulkParameters.BULK_OPTION_UPLOAD_SUPPORT);
    }
    ExcelTemplateUtil.setProductUnifiedBaseTemplateCategoryC1Values(data, sheet);
    ExcelTemplateUtil.setProductUnifiedBaseTemplateTwoColumnValues(
        getCategoryMappedWithNotApplicableLeafCategoryMap(cnToAttributeNameMapping), sheet,
        CATEGORY_ATTRIBUTE_SHEET_ROW, ATTRIBUTE_COLUMN1, ATTRIBUTE_COLUMN2);
    ExcelTemplateUtil.setProductUnifiedBaseTemplateTwoColumnValues(
        getCategoryMappedWithNotApplicableLeafCategoryMap(ukuranValuesMappingWithCN), sheet,
        CATEGORY_ATTRIBUTE_SHEET_ROW, UKURAN_COLUMN1, UKURAN_COLUMN2);
  }

  private int generateC1ToCnMappingsWithAttributeValues(CategoryTreeResponse categoryTreeResponse,
      StringBuilder categoryNameMapping, XSSFWorkbook workbook, int start_row_index, List<String> warnaEligibleCnList,
      List<String> descriptiveAttributeEligibleCnList, Map<String, List<String>> cnToAttributeNameMapping,
      Map<String, List<String>> ukuranValuesMappingWithCN, Map<String, AttributeResponse> attributeResponseMap,
      List<String> variasiEligibleCnList, boolean isEnglishTemplate) {
    if (Objects.isNull(categoryTreeResponse.getChildren())) {
      List<CategoryAttributeResponse> categoryAttributeResponseList =
          getCategoryAttributeResponsesAndGenerateCnAttributeMappings(categoryTreeResponse,
              categoryNameMapping.toString(), warnaEligibleCnList, descriptiveAttributeEligibleCnList,
              cnToAttributeNameMapping, ukuranValuesMappingWithCN, attributeResponseMap, variasiEligibleCnList,
              isEnglishTemplate);
      for (CategoryAttributeResponse categoryAttributeResponse : categoryAttributeResponseList) {
        start_row_index =
            generateCnAndPredefinedAttributeMappingWithValues(categoryNameMapping, workbook, start_row_index,
                attributeResponseMap, categoryAttributeResponse, isEnglishTemplate);
      }
      return start_row_index;
    }

    if (CollectionUtils.isNotEmpty(categoryTreeResponse.getChildren())) {
      for (CategoryTreeResponse childCategory : categoryTreeResponse.getChildren()) {
        categoryNameMapping.append(Constant.ARROW_DELIMITER).append(getCategoryName(childCategory, isEnglishTemplate));
        start_row_index =
            generateC1ToCnMappingsWithAttributeValues(childCategory, categoryNameMapping, workbook, start_row_index,
                warnaEligibleCnList, descriptiveAttributeEligibleCnList, cnToAttributeNameMapping,
                ukuranValuesMappingWithCN, attributeResponseMap, variasiEligibleCnList, isEnglishTemplate);
        int index = categoryNameMapping.lastIndexOf(Constant.ARROW_DELIMITER);
        categoryNameMapping.replace(index, categoryNameMapping.length(), StringUtils.EMPTY);
      }
    }
    return start_row_index;
  }

  private int generateCnAndPredefinedAttributeMappingWithValues(StringBuilder categoryNameMapping,
      XSSFWorkbook workbook, int start_row_index, Map<String, AttributeResponse> attributeResponseMap,
      CategoryAttributeResponse categoryAttributeResponse, boolean isEnglishTemplate) {
    if (checkPredefinedAttributeCriteria(categoryAttributeResponse)) {
      AttributeResponse attributeResponse =
          getAttributeResponse(categoryAttributeResponse.getAttribute().getId(), attributeResponseMap);
      List<String> predefinedValues =
          Optional.ofNullable(attributeResponse.getPredefinedAllowedAttributeValues()).orElseGet(ArrayList::new)
              .stream().map(PredefinedAllowedAttributeValueResponse::getValue).collect(Collectors.toList());
      start_row_index = ExcelTemplateUtil
          .regenerateCategoryAttributeList(workbook, appendCategoryAsNA(categoryNameMapping.toString()),
              getAttributeName(attributeResponse, isEnglishTemplate), predefinedValues, start_row_index);
    }
    return start_row_index;
  }

  private List<CategoryAttributeResponse> getCategoryAttributeResponsesAndGenerateCnAttributeMappings(
      CategoryTreeResponse categoryTreeResponse, String categoryNameMapping, List<String> warnaEligibleCnList,
      List<String> descriptiveAttributeEligibleCnList, Map<String, List<String>> cnToAttributeNameMapping,
      Map<String, List<String>> ukuranValuesMappingWithCN, Map<String, AttributeResponse> attributeResponseMap,
      List<String> variasiEligibleCnList, boolean isEnglishTemplate) {
    CategoryDetailResponse categoryDetailResponse =
        pcbOutboundService.getCategoryDetailResponse(categoryTreeResponse.getCategoryCode());
    List<CategoryAttributeResponse> categoryAttributeResponseList = categoryDetailResponse.getCategoryAttributes();
    CommonUtils.filterHideFromSellerAttributes(productSuitabilityFeatureEnabled, categoryDetailResponse);
    generateDescriptiveAttributeEligibleCnList(categoryNameMapping, categoryAttributeResponseList,
        descriptiveAttributeEligibleCnList);
    generateWarnaEligibleCnList(categoryNameMapping, categoryAttributeResponseList, warnaEligibleCnList);
    generateCnToAttributeMapping(categoryNameMapping, categoryAttributeResponseList, cnToAttributeNameMapping,
        isEnglishTemplate);
    generateUkuranValueMappingWithCn(categoryNameMapping, categoryAttributeResponseList, ukuranValuesMappingWithCN,
        attributeResponseMap);
    generateVariasiEligibleCnList(categoryNameMapping, categoryAttributeResponseList, variasiEligibleCnList);
    return categoryAttributeResponseList;
  }

  private void generateUkuranValueMappingWithCn(String categoryNameMapping,
      List<CategoryAttributeResponse> categoryAttributeResponseList,
      Map<String, List<String>> ukuranValuesMappingWithCN, Map<String, AttributeResponse> attributeResponseMap) {
    Optional<CategoryAttributeResponse> categoryAttributeResponse = categoryAttributeResponseList.stream()
        .filter(categoryAttribute -> BulkParameters.UKURAN.equalsIgnoreCase(categoryAttribute.getAttribute().getName()))
        .filter(categoryAttribute -> AttributeType.DEFINING_ATTRIBUTE.name()
            .equalsIgnoreCase(categoryAttribute.getAttribute().getAttributeType())).findFirst();
    if (categoryAttributeResponse.isPresent()) {
      AttributeResponse attributeResponse =
          getAttributeResponse(categoryAttributeResponse.get().getAttribute().getId(), attributeResponseMap);
      List<String> allowedValues =
          attributeResponse.getAllowedAttributeValues().stream().map(AllowedAttributeValueResponse::getValue)
              .collect(Collectors.toList());
      ukuranValuesMappingWithCN.put(categoryNameMapping, allowedValues);
    }
  }

  private void generateDescriptiveAttributeEligibleCnList(String categoryNameMapping,
      List<CategoryAttributeResponse> categoryAttributeResponseList, List<String> descriptiveAttributeEligibleCnList) {
    boolean isDescriptiveAttributeEligibleCn = categoryAttributeResponseList.stream().anyMatch(
        categoryAttributeResponse -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equalsIgnoreCase(categoryAttributeResponse.getAttribute().getAttributeType()) && !categoryAttributeResponse
            .getAttribute().isVariantCreation());
    if (isDescriptiveAttributeEligibleCn) {
      descriptiveAttributeEligibleCnList.add(categoryNameMapping);
    }
  }

  private void generateVariasiEligibleCnList(String categoryNameMapping,
      List<CategoryAttributeResponse> categoryAttributeResponseList, List<String> variasiEligibleCnList) {
    boolean isEligibleCnForVariasi = categoryAttributeResponseList.stream().anyMatch(categoryAttributeResponse ->
        BulkParameters.VARIASI.equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName())
            && AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equalsIgnoreCase(categoryAttributeResponse.getAttribute().getAttributeType()) && categoryAttributeResponse
            .getAttribute().isVariantCreation());
    if (isEligibleCnForVariasi) {
      variasiEligibleCnList.add(categoryNameMapping);
    }
  }

  private void generateWarnaEligibleCnList(String categoryNameMapping,
      List<CategoryAttributeResponse> categoryAttributeResponseList, List<String> warnaEligibleCnList) {
    boolean isEligibleCnForWarna = categoryAttributeResponseList.stream().anyMatch(
        categoryAttributeResponse -> BulkParameters.WARNA
            .equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName()));
    if (isEligibleCnForWarna) {
      warnaEligibleCnList.add(categoryNameMapping);
    }
  }

  private void generateCnToAttributeMapping(String categoryNameMapping,
      List<CategoryAttributeResponse> categoryAttributeResponseList, Map<String, List<String>> cnToAttributeNameMapping,
      boolean isEnglishTemplate) {
    List<String> attributeNameCnMapping = categoryAttributeResponseList.stream()
        .filter(categoryAttributeResponse -> checkAttributeCriteria(categoryAttributeResponse))
        .map(categoryAttributeResponse -> getAttributeName(categoryAttributeResponse.getAttribute(), isEnglishTemplate))
        .collect(Collectors.toList());
    cnToAttributeNameMapping.put(categoryNameMapping, attributeNameCnMapping);
  }

  private AttributeResponse getAttributeResponse(String attributeId,
      Map<String, AttributeResponse> attributeResponseMap) {
    AttributeResponse attributeResponse;
    if (Objects.nonNull(attributeResponseMap.get(attributeId))) {
      return attributeResponseMap.get(attributeId);
    } else {
      attributeResponse = pcbOutboundService.getAttributeDetail(attributeId);
      attributeResponseMap.put(attributeResponse.getId(), attributeResponse);
    }
    return attributeResponse;
  }

  private boolean checkAttributeCriteria(CategoryAttributeResponse categoryAttributeResponse) {
    return !BulkParameters.BRAND.equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName())
        && !BulkParameters.FAMILY_COLOUR.equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName())
        && !categoryAttributeResponse.getAttribute().isVariantCreation();
  }

  private boolean checkPredefinedAttributeCriteria(CategoryAttributeResponse categoryAttributeResponse) {
    return !BulkParameters.BRAND.equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName())
        && !BulkParameters.FAMILY_COLOUR.equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName())
        && AttributeType.PREDEFINED_ATTRIBUTE.name()
        .equalsIgnoreCase(categoryAttributeResponse.getAttribute().getAttributeType()) && !categoryAttributeResponse
        .getAttribute().isVariantCreation();
  }

  private List<String> getCategoryCn(List<CategoryTreeResponse> genericTemplateCategories, boolean isEnglishTemplate) {
    Map<String, List<String>> c1ToCnMap = new HashMap<>();
    List<String> categoryList;
    for (CategoryTreeResponse genericTemplateCategoryResponse : genericTemplateCategories) {
      String categoryCn = StringUtils.EMPTY;
      List<String> cnList = new ArrayList<>();
      getAllCategoryCn(genericTemplateCategoryResponse.getChildren(), categoryCn, cnList, isEnglishTemplate);
      c1ToCnMap.put(getCategoryName(genericTemplateCategoryResponse, isEnglishTemplate), cnList);
    }
    categoryList = c1ToCnMap.entrySet().stream()
        .flatMap(categoryC1 -> categoryC1.getValue().stream().map(categoryCn -> categoryC1.getKey() + categoryCn))
        .collect(Collectors.toList());
    return categoryList;
  }

  private void getAllCategoryCn(List<CategoryTreeResponse> children, String categoryCn, List<String> cnList,
      boolean isEnglishTemplate) {
    if (CollectionUtils.isEmpty(children)) {
      cnList.add(categoryCn);
      return;
    }
    for (CategoryTreeResponse categoryTreeResponse : children) {
      categoryCn = new StringBuilder(categoryCn).append(Constant.ARROW_DELIMITER)
          .append(getCategoryName(categoryTreeResponse, isEnglishTemplate)).toString();
      getAllCategoryCn(categoryTreeResponse.getChildren(), categoryCn, cnList, isEnglishTemplate);
      int index = categoryCn.lastIndexOf(Constant.ARROW_DELIMITER);
      categoryCn = categoryCn.substring(0, index);
    }
  }

  private String getCategoryName(CategoryTreeResponse category, boolean isEnglishTemplate) {
    if (isEnglishTemplate && StringUtils.isNotBlank(category.getCategoryEnglishName())) {
      return category.getCategoryEnglishName();
    } else {
      return category.getCategoryName();
    }
  }

  private String getAttributeName(AttributeResponse attributeResponse, boolean isEnglishTemplate) {
    if (isEnglishTemplate && StringUtils.isNotBlank(attributeResponse.getNameEnglish())) {
      return attributeResponse.getNameEnglish();
    } else {
      return attributeResponse.getName();
    }
  }

  private SystemParameterConfigRequest getSystemParameterConfigRequestForCategoryAttributeRun() {
    return SystemParameterConfigRequest.builder().value(String.valueOf(new Date()))
        .variable(SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP).build();
  }

  private List<String> getCategoryMappedWithNotApplicableLeafCategoryList(List<String> categoryList) {
    return categoryList.stream().map(category -> appendCategoryAsNA(category)).collect(Collectors.toList());
  }

  private Map<String, List<String>> getCategoryMappedWithNotApplicableLeafCategoryMap(
      Map<String, List<String>> categoryMap) {
    Map<String, List<String>> category = new HashMap<>();
    for (Map.Entry<String, List<String>> entry : categoryMap.entrySet()) {
      String key = entry.getKey();
      key = appendCategoryAsNA(key);
      category.put(key, entry.getValue());
    }
    return category;
  }

  private String appendCategoryAsNA(String category) {
    if (StringUtils.countMatches(category, Constant.ARROW_DELIMITER) == 1) {
      category = category.concat(Constant.ARROW_DELIMITER + GenericBulkParameters.NOT_APPLICABLE);
    }
    return category;
  }

  @Override
  public WholeSaleCountResponse filterWholeSaleConfigBulkProcessNotes(String storeId, String bulkProcessCode)
      throws Exception {
    BulkProcess bulkProcess =
        getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId, bulkProcessCode);
    BulkProcessNotes bulkProcessNote =
        bulkProcess.getBulkProcessNotes().stream().filter(BulkProcessNotes::isWholeSaleConfig).findFirst().get();
    WholeSaleCountResponse wholeSaleCountResponse =
        new ObjectMapper().readValue(bulkProcessNote.getNotes(), WholeSaleCountResponse.class);
    return wholeSaleCountResponse;
  }

  private List<BulkProcessNotesResponse> filterBulkPromoNotes(BulkProcess bulkProcess) {
    List<BulkProcessNotesResponse> bulkProcessNotesResponseList = new ArrayList<>();
    for (BulkProcessNotes bulkProcessNotes : bulkProcess.getBulkProcessNotes()) {
      if (bulkProcessNotes.isPromoNote()) {
        BulkProcessNotesResponse bulkProcessNotesResponse = new BulkProcessNotesResponse();
        BeanUtils.copyProperties(bulkProcessNotes, bulkProcessNotesResponse);
        bulkProcessNotesResponseList.add(bulkProcessNotesResponse);
      }
    }
    return bulkProcessNotesResponseList;
  }

  @Override
  public Page<BulkProcess> findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(String storeId,
      String businessPartnerCode, String bulkProcessType, Pageable pageable) {
    return getBulkProcessRepository()
        .getBulkProcessNotification(storeId, businessPartnerCode, bulkProcessType, pageable);
  }

  @Transactional
  public void updateBulkProcessRecordAsMarkForDeleteTrue(String date) {
    getBulkProcessRepository().updateBulkProcessRecordAsMarkForDeleteTrue(date);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public BulkProcess saveOperation(BulkProcess bulkProcess) {
    return this.bulkProcessRepository.save(bulkProcess);
  }

  @Override
  @Transactional(readOnly = false)
  public BulkProcess saveBulkProcess(BulkProcess bulkProcess) {
    return this.bulkProcessRepository.save(bulkProcess);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public List<BulkProcess> saveBulkProcessList(List<BulkProcess> bulkProcessList) {
    return this.bulkProcessRepository.saveAll(bulkProcessList);
  }

  @Override
  public Page<BulkProcess> findByBusinessPartnerCodeAndCreatedBy(String storeId, String businessPartnerCode,
      String createdBy, Pageable pageable) {
    return getBulkProcessRepository()
        .findByStoreIdAndBusinessPartnerCodeAndCreatedByAndMarkForDeleteFalseOrderByStartDateDesc(storeId,
            businessPartnerCode, createdBy, pageable);
  }

  public BulkProcessRepository getBulkProcessRepository() {
    return bulkProcessRepository;
  }

  public void setBulkProcessRepository(BulkProcessRepository bulkProcessRepository) {
    this.bulkProcessRepository = bulkProcessRepository;
  }

  @Override
  public BulkPendingRequestsResponse checkForPendingBulkProcess(String storeId, String username, String type,
      String businessPartnerCode, String systemParameterConfigName, String bulkProcessType) {
    if (Constant.BULK_UPLOAD_TYPE.equals(type)) {
      return checkForPendingBulkUploadProcess(storeId, businessPartnerCode, systemParameterConfigName, bulkProcessType,
          type, username);
    } else if (Constant.BULK_DOWNLOAD_TYPE.equals(type)) {
      return checkForPendingBulkDownloadProcess(storeId, username, businessPartnerCode, bulkProcessType);
    } else if (Constant.BULK_INTERNAL_UPLOAD.equals(type)) {
      return checkForPendingBulkUploadProcess(storeId, businessPartnerCode, systemParameterConfigName, bulkProcessType,
          type, username);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          BulkProcessValidationErrorMessages.INVALID_BULK_PROCESS);
    }
  }

  private BulkPendingRequestsResponse checkForPendingBulkUploadProcess(String storeId, String businessPartnerCode,
      String systemParameterConfigName, String bulkProcessType, String type, String username) {
    SystemParameterConfig systemParameterConfig =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, systemParameterConfigName);
    long pendingRequestsCount;
    if (StringUtils.isNotBlank(bulkProcessType)) {
      if (Constant.BULK_INTERNAL_UPLOAD.equals(type)) {
        pendingRequestsCount = getPendingRequestsCountForInternalUploads(storeId, bulkProcessType, username);
      } else {
        pendingRequestsCount = countUnfinishedProcess(storeId, businessPartnerCode, bulkProcessType);
      }
    } else {
      pendingRequestsCount =
          getPendingRequestsCountByUserNameOrBpCode(storeId, businessPartnerCode, bulkProcessType, type, username);
    }
    BulkPendingRequestsResponse pendingRequestsResponse = new BulkPendingRequestsResponse();
    if (pendingRequestsCount >= maxBulkRequests || Boolean.valueOf(systemParameterConfig.getValue())) {
      pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    } else {
      pendingRequestsResponse.setBulkUpdateStatusFlag(true);
    }
    pendingRequestsResponse.setPendingRequestsCount(pendingRequestsCount);
    return pendingRequestsResponse;
  }

  private long getPendingRequestsCountForInternalUploads(String storeId, String bulkProcessType, String username) {
    return internalProcessService.countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(storeId,
        bulkProcessType,
        Arrays.asList(ProcessStatus.PICKED.name(), ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name()),
        username);
  }

  private long getPendingRequestsCountByUserNameOrBpCode(String storeId, String businessPartnerCode, String bulkProcessType, String type,
      String username) {
    long pendingRequestsCount;
    if (Constant.BULK_INTERNAL_UPLOAD.equals(type)) {
      pendingRequestsCount =
          internalProcessService.countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(storeId,
              bulkProcessType, Arrays.asList(ProcessStatus.PICKED.name(), ProcessStatus.PENDING.name(),
                  ProcessStatus.IN_PROGRESS.name()), username);
    } else {
      pendingRequestsCount =
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndMarkForDeleteFalse(
          storeId, businessPartnerCode,
          Arrays.asList(PENDING, BulkProcess.STATUS_IN_PROGRESS, STATUS_IMAGE_PROCESSING,
            STATUS_IMAGE_PROCESSING_PRIORITY_1, STATUS_IMAGE_PROCESSING_PRIORITY_2,
            BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_PUBLISHED,
            BulkProcess.STATUS_PROCESSED, STATUS_IMAGE_AND_VIDEO_PROCESSING,
            STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1,
            STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2));
    }
    return pendingRequestsCount;
  }


  private BulkPendingRequestsResponse checkForPendingBulkDownloadProcess(String storeId, String username,
      String businessPartnerCode, String bulkProcessType) {
    BulkPendingRequestsResponse bulkPendingRequestsResponse = new BulkPendingRequestsResponse();
    if (Optional.ofNullable(restrictDuplicateExternalBulkDownloadProcess).orElse(Collections.EMPTY_SET)
        .contains(bulkProcessType)) {
      BulkDownloadEntity pendingBulkDownload =
          bulkDownloadAuditRepository.findFirstByEntityTypeAndBusinessPartnerCodeAndStatusIn(bulkProcessType,
              businessPartnerCode,
              Optional.ofNullable(bulkPendingExternalDownloadStatus).orElse(Collections.EMPTY_SET));
      if (Objects.nonNull(pendingBulkDownload)) {
        bulkPendingRequestsResponse.setPendingRequestsCount(Constant.LONG_ONE);
      }
    } else if (Optional.ofNullable(restrictDuplicateInternalBulkDownloadProcess).orElse(Collections.EMPTY_SET)
        .contains(bulkProcessType)) {
      BulkDownloadEntity pendingBulkDownload =
          bulkDownloadAuditRepository.findFirstByEntityTypeAndCreatedByAndStatusIn(bulkProcessType, username,
              Optional.ofNullable(bulkPendingInternalDownloadStatus).orElse(Collections.EMPTY_SET));
      if (Objects.nonNull(pendingBulkDownload)) {
        bulkPendingRequestsResponse.setPendingRequestsCount(Constant.LONG_ONE);
      }
    }
    return bulkPendingRequestsResponse;
  }

  private long countUnfinishedProcess(String storeId, String businessPartnerCode,
      String bulkProcessType) {
    return this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
        storeId, businessPartnerCode,
        Arrays.asList(PENDING, BulkProcess.STATUS_IN_PROGRESS, STATUS_IMAGE_PROCESSING,
            STATUS_IMAGE_PROCESSING_PRIORITY_1, STATUS_IMAGE_PROCESSING_PRIORITY_2,
            BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_PUBLISHED,
          BulkProcess.STATUS_PROCESSED, STATUS_IMAGE_AND_VIDEO_PROCESSING,
          STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
        BulkCreationCommonUtil.getBulkProcessTypesForCreation(bulkProcessType));
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void findAndDeleteBulkProcessCodesBeforeXDays(String storeId) {
    Pageable pageable = PageRequest.of(0, 10);
    Page<Configuration> configuration = getConfigurationService().findByServiceKey(X_BULK, pageable);
    for (Configuration bulkProcessConfiguration : configuration.getContent()) {
      LOG.debug("Retrieved config table");
      int days = bulkProcessConfiguration.getAgeOfDeletion();
      boolean isDisabled = bulkProcessConfiguration.isDisabled();
      if (!isDisabled) {
        String folder = bulkProcessConfiguration.getFolderName();
        LOG.debug("Deleting files from following folder : {}", folder);
        if (StringUtils.isEmpty(folder)) {
          this.deleteFile(ProcessorUtils.DATA_BASE_DIR, days, true);
        } else {
          this.deleteFile(ProcessorUtils.DATA_BASE_DIR + folder, days, false);
        }
      }
    }
  }

  @Override
  @Transactional
  public void abortPendingBulkProcessBefore(String storeId) {
    int abortPendingBulkProcessLimit = Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT)
        .getValue());
    Date pendingToAbortDate = ProcessorUtils.getEarlierDateBySeconds(abortPendingBulkProcessLimit);
    LOG.info("Aborting all pending tasks for storeId {} before {}", storeId, pendingToAbortDate);
    List<String> statusList = Arrays.stream(abortSchedulerProcessStatusList.split(Constants.COMMA)).map(String::trim)
        .collect(Collectors.toList());
    bulkProcessRepository.updateStatusInPendingOrInProgressBulkProcessToAborted(statusList ,pendingToAbortDate);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void abortPendingInprogressBulkProcess(String storeId, String bulkProcessCode) throws Exception {
    BulkProcess bulkProcess = findByBulkProcessCode(storeId, bulkProcessCode);
    if (Objects.nonNull(bulkProcess) && (BulkProcess.STATUS_PENDING.equals(bulkProcess.getStatus())
        || BulkProcess.STATUS_IN_PROGRESS.equals(bulkProcess.getStatus()))) {
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      this.bulkProcessRepository.save(bulkProcess);
    } else {
      throw new ApplicationException(ErrorCategory.INVALID_STATE);
    }
  }

  @Override
  public BulkProcess abortBulkProcess(String storeId, String bulkProcessCode) throws Exception {
    BulkProcess bulkProcess = findByBulkProcessCode(storeId, bulkProcessCode);
    if (Objects.nonNull(bulkProcess) && (BulkProcess.STATUS_PENDING.equals(bulkProcess.getStatus())
        || BulkProcess.STATUS_IN_PROGRESS.equals(bulkProcess.getStatus()))) {
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      bulkProcess.setEndDate(new Date());
      saveOperation(bulkProcess);
    }
    return bulkProcess;
  }

  @Override
  public BulkProcess setErrorCountAndTotalCountAndSave(BulkProcess bulkProcess, int errorCount,
      int totalCount) {
    bulkProcess.setTotalCount(errorCount);
    bulkProcess.setErrorCount(totalCount);
    bulkProcess.setStatus(STATUS_ABORTED);
    bulkProcess.setEndDate(new Date());
    return saveOperation(bulkProcess);
  }

  @Override
  @Async
  @Trace(dispatcher = true)
  @Transactional(readOnly = false)
  public void regenerateBrandValuesInGenericBulkTemplate(String genericTemplateFileType) {
    LOG.info("Brand scheduler started!!");
    try {
      List<CategoryTreeResponse> genericTemplateCategories =
        pcbOutboundService.getGenericTemplateCategories(true, true);
      List<CategoryTreeResponse> categoryTreeResponses = genericTemplateCategories.stream().filter(
          genericTemplateCategoryResponse -> StringUtils.isEmpty(genericTemplateCategoryResponse.getParentCategory()))
          .collect(Collectors.toList());
      Map<String, List<String>> pbpBrandResponse = new HashMap<>();
      Map<String, List<String>> pbpBrandResponseEn = new HashMap<>();
      for (CategoryTreeResponse categoryTreeResponse : categoryTreeResponses) {
        List<PredefinedAllowedAttributeValueResponse> activeBrandsByCategoryIds =
            pbpOutboundService.getActiveBrandsByCategoryId(categoryTreeResponse.getId());
        pbpBrandResponse.put(categoryTreeResponse.getCategoryName(),
            activeBrandsByCategoryIds.stream().map(PredefinedAllowedAttributeValueResponse::getValue)
                .collect(Collectors.toList()));
        pbpBrandResponseEn.put(categoryTreeResponse.getCategoryEnglishName(),
            activeBrandsByCategoryIds.stream().map(PredefinedAllowedAttributeValueResponse::getValue)
                .collect(Collectors.toList()));
      }
      List<BrandWipResponse> brandWipResponses =
          pcbOutboundService.getInReviewBrands(new BrandWipSummaryRequest(null, BrandApprovalStatus.DRAFT.name()));
      List<String> inReviewBrands =
          brandWipResponses.stream().map(BrandWipResponse::getBrandName).collect(Collectors.toList());
      for (Map.Entry<String, List<String>> pbpBrandEntry : pbpBrandResponse.entrySet()) {
        pbpBrandResponse.get(pbpBrandEntry.getKey()).removeAll(inReviewBrands);
      }
      for (Map.Entry<String, List<String>> pbpBrandEntryEn : pbpBrandResponseEn.entrySet()) {
        pbpBrandResponseEn.get(pbpBrandEntryEn.getKey()).removeAll(inReviewBrands);
      }
      Map<String, byte[]> masterBrandValuesSheet =
        fileStorageService.regenerateMasterBrandValuesSheet(genericTemplateFileType);
      byte[] brandValuesSheet = ExcelTemplateUtil.regenerateBrandValuesSheet(pbpBrandResponse,
        masterBrandValuesSheet.get(FILE_BYTE_DATA));
      byte[] brandValuesSheetEnglish =
        ExcelTemplateUtil.regenerateBrandValuesSheet(pbpBrandResponseEn,
        masterBrandValuesSheet.get(FILE_BYTE_DATA_EN));
      fileStorageService.uploadRegeneratedTemplates(brandValuesSheet, brandValuesSheetEnglish,
        genericTemplateFileType, false);
      SystemParameterConfigRequest systemParameterConfigRequest = new SystemParameterConfigRequest();
      systemParameterConfigRequest.setValue(String.valueOf(new Date()));
      systemParameterConfigRequest.setVariable(SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
      systemParameterConfigService.update(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
          MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), systemParameterConfigRequest);
      LOG.info("Brand scheduler ended!!");
    } catch (Exception e) {
      LOG.error("Exception caught while regenerating brand values!!!", e);
    }
  }

  private void deleteFile(String path, int days, boolean isRoot) {
    File parentFolder = new File(path);
    File[] childFolders = parentFolder.listFiles();
    if (Objects.nonNull(childFolders)) {
      for (File child : childFolders) {
        LOG.debug("Checking for file:{}", child.getName());
        if (((!(child.getPath() + ROOT).equals(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR) && !(child.getPath()
            + ROOT).equals(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_DOWNLOAD_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DOWNLOAD_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_DOWNLOAD_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_DOWNLOAD_DIR_FOR_MASTER_PRODUCT) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_FAILED_PRODUCT_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_ORDER_DOWNLOAD_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_PRODUCT_REVIEW_DOWNLOAD) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_PRODUCT_VENDOR_DIR) && !(child.getPath() + ROOT)
            .equals(ProcessorUtils.BULK_UPDATE_DIR) && isRoot) || !isRoot) && (child.lastModified() <= Instant.now()
            .minus(Duration.ofDays(days)).toEpochMilli())) {
          LOG.info("Deleting file : {}", child.getName());
          if (child.isDirectory()) {
            try {
              FileUtils.deleteDirectory(child);
            } catch (IOException e) {
              LOG.error("error while deleting file {}: ", child.getName(), e);
            }
          } else if (child.isFile()) {
            child.delete();
          }
        }
      }
    }
  }

  @Override
  @Async
  @Trace(dispatcher = true)
  @Transactional(readOnly = false)
  public void regenerateMasterBrandValuesInGenericBulkTemplate(String genericTemplateFileType) {
    LOG.info("Master Brand scheduler started!!");
    try {
      List<AttributeResponse> brandNameAttributeList =
          pcbOutboundService.getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND);
      List<String> allActiveBrand = getPredefinedAttributeActiveBrand(brandNameAttributeList);
      Map<String, byte[]> byteDataMap =
        fileStorageService.regenerateMasterBrandValuesSheet(genericTemplateFileType);
      byte[] idTemplate = ExcelTemplateUtil.regenerateMasterBrandValuesSheet(allActiveBrand,
        byteDataMap.get(FILE_BYTE_DATA), useNewBrandSheetReading);
      byte[] engTemplate = ExcelTemplateUtil.regenerateMasterBrandValuesSheet(allActiveBrand,
        byteDataMap.get(FILE_BYTE_DATA_EN), useNewBrandSheetReading);
      fileStorageService.uploadGenericTemplateToGcs(idTemplate, engTemplate, genericTemplateFileType);
      log.info("#regenerateMasterBrandValuesInGenericBulkTemplate was Called from GCS !!!");
      SystemParameterConfigRequest systemParameterConfigRequest = new SystemParameterConfigRequest();
      systemParameterConfigRequest.setValue(String.valueOf(new Date()));
      systemParameterConfigRequest.setVariable(SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
      systemParameterConfigService.update(Constant.STORE_ID, Constant.USER_NAME, systemParameterConfigRequest);
      LOG.info("Master Brand scheduler ended!!");
    } catch (Exception e) {
      LOG.error("Exception caught while regenerating Master brand values!!!", e);
    }
  }

  @Override
  public void downloadUnmappedProductSkus(String storeId, String username, String emailTo, String requestId,
      String parentCategoryCode, String language) throws Exception {
    // call pcb to fetch category based on C1 category code
    CategoryTreeResponse categoryTreeResponse = pcbOutboundService.getCategoryTree(parentCategoryCode);
    List<String> childCategoryCodes = new ArrayList<>();
    getChildCategoryCodes(categoryTreeResponse, childCategoryCodes);
    Map<String, List<String>> categoryNameHirearchy = new HashMap<>();
    for (String categoryCode : childCategoryCodes) {
      List<String> categoryNames = new ArrayList<>();
      hasCategoryHirearchy(categoryTreeResponse, categoryCode, categoryNames);
      categoryNameHirearchy.put(categoryCode, categoryNames);
    }

    // call x-product to fetch unmapped product skus based on Cn Category Code
    Integer fetchBatchSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE).getValue());
    List<List<String>> childCategoryCodesLists = Lists.partition(childCategoryCodes, fetchBatchSize);
    List<UnmappedSkuResponse> unmappedSkuResponsesList = new ArrayList<>();
    for (List<String> childCategoryCodesList : childCategoryCodesLists) {
      List<UnmappedSkuResponse> unmappedSkuResponses = xProductOutboundService.getUnmappedSkus(childCategoryCodesList);
      unmappedSkuResponsesList.addAll(unmappedSkuResponses);
    }

    //create new excel sheet
    if (CollectionUtils.isEmpty(unmappedSkuResponsesList)) {
      Workbook workbook = poiUtil
          .createUnamappedProductSkusSheetWithoutData(username, requestId, categoryTreeResponse.getCategoryName());
      fileStorageService.uploadUnmappedSku(workbook, requestId);
    } else {
      int maxCount =
          categoryNameHirearchy.keySet().stream().mapToInt(key -> categoryNameHirearchy.get(key).size()).max()
              .getAsInt();
      Workbook workbook = poiUtil
          .createUnmappedSkusSheetWithData(username, requestId, categoryTreeResponse.getCategoryName(), maxCount,
              categoryNameHirearchy, unmappedSkuResponsesList);
      fileStorageService.uploadUnmappedSku(workbook, requestId);
    }
    sendDownloadUnmappedSkusEmail(username, emailTo, requestId, categoryTreeResponse.getCategoryName(), language);
  }

  @Override
  public long countNumberOfUploadsByUser(String storeId, String bulkProcessType, String createdBy, String status) {
    return bulkProcessRepository
        .countByStoreIdAndBulkProcessTypeAndCreatedByAndStatusAndMarkForDeleteFalse(storeId, bulkProcessType, createdBy,
            status);
  }

  // send email
  private void sendDownloadUnmappedSkusEmail(String userName, String emailTo, String requestId, String categoryName,
      String language) throws Exception {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, userName);
    emailParameters.put(EmailConstants.REQ_ID, requestId);
    emailParameters.put(EmailConstants.CATEGORY_NAME, categoryName);
    emailParameters.put(EmailConstants.FILE_PREFIX,
        fileStorageService.getFilePrefix(BulkProcessType.UNCATEGORISED_SKU.getValue()));
    BulkDownloadMailRecipient bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
    bulkDownloadMailRecipient.setEmailTo(emailTo);
    String PRODUCT_CENTER_DOWNLOAD;
    if (Constant.BAHASA_CODE.equals(language)) {
      PRODUCT_CENTER_DOWNLOAD = PRODUCT_CENTER_DOWNLOAD_ID;
    } else {
      PRODUCT_CENTER_DOWNLOAD = PRODUCT_CENTER_DOWNLOAD_EN;
    }
    mailDeliveryService.sendBulkDownloadEmail(PRODUCT_CENTER_DOWNLOAD, MAIL_SENDER, "Produk Download", emailParameters,
        Constant.DEFAULT_USERNAME, userName, bulkDownloadMailRecipient);
  }

  //save the file
  private void saveDownloadUnmappedSkuSheet(Workbook workbook, String requestId) throws IOException {
    String directory = ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT);
    ProcessorUtils.createDirectories(directory + requestId);
    ProcessorUtils
        .createXLSXFile(directory + requestId + File.separator + requestId + ProcessorUtils.FILETYPE_XLSX_EXCEL,
            workbook);
  }

  // get category hirearchy
  private boolean hasCategoryHirearchy(CategoryTreeResponse categoryTreeResponse, String categoryCode,
      List<String> categoryNames) {
    if (CollectionUtils.isEmpty(categoryTreeResponse.getChildren()) && categoryCode
        .equals(categoryTreeResponse.getCategoryCode())) {
      categoryNames.add(categoryTreeResponse.getCategoryName());
      return true;
    } else if (CollectionUtils.isNotEmpty(categoryTreeResponse.getChildren())) {
      for (CategoryTreeResponse categoryTree : categoryTreeResponse.getChildren()) {
        if (hasCategoryHirearchy(categoryTree, categoryCode, categoryNames)) {
          categoryNames.add(categoryTreeResponse.getCategoryName());
          return true;
        }
      }
    }
    return false;
  }

  // get Cn catgeory codes from catgeoryTreeResponse
  private void getChildCategoryCodes(CategoryTreeResponse categoryTreeResponse, List<String> childCategoryCodes) {
    if (CollectionUtils.isEmpty(categoryTreeResponse.getChildren())) {
      childCategoryCodes.add(categoryTreeResponse.getCategoryCode());
    } else {
      categoryTreeResponse.getChildren()
          .forEach(categoryTree -> getChildCategoryCodes(categoryTree, childCategoryCodes));
    }
  }

  @Override
  @Async
  @Trace(dispatcher = true)
  @Transactional(readOnly = false)
  public void regenerateBrandValuesInCategoryTemplate() {
    LOG.info("Category template Brand scheduler started!!");
    try {
      List<AttributeResponse> brandNameAttributeList =
          pcbOutboundService.getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND);
      List<String> allActiveBrand = getPredefinedAttributeActiveBrand(brandNameAttributeList);
      // checks if File exists else with create a new one
      Map<String, byte[]> map =
        fileStorageService.isCategoryTemplateFileExist(categoryUploadTemplateFile,
          categoryUploadTemplateFileEnglish, categoryBaseTemplateFile,
          categoryBaseTemplateFileEnglish);
      byte[] brandValuesSheet =
        CategoryExcelTemplateUtil.regenerateCategoryTemplateBrandValuesSheet(allActiveBrand,
          map.get(Constant.CATEGORY_UPLOAD_TEMPLATE));
      byte[] brandValuesSheetEn =
        CategoryExcelTemplateUtil.regenerateCategoryTemplateBrandValuesSheet(allActiveBrand,
          map.get(Constant.CATEGORY_UPLOAD_TEMPLATE_EN));
      fileStorageService.uploadRegeneratedTemplates(brandValuesSheet,brandValuesSheetEn,
        null,true);
      SystemParameterConfigRequest systemParameterConfigRequest = new SystemParameterConfigRequest();
      systemParameterConfigRequest.setValue(String.valueOf(new Date()));
      systemParameterConfigRequest.setVariable(SystemParameterConfigNames.CATEGORY_BRAND_SCHEDULER_RUN_TIME);
      systemParameterConfigService.update(Constant.STORE_ID, Constant.USER_NAME, systemParameterConfigRequest);
      LOG.info("Category template scheduler ended!!");
    } catch (Exception e) {
      LOG.error("Exception caught while regenerating Master brand values!!!", e);
    }
  }

  private List<String> getPredefinedAttributeActiveBrand(List<AttributeResponse> brandNameAttributeList) {
    List<String> finalActiveBrands = new ArrayList<>();
    List<String> finalBrandWithDefaultOnTop = new ArrayList<>(BulkParameters.DEFAULT_BRANDS);
    for (AttributeResponse attributeResponse : brandNameAttributeList) {
      if (attributeResponse.getAttributeType().equals(AttributeType.PREDEFINED_ATTRIBUTE.name())
          && Constant.ATTRIBUTE_NAME_BRAND.equals(attributeResponse.getName()) && !attributeResponse
          .isMarkForDelete()) {
        AttributeResponse attributeDetail = pcbOutboundService.getAttributeDetail(attributeResponse.getId());
        List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponseList =
            attributeDetail.getPredefinedAllowedAttributeValues();
        List<String> allBrands =
            predefinedAllowedAttributeValueResponseList.stream().map(PredefinedAllowedAttributeValueResponse::getValue)
                .collect(Collectors.toList());
        List<BrandWipResponse> brandWipResponses =
            pcbOutboundService.getInReviewBrands(new BrandWipSummaryRequest(null, BrandApprovalStatus.DRAFT.name()));
        List<String> inReviewBrands =
            brandWipResponses.stream().map(BrandWipResponse::getBrandName).collect(Collectors.toList());
        finalActiveBrands.addAll(allBrands);
        finalActiveBrands.removeAll(inReviewBrands);
      }
    }
    Collections.sort(finalActiveBrands, String.CASE_INSENSITIVE_ORDER);
    finalActiveBrands.removeAll(BulkParameters.DEFAULT_BRANDS);
    finalBrandWithDefaultOnTop.addAll(finalActiveBrands);
    return finalBrandWithDefaultOnTop;
  }

  private String getTimeLapsed(Date startDate) {
    if (Objects.nonNull(startDate)) {
      Date now = new Date();
      long timeDifference = (now.getTime() - startDate.getTime());
      if (timeDifference > 0) {
        long timeDiffDays = (timeDifference / (1000 * 60 * 60 * 24));
        long timeDiffHours = (timeDifference / (1000 * 60 * 60)) % 24;
        long timeDiffMinutes = (timeDifference / (1000 * 60)) % 60;
        String timeLapsed = StringUtils.EMPTY;
        timeLapsed = timeLapsed + timeDiffDays + " days  ";
        timeLapsed = timeLapsed + timeDiffHours + " hrs  ";
        timeLapsed = timeLapsed + timeDiffMinutes + " min";
        return timeLapsed;
      } else {
        return StringUtils.EMPTY;
      }
    }
    return StringUtils.EMPTY;
  }

  @Override
  public void sendMailIfPendingRequestMoreThanThreshold(String storeId) throws Exception {
    SystemParameterConfig thresholdPendingBulkRequests = systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    SystemParameterConfig mailTo =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    SystemParameterConfig thresholdAbortedBulkRequests = systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    SystemParameterConfig abortedRequestsRequiredBasedOnTime = systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    int thresholdPendingBulkRequestsValue;
    int thresholdAbortedBulkRequestsValue;
    Date abortedRequestsBasedOnDate;
    try {
      thresholdPendingBulkRequestsValue = Integer.parseInt(thresholdPendingBulkRequests.getValue());
      thresholdAbortedBulkRequestsValue = Integer.parseInt(thresholdAbortedBulkRequests.getValue());
      int abortedRequestsRequiredTimeInMinutes = Integer.parseInt(abortedRequestsRequiredBasedOnTime.getValue());
      abortedRequestsBasedOnDate =
          new Date(System.currentTimeMillis() - (abortedRequestsRequiredTimeInMinutes * 60 * 1000));
    } catch (Exception exp) {
      LOG.info("Error in system parameter value, Email failed to send ", exp);
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT);
    }
    List<BulkPendingProductsDTO> bulkPendingProductsDTOList = this.bulkProcessRepository.getBulkPendingRequest(storeId);
    List<BulkPendingProductsDTO> bulkInProgressRequestList =
        this.bulkProcessRepository.getBulkInProgressRequest(storeId);
    List<BulkPendingProductsDTO> bulkAbortedProductsDTOList =
        this.bulkProcessRepository.getBulkAbortedRequest(storeId, abortedRequestsBasedOnDate);
    List<BulkPendingProductResponse> bulkPendingProductResponseList = new ArrayList<>();
    List<BulkPendingProductResponse> bulkInProgressProductResponseList = new ArrayList<>();
    List<BulkPendingProductResponse> bulkAbortedProductResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(bulkPendingProductsDTOList)) {
      bulkPendingProductResponseList = bulkPendingProductsDTOList.stream().map(
          p -> new BulkPendingProductResponse(p.getId(), p.getBusinessPartnerCode(), p.getBulkProcessType(),
              p.getCreatedBy(), p.getBulkProcessCode(), p.getStartDateString().split(Constant.DOT_SPLIT)[0],
              getTimeLapsed(p.getStartDate()), p.getBulkUpdateString())).collect(Collectors.toList());
    }
    if (CollectionUtils.isNotEmpty(bulkInProgressRequestList)) {
      bulkInProgressProductResponseList = bulkInProgressRequestList.stream().map(
          p -> new BulkPendingProductResponse(p.getId(), p.getBusinessPartnerCode(), p.getBulkProcessType(),
              p.getCreatedBy(), p.getBulkProcessCode(), p.getStartDateString().split(Constant.DOT_SPLIT)[0],
              getTimeLapsed(p.getStartDate()), p.getBulkUpdateString())).collect(Collectors.toList());
    }
    if (CollectionUtils.isNotEmpty(bulkAbortedProductsDTOList)) {
      bulkAbortedProductResponseList = bulkAbortedProductsDTOList.stream().map(
          p -> new BulkPendingProductResponse(p.getId(), p.getBusinessPartnerCode(), p.getBulkProcessType(),
              p.getCreatedBy(), p.getBulkProcessCode(), p.getStartDateString().split(Constant.DOT_SPLIT)[0],
              getTimeLapsed(p.getStartDate()), p.getBulkUpdateString())).collect(Collectors.toList());
    }
    String mailSendTo = mailTo.getValue();
    if ((bulkPendingProductResponseList.size() + bulkInProgressProductResponseList.size())
        > thresholdPendingBulkRequestsValue
        || bulkAbortedProductResponseList.size() > thresholdAbortedBulkRequestsValue) {
      mailDeliveryService
          .sendPendingBulkRequestMail(mailSendTo, bulkPendingProductResponseList, bulkInProgressProductResponseList,
              bulkAbortedProductResponseList, abortedRequestsRequiredBasedOnTime.getValue());
    }
  }

  @Override
  public List<String> bulkUpdateOff2On(Map<String, Boolean> stringBooleanMap, String requestId, String username) {
    boolean updateOff2OnHistory = Boolean.TRUE;
    SimpleListStringResponse simpleListStringResponses = xProductOutboundService
        .bulkUpdateOff2OnByProductSkus(stringBooleanMap, username, requestId, updateOff2OnHistory);
    if (Objects.nonNull(simpleListStringResponses)) {
      return simpleListStringResponses.getValue();
    }
    return new ArrayList<>();
  }

  @Override
  @Transactional(readOnly = false)
  public void preProcessSubjectToVatUploadEvent(String storeId, String requestId, String userName,
      String businessPartnerCode, String bulkProcessCode, String filePath) throws Exception {
    BulkProcess bulkProcess =
        createBulkProcess(storeId, requestId, bulkProcessCode, BulkProcessType.SUBJECT_TO_VAT.getValue(),
            businessPartnerCode, filePath);
    bulkProcessRepository.saveAndFlush(bulkProcess);
    BulkUpdateQueue bulkUpdateQueue =
        getBulkUpdateQueueForVatUpdate(storeId, requestId, userName, businessPartnerCode, filePath, bulkProcessCode);
    kafkaProducer.send(kafkaTopicProperties.getSubjectToVatUploadEvent(), bulkUpdateQueue);
  }

  @Override
  public void publishBulkProductCreationEvent(String storeId, BulkProcess bulkProcess,
    String bulkProcessType) throws Exception {
    List<String> parentProducts =
        bulkProcessDataService.getDistinctParentProduct(storeId, bulkProcess.getBulkProcessCode());
    for (String parentProduct : parentProducts) {
      BulkCreateProductEventModel createProductEventModel =
          BulkCreateProductEventModel.builder().storeId(storeId).bulkProcessCode(bulkProcess.getBulkProcessCode())
              .parentProduct(parentProduct).businessPartnerCode(bulkProcess.getBusinessPartnerCode()).build();
      BulkProcessType processType = BulkProcessType.getBulkProcessType(bulkProcessType);
      if (Objects.isNull(processType)) {
        throw new IllegalArgumentException("Invalid bulkProcessType: " + bulkProcessType);
      }
      String event = getEventByBulkProcessType(bulkProcess, processType);
      LOG.info("Publish event : {}, blpCode : {}, parent :{} ", event, bulkProcess.getBulkProcessCode(), parentProduct);
      kafkaProducer.send(event, createProductEventModel);
    }
  }

  private String getEventByBulkProcessType(BulkProcess bulkProcess, BulkProcessType processType) {
    return switch (processType) {
      case CONVERTED_PRODUCT_CREATION_UPLOAD ->
        kafkaTopicProperties.getBulkGenericCreateProductForConvertedUpload();
      case EXTERNAL_CREATION_UPLOAD ->
        kafkaTopicProperties.getBulkGenericCreateProductForExternalUpload();
      default -> Constant.GENERIC.equals(bulkProcess.getNotes()) ?
        getEventByBulkProcessTypeForGeneric(processType.getValue()) :
        getEventByBulkProcessTypeForCN(processType.getValue());
    };
  }

  private String getEventByBulkProcessTypeForGeneric(String bulkProcessType) {
    Map<BulkProcessType, String> eventMapping = new HashMap<>();
    eventMapping.put(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1,
        kafkaTopicProperties.getBulkGenericCreateProductForPriority1Event());
    eventMapping.put(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2,
        kafkaTopicProperties.getBulkGenericCreateProductForPriority2Event());

    return eventMapping.getOrDefault(getBulkProcessType(bulkProcessType),
        kafkaTopicProperties.getBulkGenericCreateProductEvent());
  }

  private String getEventByBulkProcessTypeForCN(String bulkProcessType) {
    Map<BulkProcessType, String> eventMapping = new HashMap<>();
    eventMapping.put(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1,
      kafkaTopicProperties.getBulkCnCreateProductForPriority1Event());
    eventMapping.put(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2,
        kafkaTopicProperties.getBulkCnCreateProductForPriority2Event());

    return eventMapping.getOrDefault(getBulkProcessType(bulkProcessType),
      kafkaTopicProperties.getBulkCnCreateProductEvent());
  }

  private String getEventByBulkProcessTypeForUpdate(String bulkProcessType) {
    Map<BulkProcessType, String> eventMapping = new HashMap<>();
    eventMapping.put(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1,
        kafkaTopicProperties.getBulkUploadUpdateItemPriority1());
    eventMapping.put(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2,
        kafkaTopicProperties.getBulkUploadUpdateItemPriority2());

    return eventMapping.getOrDefault(getBulkProcessType(bulkProcessType),
        kafkaTopicProperties.getBulkUploadUpdateItem());
  }

  @Override
  public void publishBulkUpdateEvent(String storeId, BulkProcess bulkProcess) throws Exception {
    processToPublishBulkUpdate(storeId, bulkProcess,
        getEventByBulkProcessTypeForUpdate(bulkProcess.getBulkProcessType()),
        SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
  }

  @Override
  public void publishBulkUpdateEANEvent(String storeId, BulkProcess bulkProcess) throws Exception {
    processToPublishBulkUpdate(storeId,
        bulkProcess,
        kafkaTopicProperties.getBulkUploadUpdateEANItem(),
        SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
  }

  @Override
  public void publishBasicInfoUpdateEvent(String storeId, BulkProcess bulkProcess) {
    processToPublish(storeId, bulkProcess,
        getEventByBulkProcessTypeForBasicInfoUpdate(bulkProcess.getBulkProcessType()),
        SystemParameterConfigNames.PROCESS_DATA_BASIC_INFO_UPDATE_BATCH_SIZE);
  }

  @Override
  public void publishBulkExternalImageQCDownloadEventModel(BulkProcess bulkProcess,
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap) {
    log.info("Publish event = {}, blpCode : {}",
      kafkaTopicProperties.getExternalUploadCategoryAndBrandPrediction(),
      bulkProcess.getBulkProcessCode());
    productIdToImageQCModelMap.forEach((productId, request) -> kafkaProducer.send(
      kafkaTopicProperties.getExternalUploadCategoryAndBrandPrediction(), productId, request));
  }

  @Override
  public BulkProcess findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(String storeId,
    String bulkProcessCode, String status) {
    return bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
      storeId, bulkProcessCode, status);
  }

  private String getEventByBulkProcessTypeForBasicInfoUpdate(String bulkProcessType) {
    Map<BulkProcessType, String> eventMapping = new HashMap<>();
    eventMapping.put(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1,
        kafkaTopicProperties.getBulkBasicInfoUpdatePriority1Event());
    eventMapping.put(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2,
        kafkaTopicProperties.getBulkBasicInfoUpdatePriority2Event());
    return eventMapping.getOrDefault(getBulkProcessType(bulkProcessType),
        kafkaTopicProperties.getBulkBasicInfoUpdateEvent());
  }

  @Override
  public void publishBulkDeleteItemPickupPointEvent(String storeId, BulkProcess bulkProcess) throws Exception {
    processToPublishDeleteItemPickupPoint(storeId, bulkProcess, kafkaTopicProperties.getBulkDeleteItemPickupPoint());
  }

  @Override
  public void publishCampaignUploadEvent(String storeId, BulkProcess bulkProcess) {
    processToPublish(storeId, bulkProcess, kafkaTopicProperties.getBulkUploadCampaignItem(),
        SystemParameterConfigNames.PROCESS_DATA_CAMPAIGN_BATCH_SIZE);
  }

  @Override
  public void publishInstoreUpdateEvent(String storeId, BulkProcess bulkProcess) {
    processToPublish(storeId, bulkProcess, kafkaTopicProperties.getBulkUploadInstoreUpdate(),
        SystemParameterConfigNames.PROCESS_DATA_INSTORE_BATCH_SIZE);
  }

  private void processToPublishBulkUpdate(String storeId, BulkProcess bulkProcess, String event,
      String batchSizeVariable) {
    List<RowNumberParentCodeDTO> rowNumbersAndData =
        bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(storeId,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_PENDING);
    Map<String, Map<String, List<Integer>>> productSkuToItemSkuToRowNumbersList = new HashMap<>();
    batchRequestsByKeyValueAndReturnDomainKey(rowNumbersAndData,
        BulkParameters.BLIBLI_SKU, BulkParameters.BLIBLI_PRODUCT_SKU, productSkuToItemSkuToRowNumbersList);
    log.info("Publish event = {}, blpCode : {}", event, bulkProcess.getBulkProcessCode());
    int batchSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, batchSizeVariable).getValue());
    for (Map.Entry<String, Map<String, List<Integer>>> productSkuToItemSkuToRowNumbers : productSkuToItemSkuToRowNumbersList
        .entrySet()) {
      for (Map.Entry<String, List<Integer>> entry : productSkuToItemSkuToRowNumbers.getValue().entrySet()) {
        List<Integer> rowNumbers = entry.getValue();
        List<List<Integer>> subRows = Lists.partition(rowNumbers, batchSize);
        for (List<Integer> batch : subRows) {
          publishEventForBulkProcessDataUpdate(storeId, bulkProcess, event, batch,
              productSkuToItemSkuToRowNumbers.getKey());
        }
      }
    }
  }

  private void processToPublishDeleteItemPickupPoint(String storeId, BulkProcess bulkProcess, String event) {
    List<RowNumberParentCodeDTO> rowNumbersAndData =
        bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(storeId,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_PENDING);
    Map<String, List<Integer>> productSkuToRowNumbersListMap = new HashMap<>();
    batchRowsByKey(productSkuToRowNumbersListMap, rowNumbersAndData, BulkParameters.PRODUCT_SKU_REQUEST);
    log.info("Publish event = {}, blpCode : {}", event, bulkProcess.getBulkProcessCode());
    for (Map.Entry<String, List<Integer>> entry : productSkuToRowNumbersListMap.entrySet()) {
      publishEventForBulkProcessDataUpdate(storeId, bulkProcess, event, entry.getValue(), entry.getKey());
    }
  }

  private void processToPublish(String storeId, BulkProcess bulkProcess, String event, String batchSizeVariable) {
    List<Integer> rowNumbers = bulkProcessDataService.findRowNumberByStoreIdAndBulkProcessCodeAndStatus(storeId,
        bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_PENDING);
    log.info("Publish event = {}, blpCode : {}", event, bulkProcess.getBulkProcessCode());
    int batchSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, batchSizeVariable).getValue());
    List<List<Integer>> subRows = Lists.partition(rowNumbers, batchSize);
    for (List<Integer> batch : subRows) {
      publishEventForBulkProcessData(storeId, bulkProcess, event, batch);
    }
  }

  private void batchRequestsByKeyValueAndReturnDomainKey(List<RowNumberParentCodeDTO> rowNumberParentCodeDTOList,
      String key, String domainKeyFieldName,
      Map<String, Map<String, List<Integer>>> productSkuToItemSkuToRowNumbersList) {
    String domainKey = StringUtils.EMPTY;
    for (RowNumberParentCodeDTO rowNumberParentCodeDTO : rowNumberParentCodeDTOList) {
      String keyName = StringUtils.EMPTY;
      try {
        LinkedHashMap<String, String> rowDataJson =
            objectMapper.readValue(rowNumberParentCodeDTO.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
            });
        keyName = rowDataJson.get(key);
        domainKey = rowDataJson.get(domainKeyFieldName);
      } catch (Exception e) {
        log.error("Error when trying to extract keyName for data : {} ", rowNumberParentCodeDTO.getBulkRequestData(),
            e);
      }
      if (productSkuToItemSkuToRowNumbersList.containsKey(domainKey)) {
        Map<String, List<Integer>> itemSkuToRowNumberList = productSkuToItemSkuToRowNumbersList.get(domainKey);
        if (itemSkuToRowNumberList.containsKey(keyName)) {
          List<Integer> integers = itemSkuToRowNumberList.get(keyName);
          integers.add(rowNumberParentCodeDTO.getRowNumber());
          itemSkuToRowNumberList.put(keyName, integers);
        } else {
          itemSkuToRowNumberList.put(keyName,
            new ArrayList<>(Collections.singletonList(rowNumberParentCodeDTO.getRowNumber())));
        }
        productSkuToItemSkuToRowNumbersList.put(domainKey, itemSkuToRowNumberList);
      } else {
        Map<String, List<Integer>> itemSkuToRowNumberList = new HashMap<>();
        itemSkuToRowNumberList
            .put(keyName, new ArrayList<>(Collections.singletonList(rowNumberParentCodeDTO.getRowNumber())));
        productSkuToItemSkuToRowNumbersList.put(domainKey, itemSkuToRowNumberList);
      }
    }
  }

  private void batchRowsByKey(Map<String, List<Integer>> productSkuToRowNumbersListMap,
      List<RowNumberParentCodeDTO> rowNumberParentCodeDTOList, String keyName) {
    for (RowNumberParentCodeDTO rowNumberParentCodeDTO : rowNumberParentCodeDTOList) {
      String key = StringUtils.EMPTY;
      try {
        LinkedHashMap<String, String> rowDataJson =
            objectMapper.readValue(rowNumberParentCodeDTO.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
            });
        key = rowDataJson.get(keyName);
      } catch (Exception e) {
        log.error("Error when trying to extract key for data : {} ", rowNumberParentCodeDTO.getBulkRequestData(), e);
      }
      List<Integer> rowNumbers = productSkuToRowNumbersListMap.getOrDefault(key, new ArrayList<>());
      rowNumbers.add(rowNumberParentCodeDTO.getRowNumber());
      productSkuToRowNumbersListMap.put(key, rowNumbers);
    }
  }

  private void processToPublishCncUpdates(String storeId, BulkProcess bulkProcess, String event,
      String batchSizeVariable, String batchKey) {
    List<RowNumberParentCodeDTO> rowNumbersAndData = bulkProcessDataService
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    log.info("Publish event = {}, blpCode : {}", event, bulkProcess.getBulkProcessCode());
    int batchSize = Integer
        .parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId, batchSizeVariable).getValue());
    Map<String, List<Integer>> productSkuRowNumbersList = new HashMap<>();
    batchRequestsByProductSkus(rowNumbersAndData, productSkuRowNumbersList, batchKey);
    for (Map.Entry<String, List<Integer>> entry : productSkuRowNumbersList.entrySet()) {
      List<Integer> rowNumbers = entry.getValue();
      List<List<Integer>> subRows = Lists.partition(rowNumbers, batchSize);
      for (List<Integer> batch : subRows) {
        publishEventForBulkProcessDataWithKey(storeId, bulkProcess, event, batch, entry.getKey());
      }
    }
  }

  private void publishEventForBulkProcessData(String storeId, BulkProcess bulkProcess, String event,
      List<Integer> batch) {
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(storeId).businessPartnerCode(bulkProcess.getBusinessPartnerCode())
            .bulkProcessCode(bulkProcess.getBulkProcessCode()).rowNumbers(batch).build();
    kafkaProducer.send(event, bulkUpdateEventModel);
    log.info("event = {} got published having bulkProcessCode = {} bulkUpdateEventModel = {} ", event,
        bulkProcess.getBulkProcessCode(), bulkUpdateEventModel);
  }

  private void publishEventForBulkProcessDataWithKey(String storeId, BulkProcess bulkProcess, String event,
      List<Integer> batch, String domainEventKey) {
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(storeId).businessPartnerCode(bulkProcess.getBusinessPartnerCode())
            .bulkProcessCode(bulkProcess.getBulkProcessCode()).rowNumbers(batch).build();
    kafkaProducer.send(event, domainEventKey, bulkUpdateEventModel);
    log.info("event = {} got published having bulkProcessCode = {} bulkUpdateEventModel = {} ", event,
        bulkProcess.getBulkProcessCode(), bulkUpdateEventModel);
  }

  private void publishEventForBulkProcessDataUpdate(String storeId, BulkProcess bulkProcess, String event,
      List<Integer> batch, String domainEventKey) {
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(storeId).businessPartnerCode(bulkProcess.getBusinessPartnerCode())
            .bulkProcessCode(bulkProcess.getBulkProcessCode()).rowNumbers(batch).build();
    kafkaProducer.send(event, domainEventKey, bulkUpdateEventModel);
    log.info("event = {} got published having bulkProcessCode = {} bulkUpdateEventModel = {} ", event,
        bulkProcess.getBulkProcessCode(), bulkUpdateEventModel);
  }

  private void batchRequestsByProductSkus(List<RowNumberParentCodeDTO> rowNumbers,
      Map<String, List<Integer>> productSkuRowNumbersList, String batchKey) {
    for (RowNumberParentCodeDTO rowNumberParentCodeDTO : rowNumbers) {
      String productSku = Constant.SKU;
      try {
        LinkedHashMap<String, String> rowDataJson =
            objectMapper.readValue(rowNumberParentCodeDTO.getBulkRequestData(), new TypeReference<LinkedHashMap<String, String>>() {
            });
        String itemSku = rowDataJson.get(batchKey);
        productSku = itemSku.substring(0, itemSku.lastIndexOf(SKU_DELIMITER));
      } catch (Exception e) {
        log.error("Error when trying to extract productSku for data : {} ",
            rowNumberParentCodeDTO.getBulkRequestData(), e);
      }
      if (productSkuRowNumbersList.containsKey(productSku)) {
        List<Integer> rowNumberList = new ArrayList(productSkuRowNumbersList.get(productSku));
        rowNumberList.add(rowNumberParentCodeDTO.getRowNumber());
        productSkuRowNumbersList.put(productSku, rowNumberList);
      } else {
        productSkuRowNumbersList.put(productSku, Collections.singletonList(rowNumberParentCodeDTO.getRowNumber()));
      }
    }
  }

  @Override
  @Async
  @Transactional(timeout = Constant.TRANSACTION_TIMEOUT, readOnly = false)
  public void deleteFromDb(String storeId) {
    int days = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.DELETE_DAY_RANGE)
            .getValue());
    if (days != Constants.ZERO) {
      long startTime = Calendar.getInstance().getTimeInMillis();
      Date endUpdatedDate = DateUtils.addDays(new Date(), -days);
      Date startUpdatedDate = DateUtils.addDays(DateUtils.addDays(new Date(), -days), -days);
      LOG.info("Deleting entries older than updates date from db: {}", endUpdatedDate);
      try {
        List<String> blpCodeList = bulkProcessRepository
            .findByStatusAndUpdatedDateInBetween(storeId, endUpdatedDate, startUpdatedDate, Arrays
                .asList(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE, BulkProcess.STATUS_ABORTED));
        LOG.info("Total blp deleted : {}", blpCodeList.size());
        for (String bulkProcessCode : blpCodeList) {
          kafkaProducer.send(kafkaTopicProperties.getBulkDataDeletionEvent(), bulkProcessCode,
              new BulkDataDeletionModel(storeId, bulkProcessCode));
        }
      } catch (Exception ex) {
        LOG.error("Error while deleting from data and image db. ", ex);
        return;
      }
      long endTime = Calendar.getInstance().getTimeInMillis();
      LOG.info("Total time taken for deleting entries from data and image db in milliseconds {} ", endTime - startTime);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteBulkProcessDataByBulkProcessCode(String storeId, String bulkProcessCode) {
    bulkProcessDataService.deleteDataByBulkProcessCode(storeId, bulkProcessCode);
    bulkProcessImageService.deleteImageByBulkProcessCode(storeId, bulkProcessCode);
  }

  @Override
  public void publishBulkWorkOrderUploadEvent(String storeId, BulkProcess bulkProcess) {
    List<BulkProcessDataDTO> bulkProcessDataDTOList =
      bulkProcessDataService.getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(storeId,
        bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_PENDING);
    int batchSize = Integer.parseInt(
      systemParameterConfigService.findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE).getValue());
    List<List<BulkProcessDataDTO>> subRows = Lists.partition(bulkProcessDataDTOList, batchSize);
    for(List<BulkProcessDataDTO> bulkProcessDataDTOS : subRows){
      processBulkProcessDataForWorkOrder(bulkProcess.getStoreId(), bulkProcessDataDTOS);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void abortStruckProcessesByProcessTypes(String storeId,
    Map<String, Integer> processTypeXAbortTimeMap) {
    processTypeXAbortTimeMap.forEach((processType, time) -> {
      Date updatedDate = DateUtils.addMinutes(new Date(), -time);
      bulkProcessRepository.updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
        processType, updatedDate, getSupportedStatusForAbortJobMainTable());
    });
    processPublishedStruckProcessCodes(storeId, processTypeXAbortTimeMap);
  }

  private void processPublishedStruckProcessCodes(String storeId,
    Map<String, Integer> processTypeXAbortTimeMap) {

    int fetchBatchSize = Integer.parseInt(
      systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES).getValue());

    processTypeXAbortTimeMap.forEach((processType, abortTime) -> {
      Date updatedDateThreshold = DateUtils.addMinutes(new Date(), -abortTime);
      Pageable pageable = PageRequest.of(0, fetchBatchSize);
      List<String> supportedStatuses = getSupportedStatusForAbortJobInDataTable();
      List<String> statusesToUpdate = List.of("PENDING", "IN_PROGRESS");

      processStruckProcesses(storeId, processType, updatedDateThreshold, pageable, supportedStatuses, statusesToUpdate);
    });
  }

  private void processStruckProcesses(String storeId, String processType, Date updatedDateThreshold,
    Pageable pageable, List<String> supportedStatuses, List<String> statusesToUpdate) {
    Page<String> page =
      bulkProcessRepository.findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
        storeId, processType, supportedStatuses, updatedDateThreshold, pageable);

    List<String> processCodes =
      Optional.ofNullable(page).map(Page::getContent).orElse(Collections.emptyList());
    for (String bulkProcessCode : processCodes) {
      log.info("Proceeding with status update to fail in data table for process code: {}",
        bulkProcessCode);
      bulkProcessDataService.updateStatusToFailByBulkProcessCodeAndStatusIn(bulkProcessCode,
        statusesToUpdate);
    }
  }


  private List<String> getSupportedStatusForAbortJobMainTable() {
    return Stream.of(StringUtils.split(supportedStatusForAbortJobMainTable, ","))
      .collect(Collectors.toList());
  }

    private List<String> getSupportedStatusForAbortJobInDataTable() {
      return Stream.of(StringUtils.split(supportedStatusForAbortJobDataTable, ","))
        .collect(Collectors.toList());
    }


  private void processBulkProcessDataForWorkOrder(String storeId, List<BulkProcessDataDTO> bulkProcessDataDTOs) {
    List<WorkOrderEventModel> workOrderEventModels = bulkProcessDataDTOs.stream()
        .map(bulkProcessDataDTO -> ConverterUtil.convertToWorkOrderEventModel(storeId, bulkProcessDataDTO))
        .collect(Collectors.toList());
    for (WorkOrderEventModel workOrderEventModel : workOrderEventModels) {
      log.info("Publishing Event : {} for workOrderEventModel : {} ", kafkaTopicProperties.getBulkWorkOrderUploadEvent(),
          workOrderEventModel);
      kafkaProducer.send(kafkaTopicProperties.getBulkWorkOrderUploadEvent(), workOrderEventModel.getBusinessPartnerCode(),
          workOrderEventModel);
    }
  }


  @Async
  @Transactional(readOnly = false)
  @Override
  public void checkStuckProcessStatus(String bulkProcessCode) {
    try {
      if (StringUtils.isNotBlank(bulkProcessCode)) {
        LOG.info("Aborting the process with bulk process code : {}", bulkProcessCode);
        bulkProcessRepository.updateStatusToAbortedByBulkProcessCodes(new Date(), Arrays.asList(bulkProcessCode));
        return;
      }
      int stuckProcessTime = Integer.parseInt(
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME).getValue());
      Date updatedDate = ProcessorUtils.getEarlierDateBySeconds(stuckProcessTime);
      LOG.info("Checking stuck process before updatedDate {}", updatedDate);
      List<String> blpCodeList = bulkProcessRepository.findByStatusAndUpdatedDate(Constant.STORE_ID, updatedDate,
          Arrays.asList(STATUS_IMAGE_PROCESSING));
      if (CollectionUtils.isNotEmpty(blpCodeList)) {
        bulkProcessImageService.updatePendingImageDownloads(Constant.STORE_ID, blpCodeList, updatedDate);
      }

      blpCodeList = bulkProcessRepository.findByStatusAndUpdatedDate(Constant.STORE_ID, updatedDate,
          Arrays.asList(BulkProcess.STATUS_PUBLISHED));
      if (CollectionUtils.isNotEmpty(blpCodeList)) {
        bulkProcessDataService.updatePendingProcesses(Constant.STORE_ID, blpCodeList, updatedDate);
      }

      blpCodeList = bulkProcessRepository.findByStatusAndUpdatedDate(Constant.STORE_ID, updatedDate,
          Arrays.asList(BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_PROCESSED));
      if (CollectionUtils.isNotEmpty(blpCodeList)) {
        LOG.info("Updating the status to ABORTED for bulk process codes : {} and updatedDate : {}", blpCodeList,
            updatedDate);
        bulkProcessRepository.updateStatusToAbortedByBulkProcessCodes(updatedDate, blpCodeList);
      }
    } catch (Exception e) {
      LOG.error("Exception while checking the stuck process status. {}", e.getMessage());
    }
  }

  @Override
  public void publishBulkInstantPickupItemUpsertEvent(String storeId, BulkProcess bulkProcess) throws Exception {
    processToPublishCncUpdates(storeId, bulkProcess, kafkaTopicProperties.getBulkUpsertInstantPickupItemEvent(),
        SystemParameterConfigNames.PROCESS_DATA_UPSERT_BATCH_SIZE, ExcelHeaderNames.BLIBLI_SKU_EN);
  }

  @Override
  public void publishBulkInstantPickupItemDeleteEvent(String storeId, BulkProcess bulkProcess) throws Exception {
    processToPublishCncUpdates(storeId, bulkProcess, kafkaTopicProperties.getBulkDeleteInstantPickupItemEvent(),
        SystemParameterConfigNames.PROCESS_DATA_DELETE_BATCH_SIZE, FIELD_ITEM_SKU);
  }

  private BulkUpdateQueue getBulkUpdateQueueForVatUpdate(String storeId, String requestId, String userName,
      String businessPartnerCode, String filePath, String bulkProcessCode) {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setRequestId(requestId);
    bulkUpdateQueue.setStoreId(storeId);
    bulkUpdateQueue.setBulkProcessCode(bulkProcessCode);
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setFileName(filePath.substring(filePath.lastIndexOf(Constant.SLASH) + 1, filePath.length()));
    bulkUpdateQueue.setBusinessPartnerCode(businessPartnerCode);
    bulkUpdateQueue.setUpdatedBy(userName);
    bulkUpdateQueue.setClientHost(Constant.CLIENT_ID);
    bulkUpdateQueue.setPrivilegedMap(new HashMap<>());
    return bulkUpdateQueue;
  }

  private BulkProcess createBulkProcess(String storeId, String requestId, String bulkProcessCode,
      String bulkProcessType, String businessPartnerCode, String excelFilename) throws Exception {
    ProfileResponse profileResponse =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, businessPartnerCode);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(storeId);
    bulkProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setBulkProcessType(bulkProcessType);
    bulkProcess.setBusinessPartnerCode(businessPartnerCode);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(requestId);
    bulkProcess.setDescription(excelFilename + ". " + DESCRIPTION_PENDING);
    bulkProcess.setErrorCount(0);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setTotalCount(0);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    bulkProcess.setUploadedFile(excelFilename);
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())) {
      bulkProcess.setInternationalMerchant(profileResponse.getCompany().isInternationalFlag());
    }
    return bulkProcess;
  }

  @Override
  @Transactional(readOnly = false)
  public void downloadImages(String bulkProcessCode, List<String> imageList) {
    BulkProcess bulkProcess =
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, bulkProcessCode);
    List<BulkProcessImage> bulkProcessImageList =
        bulkProcessImageService.findByBulkProcessCodeAndImageUrl(bulkProcess, imageList);
    for (BulkProcessImage bulkProcessImage : bulkProcessImageList) {
      ImageDownloadResult imageDownloadResult = new ImageDownloadResult();
      Map<String, String> imageUrlAndNameMap = new HashMap<>();
      imageUrlAndNameMap.put(bulkProcessImage.getImageURL(),
        String.valueOf(bulkProcessImage.getSequence()));
      StringBuilder validationErrorMessage = new StringBuilder();
      try {
        imageDownloadResult = fileStorageService
            .downloadImages(bulkProcessCode, imageUrlAndNameMap, httpConnectionTimeout, httpConnectionReadTimeout,
                new HashSet<>(), Collections.singletonList(bulkProcessImage.getImageURL()),
                new BulkUploadErrorCounter(), validationErrorMessage, bulkProcess.getInternationalMerchant());
      } catch (Exception e) {
        LOG.error("Exception when downloading image for bulkProcessCode : {} image : {} ", bulkProcessCode,
            bulkProcessImage.getImageURL(), e);
        imageDownloadResult.setDownloadSuccess(false);
      }
      if (imageDownloadResult.isDownloadSuccess()) {
        bulkProcessImage.setCompleted(true);
        bulkProcessImage.setLocation(
          new StringBuilder(ProcessorUtils.DATA_BASE_DIR).append(bulkProcessCode)
            .append(File.separator).append(ProcessorUtils.DATA_RAW_DIR).append(File.separator)
            .append(imageDownloadResult.getImageFileName()).append(imageDownloadResult.getImageType()).toString());
      } else {
        bulkProcessImage.setCompleted(true);
        bulkProcessImage.setErrorMessage(String.valueOf(validationErrorMessage));
      }
    }
    bulkProcessImageService.saveBulkProcessImage(bulkProcessImageList);
  }

  @Override
  public void publishBulkImageDownloadEventModel(String bulkProcessCode, String bulkProcessType, List<String> imageList,
      boolean priorityQueueEnabled) {
    LOG.info("publish event com.gdn.mta.bulk.create.download.image for bulkProcessCode : {} and imageList : {} ",
        bulkProcessCode, imageList);
    kafkaProducer.send(
        getImageDownloadEventNameByPriority(bulkProcessType, priorityQueueEnabled),
        new BulkImageDownloadEventModel(bulkProcessCode, imageList));
  }

  @Override
  public void publishBulkExternalImageDownloadEventModel(String bulkProcessCode,
    String bulkProcessType, List<String> imageList) {
    LOG.info(
      "publish event com.gdn.mta.bulk.external.create.download.image for bulkProcessCode : {} and "
        + "imageList : {} ", bulkProcessCode, imageList);
    kafkaProducer.send(kafkaTopicProperties.getBulkExternalCreateDownloadImageEvent(),
      new BulkImageDownloadEventModel(bulkProcessCode, imageList));
  }

  @Override
  public void publishBulkBasicInfoImageDownloadEventModel(String bulkProcessCode, String bulkProcessType,
      List<String> imageList) {
    LOG.info("publish event downloading basic info images for bulkProcessCode : {} and imageList : {} ",
        bulkProcessCode, imageList);
    kafkaProducer.send(getBasicInfoImageDownloadEventNameByPriority(bulkProcessType),
        new BulkImageDownloadEventModel(bulkProcessCode, imageList));
  }

  private String getImageDownloadEventNameByPriority(String bulkProcessType, boolean priorityQueueEnabled) {
    if (priorityQueueEnabled) {
      if (BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue().equals(bulkProcessType)) {
        return kafkaTopicProperties.getBulkCreateDownloadImagePriority1();
      } else if (BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue().equals(bulkProcessType)) {
        return kafkaTopicProperties.getBulkCreateDownloadImagePriority2();
      } else {
        return kafkaTopicProperties.getBulkCreateDownloadImageEvent();
      }
    } else {
      return kafkaTopicProperties.getBulkCreateDownloadImageEvent();
    }
  }

  @Override
  public void publishBulkBasicInfoVideoDownloadEventModel(String bulkProcessCode, String bulkProcessType,
      String videoUrl, String businessPartnerCode) {
    BulkVideoDownloadRequestEventModel eventModel =
      BulkVideoDownloadRequestEventModel.builder().clientId(videoClientId).source(videoSource)
        .ownerCode(businessPartnerCode).originalUrl(videoUrl).additionalFields(
          Map.of(Constant.BULK_PROCESS_CODE, bulkProcessCode, Constant.REQUEST_CODE, bulkProcessCode))
        .build();
    LOG.info("publish event downloading basic info videos for bulkProcessCode : {} and videoUrl : {} ", bulkProcessCode,
        videoUrl);
    kafkaProducer.send(getBasicInfoVideoDownloadEventNameByPriority(bulkProcessType),
      bulkProcessCode, eventModel);
  }

  public String getBasicInfoImageDownloadEventNameByPriority(String bulkProcessType) {
    if (BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue().equals(bulkProcessType)) {
      return kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority1();
    } else if (BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue().equals(bulkProcessType)) {
      return kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority2();
    } else {
      return kafkaTopicProperties.getBulkBasicInfoDownloadImageEvent();
    }
  }

  public String getBasicInfoVideoDownloadEventNameByPriority(String bulkProcessType) {
    if (BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue().equals(bulkProcessType)) {
      return kafkaTopicProperties.getBulkBasicInfoDownloadVideoPriority1();
    } else if (BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue().equals(bulkProcessType)) {
      return kafkaTopicProperties.getBulkBasicInfoDownloadVideoPriority2();
    } else {
      return kafkaTopicProperties.getBulkBasicInfoDownloadVideoEvent();
    }
  }

  @Async
  @Override
  @Trace(dispatcher = true)
  @Transactional(readOnly = false)
  public void checkBulkProcessStatus(String storeId, String bulkProcessType) {
    if (Stream.of(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
        BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue())
      .anyMatch(bulkProcessType::equalsIgnoreCase)) {
      checkIfAllImagesAreDownloaded(storeId, bulkProcessType);
    }
    if (Stream.of(BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue()).anyMatch(bulkProcessType::equalsIgnoreCase)) {
      checkIfAllImagesAndVideosAreDownloaded(storeId, bulkProcessType);
    }
    if (BulkProcessType.DELETE_PICKUP_POINT.getValue().equals(bulkProcessType)) {
      pickupPointDeleteService.processPendingDeletePickupPointEvent(storeId, bulkProcessType);
    }
    if (QR_GENERATION.getValue().equals(bulkProcessType)) {
      processPendingQRCode(storeId, bulkProcessType);
    }
    if (EXTERNAL_CREATION_UPLOAD.getValue().equals(bulkProcessType)) {
      checkIfAllImagesAreDownloadedAndDSResponseRecieved(storeId, bulkProcessType);
    }
    checkIfAllProductsAreProcessed(storeId, bulkProcessType);
  }

  private void checkIfAllImagesAreDownloaded(String storeId, String bulkProcessType) {
    String trackerUUID = UUID.randomUUID().toString();
    Integer imageProcessFetchLimit = getBatchSizeWithBulkProcessType(storeId, bulkProcessType);
    LOG.info("Process to check image process status of batch size : {} id : {} ", imageProcessFetchLimit, trackerUUID);
    List<BulkProcess> bulkProcessList =
        bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(storeId, bulkProcessType,
            getBulkProcessStatusByProcessType(bulkProcessType), PageRequest.of(0, imageProcessFetchLimit));
    List<BulkProcess> updateBulkProcessList = new ArrayList<>();
    validateIfAllImagesAreDownloaded(storeId, bulkProcessList, updateBulkProcessList);
    LOG.info("Completed Process to check image process status of batch size : {} id : {} ", imageProcessFetchLimit,
        trackerUUID);
  }

  private void checkIfAllImagesAndVideosAreDownloaded(String storeId, String bulkProcessType) {
    Integer videoProcessFetchLimit = getBatchSizeWithBulkProcessType(storeId, bulkProcessType);
    log.info("Process to check image and video process status of batch size : {} ", videoProcessFetchLimit);
    List<BulkProcess> bulkProcessList =
        bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(storeId, bulkProcessType,
            getBulkProcessImageVideoStatusByProcessType(bulkProcessType), PageRequest.of(0, videoProcessFetchLimit));
    validateIfAllImagesAndVideosAreDownloaded(storeId, bulkProcessList);
    log.info("Completed Process to check image and video process status of batch size : {} ", videoProcessFetchLimit);
  }

  private void checkIfAllImagesAreDownloadedAndDSResponseRecieved(String storeId,
      String bulkProcessType) {
    Integer imageProcessFetchLimit = getBatchSizeWithBulkProcessType(storeId, bulkProcessType);
    log.info("Process to check DS response and image status of batch size : {} ",
        imageProcessFetchLimit);
    List<BulkProcess> bulkProcessList =
        bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(storeId,
            bulkProcessType, getBulkProcessStatusByProcessType(bulkProcessType),
            PageRequest.of(0, imageProcessFetchLimit));
    validateIfAllDsResponeRecievedAndImagesAreDownloaded(storeId, bulkProcessList);
    log.info("Completed Process to check DS response and image status of batch size : {} ",
        imageProcessFetchLimit);
  }

  public void processPendingQRCode(String storeId, String bulkProcessType) {
    int fetchBatchSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE).getValue());
    List<BulkProcess> bulkProcessList =
        findByBulkProcessTypeAndStatus(storeId, bulkProcessType, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, fetchBatchSize));
    for (BulkProcess bulkProcess : bulkProcessList) {
      try {
        if (StringUtils.isNotBlank(bulkProcess.getUploadedFile())) {
          Page<BulkProcessData> bulkProcessDataPage =
              this.bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndStatus(storeId,
                  bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, fetchItemPickupPointSize));
          if (CollectionUtils.isNotEmpty(bulkProcessDataPage.getContent())) {
            this.setProductDetails(bulkProcessDataPage.getContent(), bulkProcess.getDescription());
          }
          bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
        } else {
          DownloadQRCodeRequest downloadQRCodeRequest =
              objectMapper.readValue(bulkProcess.getNotes(), DownloadQRCodeRequest.class);
          bulkProcessDataService.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcess);
        }
        bulkProcessRepository.save(bulkProcess);
      } catch (Exception e) {
        log.error("Error while generating data for bulk process : {} ", bulkProcess, e);
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
        bulkProcess.setEndDate(new Date());
        bulkProcessRepository.save(bulkProcess);
        notificationService.sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(),
            bulkProcess.getDescription());
      }
    }
  }

  @SneakyThrows
  private void setProductDetails(List<BulkProcessData> bulkProcessDataList, String qrGenerationType) {
    List<BulkProcessData> bulkProcessDataFinalList = new ArrayList<>();
    List<QrCodeRowItemInfo> qrCodeRowItemList =
        bulkProcessDataList.stream().map(this::fetchQrRowItemInfo).flatMap(List::stream).collect(Collectors.toList());
    if (AllowedQRGenerationType.ADD_TO_BAG.getValue().equalsIgnoreCase(qrGenerationType)) {
      bulkProcessDataFinalList =
          constructBulkProcessDataForAddToBag(bulkProcessDataList);
    } else {
      bulkProcessDataFinalList =
          constructBulkProcessDataForNonAddToBag(bulkProcessDataList, qrCodeRowItemList);
    }
    this.bulkProcessDataService.saveBulkProcessData(bulkProcessDataFinalList);
  }

  private List<BulkProcessData> constructBulkProcessDataForNonAddToBag (List<BulkProcessData> bulkProcessDataList, List<QrCodeRowItemInfo> qrCodeRowItemList){
    Map<String, ItemL5ListingResponse> l5DetailResponses = this.fetchL5Details(qrCodeRowItemList);
    List<BulkProcessData> bulkProcessDataListFinalList = new ArrayList<>();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      try {
       QrCodeRowInfo qrCodeRowInfo =
            this.objectMapper.readValue(bulkProcessData.getBulkRequestData(), QrCodeRowInfo.class);
        qrCodeRowInfo.getRowItems().forEach(qrCodeRowItemInfo -> this.setL5Details(qrCodeRowItemInfo,
            l5DetailResponses.get(
                RequestHelper.toL5Id(qrCodeRowItemInfo.getItemSku(), qrCodeRowItemInfo.getPickupPointCode()))));
        bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
        bulkProcessData.setBulkRequestData(this.objectMapper.writeValueAsString(qrCodeRowInfo));
      } catch (Exception e) {
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        bulkProcessData.setErrorMessage(FAILED_TO_FETCH_PRODUCT_DETAILS);
      }
      bulkProcessDataListFinalList.add(bulkProcessData);
    }
    return bulkProcessDataListFinalList;
  }
  @SneakyThrows
  private List<BulkProcessData> constructBulkProcessDataForAddToBag (List<BulkProcessData> bulkProcessDataList){
    List<BulkProcessData> bulkProcessDataListFinalList = new ArrayList<>();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      Map<String, String> errorMap = new HashMap<>();
      QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
      try {
        qrCodeRowInfo =
            this.objectMapper.readValue(bulkProcessData.getBulkRequestData(), QrCodeRowInfo.class);
        assert(Objects.nonNull(qrCodeRowInfo.getRowItems()));
        final int  totalCount = qrCodeRowInfo.getRowItems().size();
        Iterator<QrCodeRowItemInfo> iterator = qrCodeRowInfo.getRowItems().iterator();
        while (iterator.hasNext()) {
          QrCodeRowItemInfo qrCodeRowItemInfo = iterator.next();
          if (!qrCodeRowItemInfo.isCncActivated()) {
            errorMap.put(qrCodeRowItemInfo.getItemSku(), qrCodeRowItemInfo.getPickupPointName());
            iterator.remove();
          }
        }
        bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
        if (CollectionUtils.isEmpty(qrCodeRowInfo.getRowItems()) || qrCodeRowInfo.getRowItems().size() != totalCount) {
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(errorMap));
          if (CollectionUtils.isEmpty(qrCodeRowInfo.getRowItems())) {
            bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          }
        }
        bulkProcessData.setBulkRequestData(this.objectMapper.writeValueAsString(qrCodeRowInfo));
      } catch (Exception e) {
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        if (AllowedQRGenerationType.ADD_TO_BAG.getValue()
            .equalsIgnoreCase(qrCodeRowInfo.getQrGenerationType())) {
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(errorMap));
        } else {
          bulkProcessData.setErrorMessage(FAILED_TO_FETCH_PRODUCT_DETAILS);
        }
      }
      bulkProcessDataListFinalList.add(bulkProcessData);
    }
    return bulkProcessDataListFinalList;
  }

  private void setL5Details(QrCodeRowItemInfo qrCodeRowItemInfo,
      ItemL5ListingResponse itemL5ListingResponse) {
    qrCodeRowItemInfo.setProductName(itemL5ListingResponse.getItemName());
    qrCodeRowItemInfo.setPickupPointName(itemL5ListingResponse.getPpCodeName());
    qrCodeRowItemInfo.setProductPrice(
        QRCodeGenerationUtil.convertPriceToIndonesianFormat(itemL5ListingResponse.getOfferPrice()));
    qrCodeRowItemInfo.setCncActivated(qrCodeRowItemInfo.isCncActivated() && itemL5ListingResponse.isCncActivated());
  }

  private Map<String, ItemL5ListingResponse> fetchL5Details(List<QrCodeRowItemInfo> qrCodeRowItemList) {
    List<List<QrCodeRowItemInfo>> qrRowItemBatches = Lists.partition(qrCodeRowItemList, fetchItemPickupPointSize);
    List<ItemL5ListingResponse> itemL5ListingResponses = new ArrayList<>();
    for (List<QrCodeRowItemInfo> qrRowItemBatch : qrRowItemBatches) {
      Page<ItemL5ListingResponse> itemL5DetailsByL5Ids = xProductOutboundService.getItemL5DetailsByL5Ids(
          qrRowItemBatch.stream().map(qrCodeRowItemInfo -> RequestHelper.toL5Id(qrCodeRowItemInfo.getItemSku(),
              qrCodeRowItemInfo.getPickupPointCode())).collect(Collectors.toList()), false, 0,
          fetchItemPickupPointSize);
      itemL5ListingResponses.addAll(itemL5DetailsByL5Ids.getContent());
    }
    return itemL5ListingResponses.stream().collect(Collectors.toMap(
        itemL5ListingResponse -> RequestHelper.toL5Id(itemL5ListingResponse.getItemSku(),
            itemL5ListingResponse.getPpCode()), Function.identity(), (a, b) -> b));
  }

  @SneakyThrows
  private List<QrCodeRowItemInfo> fetchQrRowItemInfo(BulkProcessData bulkProcessData) {
    QrCodeRowInfo qrCodeRowInfo =
        this.objectMapper.readValue(bulkProcessData.getBulkRequestData(), QrCodeRowInfo.class);
    return qrCodeRowInfo.getRowItems();
  }

  private String getBulkProcessStatusByProcessType(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
        return STATUS_IMAGE_PROCESSING_PRIORITY_1;
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
        return STATUS_IMAGE_PROCESSING_PRIORITY_2;
    }
    return STATUS_IMAGE_PROCESSING;
  }

  private String getBulkProcessImageVideoStatusByProcessType(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case PRODUCT_BASIC_INFO_PRIORITY_1:
        return STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1;
      case PRODUCT_BASIC_INFO_PRIORITY_2:
        return STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2;
    }
    return STATUS_IMAGE_AND_VIDEO_PROCESSING;
  }

  private Integer getBatchSizeWithBulkProcessType(String storeId, String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
        return Integer.valueOf(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_1).getValue());
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
        return Integer.valueOf(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_2).getValue());
      case PRODUCT_BASIC_INFO:
      case PRODUCT_BASIC_INFO_PRIORITY_1:
      case PRODUCT_BASIC_INFO_PRIORITY_2:
        return Integer.valueOf(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO).getValue());
    }
    return Integer.valueOf(
      systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        IMAGE_PROCESSING_FETCH_BATCH_SIZE).getValue());
  }

  private void validateIfAllImagesAreDownloaded(String storeId, List<BulkProcess> bulkProcessList,
      List<BulkProcess> updateBulkProcessList) {
    for (BulkProcess bulkProcess : bulkProcessList) {
      List<BulkProcessImage> bulkProcessImageList =
          bulkProcessImageService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
      checkBulkProcessImages(updateBulkProcessList, bulkProcess, bulkProcessImageList);
      if (CollectionUtils.isNotEmpty(updateBulkProcessList)) {
        bulkProcessRepository.saveAll(updateBulkProcessList);
      }
    }
  }

  private void validateIfAllImagesAndVideosAreDownloaded(String storeId, List<BulkProcess> bulkProcessList) {
    List<BulkProcess> updateBulkProcessList = new ArrayList<>();
    for (BulkProcess bulkProcess : bulkProcessList) {
      List<BulkProcessImage> bulkProcessImageList =
          bulkProcessImageService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
      List<BulkProcessVideo> bulkProcessVideoList =
          bulkProcessVideoService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
      CommonUtils.checkBulkProcessImagesAndVideo(bulkProcess, bulkProcessImageList, bulkProcessVideoList,
          updateBulkProcessList);
    }
    if (CollectionUtils.isNotEmpty(updateBulkProcessList)) {
      bulkProcessRepository.saveAll(updateBulkProcessList);
    }
  }

  private void validateIfAllDsResponeRecievedAndImagesAreDownloaded(String storeId,
      List<BulkProcess> bulkProcessList) {
    List<BulkProcess> updateBulkProcessList = new ArrayList<>();
    for (BulkProcess bulkProcess : bulkProcessList) {
      List<BulkProcessImage> bulkProcessImages =
          bulkProcessImageService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
      List<BulkProcessImageQC> bulkProcessImageQCS =
          bulkProcessImageQCService.fetchAllBulkProcessImageQCByBulkProcess(storeId, bulkProcess);
      CommonUtils.checkBulkProcessImagesAndImageQC(bulkProcess, bulkProcessImages,
          bulkProcessImageQCS, updateBulkProcessList);
    }
    if (CollectionUtils.isNotEmpty(updateBulkProcessList)) {
      bulkProcessRepository.saveAll(updateBulkProcessList);
    }
  }

  private void checkBulkProcessImages(List<BulkProcess> updateBulkProcessList, BulkProcess bulkProcess,
      List<BulkProcessImage> bulkProcessImageList) {
    boolean downloadComplete = true;
    if (CollectionUtils.isNotEmpty(bulkProcessImageList)) {
      for (BulkProcessImage bulkProcessImage : bulkProcessImageList) {
        if (!bulkProcessImage.isCompleted()) {
          downloadComplete = false;
        }
      }
      if (downloadComplete) {
        bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
        updateBulkProcessList.add(bulkProcess);
      }
    }
  }

  private void checkIfAllProductsAreProcessed(String storeId , String bulkProcessType) {
    String trackerUUID = UUID.randomUUID().toString();
    Integer imageProcessFetchLimit = Integer.valueOf(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.PUBLISHED_FETCH_BATCH_SIZE)
            .getValue());
    LOG.info("Process to check product process status of batch size : {} id : {} ", imageProcessFetchLimit,
        trackerUUID);
    List<BulkProcess> bulkProcessList =
        bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(storeId, bulkProcessType,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, imageProcessFetchLimit));
    if (CollectionUtils.isEmpty(bulkProcessList)) {
      return;
    }
    checkIfAllProductsAreProcessed(storeId, bulkProcessList);
    LOG.info("Completed Process to check product process status of batch size : {} id : {} ", imageProcessFetchLimit,
        trackerUUID);
  }

  private void checkIfAllProductsAreProcessed(String storeId, List<BulkProcess> bulkProcessList) {
    try {
      Map<String, BulkProcess> bulkProcessMap =
          bulkProcessList.stream().collect(Collectors.toMap(BulkProcess::getBulkProcessCode, Function.identity()));
      List<String> pendingList =
          bulkProcessDataService.getPendingBulkProcessCodes(storeId, new ArrayList<>(bulkProcessMap.keySet()));
      if (CollectionUtils.isNotEmpty(pendingList)) {
        for (String bulkProcessCode : pendingList) {
          bulkProcessMap.remove(bulkProcessCode);
        }
      }
      if (MapUtils.isEmpty(bulkProcessMap)) {
        return;
      }
      List<BulkProcess> updateBulkProcessList = new ArrayList<>();
      for (Map.Entry<String, BulkProcess> entry : bulkProcessMap.entrySet()) {
        BulkProcess bulkProcess = entry.getValue();
        bulkProcess.setStatus(BulkProcess.STATUS_PROCESSED);
        updateBulkProcessList.add(bulkProcess);
      }
      if (CollectionUtils.isNotEmpty(updateBulkProcessList)) {
        bulkProcessRepository.saveAll(updateBulkProcessList);
      }
    } catch (Exception e) {
      log.error("Error while changing the status to processed. ", e);
    }
  }

  @Override
  public void publishArchiveProductEvent(String storeId, BulkProcess bulkProcess) {
    processToPublish(storeId, bulkProcess, kafkaTopicProperties.getBulkArchiveProductRows(),
        SystemParameterConfigNames.PROCESS_DATA_ARCHIVE_BATCH_SIZE);
  }

  @Override
  public void publishVatUpdateEvent(String storeId, BulkProcess bulkProcess) {
    processToPublish(storeId, bulkProcess, kafkaTopicProperties.getBulkVatUpdateEvent(),
        SystemParameterConfigNames.PROCESS_DATA_VAT_UPDATE_BATCH_SIZE);
  }

  @Override
  public List<BulkProcess> findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(String storeId,
      String businessPartnerCode, String bulkProcessType, String pickupPointCode) {
    return getBulkProcessRepository().findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndNotes(storeId,
        businessPartnerCode, bulkProcessType, pickupPointCode);
  }

  @Override
  public List<BulkProcess> findByBulkProcessTypeAndStatus(String storeId, String bulkProcessType, String status,
      Pageable pageable) {
    return bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(storeId, bulkProcessType,
        status, pageable);
  }

  @Override
  public Page<BulkProcessStatusListingResponse> fetchProcessListingResponse(String storeId,
    String bulkProcessType, String requestId, String businessPartnerCode,
    Optional<List<String>> bulkProcessCodes, Optional<String> primaryIdentifier,
    boolean estimationsNeeded, Pageable pageable) throws Exception {
    Set<String> bulkProcessTypesForListing = getBulkProcessTypesForListing(bulkProcessType);
    Set<String> creationUploadProcessTypes = getCreateUpdateBulkProcessTypes();
    boolean creationUpdateProcessType = creationUploadProcessTypes.stream().anyMatch(bulkProcessType::contains);
    boolean downloadProcessType =
      getDownloadBulkProcessTypes().stream().anyMatch(bulkProcessType::contains);
    Calendar calendar = Calendar.getInstance();
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    calendar.add(Calendar.DATE, -Integer.parseInt(maxFetchDaysForListing));
    Page<BulkProcess> bulkProcessListing = null;
    Page<BulkDownloadEntity> bulkDownloadListing ;
    boolean setSuccessCount = false;
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses = new ArrayList<>();

    if (bulkProcessCodes.isEmpty() && creationUpdateProcessType) {
      bulkProcessListing = bulkProcessRepository
          .findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
              storeId, businessPartnerCode, bulkProcessTypesForListing, calendar.getTime(),
              primaryIdentifier.orElse(null), pageable);
      log.info("Total fetch Size for Process listing For merchant : {} with primaryIdentifier {} was {} ", businessPartnerCode,primaryIdentifier,
          bulkProcessListing.getTotalElements());
      bulkProcessStatusListingResponses = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessListing,
          suppressErrorForTotalCountListingBulkProcessTypes);
      if (estimationsNeeded && estimationFetchEnabledForListing) {
        processEstimationsForBulkListing(bulkProcessCodeXEstimationsMap, bulkProcessListing,
          bulkProcessStatusListingResponses);
      }
    } else if (bulkProcessCodes.isEmpty() && downloadProcessType) {
      bulkDownloadListing =
        bulkDownloadAuditRepository.findByBusinessPartnerCodeAndPrimaryIdentifierAndEntityTypeInAndMarkForDeleteFalseAndCreatedDateLimit(
          businessPartnerCode, bulkProcessTypesForListing, calendar.getTime(),
          primaryIdentifier.orElse(null), pageable);
      log.info("Total fetch Size for Download Process listing For merchant {} with primary Identifier : {} was {} ",
          businessPartnerCode, primaryIdentifier, bulkDownloadListing.getTotalElements());
      bulkProcessStatusListingResponses =
          RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadListing);
      return new PageImpl<>(bulkProcessStatusListingResponses,
          PageRequest.of(pageable.getPageNumber(), pageable.getPageSize()), bulkDownloadListing.getTotalElements());
    } else {
      List<BulkProcess> listingForInProgressBulkProcesses =
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
          storeId, bulkProcessCodes.get(), calendar.getTime(), pageable);
      PageImpl<BulkProcess> bulkProcessPage =
        new PageImpl<>(listingForInProgressBulkProcesses, pageable,
          listingForInProgressBulkProcesses.size());
      bulkProcessStatusListingResponses =
        RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage, suppressErrorForTotalCountListingBulkProcessTypes);
      if (estimationsNeeded && estimationFetchEnabledForListing) {
        processEstimationsForBulkListing(bulkProcessCodeXEstimationsMap, bulkProcessPage,
          bulkProcessStatusListingResponses);
      }
      bulkProcessListing = new PageImpl<>(listingForInProgressBulkProcesses, pageable,
        bulkProcessStatusListingResponses.size());
      log.info("Call to Process listing for Upload progress in switch off For merchant : {} was "
          + "{} for process-codes : {}", businessPartnerCode,
        listingForInProgressBulkProcesses.size(), bulkProcessCodes.get());
    }
    if (bulkProcessStatusListingResponses.isEmpty()) {
      log.info("Returning empty listing response For No Data fetch For request : {} by merchant :"
        + " {} ", requestId, businessPartnerCode);
      return new PageImpl<>(Collections.emptyList(),PageRequest.of(pageable.getPageNumber(),
        pageable.getPageSize()), 0);
    }
    bulkProcessStatusListingResponses.stream()
      .filter(response -> response.getBulkActivityStatus().equals(BulkActivityStatus.IN_PROGRESS))
      .forEach(bulkProcessStatusListingResponse -> setListingResponseForInProgress(bulkProcessStatusListingResponse, bulkProcessCodeXEstimationsMap));
    return new PageImpl<>(bulkProcessStatusListingResponses,
      PageRequest.of(pageable.getPageNumber(), pageable.getPageSize()),
      bulkProcessListing.getTotalElements());
  }

  private void processEstimationsForBulkListing(Map<String, Double> bulkProcessCodeXEstimationsMap,
    Page<BulkProcess> bulkProcessListing,
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses) {
    List<BulkProcessDataEstimation> bulkProcessDataEstimations =
      bulkProcessDataEstimationService.fetchAllEstimationResponsesByProcessTypes(
        bulkProcessListing.stream().map(BulkProcess::getBulkProcessType).distinct()
          .collect(Collectors.toList()));
    bulkProcessStatusListingResponses.forEach(
      bulkProcessStatusListingResponse -> setEstimationsForListingResponses(
        bulkProcessStatusListingResponse, bulkProcessDataEstimations,
        bulkProcessCodeXEstimationsMap));
  }

  @Override
  public void setEstimationsForListingResponses(
    BulkProcessStatusListingResponse bulkProcessStatusListingResponse,
    List<BulkProcessDataEstimation> bulkProcessDataEstimations,
    Map<String, Double> bulkProcessCodeXEstimationsMap) {
    LocalDateTime createdDateForPendingProcess =
      bulkProcessStatusListingResponse.getCreatedDate().toInstant().atZone(ZoneId.systemDefault())
        .toLocalDateTime();
    int processHour = createdDateForPendingProcess.getHour();
    List<BulkProcessDataEstimation> bulkProcessDataEstimationForPendingDataList =
      bulkProcessDataEstimations.stream().filter(
          bulkProcessDataEstimation -> bulkProcessDataEstimation.getProcessType()
            .equals(bulkProcessStatusListingResponse.getBulkProcessType()))
        .collect(Collectors.toList());
    if (bulkProcessStatusListingResponse.getBulkActivityStatus()
      .equals(BulkActivityStatus.PENDING)) {
      setBulkProcessCompletionEstimatesForPendingProcess(bulkProcessStatusListingResponse,
        processHour, bulkProcessDataEstimationForPendingDataList);
    }  if (bulkProcessStatusListingResponse.getBulkActivityStatus()
      .equals(BulkActivityStatus.IN_PROGRESS)) {
      setBulkProcessCompletionEstimatesForInProgressProcess(bulkProcessStatusListingResponse,
        processHour, bulkProcessDataEstimationForPendingDataList, bulkProcessCodeXEstimationsMap);
    }
  }

  private void setBulkProcessCompletionEstimatesForInProgressProcess(
    BulkProcessStatusListingResponse bulkProcessStatusListingResponse, int processHour,
    List<BulkProcessDataEstimation> bulkProcessDataEstimationForPendingDataList,
    Map<String, Double> bulkProcessCodeXEstimationsMap) {
    String deltaTimeEstimations;
    BulkProcessDataEstimation processDataEstimation =
      bulkProcessDataEstimationForPendingDataList.stream()
        .filter(Predicate.not(BulkProcessDataEstimation::isProcessLevelFetch)).findFirst()
        .orElse(null);
    if (Objects.nonNull(processDataEstimation)) {
      deltaTimeEstimations = processDataEstimation.getDeltaTimeEstimations();
    } else {
      log.info("No Record Level Estimations were fetched for the request : {} ",
        bulkProcessStatusListingResponse);
      return;
    }
    try {
      TypeReference<Map<Integer, Double>> typeRef = new TypeReference<Map<Integer, Double>>() {
      };
      Map<Integer, Double> deltaHourXEstimationForRecordLevelMap =
        objectMapper.readValue(deltaTimeEstimations, typeRef);
      deltaHourXEstimationForRecordLevelMap.entrySet()
        .removeIf(entry -> Objects.isNull(entry.getValue()) || entry.getValue() == 0.0);
      double averageForRecordLevelFetch =
        deltaHourXEstimationForRecordLevelMap.values().stream().filter(Objects::nonNull)
          .mapToDouble(Double::doubleValue).average().orElse(0.0);
      Double estimationForRecord = deltaHourXEstimationForRecordLevelMap.getOrDefault(processHour,
        averageForRecordLevelFetch);
      if (estimationForRecord != 0.0) {
        Double estimationsForTotalRecords =
          estimationForRecord * bulkProcessStatusListingResponse.getTotalRowCountRequested();
        LocalDateTime currentDateTime = LocalDateTime.now();
        LocalDateTime estimatedDateTime =
          currentDateTime.plusSeconds(estimationsForTotalRecords.longValue());
        Date estimatedDate =
          Date.from(estimatedDateTime.atZone(ZoneId.systemDefault()).toInstant());
        bulkProcessStatusListingResponse.setEstimatedCompletionTime(estimatedDate);
        bulkProcessCodeXEstimationsMap.put(bulkProcessStatusListingResponse.getBulkProcessCode(),
          estimationForRecord);
      }
    } catch (JsonProcessingException e) {
      log.error("Error While Fetching the Record Level Estimations For Bulk Process : {}  in "
          + "state : {} ", bulkProcessStatusListingResponse.getBulkProcessCode(),
        bulkProcessStatusListingResponse);
      bulkProcessStatusListingResponse.setEstimatedCompletionTime(null);
    }
  }

  private void setBulkProcessCompletionEstimatesForPendingProcess(
    BulkProcessStatusListingResponse bulkProcessStatusListingResponse, int processHour,
    List<BulkProcessDataEstimation> bulkProcessDataEstimationForPendingDataList) {
    String deltaTimeEstimations;
    long pendingProcessInQueue =
      bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
        bulkProcessStatusListingResponse.getStoreId(),
        bulkProcessStatusListingResponse.getBulkProcessType(),
        List.of(BulkProcess.STATUS_PENDING, BulkProcess.STATUS_READY_TO_PROCESS),
        bulkProcessStatusListingResponse.getCreatedDate());
    log.info("pendingProcessInQueue was : {} ", pendingProcessInQueue);

    BulkProcessDataEstimation processDataEstimation =
      bulkProcessDataEstimationForPendingDataList.stream()
        .filter(BulkProcessDataEstimation::isProcessLevelFetch).findFirst().orElse(null);
    if (Objects.nonNull(processDataEstimation)) {
      deltaTimeEstimations = processDataEstimation.getDeltaTimeEstimations();
    } else {
      log.info("No Estimations were fetched for Process level fetch for the request : {} ",
          bulkProcessStatusListingResponse);
      return;
    }
    try {
      TypeReference<Map<Integer, Double>> typeRef = new TypeReference<Map<Integer, Double>>() {
      };
      Map<Integer, Double> deltaHourXEstimationForProcessLevelMap =
        objectMapper.readValue(deltaTimeEstimations, typeRef);
      deltaHourXEstimationForProcessLevelMap.entrySet()
        .removeIf(entry -> Objects.isNull(entry.getValue()) || entry.getValue() == 0.0);
      double averageForProcessLevelFetch =
        deltaHourXEstimationForProcessLevelMap.values().stream().filter(Objects::nonNull)
          .mapToDouble(Double::doubleValue).average().orElse(0.0);
      Double estimationForProcess = deltaHourXEstimationForProcessLevelMap.getOrDefault(processHour,
        averageForProcessLevelFetch);
      // if there is no pending, process level estimation is returned else the total process in
      // queue and current process is considered
      double estimationForProcessWithPendingQueue = pendingProcessInQueue >= 1 ?
        (pendingProcessInQueue + 1) * estimationForProcess : estimationForProcess;
      if (estimationForProcessWithPendingQueue != 0.0) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        LocalDateTime estimatedDateTime =
          currentDateTime.plusSeconds(estimationForProcess.longValue());
        Date estimatedDate =
          Date.from(estimatedDateTime.atZone(ZoneId.systemDefault()).toInstant());
        bulkProcessStatusListingResponse.setEstimatedCompletionTime(estimatedDate);
      }
    } catch (IOException e) {
      log.error("Error While Fetching the Estimations For Bulk Process : {}  in state : {} ",
        bulkProcessStatusListingResponse.getBulkProcessCode(), bulkProcessStatusListingResponse);
      bulkProcessStatusListingResponse.setEstimatedCompletionTime(null);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void evaluateBulkProcessEstimation(String storeId, String username)
    throws JsonProcessingException {
    bulkProcessDataEstimationService.updateBulkDataEstimationByProcessTypes(storeId,username);
  }

  @Override
  public boolean checkForQrProcessAllowed(String storeId, String businessPartnerCode) {
    long maxAllowed = Long.parseLong(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER).getValue());
    long pendingTasks =
        countUnfinishedProcess(storeId, businessPartnerCode, QR_GENERATION.getValue());
    return pendingTasks < maxAllowed;
  }

  @Override
  public void publishForQRCodeGenerationEvent(String storeId, BulkProcess bulkProcess) {
    processToPublish(storeId, bulkProcess, kafkaTopicProperties.getGenerateQrCodeRow(),
        SystemParameterConfigNames.QR_GENERATION_BATCH_SIZE);
  }

  @Override
  @Transactional(readOnly = false)
  public void insertQrExcelRequest(String storeId, QrExcelUploadRequest qrExcelUploadRequest,
    String requestId, String username) throws Exception {
    String bulkProcessCode = GdnUUIDHelper.generateUUID();
    uploadExcelFileAndSaveBulkProcess(storeId, qrExcelUploadRequest, requestId, bulkProcessCode);
    QRCodeExcelQueue qrCodeExcelQueue = QRCodeExcelQueue.builder().bulkProcessCode(bulkProcessCode)
      .downloadQRCodeRequest(qrExcelUploadRequest.getDownloadQRCodeRequest()).storeId(storeId)
      .filename(qrExcelUploadRequest.getFileName()).build();
    kafkaProducer.send(kafkaTopicProperties.getGenerateQrCodeExcel(), qrCodeExcelQueue);
  }

  private void uploadExcelFileAndSaveBulkProcess(String storeId, QrExcelUploadRequest qrExcelUploadRequest,
    String requestId, String bulkProcessCode) throws Exception {
    BulkUpdateProcessDTO bulkUpdateProcessDTO =
      BulkUpdateProcessDTO.builder().bulkProcessType(BulkProcessType.QR_GENERATION.getValue())
        .fileContent(qrExcelUploadRequest.getFileContent())
        .businessPartnerCode(qrExcelUploadRequest.getDownloadQRCodeRequest().getMerchantCode())
        .fileName(qrExcelUploadRequest.getFileName()).build();
    fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, qrExcelUploadRequest.getFileName());
    BulkProcess bulkProcess = createBulkProcess(storeId, requestId, bulkProcessCode,
      BulkProcessType.QR_GENERATION.getValue(),
      qrExcelUploadRequest.getDownloadQRCodeRequest().getMerchantCode(),
      qrExcelUploadRequest.getFileName());
    bulkProcess.setDescription(qrExcelUploadRequest.getDownloadQRCodeRequest().getQrGenerationType());
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcessRepository.saveAndFlush(bulkProcess);
  }

  @Override
  @Transactional(readOnly = false)
  public void insertQrExcelRequest(QRCodeExcelQueue qrCodeExcelQueue) throws Exception {
    String storeId = qrCodeExcelQueue.getStoreId();
    BulkProcess bulkProcess = this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId,
        qrCodeExcelQueue.getBulkProcessCode());
    try {
      Sheet qrCodeExcelSheet = fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), bulkProcess);
      POIUtil.validateNumberOfRows(qrCodeExcelSheet, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);

      ProfileResponse profileResponse =
          businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
              qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode());
      bulkProcess.setInternationalMerchant(profileResponse.getCompany().isInternationalFlag());
      bulkProcess.setStartDate(new Date());
      bulkProcessRepository.save(bulkProcess);
      int maxQrCodesLimit = Integer.parseInt(this.systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT).getValue());
      if (maxQrCodesLimit < qrCodeExcelSheet.getLastRowNum()) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            Boolean.TRUE.equals(bulkProcess.getInternationalMerchant()) ?
                (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) :
                (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN) + maxQrCodesLimit);
      }
      QRCodeGenerationUtil.validateHeadersForExcelUpload(POIUtil.readHeadersFromExcel(qrCodeExcelSheet, 0),
          qrCodeExcelQueue.getDownloadQRCodeRequest().getQrGenerationType(),
          profileResponse.getCompany().isCncActivated());
      generateAndSaveBulkProcessData(bulkProcess, qrCodeExcelSheet, qrCodeExcelQueue);
    } catch (Exception e) {
      log.error("Exception occurred while bulk qr generation for request : {} ,{} ", qrCodeExcelQueue, e);
      bulkProcess.setStatus(STATUS_ABORTED);
      bulkProcess.setEndDate(new Date());
      bulkProcessRepository.save(bulkProcess);
      notificationService.sendGenerateQrCodeFailedNotification(
          qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode(),
          qrCodeExcelQueue.getDownloadQRCodeRequest().getQrGenerationType());
    }
  }

  private void generateAndSaveBulkProcessData(BulkProcess bulkProcess, Sheet qrCodeExcelSheet,
    QRCodeExcelQueue qrCodeExcelQueue) throws JsonProcessingException {
    boolean isL5Upload =
        (Constant.ITEM_PICKUP_POINT.equals(qrCodeExcelQueue.getDownloadQRCodeRequest().getQrGenerationType()) ||
            Constant.ADD_TO_BAG.equals(qrCodeExcelQueue.getDownloadQRCodeRequest().getQrGenerationType()));
    List<QrCodeRowItemInfo> qrCodeRowItemInfoList = new ArrayList<>();
    qrCodeRowItemInfoList = POIUtil.readFromExcelForBulkUpdate(qrCodeExcelSheet, 1, 0, 0, new HashMap<>())
        .parallelStream().map(excelRow -> toQrCodeRowItemInfo(excelRow, qrCodeExcelQueue)).filter(Objects::nonNull)
        .collect(Collectors.toList());
    if (CollectionUtils.isEmpty(qrCodeRowItemInfoList)) {
      log.error("No records to process for qr generation request : {}", qrCodeExcelQueue);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, FAILED_NO_PRODUCTS);
    }
    if(Constant.ADD_TO_BAG.equals(qrCodeExcelQueue.getDownloadQRCodeRequest().getQrGenerationType())){
      qrCodeRowItemInfoList = fetchAndSetL5(qrCodeRowItemInfoList);
    }
    List<List<QrCodeRowItemInfo>> qrItemInfoPerPage = Lists.partition(qrCodeRowItemInfoList,
      qrCodeExcelQueue.getDownloadQRCodeRequest().getQrPerPage());
    int size = 0;
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setBulkExcelUpload(Boolean.TRUE);
    BeanUtils.copyProperties(qrCodeExcelQueue.getDownloadQRCodeRequest(), qrCodeRowInfo,
        "productDetailsRequestList");
    for (List<QrCodeRowItemInfo> qrCodeRowItemInfoBatch : qrItemInfoPerPage) {
      qrCodeRowInfo.setRowItems(qrCodeRowItemInfoBatch);
      BulkProcessData bulkProcessData =
          BulkProcessData.builder()
              .bulkRequestData(this.objectMapper.writeValueAsString(qrCodeRowInfo))
              .rowNumber(size)
              .startDate(new Date())
              .status(isL5Upload ? BulkProcessData.STATUS_DATA_FETCH_PENDING : BulkProcessData.STATUS_PENDING)
              .bulkProcessId(bulkProcess.getId()).bulkProcessCode(bulkProcess.getBulkProcessCode()).build();
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      size++;
      bulkProcessDataList.add(bulkProcessData);
    }
    this.bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
    bulkProcess.setTotalCount(bulkProcessDataList.size());
    if (!isL5Upload) {
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    }
    bulkProcessRepository.save(bulkProcess);
  }

  private List<QrCodeRowItemInfo> fetchAndSetL5(List<QrCodeRowItemInfo> qrCodeRowItemInfoList) {
    List<QrCodeRowItemInfo> finalQrList = new ArrayList<>();
    Map<String, ItemL5ListingResponse> l5DetailResponses =
        this.fetchL5Details(qrCodeRowItemInfoList);
    qrCodeRowItemInfoList.forEach(
        qrCodeRowItemInfo -> this.setL5Details(qrCodeRowItemInfo,
            l5DetailResponses.get(
                RequestHelper.toL5Id(qrCodeRowItemInfo.getItemSku(),
                    qrCodeRowItemInfo.getPickupPointCode()))));
    List<QrCodeRowItemInfo> cncQrCodeItemInfo = new ArrayList<>();
    List<QrCodeRowItemInfo> nonCncQrCodeItemInfo = new ArrayList<>();
    Iterator<QrCodeRowItemInfo> iterator = qrCodeRowItemInfoList.iterator();
    while (iterator.hasNext()) {
      QrCodeRowItemInfo qrCodeRowItemInfo = iterator.next();
      if (qrCodeRowItemInfo.isCncActivated()) {
        cncQrCodeItemInfo.add(qrCodeRowItemInfo);
      } else {
        nonCncQrCodeItemInfo.add(qrCodeRowItemInfo);
      }
    }
    finalQrList.addAll(cncQrCodeItemInfo);
    finalQrList.addAll(nonCncQrCodeItemInfo);
    return finalQrList;
  }

  private QrCodeRowItemInfo toQrCodeRowItemInfo(Map<String, String> excelRow,
    QRCodeExcelQueue qrCodeExcelQueue) {
    String skuIdentifier;
    switch (AllowedQRGenerationType.valueOf(qrCodeExcelQueue.getDownloadQRCodeRequest().getQrGenerationType())) {
      case PRODUCT: {
        skuIdentifier = excelRow.get(ExcelHeaderNames.PRODUCT_SKU);
        if (checkSkuWithSeller(qrCodeExcelQueue, skuIdentifier)) {
          return null;
        }
        return QrCodeRowItemInfo.builder().productSku(excelRow.get(ExcelHeaderNames.PRODUCT_SKU))
            .cncActivated(QR_CODE_CNC.equalsIgnoreCase(excelRow.getOrDefault(ExcelHeaderNames.FULFILLMENT_TYPE, null)))
            .build();
      }
      case ITEM: {
        skuIdentifier = excelRow.get(ExcelHeaderNames.ITEM_SKU);
        if (checkSkuWithSeller(qrCodeExcelQueue, skuIdentifier)) {
          return null;
        }
        return QrCodeRowItemInfo.builder().itemSku(excelRow.get(ExcelHeaderNames.ITEM_SKU))
            .cncActivated(QR_CODE_CNC.equalsIgnoreCase(excelRow.getOrDefault(ExcelHeaderNames.FULFILLMENT_TYPE, null)))
            .build();
      }
      case ITEM_PICKUP_POINT: {
        skuIdentifier = excelRow.get(ExcelHeaderNames.ITEM_SKU);
        if (checkSkuWithSeller(qrCodeExcelQueue, skuIdentifier)) {
          return null;
        }
        return QrCodeRowItemInfo.builder().itemSku(excelRow.get(ExcelHeaderNames.ITEM_SKU))
            .pickupPointCode(excelRow.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN))
            .cncActivated(QR_CODE_CNC.equalsIgnoreCase(excelRow.getOrDefault(ExcelHeaderNames.FULFILLMENT_TYPE, null)))
            .build();
      }
      case ADD_TO_BAG: {
        skuIdentifier = excelRow.get(ExcelHeaderNames.ITEM_SKU);
        if (checkSkuWithSeller(qrCodeExcelQueue, skuIdentifier)) {
          return null;
        }
        return QrCodeRowItemInfo.builder()
            .itemSku(excelRow.get(ExcelHeaderNames.ITEM_SKU))
            .pickupPointCode(excelRow.get(ExcelHeaderNames.PICKUP_POINT_CODE_EN))
            .cncActivated(true)
            .build();
      }
      default:
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Incorrect QR Generation " + "type");
    }
  }

  private boolean checkSkuWithSeller(QRCodeExcelQueue qrCodeExcelQueue, String skuIdentifier) {
    return StringUtils.isBlank(skuIdentifier) || !skuIdentifier.startsWith(
        qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode());
  }

  private void setListingResponseForInProgress(
    BulkProcessStatusListingResponse bulkProcessStatusListingResponse,
    Map<String, Double> bulkProcessCodeXEstimationsMap) {
    if (Optional.of(bulkProcessStatusListingResponse)
      .map(BulkProcessStatusListingResponse::getBulkProcessCode).isPresent()) {
      List<BulkProcessData> bulkProcessData = bulkProcessDataService.findByStoreIdAndBulkProcess(
        bulkProcessStatusListingResponse.getStoreId(), new BulkProcess(bulkProcessStatusListingResponse.getBulkProcessCode(),
          bulkProcessStatusListingResponse.getBulkProcessType(), bulkProcessStatusListingResponse.getBusinessPartnerCode(),
          bulkProcessStatusListingResponse.getUploadDate(), null,
          bulkProcessStatusListingResponse.getBulkActivityStatus().toString(), null, new ArrayList<>()));
      if(CollectionUtils.isNotEmpty(bulkProcessData)){
      int successDataRows = bulkProcessData.stream().filter(data -> data.getStatus().equalsIgnoreCase(BulkProcessData.STATUS_SUCCESS))
        .mapToInt(data -> 1).sum();
      int failedDataRows = bulkProcessData.stream().filter(data -> data.getStatus().equalsIgnoreCase(BulkProcessData.STATUS_FAIL))
        .mapToInt(data -> 1).sum();
      bulkProcessStatusListingResponse.setSuccessRowCount(successDataRows);
        bulkProcessStatusListingResponse.setProcessCompletionPercentage(
            RequestHelper.calculateCompletionPercentage(bulkProcessStatusListingResponse.getTotalRowCountRequested(),
                Math.toIntExact(successDataRows + failedDataRows),
                bulkProcessStatusListingResponse.getBulkProcessType(),
                suppressErrorForTotalCountListingBulkProcessTypes));
        if (!Optional.ofNullable(skipOverrideParentStatusBulkProcessTypes).orElse(new HashSet<>())
            .contains(bulkProcessStatusListingResponse.getBulkProcessType())) {
          BulkActivityStatus activityStatus =
              (Double.compare(bulkProcessStatusListingResponse.getProcessCompletionPercentage(), 100) == 0) ?
                  (failedDataRows != 0 ?
                      (failedDataRows == bulkProcessStatusListingResponse.getTotalRowCountRequested() ?
                          BulkActivityStatus.FAILED :
                          BulkActivityStatus.PARTIAL_SUCCESS) :
                      BulkActivityStatus.SUCCESS) :
                  bulkProcessStatusListingResponse.getBulkActivityStatus();
          EnumSet<BulkActivityStatus> completionStatus =
              EnumSet.of(BulkActivityStatus.FAILED, BulkActivityStatus.PARTIAL_SUCCESS, BulkActivityStatus.SUCCESS);
          bulkProcessStatusListingResponse.setProcessCompleted(completionStatus.contains(activityStatus));
          bulkProcessStatusListingResponse.setBulkActivityStatus(activityStatus);
        }
      if (bulkProcessCodeXEstimationsMap.containsKey(
        bulkProcessStatusListingResponse.getBulkProcessCode())) {
        Double currentEstimationsForInProgress =
          bulkProcessCodeXEstimationsMap.get(bulkProcessStatusListingResponse.getBulkProcessCode());
        int totalPendingRecordsInProgress =
          bulkProcessStatusListingResponse.getTotalRowCountRequested() - Math.toIntExact(
            successDataRows + failedDataRows);
        double estimationForDoneRows =
          currentEstimationsForInProgress * totalPendingRecordsInProgress;
        LocalDateTime currentDateTime = LocalDateTime.now();
        LocalDateTime estimatedDateTime =
          currentDateTime.plusSeconds(Double.valueOf(estimationForDoneRows).longValue());
        Date estimatedDate =
          Date.from(estimatedDateTime.atZone(ZoneId.systemDefault()).toInstant());
        bulkProcessStatusListingResponse.setEstimatedCompletionTime(estimatedDate);
       }
      }
    }
  }

  private Set<String> getBulkProcessTypesForListing(String bulkProcessType) {
    if (BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue().equals(bulkProcessType)) {
      return Stream.of(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue(),
        EXTERNAL_CREATION_UPLOAD.getValue(),
        BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()).collect(Collectors.toSet());
    }
    if (BulkProcessType.PRODUCT_LEVEL_3.getValue().equals(bulkProcessType)) {
      return Stream.of(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue(),
        BulkProcessType.IN_STORE.getValue(), BulkProcessType.SUBJECT_TO_VAT.getValue(),
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(),
        BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue()).collect(Collectors.toSet());
    }
    if (BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue().equals(bulkProcessType)) {
      return Stream.of(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
          BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()).collect(Collectors.toSet());
    }
    return Stream.of(bulkProcessType).collect(Collectors.toSet());
  }

  private void updateSuccessCount(BulkProcess bulkProcess) {
    int successRowCount =
      bulkProcessDataService.findByStoreIdAndBulkProcess(bulkProcess.getStoreId(), bulkProcess)
        .stream().filter(bulkProcessData -> bulkProcessData.getStatus()
          .equalsIgnoreCase(BulkProcessData.STATUS_SUCCESS))
        .mapToInt(bulkProcessData -> 1) // Map each successful BulkProcessData to 1 for counting
        .sum();
    bulkProcess.setSuccessCount(successRowCount);
  }

  private Set<String> getCreateUpdateBulkProcessTypes() {
    return Stream.of(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue(), BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue(), BulkProcessType.IN_STORE.getValue(),
        BulkProcessType.SUBJECT_TO_VAT.getValue(), BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue(),
        BulkProcessType.CAMPAIGN.getValue(), BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(),
        BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
        BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()).collect(Collectors.toSet());

  }

  private Set<String> getDownloadBulkProcessTypes() {
    return Stream.of(BulkProcessEntity.CAMPAIGN_PRODUCT.name(), BulkProcessEntity.PRODUCT.name(),
        BulkProcessEntity.REVIEW_PRODUCTS.name()).collect(Collectors.toSet());
  }

  private BulkProcessServiceBean getBulkProcessServiceBean() {
    return applicationContext.getBean(BulkProcessServiceBean.class);
  }

  @Override
  public void preProcessWorkOrder(String storeId, String requestId, BulkUpdateProcessDTO bulkWorkOrderDTO)
      throws Exception {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkWorkOrderDTO.getBusinessPartnerCode();
    final String fileName = bulkWorkOrderDTO.getFileName();
    log.info(
        "invoking preProcess for work order creation. storeId: {}, bulkProcessCode: {}, " + "businessPartnerCode: {}",
        storeId, bulkProcessCode, businessPartnerCode);
    BulkUpdateQueue bulkUpdateQueue =
        uploadWorkOrderFileAndGetBulkUpdateQueue(storeId, requestId, bulkWorkOrderDTO, bulkProcessCode, fileName);
    BulkProcess bulkProcess =
        BulkCreationCommonUtil.getWorkOrderBulkProcess(storeId, requestId, bulkProcessCode, bulkWorkOrderDTO, 0, 0);
    ProfileResponse profileResponse =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, businessPartnerCode);
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())) {
      bulkProcess.setInternationalMerchant(profileResponse.getCompany().isInternationalFlag());
    }
    bulkProcess.setDescription(new StringBuilder(fileName).append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING).toString());
    bulkProcess.setUploadedFile(fileName);
    getBulkProcessServiceBean().saveOperation(bulkProcess);
    kafkaProducer.send(kafkaTopicProperties.getWorkOrderEvent(), bulkProcessCode, bulkUpdateQueue);
    log.info(
        "preProcessing done for work order creation. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
            + "{}, businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public BulkProcess validateAndUpdateWorkOrder(BulkUpdateQueue bulkUpdateQueue) {
    log.info("Post processing for work order bulkProcessCode {}  BulkUpdateQueue {} ",
            bulkUpdateQueue.getBulkProcessCode(), bulkUpdateQueue);
    String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    String storeId = bulkUpdateQueue.getStoreId();
    BulkProcess bulkProcess =
            findByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode, BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      log.warn("Bulk work order creation for bulkProcessCode : {} is already processed or being processed",
              bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
              "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
                      + " is already processed or being processed");
    }
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    return bulkProcessRepository.save(bulkProcess);
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void processWorkOrder(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess) {
    try {
      validateBulkWorkOrderRequest(bulkUpdateQueue, bulkProcess, new BulkUploadErrorCounter());
    } catch (ApplicationRuntimeException | IOException e) {
      if (headerValidationCheck) {
        String bundleDownloadLink = bundlingTemplateDownloadLinks.get(bulkProcess.getBulkProcessType());
        if (Optional.ofNullable(e.getMessage()).orElse(org.apache.commons.lang3.StringUtils.EMPTY)
                .contains(BulkWorkOrderConstants.HEADER_MISMATCH_CONSTANT)) {
          log.error("Invalid Headers found and sending notification for bulkProcessCode {} ",
                  bulkProcess.getBulkProcessCode());
          bulkProcess.setDescription(HEADER_VALIDATION_ERROR);
          bulkProcess.setEndDate(new Date());
          notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
                  fileStorageServiceBean.getDownloadLinkHtml(bundleDownloadLink));
        }
      }
      log.error("Error while processing work order request bulkProcessCode {} - Error : {} ",
              bulkProcess.getBulkProcessCode(), e);
      bulkProcess.setNotes(e.getMessage());
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      bulkProcessRepository.save(bulkProcess);
    }
  }

  private void validateBulkWorkOrderRequest(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess,
      BulkUploadErrorCounter counter) throws IOException {
    Sheet excelSheetData = fileStorageService.getFileDataByFileName(bulkUpdateQueue.getFileName(), bulkProcess);
    POIUtil.validateNumberOfRows(excelSheetData, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(excelSheetData, 0);
    List<Map<String, String>> dataFromExcel =
        POIUtil.readStringValueFromExcelForBulkUpdate(excelSheetData, READ_START_FROM_INDEX,
          HEADER_ROW_ID, OFFSET, bulkProcess.getBulkProcessType());
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setStartDate(new Date());
    bulkProcess.setTotalCount(dataFromExcel.size());
    bulkProcess.setBulkProcessNotes(Collections.emptyList());
    if (headers.size() == 0 || dataFromExcel.size() == 0) {
      log.error("No data found for work order creation. storeId: {}, bulkProcessCode: {}, " + "businessPartnerCode: {}",
          bulkProcess.getStoreId(), bulkProcess.getBulkProcessCode(), bulkProcess.getBusinessPartnerCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, FILE_BLANK_ERROR);
    }
    validateHeadersAndProcessExcelFileForBulkWorkOrder(headers, dataFromExcel, bulkProcess,
        new BulkUploadErrorCounter());
    bulkProcessRepository.save(bulkProcess);
  }

  public static void updateBulkStatusAborted(BulkProcess bulkProcess, String errorMessage,
      BulkUploadErrorCounter counter) {
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    bulkProcess.setNotes(errorMessage);
    bulkProcess.setErrorCount(counter.getInputErrorCount());
    bulkProcess.setSystemErrorCount(counter.getSystemError());
  }

  private void validateHeadersAndProcessExcelFileForBulkWorkOrder(Map<Integer, String> headers,
      List<Map<String, String>> data, BulkProcess bulkProcess, BulkUploadErrorCounter bulkUploadErrorCounter)
      throws JsonProcessingException {
    String bulkProcessType = bulkProcess.getBulkProcessType();
    switch (bulkProcessType) {
      case BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE:
        validateWorkOrderRequestForAssembly(headers, data, bulkProcess, bulkUploadErrorCounter);
        break;
      case BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE:
        validateWorkOrderRequestForDisAssembly(headers, data, bulkProcess, bulkUploadErrorCounter);
        break;
      case BulkWorkOrderConstants.TRANSFER_BULK_PROCESS_TYPE:
        validateWorkOrderRequestForTransferRequest(headers, data, bulkProcess, bulkUploadErrorCounter);
        break;
      default:
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            BulkWorkOrderConstants.INVALID_WORK_ORDER_BULK_PROCESS_TYPE);
    }
  }


  private void validateWorkOrderRequestForAssembly(Map<Integer, String> headers, List<Map<String, String>> data,
      BulkProcess bulkProcess, BulkUploadErrorCounter bulkUploadErrorCounter) throws JsonProcessingException {
    int rowCount = -1;
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    if (!headers.values().containsAll(BulkWorkOrderConstants.ASSEMBLY_HEADERS)) {
      log.error("Invalid headers. Expected : {} uploaded Headers : {} ", BulkWorkOrderConstants.ASSEMBLY_HEADERS,
          headers);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(BulkWorkOrderConstants.HEADER_MISMATCH, BulkWorkOrderConstants.ASSEMBLY_HEADERS));
    }
    for (Map<String, String> row : data) {
      StringBuilder errorMessage = new StringBuilder();
      int inputError = 0, systemError = 0;
      String wareHouseCode = BulkCreationCommonUtil.getWareHouseCodeFromWareHouseCodeAndName(wareHouseNameDelimeter,
          StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.WAREHOUSE)));
      rowCount++;
      if (!ITEM_SKU_PATTERN
          .matcher(org.apache.commons.lang3.StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.ITEM_SKU)))
          .matches()) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (!Optional.ofNullable(row.get(BulkWorkOrderConstants.ITEM_SKU)).orElse(StringUtils.EMPTY)
          .startsWith(bulkProcess.getBusinessPartnerCode())) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (StringUtils.isBlank(wareHouseCode)) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      try {
        if (!(StringUtils.isNotBlank(row.get(BulkWorkOrderConstants.STOCK).trim())
            && Integer.parseInt(row.get(BulkWorkOrderConstants.STOCK).trim()) > 0)) {
          inputError++;
          errorMessage.append(BulkWorkOrderConstants.STOCK_INVALID.get(bulkProcess.getInternationalMerchant()));
        }
      } catch (Exception e) {
        inputError++;
        log.error("Error while Reading stock - row : {} ", rowCount);
        errorMessage.append(BulkWorkOrderConstants.STOCK_NOT_INTEGER.get(bulkProcess.getInternationalMerchant()))
            .append(Constant.COMMA);
      }
      if ((StringUtils.isBlank(row.get(BulkWorkOrderConstants.BUNDLING_TYPE).trim())
          || !BulkWorkOrderConstants.BUNDLING_TYPES.contains(row.get(BulkWorkOrderConstants.BUNDLING_TYPE).trim()))) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.BUNDLING_TYPE_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setBulkProcessId(UUID.randomUUID().toString());
      bulkProcessData.setParentProduct(row.get(BulkWorkOrderConstants.ITEM_SKU).trim());
      bulkProcessData.setInputErrorCount(inputError > 0 ? 1 : 0);
      bulkProcessData.setSystemErrorCount(systemError);
      bulkProcessData.setRowNumber(rowCount);
      bulkProcessData.setBulkRequestData(getBulkRequestData(row, BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE));
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setCreatedDate(new Date());
      bulkProcessData.setStatus(inputError > 0 ? BulkProcessData.STATUS_FAIL : BulkProcessData.STATUS_PENDING);
      bulkProcessData.setErrorMessage(errorMessage.toString());
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessDataList.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
  }

  private String getBulkRequestData(Map<String, String> rowData, String bulkProcessType)
      throws JsonProcessingException {
    String itemSku = null;
    String itemName = null;
    if (bulkProcessType.equals(BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE) || bulkProcessType
        .equals(BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE)) {
      itemSku = StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.ITEM_SKU));
      itemName = StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.ITEM_NAME));
    } else {
      itemSku = StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.SOURCE_ITEM_SKU));
      itemName = StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.SOURCE_ITEM_NAME));
    }
    WorkOrderDataModel workOrderDataModel = new WorkOrderDataModel();
    workOrderDataModel.setBundleType(StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.BUNDLING_TYPE)));
    workOrderDataModel
        .setDestinationItemName(StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.DESTINATION_ITEM_NAME)));
    workOrderDataModel
        .setDestinationItemSku(StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.DESTINATION_ITEM_SKU)));
    workOrderDataModel.setSourceItemName(itemName);
    workOrderDataModel.setStock(rowData.get(BulkWorkOrderConstants.STOCK).trim());
    List<String> childSkus = new ArrayList<>();
    List<String> cogsPercents = new ArrayList<>();
    String childSkusValue = rowData.get(BulkWorkOrderConstants.CHILD_SKUS);
    String cogsValue = rowData.getOrDefault(BulkWorkOrderConstants.COGS, null);
    if (childSkusValue != null) {
      childSkus = Arrays.stream(childSkusValue.split(Constant.COMMA)).map(StringUtils::trimToEmpty)
          .collect(Collectors.toList());
    }
    if (cogsValue != null) {
      cogsPercents =
          Arrays.stream(cogsValue.split(Constant.COMMA)).map(StringUtils::trimToEmpty).collect(Collectors.toList());
    }
    List<ChildSkuAndCogsMapping> childSkuAndCogsMappings = new ArrayList<>();
    if (childSkus.size() == cogsPercents.size() && isValidCogsPercentage(cogsPercents)) {
      for (int i = 0; i < childSkus.size(); i++) {
        if (ITEM_SKU_PATTERN.matcher(org.apache.commons.lang3.StringUtils.trimToEmpty(childSkus.get(i))).matches()) {
          childSkuAndCogsMappings.add(new ChildSkuAndCogsMapping(childSkus.get(i), cogsPercents.get(i)));
        }
      }
    }
    workOrderDataModel.setWarehouseCode(BulkCreationCommonUtil
        .getWareHouseCodeFromWareHouseCodeAndName(wareHouseNameDelimeter,
            StringUtils.trimToEmpty(rowData.get(BulkWorkOrderConstants.WAREHOUSE))));
    workOrderDataModel.setChildSkuAndCogsMapping(childSkuAndCogsMappings);
    workOrderDataModel.setSourceItemSku(itemSku);
    return objectMapper.writeValueAsString(workOrderDataModel);
  }

  private void validateWorkOrderRequestForDisAssembly(Map<Integer, String> headers, List<Map<String, String>> data,
      BulkProcess bulkProcess, BulkUploadErrorCounter bulkUploadErrorCounter) throws JsonProcessingException {
    int rowCount = -1;
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    if (!headers.values().containsAll(BulkWorkOrderConstants.DISASSEMBLY_HEADERS)) {
      log.error("Invalid headers. Expected : {} uploaded Headers : {} ", BulkWorkOrderConstants.DISASSEMBLY_HEADERS,
          headers);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(BulkWorkOrderConstants.HEADER_MISMATCH, BulkWorkOrderConstants.DISASSEMBLY_HEADERS));
    }
    for (Map<String, String> row : data) {
      StringBuilder errorMessage = new StringBuilder();
      int inputError = 0, systemError = 0;
      rowCount++;
      String wareHouseCode = BulkCreationCommonUtil.getWareHouseCodeFromWareHouseCodeAndName(wareHouseNameDelimeter,
          StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.WAREHOUSE)));
      if (!ITEM_SKU_PATTERN
          .matcher(org.apache.commons.lang3.StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.ITEM_SKU)))
          .matches()) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (!Optional.ofNullable(row.get(BulkWorkOrderConstants.ITEM_SKU)).orElse(StringUtils.EMPTY)
          .startsWith(bulkProcess.getBusinessPartnerCode())) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (StringUtils.isBlank(wareHouseCode)) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      try {
        if (!(StringUtils.isNotBlank(row.get(BulkWorkOrderConstants.STOCK).trim())
            && Integer.parseInt(row.get(BulkWorkOrderConstants.STOCK).trim()) > 0)) {
          inputError++;
          errorMessage.append(BulkWorkOrderConstants.STOCK_INVALID.get(bulkProcess.getInternationalMerchant()));
        }
      } catch (Exception e) {
        inputError++;
        log.error("Error while Reading stock - row : {} ", rowCount);
        errorMessage.append(BulkWorkOrderConstants.STOCK_NOT_INTEGER.get(bulkProcess.getInternationalMerchant()))
            .append(Constant.COMMA);
      }
      List<String> childSkus = new ArrayList<>();
      List<String> cogsPercents = new ArrayList<>();
      String childSkusValue = row.get(BulkWorkOrderConstants.CHILD_SKUS);
      String cogsValue = row.get(BulkWorkOrderConstants.COGS);
      if (childSkusValue != null) {
        childSkus = Arrays.stream(childSkusValue.split(Constant.COMMA)).map(StringUtils::trimToEmpty)
            .collect(Collectors.toList());
      }
      if (cogsValue != null) {
        cogsPercents =
            Arrays.stream(cogsValue.split(Constant.COMMA)).map(StringUtils::trimToEmpty).collect(Collectors.toList());
      }
      if (childSkus.size() != cogsPercents.size()) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.COGS_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (!isValidCogsPercentage(cogsPercents)) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.COGS_NOT_SUM_TO_100.get(bulkProcess.getInternationalMerchant()));
      }
      for (String childSku : childSkus) {
        if (!ITEM_SKU_PATTERN.matcher(org.apache.commons.lang3.StringUtils.trimToEmpty(childSku)).matches()) {
          inputError++;
          errorMessage.append(BulkWorkOrderConstants.CHILD_SKU_INVALID.get(bulkProcess.getInternationalMerchant()));
          break;
        }
      }
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setBulkProcessId(UUID.randomUUID().toString());
      bulkProcessData.setInputErrorCount(inputError);
      bulkProcessData.setSystemErrorCount(systemError);
      bulkProcessData.setRowNumber(rowCount);
      bulkProcessData.setParentProduct(row.get(BulkWorkOrderConstants.ITEM_SKU).trim());
      bulkProcessData.setBulkRequestData(getBulkRequestData(row, BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE));
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setCreatedDate(new Date());
      bulkProcessData.setStatus(inputError > 0 ? BulkProcessData.STATUS_FAIL : BulkProcessData.STATUS_PENDING);
      bulkProcessData.setErrorMessage(errorMessage.toString());
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessDataList.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
  }

  private static boolean isValidCogsPercentage(List<String> cogsPercents) {
    int sum = 0;
    if (CollectionUtils.isNotEmpty(cogsPercents)) {
      for (String cogsPercent : cogsPercents) {
        try {
          int cogs = Integer.parseInt(cogsPercent);
          if (cogs < 0)
            return false;
          sum += cogs;
        } catch (Exception e) {
          log.error("Error while Parsing cogs Percent {} for cogsPercentValue {} ", e, cogsPercents);
          return false;
        }
      }
    }
    return sum == 100;
  }

  private void validateWorkOrderRequestForTransferRequest(Map<Integer, String> headers, List<Map<String, String>> data,
      BulkProcess bulkProcess, BulkUploadErrorCounter bulkUploadErrorCounter) throws JsonProcessingException {
    int rowCount = -1;
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    if (!headers.values().containsAll(BulkWorkOrderConstants.TRANSFER_REQUEST_HEADERS)) {
      log.error("Invalid headers. Expected : {} uploaded Headers : {} ",
          BulkWorkOrderConstants.TRANSFER_REQUEST_HEADERS, headers);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(BulkWorkOrderConstants.HEADER_MISMATCH, BulkWorkOrderConstants.TRANSFER_REQUEST_HEADERS));
    }

    for (Map<String, String> row : data) {
      StringBuilder errorMessage = new StringBuilder();
      int inputError = 0, systemError = 0;
      rowCount++;
      String wareHouseCode = BulkCreationCommonUtil.getWareHouseCodeFromWareHouseCodeAndName(wareHouseNameDelimeter,
          StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.WAREHOUSE)));
      if (!ITEM_SKU_PATTERN.matcher(
              org.apache.commons.lang3.StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.SOURCE_ITEM_SKU)))
          .matches()) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.SOURCE_ITEM_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (!Optional.ofNullable(row.get(BulkWorkOrderConstants.SOURCE_ITEM_SKU)).orElse(StringUtils.EMPTY)
          .startsWith(bulkProcess.getBusinessPartnerCode())) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (!ITEM_SKU_PATTERN.matcher(
              org.apache.commons.lang3.StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.DESTINATION_ITEM_SKU)))
          .matches()) {
        inputError++;
        errorMessage.append(
            BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (!Optional.ofNullable(row.get(BulkWorkOrderConstants.DESTINATION_ITEM_SKU)).orElse(StringUtils.EMPTY)
          .startsWith(bulkProcess.getBusinessPartnerCode())) {
        inputError++;
        errorMessage.append(
            BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      if (StringUtils.isBlank(wareHouseCode)) {
        inputError++;
        errorMessage.append(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(bulkProcess.getInternationalMerchant()));
      }
      try {
        if (!(StringUtils.isNotBlank(StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.STOCK)))
            && Integer.parseInt(StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.STOCK))) > 0)) {
          inputError++;
          errorMessage.append(BulkWorkOrderConstants.STOCK_INVALID.get(bulkProcess.getInternationalMerchant()));
        }
      } catch (Exception e) {
        inputError++;
        log.error("Error while Reading stock - row : {} ", rowCount);
        errorMessage.append(BulkWorkOrderConstants.STOCK_NOT_INTEGER.get(bulkProcess.getInternationalMerchant()))
            .append(Constant.COMMA);
      }

      if (StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.SOURCE_ITEM_SKU))
          .equals(StringUtils.trimToEmpty(row.get(BulkWorkOrderConstants.DESTINATION_ITEM_SKU)))) {
        inputError++;
        log.error("Source SKU  and destination SKU  cannot be same");
        errorMessage.append(String.format(BulkWorkOrderConstants.SOURCE_AND_DESTINATION_ITEM_SKU_CANNOT_BE_SAME,
            row.get(BulkWorkOrderConstants.SOURCE_ITEM_SKU), row.get(BulkWorkOrderConstants.DESTINATION_ITEM_SKU)));
      }
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setBulkProcessId(UUID.randomUUID().toString());
      bulkProcessData.setInputErrorCount(inputError);
      bulkProcessData.setSystemErrorCount(systemError);
      bulkProcessData.setRowNumber(rowCount);
      bulkProcessData.setParentProduct(row.get(BulkWorkOrderConstants.SOURCE_ITEM_SKU).trim());
      bulkProcessData.setBulkRequestData(getBulkRequestData(row, BulkWorkOrderConstants.TRANSFER_BULK_PROCESS_TYPE));
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setCreatedDate(new Date());
      bulkProcessData.setStatus(inputError > 0 ? BulkProcessData.STATUS_FAIL : BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setErrorMessage(errorMessage.toString());
      bulkProcessDataList.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
  }

  private BulkUpdateQueue uploadWorkOrderFileAndGetBulkUpdateQueue(String storeId, String requestId,
      BulkUpdateProcessDTO bulkWorkOrderDTO, String bulkProcessCode, String fileName) throws Exception {
    fileStorageService.createBulkFile(bulkWorkOrderDTO, bulkProcessCode, fileName);
    return BulkCreationCommonUtil.getWorkOrderBulkUpdateQueue(storeId, requestId, bulkProcessCode, bulkWorkOrderDTO);
  }

  @Override
  @Async
  @Transactional(readOnly = false)
  public void regenerateTemplateByFileType(String storeId, GenericTemplateFileType genericTemplateFileType,
      String requestId) {
    List<KafkaEventLog> changeEvent = kafkaEventLogService.getKafkaEventLogs();
    LOG.info("Category and Attribute scheduler started for type : {} !!", genericTemplateFileType);
    boolean regenerationRequired =
        regenerateCategoryAttributeMappingInGenericBulkTemplateByFileType(genericTemplateFileType, changeEvent, true);
    if (regenerationRequired) {
      kafkaEventLogService.deleteKafkaEventLogsMarkForDelete(changeEvent);
    }
  }
}
