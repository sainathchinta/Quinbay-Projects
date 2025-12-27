package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.BulkProcessValidationErrorMessages.HEADER_VALIDATION_ERROR;
import static com.gdn.mta.bulk.dto.BulkProcessType.ASSEMBLY_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.CAMPAIGN_ERROR;
import static com.gdn.mta.bulk.dto.BulkProcessType.DISASSEMBLY_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.TRANSFER_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.getBulkProcessType;
import static com.gdn.mta.bulk.util.BulkParameters.SYSTEM_ERROR;
import static com.gdn.mta.bulk.util.BulkParameters.getBasicInfoHeaderList;
import static com.gdn.partners.bulk.util.Constant.BLIBLI_SKU_HEADER;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
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
import java.util.TreeMap;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.BulkUpdateChangeType;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.helper.BulkCampaignProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.helper.BulkProductProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DeletePickupPointResponseEventModel;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.DeleteUpdatePickUpPointResponse;
import com.gdn.mta.bulk.models.download.responsedata.DeleteUpdatePickupPointDataResponse;
import com.gdn.mta.bulk.service.download.BulkProcessFileGeneration;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.FastExcelUtils;
import com.gdn.mta.bulk.util.ResponseHelper;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.dto.FailedItemInfoDto;
import com.gdn.x.campaign.dto.ItemInfoDto;
import com.gdn.x.campaign.dto.ProductCampaignAvailabilityInfoDto;
import com.gdn.x.campaign.response.FailedProductsResponse;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.dhatim.fastexcel.reader.ReadableWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.VatUpdateDto;
import com.gdn.mta.bulk.dto.WholeSaleCount;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.feignConfig.XCampaignFeign;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.repository.campaign.CampaignRepository;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.MasterDataBulkParameters;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.dto.MerchantDto;
import com.gdn.x.campaign.request.CampaignProductRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;


/**
 * Created by virajjasani on 26/07/16.
 */
@Service("ProductLevel3BulkUpdateService")
@Slf4j
public class ProductLevel3BulkUpdateServiceBean implements BulkUpdateService {
  private static final Logger LOGGER = LoggerFactory
      .getLogger(ProductLevel3BulkUpdateServiceBean.class);

  private static final String ERROR_COLUMN = "Error";
  private static final String VAT_UPDATE_ERROR = "Invalid VAT flag";
  private static final String MULTI_PICKUP_POINTS_NOT_ALLOWED_FOR_SELLER =
      "Seller is multip pickuppoint disabled so cannot cannot have more than one pickupPoint";
  private static final String VALIDATION_FAILED_FOR_ALL_ROWS = "Validation for all rows failed for bulk update";
  private static final String X_BULK_CLIENT = "x-bulk";
  private static final ImmutableList<String> VAT_ALLOWED_MERCHANT_TYPES = ImmutableList.of("TD", "TC");
  private static final String FREE_SAMPLE_SHIPPING_ERROR =
      "Action not Allowed : Shipping Cannot be Online for Free sample Products";

  private static final String FREE_SAMPLE_CNC_SHIPPING_ERROR =
    "Action not Allowed : CNC Cannot be Online for Free sample Products";
  private static final String ONLY_EXTERNAL_USER_SHIPPING_ERROR =
      "Action not Allowed : Can not process invalid input data for Shipping";
  private static final String ERROR_MSG = "Can not process invalid input data :";
  private static final String CAMPAIGN_NOTIF_MSG =
    "Penambahan produk promo selesai. Produk berhasil ditambahkan: %s. Produk gagal"
      + " ditambahkan: %s. ";
  private static final String SUBJECT_TO_VAT_NOTIF_MSG =
          "Pembaruan PPN Sudah Selesai. PPN berhasil ditambahkan untuk: %s. Gagal ditambahkan: %s.";

  private static final String FREE_SAMPLE_B2B_ERROR =
    "Action not Allowed : B2B Cannot be Online for Free sample Products";
  private static final int CAMPAIGN_NOTIFICATION_FILE_LENGTH = 8;
  private static final String GENERATE_QR_CODE = "GENERATE_QR_CODE";
  private static final int BULK_UPDATE_FIRST_ROW_INDEX = 4;
  public static final int BULK_BASIC_INFO_INSTORE_SHIPPING_WEIGHT_COLUMN = 20;
  public static final int BULK_BASIC_INFO_SHIPPING_WEIGHT_COLUMN = 19;
  private static final int BASIC_INFO_HEADER_INDEX = 0;
  public static final String INVALID_HEADER_ARCHIVAL_ERROR_MESSAGE =
      "The uploaded Excel file contains invalid or missing headers. Please upload correct "
          + "archival file and try.";

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkProcessHelperFactory bulkProcessHelperFactory;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private BulkProcessFileGeneration bulkProcessFileGeneration;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private CampaignRepository campaignRepository;

  @Autowired
  private XCampaignFeign xCampaignFeign;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkDownloadService bulkDownloadService;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private BulkArchiveService bulkArchiveService;

  @Autowired
  private FileStorageService fileStoreService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Value("${bulk.process.batch.size:100}")
  private int bulkUpdateBatchSize;

  @Value("${bulk.process.batch.size:100}")
  private int bulkArchiveBatchSize;

  @Value("${campaign.availability.batch.size}")
  private int partitionSize;

  @Value("${campaign.validateUpdateDiscountPrice.batch.size}")
  private int campaignValidatePartitionSize;

  @Value("${static.baseUrl}")
  private String staticBaseUrl;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${pricing.multipickuppoint.enabled}")
  private boolean pricingMultiPickupPointEnabled;

  @Value("${call.promo.price.validate}")
  private boolean callPromoPriceValidate;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${pricing.campaign.recommendation.enabled}")
  private boolean pricingCampaignRecommendationEnabled;

  @Value("${generate.instore.error.file}")
  private boolean generateInstoreErrorFile;

  @Value("${error.file.name.size}")
  private int errorFileNameSize;

  @Value("${generate.vat.error.file}")
  private boolean generateVatErrorFile;

  @Value("${gcs.subjectToVat.error.upload.path}")
  private String gcsSubjectToVatErrorPath;

  @Value("${header.validation.check}")
  private boolean headerValidationCheck;

  @Value("${update.product.download.link}")
  private String updateProductDownloadLink;

  @Value("${campaign.final.price.precision.enabled}")
  private boolean campaignFinalPricePrecisionEnabled;

  @Value("${campaign.final.price.precision}")
  private int campaignFinalPricePrecision;

  @Value("${campaign.final.price.rounding.mode}")
  private int campaignFinalPriceRoundingMode;

  @Value("${bulk.update.item.pickup.point.list.fetch.size}")
  private int itemPickupPointListFetchSize;

  @Value("${delete.pickup.point.status.improvement.enabled}")
  private boolean deletePickupPointStatusImprovement;

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

  @Value("${check.for.bulk.update.change.enabled}")
  private boolean checkForBulkUpdateChangeEnabled;

  @Value("${remove.failed.data.from.passed}")
  private boolean removeFailedDataFromPassedData;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${bulk.campaign.error.handling}")
  private boolean bulkCampaignErrorHandling;

  @Value("${bulk.update.template.column.width}")
  private int bulkUpdateTemplateColumnWidth;

  @Value("${validate.warehouse.variant.deletion.enabled}")
  private boolean validateWarehouseVariantDeletionEnabled;

  @Value("${supported.merchant.for.warehouse.stock.validation}")
  private String supportedMerchantsForWarehouseStockValidation;

  @Value("${bulk.update.using.fast.excel.enabled}")
  private boolean bulkUpdateUsingFastExcelEnabled;

  @Value("${bulk.update.fast.excel.columns.to.parse.as.double}")
  private String columnsToParseAsDouble;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Autowired
  private BulkCampaignProductProcessHelper bulkCampaignProductProcessHelper;

  @Autowired
  private BulkProductProcessHelper helper;

  public BulkProcessRepository getBulkProcessRepository() {
    return bulkProcessRepository;
  }

  public void setBulkProcessRepository(BulkProcessRepository bulkProcessRepository) {
    this.bulkProcessRepository = bulkProcessRepository;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public BusinessPartnerRepository getBusinessPartnerRepository() {
    return businessPartnerRepository;
  }

  public void setBusinessPartnerRepository(BusinessPartnerRepository businessPartnerRepository) {
    this.businessPartnerRepository = businessPartnerRepository;
  }

  public ProductLevel3Repository getProductLevel3Repository() {
    return productLevel3Repository;
  }

  public void setProductLevel3Repository(ProductLevel3Repository productLevel3Repository) {
    this.productLevel3Repository = productLevel3Repository;
  }

  public BulkUpdateServiceUtil getBulkUpdateServiceUtil() {
    return bulkUpdateServiceUtil;
  }

  public void setBulkUpdateServiceUtil(BulkUpdateServiceUtil bulkUpdateServiceUtil) {
    this.bulkUpdateServiceUtil = bulkUpdateServiceUtil;
  }

  public CampaignRepository getCampaignRepository() {
    return campaignRepository;
  }

  public void setCampaignRepository(CampaignRepository campaignRepository) {
    this.campaignRepository = campaignRepository;
  }

  private static final Set<BulkProcessType> workOrderTypes = ImmutableSet.of(ASSEMBLY_REQUEST,
    DISASSEMBLY_REQUEST, TRANSFER_REQUEST);

  @Override
  public void preProcessBulkUpdate(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO,
      Set<String> accessiblePickupPoints) throws Exception {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();
    LOGGER.info("invoking preProcess for bulk update. storeId: {}, bulkProcessCode: {}, "
        + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      boolean updatePriorityQueueEnabled = Boolean.parseBoolean(systemParameterConfigService
          .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)
          .getValue());
      boolean isTrustedSeller = false;
      if (updatePriorityQueueEnabled) {
        ProfileResponse profileResponse = businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
            bulkUpdateProcessDTO.getBusinessPartnerCode());
        if (profileResponse.isTrustedSeller()) {
          isTrustedSeller = true;
        }
      }
      BulkUpdateQueue bulkUpdateQueue =
          createFileAndGetBulkUpdateQueue(storeId, requestId, bulkUpdateProcessDTO, bulkProcessCode, fileName,
              isTrustedSeller, updatePriorityQueueEnabled, accessiblePickupPoints);
      if (updatePriorityQueueEnabled && isTrustedSeller) {
        kafkaProducer.send(kafkaTopicProperties.getBulkUpdatePriority1Event(), bulkUpdateQueue);
        LOGGER.info("published {} for bulkProcessCode {} ", kafkaTopicProperties.getBulkUpdatePriority1Event(),
            bulkUpdateQueue.getBulkProcessCode());
      } else {
        kafkaProducer.send(kafkaTopicProperties.getBulkUploadEvent(), bulkUpdateQueue);
      }

      LOGGER.info(
          "preProcessing done. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
              + "{}, businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      LOGGER.error(
          "Error occurred while preProcessing bulk update. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode, e);
      this.trackerService
          .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
              TrackerConstants.HYPHEN, TrackerConstants.FAILED,
              MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  /**
   * save BulkProcess to db after get the default description
   * @param storeId
   * @param requestId
   * @param bulkProcessCode
   * @param bulkAddCampaignProductDTO
   * @param fileName
   */
  private void saveBulkProcessToDb(String storeId, String requestId, String bulkProcessCode,
      BulkAddCampaignProductDTO bulkAddCampaignProductDTO, String fileName) throws Exception {
    BulkProcess bulkProcess = getBulkUpdateServiceUtil()
        .getBulkProcess(storeId, requestId, bulkProcessCode, bulkAddCampaignProductDTO, 0, 0, false, false);
    bulkProcess.setDescription(fileName + BulkUpdateServiceUtil.END_SYMBOL
        + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
    bulkProcess.setUploadedFile(fileName);
    bulkProcess.setPrimaryIdentifier(bulkAddCampaignProductDTO.getCampaignCode());
    savePreProcessBulkData(bulkProcess);
  }

  /**
   * save file excel product campaign into directory
   * @param bulkProcessCode
   * @param fileName
   * @param bulkAddCampaignProductDTO
   * @throws Exception
   */
  private void saveProductCampaignFileToDirectory(String bulkProcessCode, String fileName,
      BulkAddCampaignProductDTO bulkAddCampaignProductDTO) throws Exception {
    BulkUpdateProcessDTO bulkUpdateProcessDTO =
      BulkUpdateProcessDTO.builder().bulkProcessType(BulkProcessType.CAMPAIGN.getValue())
        .fileContent(bulkAddCampaignProductDTO.getFileContent()).build();
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, fileName);
  }

  @Override
  public void preProcessCampaignProductBulkUpdate(String storeId, String requestId,
      BulkAddCampaignProductDTO bulkAddCampaignProductDTO) throws Exception {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkAddCampaignProductDTO.getBusinessPartnerCode();
    LOGGER.info("Invoking pre process for campaign product bulk add with storeId = {}, bulkProcessCode: {}, "
        + "and businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    final String fileName = bulkAddCampaignProductDTO.getFileName();
    try {
      // Save excel file to directory
      this.saveProductCampaignFileToDirectory(bulkProcessCode, fileName, bulkAddCampaignProductDTO);

      // Save BulkProcess to DB
      this.saveBulkProcessToDb(storeId, requestId, bulkProcessCode, bulkAddCampaignProductDTO, fileName);

      // Construct to DTO
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue =
          getBulkUpdateServiceUtil().getBulkAddCampaignProductQueue(storeId, requestId,
              bulkProcessCode, bulkAddCampaignProductDTO);

      // Publish to Kafka
      kafkaProducer.send(kafkaTopicProperties.getBulkUploadCampaignEvent(), bulkAddCampaignProductQueue);

      LOGGER.info(
          "preProcessing done. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
              + "{}, businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      LOGGER.error(
          "Error occurred while preProcessing bulk update. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode, e);
    }
  }

  @Override
  public void processBulkUpdateEvent(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    if (bulkUpdateUsingFastExcelEnabled) {
      processBulkUpdateV2(bulkUpdateQueue);
    } else {
      processBulkUpdate(bulkUpdateQueue);
    }
  }

  @Override
  public void processBulkUpdate(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    LOGGER.info("invoking postProcessing for bulk update. bulkUpdateQueue: {}", bulkUpdateQueue);
    BulkProcess bulkProcess = getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      LOGGER.warn("Bulk update for bulkProcessCode : {} is already processed or being processed",
          bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
              + " is already processed or being processed");
    }
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    int totalProduct;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    int maxRowSize = 0;
    try {
      ProfileResponse profileResponse = fetchBusinessPartnerProfile(storeId, bulkUpdateQueue.getBusinessPartnerCode());
      MerchantStatusType merchantStatusType =
        BulkCreationCommonUtil.getMerchantType(profileResponse);
      boolean includePoQuotaHeaderValidation = preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse);
      Sheet excelSheetData =
        validateBulkUpdateRequest(bulkUpdateQueue, bulkUpdateQueue.getPrivilegedMap(), bulkProcess,
          counter, merchantStatusType, includePoQuotaHeaderValidation);
      POIUtil.validateNumberOfRows(excelSheetData, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);
      List<Map<String, String>> productDataFromExcel =
          POIUtil.readFromExcelForBulkUpdateHavingSellerSku(excelSheetData, BULK_UPDATE_FIRST_ROW_INDEX, 0, 0,
              cncForWarehouseFeatureSwitch, profileResponse, false);
      BulkUpdateServiceUtil.validateRowDataFromExcel(productDataFromExcel);
      totalProduct = productDataFromExcel.size();
      bulkProcess.setTotalCount(totalProduct);
      bulkProcess.setInternationalMerchant(profileResponse.getCompany().isInternationalFlag());
      maxRowSize = this.generateBulkProcessData(bulkProcess, productDataFromExcel, bulkUpdateQueue.getPrivilegedMap(),
        profileResponse);
    } catch (Exception e) {
      handleExceptionForBulkUpdate(bulkUpdateQueue, e, bulkProcess, storeId, bulkProcessCode,
        counter);
    }
  }

  @Override
  public void processBulkUpdateV2(BulkUpdateQueue bulkUpdateQueue) {
    final String storeId = bulkUpdateQueue.getStoreId();
    final String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    LOGGER.info("Invoking postProcessing for bulk update (v2). bulkUpdateQueue: {}",
      bulkUpdateQueue);
    BulkProcess bulkProcess =
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId,
        bulkProcessCode, BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
        "Bulk file with bulk process code: " + bulkUpdateQueue.getBulkProcessCode()
          + " is already processed or being processed");
    }
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    int totalProduct = 0;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    int headerRowIndex = 0;
    int offsetCellsFromLast = 0;
    try (InputStream is = fileStorageServiceBean.openGcsInputStream(bulkUpdateQueue, bulkProcess);
      ReadableWorkbook workbook = new ReadableWorkbook(is)) {
      Map<Integer, String> headers =
        FastExcelUtils.readHeaders(workbook, headerRowIndex, offsetCellsFromLast,
          bulkProcess.getBulkProcessCode());
      Set<String> availableHeaders = new HashSet<>(headers.values());
      ProfileResponse profileResponse =
        fetchBusinessPartnerProfile(storeId, bulkUpdateQueue.getBusinessPartnerCode());
      MerchantStatusType merchantStatusType =
        BulkCreationCommonUtil.getMerchantType(profileResponse);
      boolean includePoQuotaHeaderValidation = preOrderQuotaFeatureSwitch && CommonUtils.isOMGSeller(profileResponse);
      boolean authorized =
        bulkUpdateServiceUtil.authorizeBulkUpdateHeaders(bulkUpdateQueue.getPrivilegedMap(),
          bulkProcess, counter, merchantStatusType, cncForWarehouseFeatureSwitch, headers, storeId,
          bulkProcessCode, bulkUpdateQueue.getBusinessPartnerCode(), availableHeaders, includePoQuotaHeaderValidation);
      GdnPreconditions.checkArgument(authorized,
        BulkErrorCategory.EXCEL_HEADER_ERROR + Constant.HYPHEN_SPACED
          + BulkUpdateServiceUtil.HEADER_MISMATCH);
      Set<String> columnsToParseAsDoubleSet = parseColumnsToParseAsDouble();
      List<Map<String, String>> productDataFromExcel =
        FastExcelUtils.readDataRows(workbook, headers, BULK_UPDATE_FIRST_ROW_INDEX,
          bulkMaxNumberOfRows, validateBulkMaxNumberOfRows, bulkProcess.getBulkProcessCode(), columnsToParseAsDoubleSet);
      BulkUpdateServiceUtil.validateRowDataFromExcel(productDataFromExcel);
      totalProduct = productDataFromExcel.size();
      bulkProcess.setTotalCount(totalProduct);
      bulkProcess.setInternationalMerchant(profileResponse.getCompany().isInternationalFlag());
      this.generateBulkProcessData(bulkProcess, productDataFromExcel,
        bulkUpdateQueue.getPrivilegedMap(), profileResponse);
    } catch (Exception e) {
      handleExceptionForBulkUpdate(bulkUpdateQueue, e, bulkProcess, storeId, bulkProcessCode,
        counter);
    }
  }

  public void handleExceptionForBulkUpdate(BulkUpdateQueue bulkUpdateQueue, Exception e,
    BulkProcess bulkProcess, String storeId, String bulkProcessCode,
    BulkUpdateErrorCounter counter) {
    LOGGER.error("Error while postProcessing for bulk update. bulkUpdateQueue: {} ",
      bulkUpdateQueue, e);
    if (headerValidationCheck) {
      if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
        .contains(BulkUpdateServiceUtil.HEADER_MISMATCH)) {
        bulkProcess.setDescription(HEADER_VALIDATION_ERROR);
        notificationService.sendBulkUploadedNotification(bulkProcess,
          NotificationType.BULK_UPDATED.getValue(),
          fileStorageServiceBean.getDownloadLinkHtml(updateProductDownloadLink));
      } else if (Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
        .contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) || Optional.ofNullable(
          e.getMessage()).orElse(StringUtils.EMPTY)
        .contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN)) {
        bulkProcess.setDescription(
          e.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY));
        notificationService.sendBulkUploadedNotification(bulkProcess,
          NotificationType.BULK_UPDATED.getValue(), StringUtils.EMPTY);
      }
    }
    this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT,
      TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE, TrackerConstants.HYPHEN, TrackerConstants.FAILED,
      bulkUpdateQueue.getUpdatedBy());
    BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode,
      e.getMessage(), counter);
    bulkProcessService.saveOperation(bulkProcess);
  }

  @Override
  public int generateBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> productDataFromExcel,
      Map<String, Boolean> privilegedMap, ProfileResponse profileResponse) throws Exception {
    boolean isInternationalMerchant = bulkProcess.getInternationalMerchant();
    int maxRowProcessingSize = Integer.parseInt(systemParameterConfigService
      .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)
      .getValue());
    boolean updatePriorityQueueEnabled = Boolean.parseBoolean(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED).getValue());
    int trustedSellerMaxRowSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE).getValue());
    int regularSellerMaxRowSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE).getValue());
    int regularSellerMinRowSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE).getValue());
    log.info("Inserting row data : {} {}", bulkProcess.getBulkProcessCode(), productDataFromExcel);
    int rowNumber = 1;
    List<BulkProcessData> requestData = new ArrayList<>();
    for (Map<String, String> userData : productDataFromExcel) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setInputErrorCount(0);
      bulkProcessData.setSystemErrorCount(0);
      bulkProcessData.setRowNumber(rowNumber);
      rowNumber = rowNumber + 1;
      bulkProcessData.setParentProduct(userData.get(BLIBLI_SKU_HEADER));
      userData.put(Constant.PRIVILEGED_MAP, objectMapper.writeValueAsString(privilegedMap));
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(userData));
      requestData.add(bulkProcessData);
    }

    bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(bulkProcess.getBusinessPartnerCode(),
        requestData, false, profileResponse);

    log.info("Saving row data : {}", requestData);
    if (rowNumber > maxRowProcessingSize) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        isInternationalMerchant ?
          (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) :
          (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN) + maxRowProcessingSize);
    }
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    boolean isTrustedSeller = profileResponse.isTrustedSeller();
    bulkProcess.setBulkProcessType(updatePriorityQueueEnabled ?
        getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(bulkProcess.getBulkProcessType(),
            productDataFromExcel.size(), trustedSellerMaxRowSize, regularSellerMinRowSize, regularSellerMaxRowSize,isTrustedSeller) :
        bulkProcess.getBulkProcessType());
    this.bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcessService.saveOperation(bulkProcess);
    return maxRowProcessingSize;
  }

  public static String getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(String bulkProcessType, int userInputRows,
      int trustedSellerRowCount, int regularSellerMinRowCount, int regularSellerMaxCount, boolean isTrustedSeller) {

    if (isTrustedSeller) {
      if (userInputRows <= trustedSellerRowCount) {
        return BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue();
      } else {
        return BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue();
      }
    } else {
      if (userInputRows <= regularSellerMinRowCount) {
        return BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue();
      } else if(userInputRows <= regularSellerMaxCount) {
        return BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue();
      }
      else{
        return bulkProcessType;
      }
    }

  }

  @Override
  public void processBulkUpdateItem(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    BulkProcess bulkProcess = bulkProcessService
        .findByBulkProcessCode(bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    boolean isMultiPickupPointSeller;
    try {
      ProfileResponse profileResponse =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBusinessPartnerCode());
      isMultiPickupPointSeller = ConverterUtil.checkIfMPPIsAllowed(mppAllowedSellers, profileResponse);
      AuditTrailInfo auditTrailInfo = null;
      if (StringUtils.isNotBlank(bulkProcess.getNotes())) {
        try {
          auditTrailInfo = this.objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
        } catch (Exception e) {
          log.error("Error parsing bulk process and it's notes for : {}, error - ", bulkProcess, e);
        }
      }
      Set<String> accessiblePickupPoints = new HashSet<>();
      if (Objects.nonNull(auditTrailInfo)) {
        accessiblePickupPoints = auditTrailInfo.getAccessiblePickupPointCodes();
      }
      LOGGER.info("Start processing for rows : {} for blpCode {}", bulkUpdateEventModel.getRowNumbers(),
          bulkProcess.getBulkProcessCode());
      List<Map<String, String>> productDataFromExcel = new ArrayList<>();
      Map<String, Boolean> privilegedMap = new HashMap<>();
      for (BulkProcessData bulkProcessData : bulkProcessDataList) {
        productDataFromExcel.add(getBulkUpdateServiceUtil().setBlpInitialData(bulkProcessData, privilegedMap));
      }
      bulkProcessDataList = bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
      LOGGER.info("Bulk process code {} for row {} saved as in-progress",
        bulkProcess.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      List<Map<String, String>> validationPassedData = new ArrayList<>();
      List<Map<String, String>> validationFailedData = new ArrayList<>();
      BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
      List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList;
      Set<String> excelPickupPointCodes = productDataFromExcel.stream()
          .map(productDataForRow -> productDataForRow.get(BulkParameters.PICKUP_POINT_HEADER))
          .filter(StringUtils::isNotBlank).collect(Collectors.toSet());
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO =
          getBulkUpdateServiceUtil().validateExcelDatasBulkUpdateProduct(productDataFromExcel,
              getPickUpPoints(bulkProcess.getBusinessPartnerCode(), excelPickupPointCodes), validationPassedData,
              validationFailedData, counter, getMinimumPrice(Constant.STORE_ID), accessiblePickupPoints, true,
              bulkProcess.getBusinessPartnerCode());
      counter.setInputErrorCounter(validationFailedData.size());
      if (!validationFailedData.isEmpty()) {
        this.trackerService
            .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
            TrackerConstants.HYPHEN, TrackerConstants.FAILED, bulkProcess.getCreatedBy());
      }
      if (!isMultiPickupPointSeller && productDataFromExcel.size() != 1) {
        throw new ApplicationException(ErrorCategory.VALIDATION,
          MULTI_PICKUP_POINTS_NOT_ALLOWED_FOR_SELLER);
      }
      bulkUpdateSuccessDTOList =
          getBulkUpdateSuccessDTOList(bulkUpdateEventModel, privilegedMap, listBulkUpdateErrorDTO, validationPassedData,
              counter, isMultiPickupPointSeller, bulkProcess.getCreatedBy(), profileResponse);
      getBulkUpdateServiceUtil().setDataFinalStatusL5(bulkProcessDataList, listBulkUpdateErrorDTO,
        bulkUpdateSuccessDTOList);
      updateScheduleRemovalInBulkProcess(bulkUpdateSuccessDTOList, profileResponse, bulkProcessDataList);
      bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
    } catch (Exception e) {
      getBulkUpdateServiceUtil().setFinalStatusForSystemFailure(bulkProcessDataList, bulkProcess);
      bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
      LOGGER.error("Error while postProcessing for bulk update. bulkUpdateQueue: {}", bulkUpdateEventModel, e);
      this.trackerService
          .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
              TrackerConstants.HYPHEN, TrackerConstants.FAILED, bulkProcess.getCreatedBy());
    }
  }

  private void updateScheduleRemovalInBulkProcess(List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList,
    ProfileResponse profileResponse, List<BulkProcessData> bulkProcessDataList) {
    boolean scheduleRemoval = bulkUpdateSuccessDTOList.stream()
      .anyMatch(successData -> Boolean.TRUE.equals(successData.getScheduleRemoval()));
    boolean isInternational = profileResponse.getCompany().isInternationalFlag();
    String productSku =
      bulkUpdateSuccessDTOList.stream().map(BulkUpdateSuccessDTO::getProductSku).findFirst()
        .orElse(StringUtils.EMPTY);
    productSku = Optional.of(productSku).filter(sku -> sku.contains(Constant.DASH))
      .map(sku -> sku.substring(0, sku.lastIndexOf(Constant.DASH))).orElse(productSku);
    String productName =
      bulkUpdateSuccessDTOList.stream().map(BulkUpdateSuccessDTO::getProductName)
        .filter(StringUtils::isNotBlank).findFirst().orElse(StringUtils.EMPTY);
    getBulkUpdateServiceUtil().setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      scheduleRemoval, isInternational, productSku, productName);
  }

  @Override
  public List<BulkUpdateSuccessDTO> getBulkUpdateSuccessDTOList(BulkUpdateEventModel bulkUpdateEventModel,
      Map<String, Boolean> privilegedMap, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      List<Map<String, String>> validationPassedData, BulkUpdateErrorCounter counter, boolean isMultiPickupPointSeller,
      String createdBy, ProfileResponse profileResponse) throws Exception {
    return processBulkUpdateL5(bulkUpdateEventModel.getBusinessPartnerCode(), privilegedMap, listBulkUpdateErrorDTO,
        validationPassedData, counter, isMultiPickupPointSeller, createdBy, new ArrayList<>(), Constant.CLIENT_ID,
        profileResponse);
  }


  @Override
  public List<String> getPickUpPoints(String businessPartnerCode, Set<String> pickupPointCodes) throws Exception {
    List<PickupPointResponse> pickupPointResponseList = this.pickupPointService
      .getPickupPointSummaryFilter(0,
        PickupPointFilterRequest.builder().businessPartnerCode(businessPartnerCode).codes(pickupPointCodes).build());
    return getPickupPointCodesV2(pickupPointResponseList);
  }

  @Override
  public Integer getMinimumPrice(String storeId) {
    SystemParameterConfig minimumPrice =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.MINIMUM_PRICE);
    if (Objects.nonNull(minimumPrice)) {
      return Integer.parseInt(minimumPrice.getValue());
    }
    return null;
  }

  public ProfileResponse fetchBusinessPartnerProfile(String storeId, String businessPartnerCode) throws Exception {
    ProfileResponse businessPartnerProfile =
        getBusinessPartnerRepository().filterByBusinessPartnerCodeV2(
            storeId, businessPartnerCode);
    if (Objects.isNull(businessPartnerProfile)) {
      LOGGER.error("No business partner found for businessPartnerCode: {}", businessPartnerCode);
      throw new Exception(
          BulkUpdateServiceUtil.BUSINESS_PARTNER_NULL_ERROR);
    }
    return businessPartnerProfile;
  }

  private void updateBulkFinalStatusAndSendNotification(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue,
      int savedProduct, int totalProduct, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      BulkUpdateErrorCounter counter, List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList,
      boolean isInternationalMerchant) {
    bulkProcess.setEndDate(Calendar.getInstance().getTime());
    bulkProcess.setEndDate(new Date());
    bulkProcess.setSuccessCount(savedProduct);
    bulkProcess.setErrorCount(totalProduct - savedProduct);
    bulkProcess.setInputErrorCount(counter.getInputErrorCounter());
    bulkProcess.setSystemErrorCount(counter.getSystemErrorCounter());
    long sameThresholdCount = bulkUpdateSuccessDTOList.stream()
        .filter(bulkUpdateSuccessDTO -> Objects.nonNull(bulkUpdateSuccessDTO.getSameThreshold()))
        .filter(bulkUpdateSuccessDTO -> bulkUpdateSuccessDTO.getSameThreshold()).count();
    long differentThresholdCount = bulkUpdateSuccessDTOList.stream()
        .filter(bulkUpdateSuccessDTO -> Objects.nonNull(bulkUpdateSuccessDTO.getSameThreshold()))
        .filter(bulkUpdateSuccessDTO -> !bulkUpdateSuccessDTO.getSameThreshold()).count();
    try {
      if (CollectionUtils.isEmpty(listBulkUpdateErrorDTO)) {
        if (isInternationalMerchant) {
          bulkProcess.setDescription(BulkUpdateServiceUtil.ALL_PRODUCTS_UPDATED_EN);
        } else {
          bulkProcess.setDescription(BulkUpdateServiceUtil.ALL_PRODUCTS_UPDATED_IN);
        }
        getBulkUpdateServiceUtil().updateBulkProcessNotes(listBulkUpdateErrorDTO, bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), bulkProcess, false, null);
        notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, bulkProcess.getDescription(), false, false);
      } else if (CollectionUtils.isEmpty(bulkUpdateSuccessDTOList)) {
        if (isInternationalMerchant) {
          bulkProcess.setDescription(BulkUpdateServiceUtil.ALL_PRODUCTS_FAILED_EN);
        } else {
          bulkProcess.setDescription(BulkUpdateServiceUtil.ALL_PRODUCTS_FAILED_IN);
        }
        getBulkUpdateServiceUtil().updateBulkProcessNotes(listBulkUpdateErrorDTO, bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), bulkProcess, false, null);
        notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, bulkProcess.getDescription(), false, false);
        return;
      } else if (bulkUpdateSuccessDTOList.size() < totalProduct && sameThresholdCount == 0
          && differentThresholdCount == 0) {
        if (isInternationalMerchant) {
          bulkProcess.setDescription(String
              .format(BulkUpdateServiceUtil.PARTIAL_UPDATE_EN, bulkUpdateSuccessDTOList.size(), totalProduct));
        } else {
          bulkProcess.setDescription(String
              .format(BulkUpdateServiceUtil.PARTIAL_UPDATE_IN, bulkUpdateSuccessDTOList.size(), totalProduct));
        }
        getBulkUpdateServiceUtil().updateBulkProcessNotes(listBulkUpdateErrorDTO, bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), bulkProcess, false, null);
        notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, bulkProcess.getDescription(), false, false);
      }

      if (sameThresholdCount != 0 || differentThresholdCount != 0) {
        if (sameThresholdCount == 0) {
          if (isInternationalMerchant) {
            bulkProcess.setDescription(String
                .format(BulkUpdateServiceUtil.DIFFERENT_THRESHOLD, bulkUpdateSuccessDTOList.size(),
                    differentThresholdCount, listBulkUpdateErrorDTO.size()));
          } else {
            bulkProcess.setDescription(String
                .format(BulkUpdateServiceUtil.DIFFERENT_THRESHOLD_IN, bulkUpdateSuccessDTOList.size(),
                    differentThresholdCount, listBulkUpdateErrorDTO.size()));
          }
        } else if (sameThresholdCount != 0) {
          if (isInternationalMerchant) {
            bulkProcess.setDescription(String
                .format(BulkUpdateServiceUtil.SAME_THRESHOLD, bulkUpdateSuccessDTOList.size(), sameThresholdCount,
                    listBulkUpdateErrorDTO.size()));
          } else {
            bulkProcess.setDescription(String
                .format(BulkUpdateServiceUtil.SAME_THRESHOLD_IN, bulkUpdateSuccessDTOList.size(),
                    sameThresholdCount, listBulkUpdateErrorDTO.size()));
          }
        } else {
          if (isInternationalMerchant) {
            bulkProcess.setDescription(String
                .format(BulkUpdateServiceUtil.BOTH_SAME_AND_DIFFERENT_THRESHOLD, bulkUpdateSuccessDTOList.size(),
                    differentThresholdCount, sameThresholdCount, listBulkUpdateErrorDTO.size()));
          } else {
            bulkProcess.setDescription(String.format(BulkUpdateServiceUtil.BOTH_SAME_AND_DIFFERENT_THRESHOLD_IN,
                bulkUpdateSuccessDTOList.size(), differentThresholdCount, sameThresholdCount,
                listBulkUpdateErrorDTO.size()));
          }
        }
        String downloadPath =
          createWholesaleErrorWorkbook(bulkProcess.getBulkProcessCode(), listBulkUpdateErrorDTO,
            bulkUpdateSuccessDTOList);
        getBulkUpdateServiceUtil().updateBulkProcessNotes(listBulkUpdateErrorDTO, bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), bulkProcess, true,
            new WholeSaleCount(differentThresholdCount, sameThresholdCount, listBulkUpdateErrorDTO.size(),
                downloadPath));
        notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, bulkProcess.getDescription(), false, true);
      }
    } catch (Exception e) {
      LOGGER.error("Error while sending notifications for bulk proccess code : {}, filename: {}",
          bulkProcess.getBulkProcessCode(), bulkUpdateQueue.getFileName(), e);
    }
  }

  private String createWholesaleErrorWorkbook(String bulkProcessCode,
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList) throws Exception {
    List<List<String>> xlData = new ArrayList<>();
    List<BulkUpdateSuccessDTO> updateWholesale = bulkUpdateSuccessDTOList.stream()
        .filter(bulkUpdateSuccessDTO -> Objects.nonNull(bulkUpdateSuccessDTO.getSameThreshold()))
        .filter(bulkUpdateSuccessDTO -> bulkUpdateSuccessDTO.getSameThreshold()).collect(Collectors.toList());
    List<BulkUpdateSuccessDTO> turnOffWholesale = bulkUpdateSuccessDTOList.stream()
        .filter(bulkUpdateSuccessDTO -> Objects.nonNull(bulkUpdateSuccessDTO.getSameThreshold()))
        .filter(bulkUpdateSuccessDTO -> !bulkUpdateSuccessDTO.getSameThreshold()).collect(Collectors.toList());
    for (BulkUpdateSuccessDTO bulkUpdateSuccessDTO : updateWholesale) {
      List<String> cellValue = new ArrayList<>();
      cellValue.add(bulkUpdateSuccessDTO.getProductSku());
      cellValue.add(bulkUpdateSuccessDTO.getPickupPointCode());
      cellValue.add(ProductUpdateErrorMessages.WHOLE_SALE_UPDATED);
      xlData.add(cellValue);
    }
    for (BulkUpdateSuccessDTO bulkUpdateSuccessDTO : turnOffWholesale) {
      List<String> cellValue = new ArrayList<>();
      cellValue.add(bulkUpdateSuccessDTO.getProductSku());
      cellValue.add(bulkUpdateSuccessDTO.getPickupPointCode());
      cellValue.add(ProductUpdateErrorMessages.WHOLE_SALE_TURN_OFF);
      xlData.add(cellValue);
    }
    for (BulkUpdateErrorDTO bulkUpdateErrorDTO : listBulkUpdateErrorDTO) {
      List<String> cellValue = new ArrayList<>();
      cellValue.add(bulkUpdateErrorDTO.getProductSku());
      cellValue.add(bulkUpdateErrorDTO.getPickupPointCode());
      cellValue.add(bulkUpdateErrorDTO.getReason());
      xlData.add(cellValue);
    }
    if (CollectionUtils.isEmpty(xlData)) {
      return StringUtils.EMPTY;
    }
    return bulkDownloadService.generateWholeSaleErrorWorkbookBulkDownload(bulkProcessCode, xlData);
  }

  private void sendSecondaryNotificationForDiscountAndPromo(List<BulkUpdateSuccessDTO> campaignMappedProductUpdate,
      List<BulkUpdateSuccessDTO> aboveMaxDiscountProduct, List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList,
      BulkProcess bulkProcess, boolean isInternationalFlag) throws JsonProcessingException{
    if (CollectionUtils.isNotEmpty(campaignMappedProductUpdate) && CollectionUtils
        .isEmpty(aboveMaxDiscountProduct)) {
      String description;
      if(isInternationalFlag) {
        description = String
            .format(BulkUpdateServiceUtil.PROMO_PRODUCTS_UPDATED_EN, campaignMappedProductUpdate.size(),
                bulkUpdateSuccessDTOList.size());
      } else {
        description = String
            .format(BulkUpdateServiceUtil.PROMO_PRODUCTS_UPDATED_IN, campaignMappedProductUpdate.size(),
                bulkUpdateSuccessDTOList.size());
      }
      for (BulkUpdateSuccessDTO bulkUpdateSuccessDTO : campaignMappedProductUpdate) {
        getBulkUpdateServiceUtil()
            .updateBulkProcessNotesForPromoRegistered(bulkProcess.getStoreId(), bulkUpdateSuccessDTO,
                bulkProcess);
      }
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, description, true, false);
    } else if (CollectionUtils.isNotEmpty(campaignMappedProductUpdate) && CollectionUtils
        .isNotEmpty(aboveMaxDiscountProduct)) {
      String description;
      if(isInternationalFlag) {
        description = String
            .format(BulkUpdateServiceUtil.PROMO_UPDATED_WITH_MAX_DISCOUNT_EN, campaignMappedProductUpdate.size(),
                bulkUpdateSuccessDTOList.size());
      } else {
        description = String
            .format(BulkUpdateServiceUtil.PROMO_UPDATED_WITH_MAX_DISCOUNT_IN, campaignMappedProductUpdate.size(),
                bulkUpdateSuccessDTOList.size());
      }
      for (BulkUpdateSuccessDTO bulkUpdateSuccessDTO : campaignMappedProductUpdate) {
        getBulkUpdateServiceUtil()
            .updateBulkProcessNotesForPromoRegistered(bulkProcess.getStoreId(), bulkUpdateSuccessDTO,
                bulkProcess);
      }
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, description, true, false);
    } else if (CollectionUtils.isEmpty(campaignMappedProductUpdate) && CollectionUtils
        .isNotEmpty(aboveMaxDiscountProduct)) {
      String description;
      if(isInternationalFlag) {
        description = String
            .format(BulkUpdateServiceUtil.PRODUCT_UPDATED_WITH_MAX_DISCOUNT_EN, aboveMaxDiscountProduct.size(),
                bulkUpdateSuccessDTOList.size());
      } else {
        description = String
            .format(BulkUpdateServiceUtil.PRODUCT_UPDATED_WITH_MAX_DISCOUNT_IN, aboveMaxDiscountProduct.size(),
                bulkUpdateSuccessDTOList.size());
      }
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, description, false, false);
    }
  }

  private List<BulkUpdateErrorDTO> validateProductCampaignDataFromExcel(
      List<Map<String, String>> productDataFromExcel, int totalCampaignProduct,
      BulkProcess bulkProcess, BulkUpdateErrorCounter bulkUpdateErrorCounter,
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue,
      List<Map<String, String>> validationPassedData,
      List<Map<String, String>> validationFailedData) {
    BulkUpdateServiceUtil.validateRowDataFromExcel(productDataFromExcel);
    bulkProcess.setTotalCount(totalCampaignProduct);
    // Validate value in every excel field
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = getBulkUpdateServiceUtil()
        .validateExcelDatasBulkUpdateCampaignProduct(productDataFromExcel, validationPassedData,
            validationFailedData, bulkUpdateErrorCounter, bulkAddCampaignProductQueue);
    bulkUpdateErrorCounter.setInputErrorCounter(validationFailedData.size());
    return bulkUpdateErrorDTOList;
  }

  @Override
  public void processCampaignProductBulkUpdate(
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue) throws Exception {
    LOGGER.info(
        "Invoking post processing for campaign product bulk add with bulkAddCampaignProductQueue = {}",
        bulkAddCampaignProductQueue);

    String storeId = bulkAddCampaignProductQueue.getStoreId();
    String bulkProcessCode = bulkAddCampaignProductQueue.getBulkProcessCode();

    // Get Bulk Process by Bulk Process Code from DB
    BulkProcess bulkProcess = getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      LOGGER.warn(
          "Bulk campaign update for bulkProcessCode : {} is already processed or being processed",
          bulkAddCampaignProductQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkAddCampaignProductQueue.getBulkProcessCode()
              + " is already processed or being processed");
    }
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setNotes(objectMapper.writeValueAsString(bulkAddCampaignProductQueue));
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    int savedCampaignProduct = 0;
    int totalCampaignProduct = 0;

    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    try {
      // Read excel file
      Sheet excelSheetData = validateBulkCampaignProductUpdateRequest(bulkAddCampaignProductQueue,
              bulkProcessCode, bulkAddCampaignProductQueue.getFileName(), bulkProcess, bulkUpdateErrorCounter);
      POIUtil.validateNumberOfRows(excelSheetData, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);

      // Start from 5th row
      List<Map<String, String>> productDataFromExcel = POIUtil
          .readFromExcelForBulkUpdate(excelSheetData, Constant.CAMPAIGN_BULK_DATA_START,
              Constant.CAMPAIGN_BULK_HEADER_INDEX, 1, new HashMap<>());
      totalCampaignProduct = productDataFromExcel.size();

      //if switch is true, will follow the new workflow, else existing workflow
      SystemParameterConfig bulkSwitchParameter = systemParameterConfigService
          .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
      boolean bulkSwitch = false;
      if (Objects.nonNull(bulkSwitchParameter)) {
        bulkSwitch = Boolean.valueOf(bulkSwitchParameter.getValue());
      }
      if (bulkSwitch) {
        saveBulkProcessData(bulkProcess, productDataFromExcel);
        return;
      }

      List<Map<String, String>> validationPassedData = new ArrayList<>();
      List<Map<String, String>> validationFailedData = new ArrayList<>();
      bulkUpdateErrorDTOList = this.validateProductCampaignDataFromExcel(productDataFromExcel,
          totalCampaignProduct, bulkProcess, bulkUpdateErrorCounter, bulkAddCampaignProductQueue,
          validationPassedData, validationFailedData);

      // if there are failed datas, system will delete it dan create a file contains of it
      if (!validationFailedData.isEmpty()) {
        BulkUpdateServiceUtil.deletePassedDataFromExcel(excelSheetData, validationPassedData,
            bulkProcessCode, bulkAddCampaignProductQueue.getFileName(),
            ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR);
      }

      savedCampaignProduct = this.processBulkCreateCampaign(bulkUpdateErrorDTOList,
          validationPassedData, bulkUpdateErrorCounter, bulkAddCampaignProductQueue);

      String status = BulkProcess.STATUS_ABORTED;
      if(validationPassedData.size() == totalCampaignProduct){
        status = BulkProcess.STATUS_FINISHED;
      }else if(CollectionUtils.isNotEmpty(validationPassedData)){
        status = BulkProcess.STATUS_PARTIALLY_DONE;
      }

      bulkProcess.setStatus(status);

    } catch (Exception e) {
      LOGGER.error(
          "Error while post processing for campaign product bulk add with bulkAddCampaignProductQueue = {}",
          bulkAddCampaignProductQueue, e);
      BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode,
          e.getMessage(), bulkUpdateErrorCounter);
    }
    List<String> descriptions =
        Lists.newArrayList(BulkUpdateServiceUtil.PRODUCTS_CAMPAIGN_ADD,
            BulkUpdateServiceUtil.PRODUCTS_CAMPAIGN_SUCCESSFULLY_ADDED,
            BulkUpdateServiceUtil.PRODUCTS_CAMPAIGN_NOT_ADDED);
    getBulkUpdateServiceUtil().updateBulkProductFinalStatus(bulkProcess,
        bulkAddCampaignProductQueue, savedCampaignProduct, totalCampaignProduct,
        bulkUpdateErrorDTOList, bulkUpdateErrorCounter, descriptions);
    bulkProcessService.saveOperation(bulkProcess);
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess, StringUtils.EMPTY, false, false);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> dataMaps) throws Exception {
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
      String parent = StringUtils.EMPTY;
      if (multiPickupPointEnabled) {
        if (StringUtils.isBlank(dataMap.get(BulkParameters.BLIBLI_SKU))) {
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setInputErrorCount(1);
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new BulkUpdateErrorDTO(bulkProcessData.getParentProduct(),
              BulkUpdateServiceUtil.BLIBLI_SKU_BLANK)));
          parent = bulkProcess.getBulkProcessCode() + rowNumber;
        } else if (StringUtils.isBlank(dataMap.get(BulkParameters.PICKUP_POINT_CODE))) {
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setInputErrorCount(1);
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new BulkUpdateErrorDTO(bulkProcessData.getParentProduct(),
              BulkUpdateServiceUtil.BLIBLI_SKU_PP_CODE_BLANK)));
          parent = bulkProcess.getBulkProcessCode() + rowNumber;
        } else {
          parent = dataMap.get(BulkParameters.BLIBLI_SKU) + Constant.DASH + dataMap
            .get(BulkParameters.PICKUP_POINT_CODE);
        }
      }
      else {
        parent = dataMap.get(BulkParameters.BLIBLI_SKU);
      }
      if (StringUtils.isBlank(parent)) {
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setInputErrorCount(1);
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
          new BulkUpdateErrorDTO(bulkProcessData.getParentProduct(),
            BulkUpdateServiceUtil.BLIBLI_SKU_BLANK)));
        parent = bulkProcess.getBulkProcessCode() + rowNumber;
      }
      bulkProcessData.setParentProduct(parent);
      bulkProcessData.setRowNumber(rowNumber);
      rowNumber = rowNumber + 1;
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(dataMap));
      BulkUpdateServiceUtil.addToMap(parent, bulkProcessData, distinctParentMap);
    }
    List<BulkProcessData> requestData = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(distinctParentMap, requestData, true, false);
    bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setTotalCount(rowNumber - 1);
    if (errorCount == bulkProcess.getTotalCount()) {
      bulkProcess.setStatus(BulkProcess.STATUS_PROCESSED);
    }
    bulkProcessService.saveOperation(bulkProcess);
  }

  @Override
  public void preProcessBulkArchiveItems(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO) {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();
    LOGGER.info("invoking preProcess for bulk archive. storeId: {}, bulkProcessCode: {}, "
        + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      BulkUpdateQueue bulkUpdateQueue =
      createFileAndGetBulkUpdateQueue(storeId, requestId, bulkUpdateProcessDTO, bulkProcessCode, fileName, false,
          false, null);
      kafkaProducer.send(kafkaTopicProperties.getBulkArchiveItems(), bulkUpdateQueue);
      LOGGER.info(
          "preProcessing done for bulk archive. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
              + "{}, businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      LOGGER.error(
          "Error occurred while preProcessing bulk archive. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode, e);
      this.trackerService
          .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
              TrackerConstants.HYPHEN, TrackerConstants.FAILED,
              MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  @Override
  public void preProcessBulkArchiveProducts(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO) {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();
    LOGGER.info("invoking preProcess for bulk archive productSkus. storeId: {}, bulkProcessCode: {}, "
        + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      bulkUpdateProcessDTO.setBulkProcessType(Constant.ARCHIVE_BULK_PROCESS_TYPE);
      BulkUpdateQueue bulkUpdateQueue =
          createFileAndGetBulkUpdateQueue(storeId, requestId, bulkUpdateProcessDTO, bulkProcessCode, fileName, false,
              false, null);
      kafkaProducer.send(kafkaTopicProperties.getBulkArchiveProducts(), bulkUpdateQueue);
      LOGGER.info(
          "preProcessing done for bulk archive productSkus. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
              + "{}, businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      LOGGER.error("Error occurred while preProcessing bulk archive productSkus. storeId: {}, bulkProcessCode: {}, "
          + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode, e);
      this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED,
          MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  @Override
  public void preProcessBulkUpdateOff2On(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO) {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String businessPartnerCode = bulkUpdateProcessDTO.getBusinessPartnerCode();
    LOGGER.info("invoking preProcess for bulk update off2On. storeId: {}, bulkProcessCode: {}, "
        + "businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    final String fileName = bulkUpdateProcessDTO.getFileName();
    try {
      BulkUpdateQueue bulkUpdateQueue =
      createFileAndGetBulkUpdateQueue(storeId, requestId, bulkUpdateProcessDTO, bulkProcessCode, fileName, false,
          false, null);
      kafkaProducer.send(kafkaTopicProperties.getBulkUpdateOff2On(), bulkUpdateQueue);
      LOGGER.info(
          "preProcessing done for bulk off2on update. Sent object for Queue processing. storeId: {}, bulkProcessCode: "
              + "{}, businessPartnerCode: {}", storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      LOGGER.error(
          "Error occurred while preProcessing bulk update odff2on. storeId: {}, bulkProcessCode: {}, "
              + "businessPartnerCode: {} ", storeId, bulkProcessCode, businessPartnerCode, e);
      this.trackerService
          .sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
              TrackerConstants.HYPHEN, TrackerConstants.FAILED,
              MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    }
  }

  private BulkUpdateQueue createFileAndGetBulkUpdateQueue(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO, String bulkProcessCode, String fileName, boolean isTrustedSeller,
      boolean updatePriorityQueueEnabled, Set<String> accessiblePickupPoints) throws Exception {
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, fileName);
    BulkProcess bulkProcess = getBulkUpdateServiceUtil()
        .getBulkProcess(storeId, requestId, bulkProcessCode, bulkUpdateProcessDTO, 0, 0, isTrustedSeller,
            updatePriorityQueueEnabled);
    bulkProcess.setDescription(new StringBuilder(fileName)
        .append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING).toString());
    bulkProcess.setUploadedFile(fileName);
    if (CollectionUtils.isNotEmpty(accessiblePickupPoints)) {
      AuditTrailInfo auditTrailInfo =
          AuditTrailInfo.builder().accessiblePickupPointCodes(accessiblePickupPoints).build();
      bulkProcess.setNotes(this.objectMapper.writeValueAsString(auditTrailInfo));
    }
    savePreProcessBulkData(bulkProcess);
    return getBulkUpdateServiceUtil().getBulkUpdateQueue(storeId, requestId, bulkProcessCode,
        bulkUpdateProcessDTO);
  }

  @Override
  public void processBulkArchiveProducts(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    LOGGER.info("Post processing for bulk archive products . BulkUpdateQueue {}", bulkUpdateQueue);
    String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    String storeId = bulkUpdateQueue.getStoreId();
    BulkProcess bulkProcess = getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      LOGGER.warn(
          "Bulk archive products for bulkProcessCode : {} is already processed or being processed",
          bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
              + " is already processed or being processed");
    }
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setStartDate(new Date());
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    int maxArchiveRowsCount =
      Integer.parseInt(this.systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE).getValue());
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean bulkArchiveImplementationSwitch = Boolean.parseBoolean(
      this.systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION).getValue());
    Sheet bulkArchiveSheet = null;
    boolean maxRowsExceeded = false;
    boolean isInternationalMerchant = isInternationalMerchant(storeId, bulkProcess);
    if (bulkArchiveImplementationSwitch) {
      try {
        bulkArchiveSheet =
          validateBulkProductsArchiveRequest(bulkUpdateQueue, bulkProcess, counter);
        maxRowsExceeded = bulkArchiveSheet.getLastRowNum() >= maxArchiveRowsCount;
        if (maxRowsExceeded) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, isInternationalMerchant ?
            (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) :
            (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN) + maxArchiveRowsCount);
        }
      } catch (Exception ex) {
        logErrorAndUpdateBulkProcessStatus(storeId, bulkProcess, ex);
        updateFinalStatusAndSendNotification(bulkProcess, bulkUpdateQueue, 0, 0, counter,
          isInternationalMerchant, maxRowsExceeded, maxArchiveRowsCount);
        return;
      }
      bulkProcess.setDescription(bulkUpdateQueue.getFileName());
      this.bulkArchiveService.addProcessDataAndUpdateProcess(storeId, bulkArchiveSheet,
        bulkProcess);
    } else {
      int totalProducts = 0;
      int archivedProductsCount = 0;
      List<String> failedProductSkus = new ArrayList<>();
      List<Map<String, String>> successData = new ArrayList<>();
      List<Map<String, String>> failureData = new ArrayList<>();
      List<String> failedProductSkusInValidation = new ArrayList<>();
      try {
        AuditTrailInfo auditTrailInfo = getAuditTrailInfo(bulkUpdateQueue);
        bulkArchiveSheet =
          validateBulkProductsArchiveRequest(bulkUpdateQueue, bulkProcess, counter);
        maxRowsExceeded = bulkArchiveSheet.getLastRowNum() >= maxArchiveRowsCount;
        List<String> productSkus =
          getProductSkus(bulkArchiveSheet, BulkParameters.BLIBLI_PRODUCT_SKU);
        totalProducts = productSkus.size();
        bulkProcess.setTotalCount(totalProducts);
        List<String> validProductSkus =
          validateProductSku(bulkProcess, failedProductSkusInValidation, productSkus);
        counter.setInputErrorCounter(productSkus.size() - validProductSkus.size());
        // Call PBP to archive products
        archivedProductsCount =
          bulkArchiveProductSkus(validProductSkus, auditTrailInfo, counter, failedProductSkus);
        checkBulkStatusAndDeleteFile(bulkProcessCode, bulkProcess, totalProducts,
          archivedProductsCount);
        createErrorFile(bulkUpdateQueue, bulkProcess, counter, failedProductSkus, successData,
          failureData, failedProductSkusInValidation, bulkArchiveSheet,
          BulkParameters.BLIBLI_PRODUCT_SKU);
      } catch (Exception ex) {
        logErrorAndUpdateBulkProcessStatus(storeId, bulkProcess, ex);
      }
      updateFinalStatusAndSendNotification(bulkProcess, bulkUpdateQueue, archivedProductsCount,
        totalProducts, counter, isInternationalMerchant, maxRowsExceeded, maxArchiveRowsCount);
    }
  }

  void logErrorAndUpdateBulkProcessStatus(String storeId, BulkProcess bulkProcess, Exception ex) {
    LOGGER.error("Error occurred while processing bulk archive request : storeId {} ,"
        + "bulkProcessCode {} , businessPartnerCode {}", storeId, bulkProcess.getBulkProcessCode(),
      bulkProcess.getBusinessPartnerCode(), ex);
    this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT,
      TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE, TrackerConstants.HYPHEN,
      TrackerConstants.FAILED,
      MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
  }

  void updateFinalStatusAndSendNotification(BulkProcess bulkProcess,
    BulkUpdateQueue bulkUpdateQueue, int archivedProductsCount, int totalProducts,
    BulkUpdateErrorCounter counter, boolean isInternationalMerchant, boolean maxRowsExceeded,
    int maxArchiveRowsCount) throws Exception {
    updateBulkArchiveFinalStatus(bulkProcess, bulkUpdateQueue, archivedProductsCount, totalProducts,
      Collections.emptyList(), counter, isInternationalMerchant);
    if (maxRowsExceeded) {
      bulkProcess.setDescription(isInternationalMerchant ? (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) :
        (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN) + maxArchiveRowsCount);
    }
    if (counter.getHeaderValidationCounter() > 0) {
      bulkProcess.setDescription(INVALID_HEADER_ARCHIVAL_ERROR_MESSAGE);
    }
    LOGGER.info("PostProcessing for bulk archive successfully completed. bulkUpdateQueue: {}",
      bulkUpdateQueue);
    notificationService.sendNotification(bulkProcess, NotificationType.BULK_ARCHIVED.getValue(),
        StringUtils.equals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcess.getStatus()));
  }

  @Override
  public void processBulkVatUpdate(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    LOGGER.info("Post processing for bulk vat update . BulkUpdateQueue {}", bulkUpdateQueue);
    String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    String storeId = bulkUpdateQueue.getStoreId();

    BulkProcess bulkProcess = getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcess.STATUS_PENDING);

    //check if entry is present in db
    if (Objects.isNull(bulkProcess)) {
      LOGGER.warn("Bulk vat update for bulkProcessCode : {} is already processed or being processed",
          bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
              + " is already processed or being processed");
    }

    //update the bulk process state to in progress
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess.setStartDate(new Date());
    bulkProcess.setNotes(bulkUpdateQueue.getFileName());
    bulkProcess.setUploadedFile(bulkUpdateQueue.getFileName());
    bulkProcess = bulkProcessRepository.save(bulkProcess);

    //initialise variables
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    int totalItemCodes = 0;
    int vatUpdatedCount = 0;
    List<BatchVatUpdateResponse> failedItemCodes = new ArrayList<>();
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    List<VatUpdateDto> failedVatUpdateDtoInValidation = new ArrayList<>();
    boolean isInternationalMerchant = false;

    // start processing the sheet
    try {
      isInternationalMerchant = isInternationalMerchant(storeId, bulkProcess);
      AuditTrailInfo auditTrailInfo = getAuditTrailInfo(bulkUpdateQueue);
      Sheet bulkVatUpdateSheet = validateBulkVatUpdateRequest(bulkUpdateQueue, bulkProcess, counter);
      POIUtil.validateNumberOfRows(bulkVatUpdateSheet, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);

      List<VatUpdateDto> vatUpdateDtoList =
          getSkuCodeAndVatFlagFromSheet(bulkVatUpdateSheet, BulkParameters.SKU_CODE_HEADER,
              BulkParameters.SUBJECT_TO_VAT);


      boolean bulkSwitch = Boolean.parseBoolean(
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH).getValue());
      if (bulkSwitch) {
        saveBulkProcessDataForVatUpdate(bulkProcess, vatUpdateDtoList);
        return;
      }

      totalItemCodes = vatUpdateDtoList.size();
      bulkProcess.setTotalCount(totalItemCodes);

      List<VatUpdateDto> validVatUpdateDtoList = validateVatUpdateDto(failedVatUpdateDtoInValidation, vatUpdateDtoList);
      counter.setInputErrorCounter(vatUpdateDtoList.size() - validVatUpdateDtoList.size());

      // Call PCB to update vat flag
      vatUpdatedCount = bulkVatUpdate(validVatUpdateDtoList, auditTrailInfo, counter, failedItemCodes, false);

      // update final status and generate error file
      checkBulkStatusAndDeleteFile(bulkProcessCode, bulkProcess, totalItemCodes, vatUpdatedCount);
      createVatUpdateErrorFile(bulkUpdateQueue, bulkProcess, failedItemCodes, successData, failureData,
          failedVatUpdateDtoInValidation.stream().map(VatUpdateDto::getItemCode).collect(Collectors.toList()),
          bulkVatUpdateSheet, BulkParameters.SKU_CODE_HEADER);
    } catch (Exception ex) {
      LOGGER.error("Error occurred while processing bulk vat update request : storeId {} ,"
              + "bulkProcessCode {} , businessPartnerCode {}", storeId, bulkProcessCode,
          bulkProcess.getBusinessPartnerCode(), ex);
      this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED, MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    }
    updateBulkVatUpdateFinalStatus(bulkProcess, bulkUpdateQueue, vatUpdatedCount, totalItemCodes,
        Collections.emptyList(), counter, isInternationalMerchant);
    LOGGER.info("PostProcessing for bulk vat update successfully completed. bulkUpdateQueue: {}", bulkUpdateQueue);
    notificationService.sendNotification(bulkProcess, NotificationType.VAT_UPDATE.getValue(),
        StringUtils.equals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcess.getStatus()));
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveBulkProcessDataForVatUpdate(BulkProcess bulkProcess, List<VatUpdateDto> vatUpdateDtoList)
      throws Exception {
    log.info("Inserting data rows for bulk process code = {} ", bulkProcess.getBulkProcessCode());
    int rowNumber = 1;
    List<BulkProcessData> requestData = new ArrayList<>();
    for (VatUpdateDto vatUpdateDto : vatUpdateDtoList) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setRowNumber(rowNumber);
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(vatUpdateDto));
      rowNumber += 1;
      requestData.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setTotalCount(rowNumber - 1);
    bulkProcessService.saveOperation(bulkProcess);
  }

  private Sheet validateBulkVatUpdateRequest(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess,
      BulkUpdateErrorCounter counter) throws IOException {
    Sheet excelSheetData = getVatExcelSheetData(bulkUpdateQueue, bulkProcess);
    GdnPreconditions.checkArgument(getBulkUpdateServiceUtil()
            .validateExcelFileForBulkVatUpdate(excelSheetData, bulkProcess, bulkUpdateQueue, counter),
        BulkErrorCategory.EXCEL_HEADER_ERROR + " - " + BulkUpdateServiceUtil.HEADER_MISMATCH);
    return excelSheetData;
  }

  private List<VatUpdateDto> getSkuCodeAndVatFlagFromSheet(Sheet bulkVatSheet, String skuCodeColumnName,
      String vatFlagColumnName) {
    return POIUtil.readFromExcelForBulkUpdate(bulkVatSheet, 1, 0, 0, new HashMap<>()).parallelStream().map(
        excelRow -> new VatUpdateDto(excelRow.get(skuCodeColumnName),
            excelRow.get(vatFlagColumnName).split(Constant.DOT_REGEX)[0])).distinct().collect(Collectors.toList());
  }

  private List<VatUpdateDto> validateVatUpdateDto(
      List<VatUpdateDto> failedVatUpdateDtoInValidation, List<VatUpdateDto> vatUpdateDtoList) {
    List<VatUpdateDto> validVatUpdateDtoList = new ArrayList<>();
    for (VatUpdateDto vatUpdateDto : vatUpdateDtoList) {
      if (BulkUpdateServiceUtil.validateVatUpdateDto(vatUpdateDto)) {
        validVatUpdateDtoList.add(vatUpdateDto);
      } else {
        failedVatUpdateDtoInValidation.add(vatUpdateDto);
      }
    }
    return validVatUpdateDtoList;
  }

  private int bulkVatUpdate(List<VatUpdateDto> validVatUpdateDtoList, AuditTrailInfo auditTrailInfo,
      BulkUpdateErrorCounter counter, List<BatchVatUpdateResponse> failedItemCodes, boolean validateItemCode) throws ApplicationException {
    int successCount = 0;
    int bulkVatUpdateBatchSize = Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)
        .getValue());
    for (List<VatUpdateDto> vatUpdateDtos : Lists.partition(validVatUpdateDtoList, bulkVatUpdateBatchSize)) {
      Map<String, Boolean> vatFlagUpdateMap = vatUpdateDtos.stream().collect(Collectors
          .toMap(oldValue -> oldValue.getItemCode(),
              oldValue -> BulkParameters.ENABLE_VALUE.equals(oldValue.getVatApplicable()),
              (oldValue, newValue) -> newValue));
      Map<String, Boolean> finalVatFlagUpdateMap =
          validateMerchantTypeBySkuCode(failedItemCodes, vatFlagUpdateMap, auditTrailInfo.getBusinessPartnerCode(),
              auditTrailInfo.getUsername(), auditTrailInfo.getRequestId(), validateItemCode);
      if (!finalVatFlagUpdateMap.isEmpty()) {
        List<BatchVatUpdateResponse> response = pcbOutboundService
            .batchVatFlagUpdate(auditTrailInfo.getUsername(), auditTrailInfo.getBusinessPartnerCode(),
                finalVatFlagUpdateMap);
        if (Objects.nonNull(response)) {
          successCount += finalVatFlagUpdateMap.size() - response.size();
          counter.setSystemErrorCounter(counter.getSystemErrorCounter() + response.size());
          failedItemCodes.addAll(response);
        }
      }
    }
    return successCount;
  }

  private Map<String, Boolean> validateMerchantTypeBySkuCode(List<BatchVatUpdateResponse> batchVatUpdateResponseList,
      Map<String, Boolean> vatFlagUpdateMap, String businessPartnerCode, String username, String requestId,
      boolean validateItemCode) {
    List<ItemCodeDetailResponse> itemCodeDetailResponses = xProductOutboundService
        .getItemDetailsByItemCodes(requestId, username, new SimpleSetStringRequest(vatFlagUpdateMap.keySet()));
    Map<String, Boolean> finalVatUpdatemap = new HashMap<>();
    List<String> errorItemCodes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemCodeDetailResponses)) {
      for (ItemCodeDetailResponse itemCodeDetailResponse : itemCodeDetailResponses) {
        if (!itemCodeDetailResponse. getMerchantCode().equals(businessPartnerCode) && !finalVatUpdatemap.keySet()
            .contains(itemCodeDetailResponse.getItemCode()) && !errorItemCodes
            .contains(itemCodeDetailResponse.getItemCode())) {
          batchVatUpdateResponseList.add(BatchVatUpdateResponse.builder().skuCode(itemCodeDetailResponse.getItemCode())
              .vatFlagRequest(StringUtils.EMPTY)
              .errorMessage(BulkProcessValidationErrorMessages.ITEM_OWNED_BY_DIFFERENT_SELLER).build());
          errorItemCodes.add(itemCodeDetailResponse.getItemCode());
        } else if (itemCodeDetailResponse.getMerchantCode().equals(businessPartnerCode) && VAT_ALLOWED_MERCHANT_TYPES
            .contains(itemCodeDetailResponse.getMerchantType())) {
          finalVatUpdatemap
              .put(itemCodeDetailResponse.getItemCode(), vatFlagUpdateMap.get(itemCodeDetailResponse.getItemCode()));
          batchVatUpdateResponseList.removeIf(i -> i.getSkuCode().equals(itemCodeDetailResponse.getItemCode()));
        } else if (!errorItemCodes.contains(itemCodeDetailResponse.getItemCode()) && !finalVatUpdatemap.keySet()
            .contains(itemCodeDetailResponse.getItemCode())) {
          batchVatUpdateResponseList.add(BatchVatUpdateResponse.builder().skuCode(itemCodeDetailResponse.getItemCode())
              .vatFlagRequest(StringUtils.EMPTY).errorMessage(BulkProcessValidationErrorMessages.SELLER_TYPE_ERROR)
              .build());
          errorItemCodes.add(itemCodeDetailResponse.getItemCode());
        }
        List<String> itemCodesPresentInXProduct =
            itemCodeDetailResponses.stream().map(ItemCodeDetailResponse::getItemCode).collect(Collectors.toList());
        validateItemCode(batchVatUpdateResponseList, vatFlagUpdateMap, validateItemCode, itemCodesPresentInXProduct, errorItemCodes);
      }
    } else {
      vatFlagUpdateMap.keySet().forEach(itemCode -> batchVatUpdateResponseList.add(
          BatchVatUpdateResponse.builder().skuCode(itemCode).vatFlagRequest(StringUtils.EMPTY)
              .errorMessage(BulkProcessValidationErrorMessages.ITEM_NOT_FOUND).build()));
    }
    return finalVatUpdatemap;
  }

  private void validateItemCode(List<BatchVatUpdateResponse> batchVatUpdateResponseList,
      Map<String, Boolean> vatFlagUpdateMap, boolean validateItemCode, List<String> itemCodesPresentInXProduct,
      List<String> errorItemCodes) {
    if (validateItemCode) {
      for (Map.Entry<String, Boolean> vatFlagUpdate : vatFlagUpdateMap.entrySet()) {
        if (!itemCodesPresentInXProduct.contains(vatFlagUpdate.getKey())) {
          batchVatUpdateResponseList.add(
              BatchVatUpdateResponse.builder().skuCode(vatFlagUpdate.getKey()).vatFlagRequest(StringUtils.EMPTY)
                  .errorMessage(BulkProcessValidationErrorMessages.ITEM_NOT_FOUND).build());
          errorItemCodes.add(vatFlagUpdate.getKey());
        }
      }
    }
  }

  private Map<String, String> getFailedSkuCodeErrorMap(List<BatchVatUpdateResponse> vatUpdateFailureDtos) {
    Map<String, String> errorMap = new HashMap<>();
    vatUpdateFailureDtos.forEach(
        vatUpdateFailureDto -> errorMap.put(vatUpdateFailureDto.getSkuCode(), vatUpdateFailureDto.getErrorMessage()));
    return errorMap;
  }

  private void updateSuccessAndFailureSkusForVatUpdate(List<Map<String, String>> excelData,
      List<Map<String, String>> successData, List<Map<String, String>> failureData,
      Map<String, String> failedDataMap, List<String> failedInValidation, Map<String, String> skuAndErrorMap,
      String columnName) {
    for (Map<String, String> row : excelData) {
      String error = validateRowAndGetErrorForVatUpdate(row, failedDataMap, failedInValidation, columnName);
      if (StringUtils.isBlank(error)) {
        successData.add(row);
      } else {
        row.put(MasterDataBulkParameters.FAILURE_REASON, error);
        failureData.add(row);
        skuAndErrorMap.put(row.get(columnName), error);
      }
    }
  }

  private String validateRowAndGetErrorForVatUpdate(Map<String, String> row, Map<String, String> failedDataMap,
      List<String> failedInValidation, String columnName) {
    StringBuilder stringBuilder = new StringBuilder();
    if (failedDataMap.keySet().contains(row.get(columnName))) {
      stringBuilder.append(failedDataMap.get(row.get(columnName)));
    } else if (failedInValidation.contains(row.get(columnName))) {
      stringBuilder.append(Constant.ERROR_IN_SKU_CODE_UPDATE);
    }
    return stringBuilder.toString();
  }

  private void createVatUpdateErrorFile(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess,
      List<BatchVatUpdateResponse> failedItemCodes, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, List<String> failedInValidation, Sheet bulkVatUpdateSheet,
      String blibliProductSku) throws Exception {
    if (BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus())) {
      createVatErrorFileForPartialSuccess(bulkUpdateQueue.getFileName(), bulkVatUpdateSheet,
        failedInValidation, successData, failureData, failedItemCodes, bulkProcess,
        blibliProductSku);
    }
  }

  private String createVatErrorFileForPartialSuccess(String fileName, Sheet bulkVatUpdateSheet,
      List<String> failedInValidation, List<Map<String, String>> successData, List<Map<String, String>> failureDataRow, List<BatchVatUpdateResponse> failedData, BulkProcess bulkProcess, String columnName)
    throws Exception {
    Map<String, String> skuCodeErrorMap = new HashMap<>();
    Row row = bulkVatUpdateSheet.getRow(0);
    //adding error column
    Cell cell = row.createCell(3);
    cell.setCellValue(ERROR_COLUMN);
    List<Map<String, String>> productDataFromExcel = POIUtil.readFromExcelForBulkUpdate(bulkVatUpdateSheet, 1, 0, 0,
        new HashMap<>());
    updateSuccessAndFailureSkusForVatUpdate(productDataFromExcel, successData, failureDataRow,
        getFailedSkuCodeErrorMap(failedData), failedInValidation, skuCodeErrorMap, columnName);
    //updating error reason in error column
    for (int i = 1; i <= bulkVatUpdateSheet.getPhysicalNumberOfRows(); i++) {
      Row row1 = bulkVatUpdateSheet.getRow(i);
      if (row1 != null) {
        Cell cell1 = row1.getCell(1);
        if (skuCodeErrorMap.containsKey(cell1.toString())) {
          Cell cell2 = row1.createCell(3);
          cell2.setCellValue(skuCodeErrorMap.get(cell1.toString()));
        }
      }
    }
    byte[] deletePassedProduct = BulkUpdateServiceUtil
      .deletePassedProductFromExcel(bulkVatUpdateSheet, successData,
        bulkProcess.getBulkProcessCode(), fileName, ProcessorUtils.BULK_VAT_UPDATE_DIR);
    BulkUpdateProcessDTO processDTO =
            BulkUpdateProcessDTO.builder().bulkProcessType( BulkProcessType.SUBJECT_TO_VAT_ERROR.getValue())
                    .fileContent(deletePassedProduct).build();
    fileStoreService.createBulkFile(processDTO,StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize), fileName);
    return fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);

  }

  private void updateBulkVatUpdateFinalStatus(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, int savedProduct,
      int totalProduct, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, BulkUpdateErrorCounter counter,
      boolean isInternationalMerchant) throws IOException {
    String failedMessage = getFailedMessageString(bulkProcess, isInternationalMerchant);
    if (isInternationalMerchant) {
      bulkProcess.setDescription(
          new StringBuilder(bulkUpdateQueue.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_SUCCESSFULLY_UPDATED_EN, savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_FAILED_UPDATE_EN, totalProduct - savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL).append(failedMessage).toString());
    } else {
      bulkProcess.setDescription(
          new StringBuilder(bulkUpdateQueue.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_SUCCESSFULLY_UPDATED, savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_FAILED_UPDATE, totalProduct - savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL).append(failedMessage).toString());
    }
    saveBulkProcess(bulkProcess, bulkUpdateQueue, savedProduct, totalProduct, listBulkUpdateErrorDTO, counter);
  }

  private AuditTrailInfo getAuditTrailInfo(BulkUpdateQueue bulkUpdateQueue) {
    return AuditTrailInfo.builder().businessPartnerCode(bulkUpdateQueue.getBusinessPartnerCode())
        .requestId(bulkUpdateQueue.getRequestId()).username(bulkUpdateQueue.getUpdatedBy()).build();
  }

  private List<String> getProductSkus(Sheet bulkArchiveSheet, String blibliProductSku) {
    return POIUtil.readFromExcelForBulkUpdate(bulkArchiveSheet, 1, 0, 0, new HashMap<>()).parallelStream()
        .map(excelRow -> excelRow.get(blibliProductSku)).filter(StringUtils::isNotBlank).distinct()
        .collect(Collectors.toList());
  }

  private void createErrorFile(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess, BulkUpdateErrorCounter counter,
      List<String> failedProductSkus, List<Map<String, String>> successData, List<Map<String, String>> failureData,
      List<String> failedProductSkusInValidation, Sheet bulkArchiveSheet, String blibliProductSku)
    throws Exception {
    if (BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus())) {
      createErrorFileForPartialSuccess(bulkUpdateQueue.getFileName(), bulkArchiveSheet, failedProductSkusInValidation, successData,
          failureData, counter, failedProductSkus, bulkProcess, blibliProductSku);
    }
  }

  private void checkBulkStatusAndDeleteFile(String bulkProcessCode, BulkProcess bulkProcess, int totalProducts,
      int archivedProductsCount) {
    if (archivedProductsCount == totalProducts) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      String status = BulkProcess.STATUS_ABORTED;
      if (archivedProductsCount > 0) {
        status = BulkProcess.STATUS_PARTIALLY_DONE;
      }
      bulkProcess.setStatus(status);
    }
  }

  private List<String> validateProductSku(BulkProcess bulkProcess, List<String> failedProductSkusInValidation,
      List<String> productSkus) {
    List<String> validProductSkus = new ArrayList<>();
    for (String productSku : productSkus) {
      if (BulkUpdateServiceUtil
          .validateProductSkuAndSellerCodeWhenUpload(productSku, bulkProcess.getBusinessPartnerCode())) {
        validProductSkus.add(productSku);
      } else {
        failedProductSkusInValidation.add(productSku);
      }
    }
    return validProductSkus;
  }

  @Override
  public void processBulkUpdateOff2On(BulkUpdateQueue bulkUpdateQueue) throws Exception {
    LOGGER.info("Post processing for bulk update off2On . BulkUpdateQueue {}", bulkUpdateQueue);
    String bulkProcessCode = bulkUpdateQueue.getBulkProcessCode();
    String storeId = bulkUpdateQueue.getStoreId();
    BulkProcess bulkProcess = getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      LOGGER.warn(
          "Bulk update off2On for bulkProcessCode : {} is already processed or being processed",
          bulkUpdateQueue.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkUpdateQueue.getBulkProcessCode()
              + " is already processed or being processed");
    }
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    int totalProducts = 0;
    int updatedProducts = 0;
    List<String> failedProductSkus = new ArrayList<>();
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> finalSuccessData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    boolean isInternationalMerchant = false;
    try {
      isInternationalMerchant = isInternationalMerchant(storeId, bulkProcess);
      Sheet bulkUpdateOff2OnSheet = validateBulkUpdateOff2OnRequest(bulkUpdateQueue, bulkProcess, counter);
      POIUtil.validateNumberOfRows(bulkUpdateOff2OnSheet, bulkProcess.getBulkProcessCode(), bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);

      List<Map<String, String>> off2OnProductSkuRequest =
          POIUtil.readFromExcelForBulkUpdateWithCustomHeaders(bulkUpdateOff2OnSheet, 1, getBulkOff2OnUpdateHeaderMap());

      totalProducts = setCounterValues(bulkProcess, counter, off2OnProductSkuRequest);

      boolean instoreSwitch = Boolean.valueOf(systemParameterConfigService
          .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH).getValue());
      if (instoreSwitch) {
        bulkProcess.setInternationalMerchant(isInternationalMerchant);
        bulkProcess.setTotalCount(totalProducts);
        processBulkInstoreUpdateData(bulkProcess, off2OnProductSkuRequest);
        return;
      }

      validateExcelData(off2OnProductSkuRequest, successData, failureData, bulkProcess.getBusinessPartnerCode(),
          counter);

      counter.setInputErrorCounter(off2OnProductSkuRequest.size() - successData.size());

      // Call x-product
      bulkUpdateOff2On(successData, failedProductSkus, bulkUpdateQueue.getRequestId(),
          bulkUpdateQueue.getUpdatedBy(), counter);

      checkBulkStatusAndDeleteFile(bulkProcessCode, bulkProcess, totalProducts,
          successData.size() - failedProductSkus.size());

      generateFileForPartialSuccess(bulkUpdateQueue, bulkProcess, failedProductSkus, successData, finalSuccessData,
          bulkUpdateOff2OnSheet);

      updatedProducts = finalSuccessData.size();
    } catch (Exception ex) {
      LOGGER.error("Error occurred while processing bulk off2on update : storeId {} ,"
              + "bulkProcessCode {} , businessPartnerCode {} ", storeId, bulkProcessCode,
          bulkProcess.getBusinessPartnerCode(), ex);
      this.trackerService.sendTracker(TrackerConstants.PRODUCT_UPDATE_EVENT, TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE,
          TrackerConstants.HYPHEN, TrackerConstants.FAILED, MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    }
    updateOff2OnUpdateFinalStatus(bulkProcess, bulkUpdateQueue, updatedProducts, totalProducts, Collections.emptyList(),
        counter, isInternationalMerchant);
    notificationService.sendNotification(bulkProcess, NotificationType.BULK_ARCHIVED.getValue(),
        StringUtils.equals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcess.getStatus()));
  }

  private void processBulkInstoreUpdateData(BulkProcess bulkProcess, List<Map<String, String>> off2OnProductSkuRequest)
      throws JsonProcessingException {
    if (CollectionUtils.isEmpty(off2OnProductSkuRequest)) {
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    } else {
      bulkProcessDataService.saveBulkProcessData(
          RequestHelper.generateBulkProcessDateForInstoreUpdate(bulkProcess, off2OnProductSkuRequest));
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    }
    bulkProcessRepository.save(bulkProcess);
  }

  private boolean isInternationalMerchant(String storeId, BulkProcess bulkProcess) throws Exception {
    ProfileResponse businessPartner = this.businessPartnerRepository
        .filterByBusinessPartnerCodeV2(storeId, bulkProcess.getBusinessPartnerCode());
    return businessPartner.getCompany().isInternationalFlag();
  }

  private int setCounterValues(BulkProcess bulkProcess, BulkUpdateErrorCounter counter,
      List<Map<String, String>> off2OnProductSkuRequest) {
    int totalProducts;
    totalProducts = off2OnProductSkuRequest.size();
    bulkProcess.setTotalCount(totalProducts);
    bulkProcess.setSystemErrorCount(0);
    counter.setSystemErrorCounter(0);
    return totalProducts;
  }

  private void generateFileForPartialSuccess(BulkUpdateQueue bulkUpdateQueue,
      BulkProcess bulkProcess, List<String> failedProductSkus, List<Map<String, String>> successData,
      List<Map<String, String>> finalSuccessData, Sheet bulkUpdateOff2OnSheet) throws IOException {
    String filePath;
    if (BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus())) {
      filePath =
          ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue
              .getBulkProcessCode() + ProcessorUtils.getFileFormat(bulkUpdateQueue.getFileName());
      for (Map<String, String> productRow : successData) {
        if (!failedProductSkus.contains(String.valueOf(productRow.get(BulkParameters.BLIBLI_PRODUCT_SKU)))) {
          finalSuccessData.add(productRow);
        }
      }
      BulkUpdateServiceUtil
          .deletePassedProductFromExcel(bulkUpdateOff2OnSheet, finalSuccessData, bulkProcess.getBulkProcessCode(),
              filePath, ProcessorUtils.BULK_UPDATE_DIR);
    } else {
      finalSuccessData.addAll(successData);
    }
  }

  private Map<Integer, String> getBulkOff2OnUpdateHeaderMap() {
    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, BulkParameters.BLIBLI_PRODUCT_SKU);
    headers.put(1, BulkParameters.PARENT_PRODUCT_NAME);
    headers.put(2, BulkParameters.OFF2ON_VALUE);
    return headers;
  }

  private void validateExcelData(List<Map<String, String>> excelData, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, String businessPartnerCode, BulkUpdateErrorCounter counter) {
    for (Map<String, String> row : excelData) {
      String error = validateRowAndGetError(row, businessPartnerCode);
      if (StringUtils.isBlank(error)) {
        successData.add(row);
      } else {
        row.put(MasterDataBulkParameters.FAILURE_REASON, error);
        failureData.add(row);
        counter.incrementInputErrorCounter();
      }
    }
  }

  private String validateRowAndGetError(Map<String, String> row, String businessPartnerCode) {
    StringBuilder stringBuilder = new StringBuilder();
    if (!validateProductSku(row, businessPartnerCode)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.PRODUCT_SKU_NOT_VALID_ERROR);
    }
    if (!validateOff2OnFlag(row)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_INSTORE_FLAG);
    }
    return stringBuilder.toString();
  }

  private boolean validateProductSku(Map<String, String> row, String businessPartnerCode) {
    if (StringUtils.isBlank(row.get(BulkParameters.BLIBLI_PRODUCT_SKU)) || !BulkUpdateServiceUtil
        .validateProductSkuWhenUpload(row.get(BulkParameters.BLIBLI_PRODUCT_SKU)) || !(row
        .get(BulkParameters.BLIBLI_PRODUCT_SKU).startsWith(businessPartnerCode))) {
      LOGGER.error(Constant.PRODUCT_SKU_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateOff2OnFlag(Map<String, String> row) {
    if (StringUtils.isBlank(row.get(BulkParameters.OFF2ON_VALUE))) {
      LOGGER.error(Constant.OFF2ON_EMPTY_ERROR, row);
      return false;
    }
    try {
      if ((int) Double.parseDouble(row.get(BulkParameters.OFF2ON_VALUE)) != 0
          && (int) Double.parseDouble(row.get(BulkParameters.OFF2ON_VALUE)) != 1) {
        LOGGER.error(Constant.OFF2ON_VALUE_INVALID, row);
        return false;
      }
    } catch (Exception e) {
      LOGGER.error(Constant.OFF2ON_VALUE_INVALID, row);
      return false;
    }
    return true;
  }

  private void createErrorFileForPartialSuccess(String fileName, Sheet bulkArchiveSheet,
      List<String> failedItemSkusInValidation, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, BulkUpdateErrorCounter counter, List<String> failedItemSkus,
      BulkProcess bulkProcess, String columnName) throws Exception {
    Map<String, String> itemSkuAndErrorMap = new HashMap<>();
    Row row = bulkArchiveSheet.getRow(0);
    //adding error column
    Cell cell = row.createCell(2);
    cell.setCellValue(ERROR_COLUMN);
    List<Map<String, String>> productDataFromExcel = POIUtil.readFromExcelForBulkUpdate(bulkArchiveSheet, 1, 0, 0,
        new HashMap<>());
    updateSuccessAndFailureSkus(productDataFromExcel, successData, failureData, counter, failedItemSkus,
        failedItemSkusInValidation, itemSkuAndErrorMap, columnName);
    //updating error reason in error column
    for (int i = 1; i <= bulkArchiveSheet.getPhysicalNumberOfRows(); i++) {
      Row row1 = bulkArchiveSheet.getRow(i);
      if (row1 != null) {
        Cell cell1 = row1.getCell(0);
        if (itemSkuAndErrorMap.containsKey(cell1.toString())) {
          Cell cell2 = row1.createCell(2);
          cell2.setCellValue(itemSkuAndErrorMap.get(cell1.toString()));
        }
      }
    }
    byte[] deletePassedProduct = BulkUpdateServiceUtil
      .deletePassedProductFromExcel(bulkArchiveSheet, successData, bulkProcess.getBulkProcessCode(),
        fileName, ProcessorUtils.BULK_UPDATE_DIR);
    BulkUpdateProcessDTO processDTO =
      BulkUpdateProcessDTO.builder().fileContent(deletePassedProduct)
        .bulkProcessType(BulkProcessType.ARCHIVE.getValue()).build();
    fileStoreService.createBulkFile(processDTO, bulkProcess.getBulkProcessCode(), fileName);
  }

  private void updateSuccessAndFailureSkus(List<Map<String, String>> excelData,
      List<Map<String, String>> successData, List<Map<String, String>> failureData, BulkUpdateErrorCounter counter,
      List<String> failedItemSkus, List<String> failedItemSkusInValidation, Map<String, String> SkuAndErrorMap,
      String columnName) {
    for (Map<String, String> row : excelData) {
      String error = validateRowAndGetError(row, failedItemSkus, failedItemSkusInValidation, columnName);
      if (StringUtils.isBlank(error)) {
        successData.add(row);
      } else {
        row.put(MasterDataBulkParameters.FAILURE_REASON, error);
        failureData.add(row);
        SkuAndErrorMap.put(row.get(columnName), error);
      }
    }
  }

  private String validateRowAndGetError(Map<String, String> row, List<String> failedItemSkus,
      List<String> failedItemSkusInValidation, String columnName) {
    StringBuilder stringBuilder = new StringBuilder();
    if (failedItemSkus.contains(row.get(columnName))) {
      stringBuilder.append(Constant.ERROR_IN_ITEM_SKU_UPDATE);
    } else if (failedItemSkusInValidation.contains(row.get(columnName))) {
      stringBuilder.append(Constant.ERROR_IN_ITEM_SKU);
    }
    return stringBuilder.toString();
  }

  private int bulkArchiveProductSkus(List<String> passedProductSkus, AuditTrailInfo auditTrailInfo,
      BulkUpdateErrorCounter counter, List<String> failedProductSkus) throws ApplicationException {
    int successCount = 0;
    int bulkArchiveBatchSizeNew = Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE).getValue());
    for (List<String> productSkus : Lists.partition(passedProductSkus, bulkArchiveBatchSizeNew)) {
      List<String> response = productBusinessPartnerRepository
          .bulkArchiveProductSkus(auditTrailInfo.getUsername(), auditTrailInfo.getRequestId(), true,
              new SimpleListStringRequest(productSkus), new HashMap<>());
      successCount += productSkus.size() - response.size();
      counter.setSystemErrorCounter(counter.getSystemErrorCounter() + response.size());
      failedProductSkus.addAll(response);
    }
    return successCount;
  }

  private void bulkUpdateOff2On(List<Map<String, String>> successData, List<String> failedProductSkus, String requestId,
      String userName, BulkUpdateErrorCounter counter) {
    int bulkOff2OnUpdateBatchSize = Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)
        .getValue());
    for (List<Map<String, String>> listData : Lists.partition(successData, bulkOff2OnUpdateBatchSize)) {
      Map<String, Boolean> request = getBulkUpdateRequest(listData);
      List<String> failedProductSkusResponse = bulkProcessService.bulkUpdateOff2On(request, requestId, userName);
      failedProductSkus.addAll(failedProductSkusResponse);
      counter.setSystemErrorCounter(counter.getSystemErrorCounter() + failedProductSkusResponse.size());
    }
  }

  private Map<String, Boolean> getBulkUpdateRequest(List<Map<String, String>> listData) {
    return listData.stream().collect(Collectors.toMap(row -> row.get(BulkParameters.BLIBLI_PRODUCT_SKU),
        row -> 1 == (int) Double.parseDouble(row.get(BulkParameters.OFF2ON_VALUE)), (a, b) -> a));
  }


  private Sheet validateBulkArchiveRequest(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess,
      BulkUpdateErrorCounter counter)
      throws IOException {
    Sheet excelSheetData = fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
    GdnPreconditions.checkArgument(getBulkUpdateServiceUtil()
            .validateExcelFileForBulkArchive(excelSheetData, bulkProcess, bulkUpdateQueue, counter),
        BulkErrorCategory.EXCEL_HEADER_ERROR + " - " + BulkUpdateServiceUtil.HEADER_MISMATCH);
    return excelSheetData;
  }

  private Sheet validateBulkProductsArchiveRequest(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess,
      BulkUpdateErrorCounter counter)
      throws IOException {
    Sheet excelSheetData = fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
    GdnPreconditions.checkArgument(getBulkUpdateServiceUtil()
            .validateExcelFileForBulkProductsArchive(excelSheetData, bulkProcess, bulkUpdateQueue, counter),
        BulkErrorCategory.EXCEL_HEADER_ERROR + " - " + BulkUpdateServiceUtil.HEADER_MISMATCH);
    return excelSheetData;
  }

  private Sheet validateBulkUpdateOff2OnRequest(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess,
      BulkUpdateErrorCounter counter) throws IOException {
    Sheet excelSheetData = fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
    GdnPreconditions.checkArgument(getBulkUpdateServiceUtil()
            .validateExcelFileForBulkUpdateOff2On(excelSheetData, bulkProcess, bulkUpdateQueue, counter),
        BulkErrorCategory.EXCEL_HEADER_ERROR + " - " + BulkUpdateServiceUtil.HEADER_MISMATCH);
    return excelSheetData;
  }

  private Sheet getVatExcelSheetData(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess)
    throws IOException {
    return fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
  }

  @Override
  public List<BulkUpdateSuccessDTO> processBulkUpdateL5(String businessPartnerCode, Map<String, Boolean> privilegedMap,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<Map<String, String>> validationPassedData,
      BulkUpdateErrorCounter counter, boolean isMultiPickupPointSeller, String createdBy,
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3Responses, String clientHost,
      ProfileResponse profileResponse)
      throws Exception {
    Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet = new HashSet<>();
    List<BulkUpdateSuccessDTO> savedProduct = new ArrayList<>();
    List<Map<String, String>> failedProductList = new ArrayList<>();
    Map<String, ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseSkuMap = new HashMap<>();
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    updateValidationResultsL5(validationPassedData, businessPartnerCode, privilegedMap, failedProductList,
        itemPickupPointListingL3ResponseSkuMap, listBulkUpdateErrorDTO, isMultiPickupPointSeller,
        productVariantUpdateRequest, itemPickupPointListingL3Responses, profileResponse, bulkUpdateChangeTypeSet);
    counter.setInputErrorCounter(counter.getInputErrorCounter() + failedProductList.size());
    savedProduct = updateItemPickupPointsInPBP(listBulkUpdateErrorDTO, validationPassedData, createdBy, clientHost,
        bulkUpdateChangeTypeSet, savedProduct, failedProductList, productVariantUpdateRequest);
    BulkUpdateServiceUtil.updateBulkErrorsL5(failedProductList, listBulkUpdateErrorDTO, counter, validationPassedData);
    counter.setSystemErrorCounter(itemsPriceStockImagesUpdateResponse.getVariantsErrorList().size());
    return savedProduct;
  }

  private List<BulkUpdateSuccessDTO> updateItemPickupPointsInPBP(List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      List<Map<String, String>> validationPassedData, String createdBy, String clientHost,
      Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet, List<BulkUpdateSuccessDTO> savedProduct,
      List<Map<String, String>> failedProductList, ProductVariantUpdateRequest productVariantUpdateRequest)
      throws Exception {
    if (validationPassedData.size() != failedProductList.size()) {
      if (checkForBulkUpdateChangeEnabled && CollectionUtils.isNotEmpty(bulkUpdateChangeTypeSet)) {
        savedProduct =
            updateItemPickupPointsInBulk(productVariantUpdateRequest, listBulkUpdateErrorDTO, createdBy, clientHost);
        savedProduct.stream().findFirst()
            .ifPresent(product -> product.setChangeType(String.valueOf(bulkUpdateChangeTypeSet)));
      } else if (!checkForBulkUpdateChangeEnabled) {
        savedProduct =
            updateItemPickupPointsInBulk(productVariantUpdateRequest, listBulkUpdateErrorDTO, createdBy, clientHost);
      } else {
        Map<String, String> excelData = validationPassedData.stream().findFirst().orElse(new HashMap<>());
        log.info("Bulk update skipped for itemSku : {} and pickup point code : {} ",
            excelData.getOrDefault(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY),
            excelData.getOrDefault(BulkParameters.PICKUP_POINT_HEADER, StringUtils.EMPTY));
        updateBulkErrorAndSuccessDtoListForAddAndModifiedPickupPoint(productVariantUpdateRequest, new ArrayList<>(),
            savedProduct, new HashMap<>(), null);
        Optional.ofNullable(savedProduct).stream().flatMap(List::stream).findFirst()
            .ifPresent(product -> product.setChangeType(BulkUpdateChangeType.SKIPPED.name()));
      }
    }
    return savedProduct;
  }

  private List<BulkUpdateSuccessDTO> checkCampaignAvailability(String storeId, String requestId,
      List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList, String businessPartnerCode) {
    List<BulkUpdateSuccessDTO> campaignMappedProducts = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(bulkUpdateSuccessDTOList)) {
      List<List<BulkUpdateSuccessDTO>> partitions = new ArrayList<>();
      for (int i = 0; i < bulkUpdateSuccessDTOList.size(); i += partitionSize) {
        partitions
            .add(bulkUpdateSuccessDTOList.subList(i, Math.min(i + partitionSize, bulkUpdateSuccessDTOList.size())));
      }
      Set<String> itemSkuBatch = new HashSet<>();
      for(List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOBatch : partitions) {
        try {
          itemSkuBatch =
          bulkUpdateSuccessDTOBatch.stream().map(BulkUpdateSuccessDTO::getProductSku).collect(Collectors.toSet());
          Set<ItemInfoDto> itemInfoDtos = bulkUpdateSuccessDTOBatch.stream().map(
              bulkUpdateSuccessDTO -> new ItemInfoDto(bulkUpdateSuccessDTO.getProductSku(),
                  bulkUpdateSuccessDTO.getPickupPointCode(), new String())).collect(Collectors.toSet());
          ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest =
              ProductCampaignAvailabilityRequest.builder().itemInfo(itemInfoDtos).merchantCode(businessPartnerCode).itemSkus(itemSkuBatch)
                  .build();
          ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse;
          if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
            productCampaignAvailabilityResponse = Optional.ofNullable(
                this.xCampaignFeign.getProductCampaignV2(storeId, X_BULK_CLIENT, X_BULK_CLIENT, requestId, X_BULK_CLIENT,
                    productCampaignAvailabilityRequest).getValue())
                .orElseGet(ProductCampaignAvailabilityResponse::new);
            Map<String, Boolean> productCampaignAvailabilityMap = new HashMap<>();
            setProductCampaignAvailabilityMap(productCampaignAvailabilityResponse, productCampaignAvailabilityMap);
          } else {
            productCampaignAvailabilityResponse = Optional.ofNullable(
                this.xCampaignFeign.getProductCampaign(storeId, X_BULK_CLIENT, X_BULK_CLIENT, requestId, X_BULK_CLIENT,
                    productCampaignAvailabilityRequest).getValue())
                .orElseGet(ProductCampaignAvailabilityResponse::new);
          }
          campaignMappedProducts.addAll(
              Optional.ofNullable(bulkUpdateSuccessDTOBatch).orElseGet(() -> new ArrayList<>()).stream().filter(
                  bulkUpdateSuccessDTO -> productCampaignAvailabilityResponse.getProductCampaignAvailabilityMap()
                      .get(bulkUpdateSuccessDTO.getProductSku())).collect(Collectors.toList()));
        } catch (Exception e) {
          LOGGER.error("Error while fetching campaign availability for products : {}", itemSkuBatch, e);
        }
      }
    }
    return campaignMappedProducts;
  }

  private void setProductCampaignAvailabilityMap(
      ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse,
      Map<String, Boolean> productCampaignAvailabilityMap) {
    if (CollectionUtils.isNotEmpty(productCampaignAvailabilityResponse.getProductCampaignAvailabilityInfo())) {
      for (ProductCampaignAvailabilityInfoDto productCampaignAvailabilityInfoDto : productCampaignAvailabilityResponse
          .getProductCampaignAvailabilityInfo()) {
        productCampaignAvailabilityMap.putIfAbsent(productCampaignAvailabilityInfoDto.getItemSku(),
            productCampaignAvailabilityInfoDto.isAvailability());
      }
      productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(productCampaignAvailabilityMap);
    }
  }

  /**
   * create product campaign in batch size
   * @param bulkUpdateErrorDTOList
   * @param validationPassedData
   * @param bulkUpdateErrorCounter
   * @param bulkAddCampaignProductQueue
   * @return number of succesfully added product campaign, add failed data if the number of request
   * mismatch with number of saved products
   */
  private int processBulkCreateCampaign(
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
      List<Map<String, String>> validationPassedData, BulkUpdateErrorCounter bulkUpdateErrorCounter,
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue) {
    List<Map<String, String>> failedProductCampaignList = new ArrayList<>();
    List<CampaignProductRequest> campaignProductRequests =
        constructCampaignRequests(validationPassedData, bulkAddCampaignProductQueue);

    bulkUpdateErrorCounter
        .setInputErrorCounter(validationPassedData.size() - campaignProductRequests.size());

    int savedCampaign = this.createCampaignProductsInBulk(campaignProductRequests,
        failedProductCampaignList, bulkAddCampaignProductQueue);

    if (savedCampaign != campaignProductRequests.size()) {
      BulkUpdateServiceUtil.updateBulkErrors(failedProductCampaignList, bulkUpdateErrorDTOList,
          bulkUpdateErrorCounter, validationPassedData, new HashSet<>(), true);
    }

    return savedCampaign;
  }

  /**
   * validate header of the excel uploaded
   * @param bulkUpdateQueue
   * @param merchantStatusType
   * @param privilegedMap
   * @param bulkProcess
   * @param counter
   * @return validated Sheet
   * @throws Exception
   */
  private Sheet validateBulkUpdateRequest(BulkUpdateQueue bulkUpdateQueue,
    Map<String, Boolean> privilegedMap, BulkProcess bulkProcess,
      BulkUpdateErrorCounter counter, MerchantStatusType merchantStatusType, boolean includePoQuotaHeaderValidation) throws IOException {
    GdnPreconditions.checkArgument(bulkProcess != null,
        BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    Sheet excelSheetData = fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
    GdnPreconditions.checkArgument(getBulkUpdateServiceUtil()
            .authorizeUploadBulkUpdate(privilegedMap, excelSheetData, bulkProcess,
              bulkUpdateQueue, counter, merchantStatusType, cncForWarehouseFeatureSwitch, includePoQuotaHeaderValidation),
        BulkErrorCategory.EXCEL_HEADER_ERROR + " - " + BulkUpdateServiceUtil.HEADER_MISMATCH);
    return excelSheetData;
  }

  /**
   * validate header of excel for bulk campaign products
   * @param bulkUpdateQueue
   * @param bulkProcessCode
   * @param fileName
   * @param bulkProcess
   * @param counter
   * @return
   * @throws Exception
   */
  private Sheet validateBulkCampaignProductUpdateRequest(BulkUpdateQueue bulkUpdateQueue,
      String bulkProcessCode, String fileName,
      BulkProcess bulkProcess, BulkUpdateErrorCounter counter) throws IOException {
    GdnPreconditions.checkArgument(bulkProcess != null,
        BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    BulkProcess processDTO = new BulkProcess();
    processDTO.setBulkProcessType(BulkProcessType.CAMPAIGN.getValue());
    processDTO.setBulkProcessCode(bulkProcessCode);
    InputStream fileInputStream = new ByteArrayInputStream(
      fileStorageServiceBean.downloadFile(processDTO, ProcessorUtils.getFileFormat(fileName)));
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream,
        fileName.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    GdnPreconditions.checkArgument(getBulkUpdateServiceUtil().authorizeUploadCampaignProductBulkUpdate(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter),
        BulkUpdateServiceUtil.HEADER_MISMATCH_PRODUCT);
    return excelSheetData;
  }


  private List<BulkUpdateSuccessDTO> updateItemPickupPointsInBulk(
      ProductVariantUpdateRequest productVariantUpdateRequest, List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
      String createdBy, String clientHost) throws Exception {
    List<BulkUpdateSuccessDTO> savedProduct = new ArrayList<>();
    Map<String, String> failedPickupPointCodesReasonMap = new HashMap<>();
    productVariantUpdateRequest.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_BULK.getDesc());
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        pbpOutboundService.updateSummaryL5(createdBy, productVariantUpdateRequest, clientHost);
    for (VariantsErrorListResponse variantsErrorListResponse : itemsPriceStockImagesUpdateResponse.getVariantsErrorList()) {
      failedPickupPointCodesReasonMap.put(variantsErrorListResponse.getPickupPointCode(),
          variantsErrorListResponse.getMessage());
    }
    updateBulkErrorAndSuccessDtoListForAddAndModifiedPickupPoint(productVariantUpdateRequest, bulkUpdateErrorDTOList,
        savedProduct, failedPickupPointCodesReasonMap, itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate());
    return savedProduct;
  }

  private void updateBulkErrorAndSuccessDtoListForAddAndModifiedPickupPoint(
      ProductVariantUpdateRequest productVariantUpdateRequest, List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList,
      List<BulkUpdateSuccessDTO> savedProduct, Map<String, String> failedPickupPointCodesReasonMap,
      Boolean scheduleRemovedForStatusUpdate) {
    updateBulkErrorAndSuccessDtoList(productVariantUpdateRequest, bulkUpdateErrorDTOList, savedProduct,
        failedPickupPointCodesReasonMap, productVariantUpdateRequest.getAddPickupPoints(),
        scheduleRemovedForStatusUpdate);
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductItems())) {
      updateBulkErrorAndSuccessDtoList(productVariantUpdateRequest, bulkUpdateErrorDTOList, savedProduct,
          failedPickupPointCodesReasonMap,
          productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints(),
          scheduleRemovedForStatusUpdate);
    }
  }

  private void updateBulkErrorAndSuccessDtoList(ProductVariantUpdateRequest productVariantUpdateRequest,
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList, List<BulkUpdateSuccessDTO> savedProduct,
      Map<String, String> failedPickupPointCodesReasonMap, List<ItemPickupPointRequest> itemPickupPointRequestList,
    Boolean scheduleRemovedForStatusUpdate) {
    for (ItemPickupPointRequest itemPickupPointRequest : itemPickupPointRequestList) {
      if (!failedPickupPointCodesReasonMap.containsKey(itemPickupPointRequest.getPickupPointId())) {
        BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
            BulkUpdateSuccessDTO.builder().productName(productVariantUpdateRequest.getProductName())
                .productSku(itemPickupPointRequest.getItemSku())
                .pickupPointCode(itemPickupPointRequest.getPickupPointId()).scheduleRemoval(scheduleRemovedForStatusUpdate).build();
        savedProduct.add(bulkUpdateSuccessDTO);
      } else {
        BulkUpdateErrorDTO bulkUpdateErrorDTO =
            BulkUpdateErrorDTO.builder().productName(productVariantUpdateRequest.getProductName())
                .productSku(itemPickupPointRequest.getItemSku())
                .pickupPointCode(itemPickupPointRequest.getPickupPointId())
                .reason(failedPickupPointCodesReasonMap.get(itemPickupPointRequest.getPickupPointId())).build();
        bulkUpdateErrorDTOList.add(bulkUpdateErrorDTO);

      }
    }
  }


  private Map<String, String> failedItemPPCodeMap(List<FailedItemInfoDto> failedItemInfo) {
    Map<String, String> failedMap = new HashMap<>();
    for (FailedItemInfoDto failedItemInfoDto : failedItemInfo) {
      failedMap.putIfAbsent(
        failedItemInfoDto.getItemSku() + Constant.DASH + failedItemInfoDto.getPickUpPointCode(),
        failedItemInfoDto.getFailedReason());
    }
    return failedMap;
  }

  /**
   * construct error failed campaign data to be put in bulk process notes
   * @param campaignProductRequests
   * @param failedProductCampaign
   * @param failedSkuToReason
   */
  private void constructErrorFailedCampaignData(
    List<CampaignProductRequest> campaignProductRequests,
    List<Map<String, String>> failedProductCampaign, Map<String, String> failedSkuToReason) {
    campaignProductRequests.stream().filter(Objects::nonNull).filter(
      campaignProductRequest -> failedSkuToReason.containsKey(
        campaignProductRequest.getItemSkuId() + Constant.DASH
          + campaignProductRequest.getPickupPointCode())).
      forEach((CampaignProductRequest campaignProductRequest) -> {
      Map<String, String> failedData = new HashMap<>();
      failedData.put(BulkParameters.PRODUCT_NAME, campaignProductRequest.getSkuName());
      failedData.put(BulkParameters.BLIBLI_SKU, campaignProductRequest.getItemSkuId());
      failedData.put(BulkParameters.ITEM_SKU, campaignProductRequest.getItemSkuId());
      failedData.put(BulkParameters.PICKUP_POINT_CODE, campaignProductRequest.getPickupPointCode());
      Optional.ofNullable(failedSkuToReason).map(failedReasonMap -> failedReasonMap.get(
        campaignProductRequest.getItemSkuId() + Constant.DASH + campaignProductRequest
          .getPickupPointCode()))
        .ifPresent(failedReason -> failedData.put(BulkParameters.ERROR_CODE, failedReason));
      failedProductCampaign.add(failedData);
    });
  }

  /**
   * create product campaign in batch size
   * @param campaignProductRequests
   * @param failedProductCampaign
   * @param bulkAddCampaignProductQueue
   * @return number of succesfully added product campaign, add failed data if the number of request
   */
  private int createCampaignProductsInBulk(List<CampaignProductRequest> campaignProductRequests,
      List<Map<String, String>> failedProductCampaign,
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue){
    int savedProductCount = 0;
    List<List<CampaignProductRequest>> campaignProductRequestPartition =
        Lists.partition(campaignProductRequests, bulkUpdateBatchSize);
    for (List<CampaignProductRequest> campaignProductRequestList : campaignProductRequestPartition) {
      Map<String, String> failedSkusToReason = null;
      try {
          GdnRestSingleResponse<FailedProductsResponse>
            response = this.xCampaignFeign.addCampaignProductV2(
            StringUtils.isNotEmpty(bulkAddCampaignProductQueue.getStoreId()) ?
              bulkAddCampaignProductQueue.getStoreId() :
              Constant.STORE_ID,
            X_BULK_CLIENT, X_BULK_CLIENT,
            StringUtils.isNotEmpty(bulkAddCampaignProductQueue.getRequestId()) ?
              bulkAddCampaignProductQueue.getRequestId() :
              Constant.REQUEST_ID,
            X_BULK_CLIENT, campaignProductRequestList);
          ResponseHelper.validateResponse(response);
          failedSkusToReason = failedItemPPCodeMap(response.getValue().getFailedItemInfo());
      } catch (Exception e) {
        LOGGER.error("Error while adding products to campaign {}", campaignProductRequestList,
            e);
        failedSkusToReason = new HashMap<>();
        populateGenericErrorMessageInFailedSkusMap(campaignProductRequestList, failedSkusToReason);
      }
      constructErrorFailedCampaignData(campaignProductRequestList, failedProductCampaign,
          failedSkusToReason);
    }
    savedProductCount = campaignProductRequests.size() - failedProductCampaign.size();
    return savedProductCount;
  }

  public void populateGenericErrorMessageInFailedSkusMap(List<CampaignProductRequest> campaignProductRequestList,
      Map<String, String> failedSkusToReason) {
    if (bulkCampaignErrorHandling) {
      campaignProductRequestList.stream().map(
              request -> new StringBuilder().append(request.getItemSkuId()).append(Constant.DASH)
                  .append(request.getPickupPointCode()).toString())
          //update error message
          .forEach(key -> failedSkusToReason.putIfAbsent(key, Constant.CAMPAIGN_GENRIC_ERROR));
    }
  }

  private void updateBulkArchiveFinalStatus(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, int savedProduct,
      int totalProduct, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, BulkUpdateErrorCounter counter,
      boolean isInternationalMerchant) throws JsonProcessingException {
    String failed_message = getFailedMessageString(bulkProcess, isInternationalMerchant);
    if (isInternationalMerchant) {
      bulkProcess.setDescription(
          new StringBuilder(bulkUpdateQueue.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.SUCCESSFULLY_ARCHIVED_EN).append(BulkUpdateServiceUtil.IS)
              .append(savedProduct).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.FAILED_ARCHIVED_EN).append(BulkUpdateServiceUtil.IS)
              .append(totalProduct - savedProduct).append(BulkUpdateServiceUtil.END_SYMBOL).append(failed_message)
              .toString());
    } else {
      bulkProcess.setDescription(
          new StringBuilder(bulkUpdateQueue.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.SUCCESSFULLY_ARCHIVED).append(BulkUpdateServiceUtil.IS).append(savedProduct)
              .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.FAILED_ARCHIVED)
              .append(BulkUpdateServiceUtil.IS).append(totalProduct - savedProduct)
              .append(BulkUpdateServiceUtil.END_SYMBOL).append(failed_message).toString());
    }
    saveBulkProcess(bulkProcess, bulkUpdateQueue, savedProduct, totalProduct, listBulkUpdateErrorDTO, counter);
  }

  private void saveBulkProcess(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, int savedProduct,
      int totalProduct, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, BulkUpdateErrorCounter counter)
      throws JsonProcessingException {
    bulkProcess.setSuccessCount(savedProduct);
    bulkProcess.setErrorCount(totalProduct - savedProduct);
    getBulkUpdateServiceUtil().updateBulkProcessNotes(listBulkUpdateErrorDTO, bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), bulkProcess, false, null);
    bulkProcess.setInputErrorCount(counter.getInputErrorCounter());
    bulkProcess.setSystemErrorCount(counter.getSystemErrorCounter());
    bulkProcess.setTotalCount(totalProduct);
    this.bulkProcessService.saveOperation(bulkProcess);
  }

  private void updateOff2OnUpdateFinalStatus(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue,
      int updatedProducts, int totalProduct, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      BulkUpdateErrorCounter counter, boolean isInternationalMerchant)
      throws JsonProcessingException {
    String failed_message = getFailedMessageString(bulkProcess, isInternationalMerchant);
    getDescription(bulkProcess, bulkUpdateQueue, updatedProducts, totalProduct, isInternationalMerchant,
        failed_message);
    saveBulkProcess(bulkProcess, bulkUpdateQueue, updatedProducts, totalProduct, listBulkUpdateErrorDTO, counter);
    LOGGER.info("PostProcessing for bulk update successfully completed. bulkUpdateQueue: {}", bulkUpdateQueue);
  }

  private void getDescription(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, int updatedProducts,
      int totalProduct, boolean isInternationalMerchant, String failed_message) {
    if (isInternationalMerchant) {
      bulkProcess.setDescription(
          new StringBuilder(Optional.ofNullable(bulkUpdateQueue.getFileName()).orElse(StringUtils.EMPTY)).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.SUCCESSFULLY_UPDATED_EN).append(BulkUpdateServiceUtil.IS)
              .append(updatedProducts).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.FAILED_UPDATE_EN).append(BulkUpdateServiceUtil.IS)
              .append(totalProduct - updatedProducts).append(BulkUpdateServiceUtil.END_SYMBOL).append(failed_message)
              .toString());
    } else {
      bulkProcess.setDescription(
          new StringBuilder(Optional.ofNullable(bulkUpdateQueue.getFileName()).orElse(StringUtils.EMPTY)).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.SUCCESSFULLY_UPDATED).append(BulkUpdateServiceUtil.IS)
              .append(updatedProducts).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(BulkUpdateServiceUtil.FAILED_UPDATE).append(BulkUpdateServiceUtil.IS)
              .append(totalProduct - updatedProducts).append(BulkUpdateServiceUtil.END_SYMBOL).append(failed_message)
              .toString());
    }
  }

  private String getFailedMessageString(BulkProcess bulkProcess, boolean isInternationalMerchant) {
    bulkProcess.setEndDate(new Date());
    String failed_message = StringUtils.EMPTY;
    if (BulkProcess.STATUS_ABORTED.equals(bulkProcess.getStatus())) {
      if (isInternationalMerchant)
        failed_message = BulkUpdateServiceUtil.FAILED_MESSAGE_EN;
      else
        failed_message = BulkUpdateServiceUtil.FAILED_MESSAGE;
    }
    return failed_message;
  }

  @Transactional
  public void savePreProcessBulkData(BulkProcess bulkProcess) {
    getBulkProcessRepository().save(bulkProcess);
  }

  private Integer updateValidationResultsL5(List<Map<String, String>> validationPassedData, String businessPartnerCode,
      Map<String, Boolean> privilegedMap, List<Map<String, String>> failedProductList,
      Map<String, ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseMap,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, boolean isMultiPickupPointSeller,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3Responses, ProfileResponse profileResponse,
      Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet) throws Exception {
    Integer buyableFlagUpdatedCount = 0;
    boolean isPPCodeChangedForNonMppSeller = false;
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Map<String, String> productDataL5 = validationPassedData.stream().findFirst().orElse(new HashMap<>());
    if (MapUtils.isNotEmpty(productDataL5)) {
      itemPickupPointListingL3Request.setItemSku(productDataL5.get(BulkParameters.BLIBLI_SKU));
      itemPickupPointListingL3Request.setProductSku(productDataL5.get(BulkParameters.BLIBLI_PRODUCT_SKU));
      itemPickupPointListingL3Request.setBusinessPartnerCode(businessPartnerCode);
      Set<String> pickupPointCode = Objects.nonNull(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER)) ?
          Set.of(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER)) : Collections.emptySet();
      itemPickupPointListingL3Request.setPickupPointCodes(pickupPointCode);
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList;
      itemPickupPointListingL3ResponseList =
          getItemPickupPointListingL3Responses(itemPickupPointListingL3Responses, itemPickupPointListingL3Request);
      itemPickupPointListingL3Request.setPickupPointCodes(new HashSet<>());
      itemPickupPointListingL3Request.setFbbSortRequired(true);
      List<ItemPickupPointListingL3Response> fbbTrueItemPickupPointList =
          pbpOutboundService.getItemPickupPointListingL3Response(0, 1, itemPickupPointListingL3Request).getContent();
      fbbTrueItemPickupPointList.stream().findFirst().ifPresent(
          fbbTrueItemPickupPoint -> itemPickupPointListingL3ResponseMap.put(fbbTrueItemPickupPoint.getPickupPointCode(),
              fbbTrueItemPickupPoint));
      itemPickupPointListingL3ResponseList.addAll(fbbTrueItemPickupPointList);
      for (ItemPickupPointListingL3Response itemPickupPointL3Response : itemPickupPointListingL3ResponseList) {
        if (!isMultiPickupPointSeller) {
          itemPickupPointListingL3ResponseMap.put(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER),
              itemPickupPointL3Response);
          isPPCodeChangedForNonMppSeller =
            isPPCodeChangedForNonMppMerchant(itemPickupPointL3Response.getPickupPointCode(),
            productDataL5.get(BulkParameters.PICKUP_POINT_HEADER));
        } else {
          itemPickupPointListingL3ResponseMap.put(itemPickupPointL3Response.getPickupPointCode(),
              itemPickupPointL3Response);
        }
      }
      if (isPPCodeChangedForNonMppSeller) {
        bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.PICKUP_POINT_CHANGED_FOR_NON_MPP);
      }
      boolean freeSample = itemPickupPointListingL3ResponseMap.values().stream().filter(Objects::nonNull)
          .anyMatch(ItemPickupPointListingL3Response::isFreeSample);
      productVariantUpdateRequest.setProductSku(productDataL5.get(BulkParameters.BLIBLI_PRODUCT_SKU));
      productVariantUpdateRequest.setProductName(productDataL5.get(BulkParameters.PRODUCT_NAME));
      productVariantUpdateRequest.setBusinessPartnerCode(businessPartnerCode);
      if (CollectionUtils.isNotEmpty(itemPickupPointListingL3ResponseList)) {
        productVariantUpdateRequest.getProductItems().add(getBulkUpdateServiceUtil()
            .getProductVariantPriceStockAndImagesRequest(productDataL5, itemPickupPointListingL3ResponseList.get(0),
                privilegedMap));
      } else {
        productVariantUpdateRequest.getProductItems()
            .add(getBulkUpdateServiceUtil().getProductVariantPriceStockAndImagesRequestFromUserInput(productDataL5));
      }
      List<Map<String, String>> validationFailedData = new ArrayList<>();
      PreOrderDTO preOrderDTO = null;
      for (Map<String, String> productL5Data : validationPassedData) {
        try {
          ItemPickupPointListingL3Response itemPickupPointListingL3Response =
              itemPickupPointListingL3ResponseMap.get(productL5Data.get(BulkParameters.PICKUP_POINT_HEADER));
          CommonUtils.validateActiveProduct(productDataL5, itemPickupPointListingL3Response,
            isMultiPickupPointSeller, listBulkUpdateErrorDTO, validationFailedData);
          String skuShippingStatus = Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) ?
              productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P) :
              productL5Data.get(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);
          String bfbStatus = Boolean.TRUE.equals(privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) ?
            productL5Data.get(BulkParameters.EXTERNAL_BFB_STATUS) :
            productL5Data.get(BulkParameters.AMPHI_BFB_STATUS);
          String cncStatus = productDataL5.get(BulkParameters.CNC_STATUS_HEADER);
          if (!businessPartnerCode.equals(productL5Data.get(BulkParameters.BLIBLI_SKU).substring(0, 9))) {
            failedProductList.add(productL5Data);
          } else if ((Objects.nonNull(itemPickupPointListingL3Response))
            && (itemPickupPointListingL3Response.isFreeSample() && !StringUtils
            .equals(BulkParameters.OFFLINE_VALUE, skuShippingStatus))) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder()
              .productName(Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME)).orElse(StringUtils.EMPTY))
              .productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
              .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
              .reason(FREE_SAMPLE_SHIPPING_ERROR).build());
            validationFailedData.add(productL5Data);
          } else if (StringUtils.isNotBlank(bfbStatus) && Objects.nonNull(
            itemPickupPointListingL3Response) && (itemPickupPointListingL3Response.isFreeSample()
            && !StringUtils.equals(BulkParameters.OFFLINE_VALUE, bfbStatus))) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
                Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME))
                  .orElse(StringUtils.EMPTY)).productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
              .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
              .reason(FREE_SAMPLE_B2B_ERROR).build());
            validationFailedData.add(productL5Data);
          } else if (StringUtils.isNotBlank(cncStatus) && freeSample
            && !StringUtils.equals(BulkParameters.OFFLINE_VALUE, cncStatus)) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
                Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME))
                  .orElse(StringUtils.EMPTY)).productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
              .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
              .reason(FREE_SAMPLE_CNC_SHIPPING_ERROR).build());
            validationFailedData.add(productL5Data);
          }
          else if (privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false) && !(
              StringUtils.equals(BulkParameters.OFFLINE_VALUE, skuShippingStatus)
                  || (StringUtils.equals(BulkParameters.ONLINE_VALUE, skuShippingStatus)))) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder()
                .productName(Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME)).orElse(StringUtils.EMPTY))
                .productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
                .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
                .reason(ONLY_EXTERNAL_USER_SHIPPING_ERROR).build());
            validationFailedData.add(productL5Data);
          } else if (Objects.nonNull(itemPickupPointListingL3Response) && BulkUpdateServiceUtil.isBopisProductMadeCnc(
              bopisCncRestrictionEnabled, StringUtils.equals(BulkParameters.ONLINE_VALUE, cncStatus),
              itemPickupPointListingL3Response.getProductType())) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
                    Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME)).orElse(StringUtils.EMPTY))
                .productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
                .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
                .reason(BulkProcessValidationErrorMessages.PRODUCT_CANT_BE_SET_CNC).build());
            validationFailedData.add(productL5Data);
          } else if (Objects.nonNull(itemPickupPointListingL3Response) && BulkUpdateServiceUtil.isDimensionMissing(
              bopisCategoryRestrictionEnabled, !StringUtils.equals(BulkParameters.OFFLINE_VALUE, skuShippingStatus),
              profileResponse, CommonUtils.getDimensionMissingFromMissingFields(itemPickupPointListingL3Response),
              BulkUpdateServiceUtil.getSellerTypes(bopisCategoryValidationForSellerTypes))) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
                    Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME)).orElse(StringUtils.EMPTY))
                .productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
                .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
                .reason(BulkProcessValidationErrorMessages.PRODUCT_CANT_BE_SET_ONLINE).build());
            validationFailedData.add(productL5Data);
          } else if (eligibleForPureInStoreValidations(itemPickupPointListingL3Response, skuShippingStatus, cncStatus,
              instoreNewFlowEnabled) && validateOnlineOrCNCFlagUpdateForPureInStore(itemPickupPointListingL3Response)) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
                    Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME)).orElse(StringUtils.EMPTY))
                .productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
                .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
                .reason(BulkProcessValidationErrorMessages.PURE_INSTORE_MISSING_FIELDS_ERROR).build());
            validationFailedData.add(productL5Data);
          } else if (CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(
            validateWarehouseVariantDeletionEnabled, isPPCodeChangedForNonMppSeller,
            itemPickupPointListingL3Response, supportedMerchantsForWarehouseStockValidation,
            profileResponse)) {
            listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
                Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME))
                  .orElse(StringUtils.EMPTY)).productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
              .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER)).reason(
                BulkProcessValidationErrorMessages.PP_UPDATE_UNSUPPORTED_L5_WAREHOUSE_STOCK_AVAILABLE)
              .build());
            validationFailedData.add(productL5Data);
          } else {
            buyableFlagUpdatedCount =
                buyableFlagUpdatedCount + getBulkUpdateServiceUtil().prepareUpdateL5SummaryRequest(
                    productVariantUpdateRequest, productL5Data, privilegedMap, itemPickupPointListingL3Response,
                    isMultiPickupPointSeller, isPPCodeChangedForNonMppSeller, itemPickupPointListingL3ResponseMap,
                    bulkUpdateChangeTypeSet, profileResponse, preOrderDTO);
            identifyBulkUpdateModifications(privilegedMap, productVariantUpdateRequest, bulkUpdateChangeTypeSet,
                productL5Data, itemPickupPointListingL3Response);
          }
        } catch (Exception e) {
          LOGGER.error("Failed while fetching  data for itemSku = {}, pickupPointCode = {} ",
              productL5Data.get(BulkParameters.BLIBLI_SKU), productL5Data.get(BulkParameters.PICKUP_POINT_HEADER), e);
          if (Objects.nonNull(e.getMessage()) && e.getMessage().contains(BulkErrorCategory.STOCK_MUST_BE_NUMBER.getDescription())) {
            productL5Data.put(BulkParameters.ERROR_CODE, BulkErrorCategory.STOCK_MUST_BE_NUMBER.getDescription());
          }
          if (Objects.nonNull(e.getMessage()) && e.getMessage()
              .contains(BulkErrorCategory.INVALID_STATUS_COMBINATION.getDescription())) {
            productL5Data.put(BulkParameters.ERROR_CODE,
                BulkErrorCategory.INVALID_STATUS_COMBINATION.getDescription());
          }
          failedProductList.add(productL5Data);
        }
      }
      if (removeFailedDataFromPassedData) {
        validationPassedData.removeIf(validationFailedData::contains);
      }
    }
    return buyableFlagUpdatedCount;
  }

  private static boolean eligibleForPureInStoreValidations(
    ItemPickupPointListingL3Response itemPickupPointListingL3Response, String skuShippingStatus,
    String cncStatus, boolean instoreNewFlowEnabled) {
    return Objects.nonNull(itemPickupPointListingL3Response) && instoreNewFlowEnabled && (
      !StringUtils.equals(BulkParameters.OFFLINE_VALUE, skuShippingStatus) || StringUtils.equals(
        BulkParameters.ONLINE_VALUE, cncStatus));
  }

  private static boolean validateOnlineOrCNCFlagUpdateForPureInStore(
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    Set<String> requiredMissingFields = Set.of(Constant.DIMENSIONS_MISSING, Constant.DESCRIPTION_MISSING);
    return requiredMissingFields.stream().anyMatch(itemPickupPointListingL3Response.getMissingFields()::contains);
  }

  private void identifyBulkUpdateModifications(Map<String, Boolean> privilegedMap,
      ProductVariantUpdateRequest productVariantUpdateRequest, Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      Map<String, String> productL5Data, ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    if (checkForBulkUpdateChangeEnabled && Objects.nonNull(itemPickupPointListingL3Response)) {
      CommonUtils.checkForMerchantSkuChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
      CommonUtils.checkForPriceChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
      ItemPickupPointRequest itemPickupPointRequest =
          productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0);
      CommonUtils.checkForStockChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response,
          itemPickupPointRequest);
      CommonUtils.checkForPoQuotaChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response,
          itemPickupPointRequest);
      ProductLevel3ViewConfigResponse itemViewConfigs =
          RequestHelper.getDefaultChannelItemViewConfig(itemPickupPointListingL3Response);
      Boolean isOnlyExternal = privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
      checkForStatusChanged(bulkUpdateChangeTypeSet, productL5Data, itemViewConfigs, isOnlyExternal);
      CommonUtils.checkForCncChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
      ProductLevel3ViewConfigResponse bfbItemViewConfigs =
          RequestHelper.getBfbChannelItemViewConfig(itemPickupPointListingL3Response);
      checkForBfbStatusChanged(bulkUpdateChangeTypeSet, productL5Data, isOnlyExternal, bfbItemViewConfigs);
      CommonUtils.checkForBfbFieldsChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response,
          bfbItemViewConfigs);
    }
  }

  private void checkForBfbStatusChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      Map<String, String> productL5Data, Boolean isOnlyExternal, ProductLevel3ViewConfigResponse bfbItemViewConfigs) {
    if (Objects.nonNull(bfbItemViewConfigs)) {
      String existingBfbStatus = String.valueOf(Double.parseDouble(
          bulkDownloadServiceBeanUtil.getSkuStatusValue(bfbItemViewConfigs.getBuyable(),
              bfbItemViewConfigs.getDisplay(), isOnlyExternal)));
      String newBfbStatus = isOnlyExternal ?
          productL5Data.get(BulkParameters.EXTERNAL_BFB_STATUS) :
          productL5Data.get(BulkParameters.AMPHI_BFB_STATUS);
      if (!StringUtils.equals(existingBfbStatus, newBfbStatus)) {
        bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.BFB_STATUS_CHANGED);
      }
    }
  }

  private void checkForStatusChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      Map<String, String> productL5Data, ProductLevel3ViewConfigResponse itemViewConfigs, Boolean isOnlyExternal) {
    String existingStatus = String.valueOf(Double.parseDouble(
        bulkDownloadServiceBeanUtil.getSkuStatusValue(itemViewConfigs.getBuyable(), itemViewConfigs.getDisplay(),
            isOnlyExternal)));
    String newStatus = isOnlyExternal ?
        productL5Data.get(BulkParameters.EXTERNAL_SKU_STATUS) :
        productL5Data.get(BulkParameters.AMPHI_SKU_STATUS);
    if (!StringUtils.equals(existingStatus, newStatus)) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.STATUS_CHANGED);
    }
  }

  @Override
  public List<ItemPickupPointListingL3Response> getItemPickupPointListingL3Responses(
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3Responses,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception {
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList;
    if (CollectionUtils.isEmpty(itemPickupPointListingL3Responses)) {
      itemPickupPointListingL3ResponseList = new ArrayList<>(
          getItemPickupPointListingL3ResponseList(itemPickupPointListFetchSize, itemPickupPointListingL3Request));
    } else {
      itemPickupPointListingL3ResponseList = itemPickupPointListingL3Responses;
    }
    return itemPickupPointListingL3ResponseList;
  }

  @Override
  public List<ItemPickupPointListingL3Response> getItemPickupPointListingL3ResponseList(int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception {
    Page<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponsePage = Page.empty();
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    int page = 0;
    do {
      itemPickupPointListingL3ResponsePage =
          pbpOutboundService.getItemPickupPointListingL3Response(page, size,
              itemPickupPointListingL3Request);
      itemPickupPointListingL3ResponseList.addAll(itemPickupPointListingL3ResponsePage.getContent());
      page++;
    } while (page * itemPickupPointListFetchSize < itemPickupPointListingL3ResponsePage.getTotalElements());
    return itemPickupPointListingL3ResponseList;
  }

  /**
   * construct campaign request in batch
   * @param validationPassedData
   * @param bulkAddCampaignProductQueue
   * @return list of campaign request
   * @throws Exception
   */
  private List<CampaignProductRequest> constructCampaignRequests(
      List<Map<String, String>> validationPassedData,
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue) {
    List<CampaignProductRequest> campaignProductRequests = new ArrayList<>();
    if (validationPassedData == null || validationPassedData.isEmpty()) {
      return campaignProductRequests;
    }
    for (Map<String, String> productData : validationPassedData) {
      double currentSellingPrice = 0.0;
      double finalPrice = campaignFinalPricePrecisionEnabled ?
          new BigDecimal(Double.parseDouble(productData.get(BulkParameters.HARGA_AKHIR))).setScale(
              campaignFinalPricePrecision, RoundingMode.valueOf(campaignFinalPriceRoundingMode)).doubleValue() :
          Double.parseDouble(productData.get(BulkParameters.HARGA_AKHIR));
      CampaignProductRequest campaignProductRequest = new CampaignProductRequest();
      campaignProductRequest.setQuota((int) Double.parseDouble(productData.get(BulkParameters.KUOTA)));
      campaignProductRequest.setItemSkuId(productData.get(BulkParameters.BLIBLI_SKU));
      campaignProductRequest.setCreatedBy(bulkAddCampaignProductQueue.getUpdatedBy());
      campaignProductRequest.setCampaignCode(bulkAddCampaignProductQueue.getCampaignCode());
      campaignProductRequest.setProductCode(productData.get(BulkParameters.PRODUCT_CODE));
      campaignProductRequest.setCategoryCode(productData.get(BulkParameters.CATEGORY));
      campaignProductRequest.setProductSku(productData.get(BulkParameters.PRODUCT_SKU));
      campaignProductRequest.setSkuName(productData.get(BulkParameters.SKU_NAME));
      currentSellingPrice = Double.parseDouble(productData.get(BulkParameters.SALE_PRICE));
      campaignProductRequest
        .setNormalSellingPrice(Double.parseDouble(productData.get(BulkParameters.NORMAL_SELLING_PRICE)));
      campaignProductRequest
        .setPickupPointCode(productData.get(BulkParameters.PICKUP_POINT_CODE));
      campaignProductRequest.setDiscount(currentSellingPrice - finalPrice);
      MerchantDto merchant = new MerchantDto();
      merchant.setMerchantCode(productData.get(BulkParameters.MERCHANT_CODE));
      merchant.setMerchantName(bulkAddCampaignProductQueue.getMerchantName());
      campaignProductRequest.setMerchant(merchant);
      campaignProductRequest.setPrice(currentSellingPrice);
      campaignProductRequest.setFinalPrice(finalPrice);
      campaignProductRequests.add(campaignProductRequest);
    }
    if (!multiPickupPointEnabled && !pricingMultiPickupPointEnabled) {
      addPickupPointToCampaignProductRequest(campaignProductRequests);
    }
    return campaignProductRequests;
  }

  private void addPickupPointToCampaignProductRequest(List<CampaignProductRequest> campaignProductRequests) {
    int pickupPointBatchSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.PICKUP_POINT_BATCH_SIZE).getValue());
    for (List<CampaignProductRequest> campaignProductRequestBatch : Lists.partition(campaignProductRequests,
        pickupPointBatchSize)) {
      List<String> itemSkuList =
          campaignProductRequestBatch.stream().map(CampaignProductRequest::getItemSkuId).collect(Collectors.toList());
      SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
      simpleListStringRequest.setValue(itemSkuList);
      GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponses =
          xProductOutboundService.getItemPickupPointCodeByItemSkus(simpleListStringRequest);
      Map<String, String> itemSkuPickupPointMap = itemSkuPickupPointCodeResponses.getContent().stream().collect(
          Collectors.toMap(ItemSkuPickupPointCodeResponse::getItemSku,
              ItemSkuPickupPointCodeResponse::getPickupPointCode));
      for (CampaignProductRequest campaignProductRequest : campaignProductRequestBatch) {
        campaignProductRequest.setPickupPointCode(itemSkuPickupPointMap.get(campaignProductRequest.getItemSkuId()));
      }
    }
  }

  private List<String> getPickupPointCodesV2(List<PickupPointResponse> pointResponseList) {
    List<String> pickupPointCodes = new ArrayList<>();
    for (PickupPointResponse pickupPoint : pointResponseList) {
      pickupPointCodes.add(pickupPoint.getCode());
    }
    return pickupPointCodes;
  }

  @Override
  public void processEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    LOGGER.info("Bulk process in progress, bulk process code : {}, rowNumbers : {}",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      LOGGER.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
          bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      return;
    }
    LOGGER.info("Start processing for rows : {}", bulkProcessDataList.size());
    List<Map<String, String>> productDataFromExcel = new ArrayList<>();
    Map<String, BulkProcessData> updatedBulkDataMap = new HashMap<>();
    List<BulkProcessData> failedBulkData = new ArrayList<>();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      Map<String, String> row = setBlpInitialData(bulkProcessData);
      bulkProcessData.setStartDate(new Date());
      if (!BulkUpdateServiceUtil.validateItemSkuWhenUpload(row.get(BLIBLI_SKU_HEADER))) {
        BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
        bulkUpdateErrorDTO.setProductSku(row.get(BLIBLI_SKU_HEADER));
        bulkUpdateErrorDTO.setReason("Item sku is blank");
        bulkProcessData.setEndDate(new Date());
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setInputErrorCount(1);
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkUpdateErrorDTO));
        failedBulkData.add(bulkProcessData);
      } else {
        productDataFromExcel.add(row);
        updatedBulkDataMap.put(
          (row.get(BLIBLI_SKU_HEADER) + Constant.DASH + row.get(BulkParameters.PICKUP_POINT_CODE)),
          bulkProcessDataService.saveOperation(bulkProcessData));
      }
    }

    try {
      List<Map<String, String>> validationPassedData = new ArrayList<>();
      List<Map<String, String>> validationFailedData = new ArrayList<>();
      // Validate value in every excel field
      BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
      BulkAddCampaignProductQueue bulkAddCampaignProductQueue =
          objectMapper.readValue(bulkProcess.getNotes(), BulkAddCampaignProductQueue.class);
      List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList =
          bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(productDataFromExcel, validationPassedData,
              validationFailedData, bulkUpdateErrorCounter, bulkAddCampaignProductQueue);
      if (CollectionUtils.isNotEmpty(bulkUpdateErrorDTOList)) {
        for (BulkUpdateErrorDTO bulkUpdateErrorDTO : bulkUpdateErrorDTOList) {
          String itemSku = bulkUpdateErrorDTO.getProductSku();
          String pickupPointCode = bulkUpdateErrorDTO.getPickupPointCode();
          BulkProcessData bulkProcessData =
            updatedBulkDataMap.get(itemSku + Constant.DASH + pickupPointCode);
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setInputErrorCount(1);
          bulkProcessData.setEndDate(new Date());
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkUpdateErrorDTO));
          failedBulkData.add(bulkProcessData);
          updatedBulkDataMap.remove(itemSku + Constant.DASH + pickupPointCode);
        }
      }
      bulkUpdateErrorDTOList.clear();

      try{
        if (CollectionUtils.isNotEmpty(validationPassedData)) {
          this.processBulkCreateCampaign(bulkUpdateErrorDTOList, validationPassedData,
            bulkUpdateErrorCounter, bulkAddCampaignProductQueue);
        }
      } catch (Exception e) {
        log.error(
          "Error From Campaign when adding products for process code : {} for merchant : {} ",
          bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getBusinessPartnerCode(),
          e);
        for (Map<String, String> successData : validationPassedData) {
          bulkUpdateErrorDTOList.add(
            BulkUpdateErrorDTO.builder().productName(successData.get(BulkParameters.PRODUCT_NAME))
              .productSku(successData.get(BulkParameters.BLIBLI_SKU))
              .pickupPointCode(successData.get(BulkParameters.PICKUP_POINT_CODE)).reason(
                BulkUpdateServiceUtil.constructErrorMessageCampaign(
                  successData.get(BulkParameters.BLIBLI_SKU),
                  successData.get(BulkParameters.PICKUP_POINT_CODE),
                  BulkProcessValidationErrorMessages.ADD_CAMPAIGN_PRODUCT_FAILURE_MESSAGE)).build());
        }
      }

      if (CollectionUtils.isNotEmpty(bulkUpdateErrorDTOList)) {
        for (BulkUpdateErrorDTO bulkUpdateErrorDTO : bulkUpdateErrorDTOList) {
          String itemSku = bulkUpdateErrorDTO.getProductSku();
          String pickupPointCode = bulkUpdateErrorDTO.getPickupPointCode();
          BulkProcessData bulkProcessData;
          bulkProcessData = updatedBulkDataMap.get(itemSku + Constant.DASH + pickupPointCode);
          updatedBulkDataMap.remove(itemSku + Constant.DASH + pickupPointCode);
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setSystemErrorCount(1);
          bulkProcessData.setEndDate(new Date());
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkUpdateErrorDTO));
          failedBulkData.add(bulkProcessData);
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
      LOGGER.error("Error while post processing for campaign product bulk add with bulkUpdateEventModel = {}",
          bulkUpdateEventModel, e);
      updatedBulkDataMap.values().forEach(bulkProcessData -> {
        BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
        bulkProcessNotes.setNotes(Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD);
        bulkProcessNotes.setNotes(Constant.DESCRIPTION_UPLOADING_FAILED + bulkProcessData.getParentProduct());
        bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        bulkProcessData.setEndDate(new Date());
        BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
        bulkUpdateErrorDTO.setProductSku(bulkProcessData.getParentProduct());
        bulkUpdateErrorDTO.setReason(bulkProcessNotes.getNotes());
        try {
          bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkUpdateErrorDTO));
        } catch (Exception ex) {
          log.error("Could not create error message : {} ", bulkUpdateErrorDTO, ex);
        }
        bulkProcessDataService.saveOperation(bulkProcessData);
      });
    }
  }

  private Map<String, String> setBlpInitialData(BulkProcessData bulkProcessData)
      throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    LinkedHashMap<String, String> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(),
        new TypeReference<LinkedHashMap<String, String>>() {
        });
    rowDataJson.put(Constant.ROW_NUMBER, String.valueOf(bulkProcessData.getRowNumber()));
    return rowDataJson;
  }

  @Override
  public void processEventForVatUpdate(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    LOGGER.info("Bulk process in progress, bulk process code : {}, rowNumbers : {}",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      LOGGER.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
          bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      return;
    }
    boolean isInternationalMerchant = false;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BatchVatUpdateResponse> failedItemCodes = new ArrayList<>();
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.name());
    bulkUpdateQueue.setBulkProcessCode(bulkUpdateEventModel.getBulkProcessCode());
    bulkUpdateQueue.setBusinessPartnerCode(bulkUpdateEventModel.getBusinessPartnerCode());
    bulkUpdateQueue.setRequestId(bulkProcess.getRequestId());
    bulkUpdateQueue.setUpdatedBy(bulkProcess.getCreatedBy());

    try {
      isInternationalMerchant = isInternationalMerchant(bulkUpdateEventModel.getStoreId(), bulkProcess);
      AuditTrailInfo auditTrailInfo = getAuditTrailInfo(bulkUpdateQueue);
      List<VatUpdateDto> vatUpdateDtoList = new ArrayList<>();
      List<VatUpdateDto> failedVatUpdateDtoInValidation = new ArrayList<>();
      List<VatUpdateDto> validVatUpdateDtoList = new ArrayList();
      Map<String, BulkProcessData> itemCodeToBulkProcessDataMapping = new HashMap<>();
      setMandatoryParameters(bulkUpdateEventModel.getStoreId(), bulkProcess.getCreatedBy());
      for (BulkProcessData bulkProcessData : bulkProcessDataList) {
        bulkProcessData.setStartDate(new Date());
        bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
        VatUpdateDto vatUpdateDto = objectMapper.readValue(bulkProcessData.getBulkRequestData(), VatUpdateDto.class);
        vatUpdateDtoList.add(vatUpdateDto);
        itemCodeToBulkProcessDataMapping.put(vatUpdateDto.getItemCode(), bulkProcessData);
      }
      validVatUpdateDtoList = validateVatUpdateDto(failedVatUpdateDtoInValidation, vatUpdateDtoList);
      List<BulkProcessData> updatedBulkProcessData = new ArrayList<>();
      for (VatUpdateDto failedVatUpdateDto : failedVatUpdateDtoInValidation) {
        BulkProcessData bulkProcessData =
            getBulkProcessData(itemCodeToBulkProcessDataMapping, failedVatUpdateDto, VAT_UPDATE_ERROR);
        updatedBulkProcessData.add(bulkProcessData);
      }
      for (VatUpdateDto validVatUpdateDto : validVatUpdateDtoList) {
        BulkProcessData bulkProcessData =
            getBulkProcessData(itemCodeToBulkProcessDataMapping, validVatUpdateDto, StringUtils.EMPTY);
        updatedBulkProcessData.add(bulkProcessData);
      }
      bulkVatUpdate(validVatUpdateDtoList, auditTrailInfo, counter, failedItemCodes, true);
      for (BatchVatUpdateResponse batchVatUpdateResponse : failedItemCodes) {
        VatUpdateDto vatUpdateDto = new VatUpdateDto();
        vatUpdateDto.setItemCode(batchVatUpdateResponse.getSkuCode());
        vatUpdateDto.setVatApplicable(batchVatUpdateResponse.getVatFlagRequest());
        BulkProcessData bulkProcessData = getBulkProcessData(itemCodeToBulkProcessDataMapping, vatUpdateDto,
            batchVatUpdateResponse.getErrorMessage());
        updatedBulkProcessData.add(bulkProcessData);
      }
      bulkProcessDataService.saveBulkProcessData(updatedBulkProcessData);

    } catch (Exception e) {
      log.error("Error occurred while processing bulk vat update request : storeId {} ,"
              + "bulkProcessCode {} , businessPartnerCode {}", bulkProcess.getStoreId(), bulkProcess.getBulkProcessCode(),
          bulkProcess.getBusinessPartnerCode(), e);
      getBulkUpdateServiceUtil().setFinalStatusForSystemFailure(bulkProcessDataList, bulkProcess);
      bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
    }
  }

  @Override
  public void setFinalStatusAndNotificationOnVatBulkUpdate(BulkProcess bulkProcess, String storeId) throws Exception {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkUpdateQueue.setFileName(bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    List<BatchVatUpdateResponse> failedVatUpdateResponses =
        rowDataList.stream().filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus()))
            .map(this::toBatchVatUpdateResponseFromJson).collect(Collectors.toList());
    bulkProcess.setEndDate(new Date());
    bulkProcess.setErrorCount(failedVatUpdateResponses.size());
    bulkProcess.setSuccessCount(bulkProcess.getTotalCount() - failedVatUpdateResponses.size());
    setVatInfoDescription(bulkProcess, bulkUpdateQueue, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount());
    String errorFileUrl = StringUtils.EMPTY;
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      errorFileUrl = createVatErrorFileForPartialSuccess(bulkUpdateQueue.getFileName(), getVatExcelSheetData(bulkUpdateQueue, bulkProcess),
              new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), failedVatUpdateResponses, bulkProcess, BulkParameters.SKU_CODE_HEADER);
    }
    if (generateVatErrorFile) {
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
              String.format(SUBJECT_TO_VAT_NOTIF_MSG, bulkProcess.getSuccessCount(), bulkProcess.getErrorCount())
                      + errorFileUrl, false, false);
    } else {
      notificationService.sendNotification(bulkProcess, NotificationType.VAT_UPDATE.getValue(),
              StringUtils.equals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcess.getStatus()));
    }
  }

  @Override
  public void sendFinalStatusEventForPickupPointDelete(String storeId, BulkProcess bulkProcess,
      List<BulkProcessData> allDataList) throws Exception {
    List<BulkProcessData> failedDataList =
        allDataList.stream().filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus()))
            .collect(Collectors.toList());
    String filePath = generateExcelFileIfPPCodeUpdated(bulkProcess, allDataList);
    bulkProcess.setEndDate(new Date());
    bulkProcess.setSuccessCount(bulkProcess.getTotalCount() - failedDataList.size());
    bulkProcess.setErrorCount(failedDataList.size());
    bulkProcess.setSystemErrorCount(
        (int) failedDataList.stream().filter(bulkProcessData -> Objects.nonNull(bulkProcessData.getSystemErrorCount()))
            .filter(bulkProcessData -> bulkProcessData.getSystemErrorCount() > 0).count());
    bulkProcess.setInputErrorCount(
        (int) failedDataList.stream().filter(bulkProcessData -> Objects.nonNull(bulkProcessData.getInputErrorCount()))
            .filter(bulkProcessData -> bulkProcessData.getInputErrorCount() > 0).count());
    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(bulkProcess.getBusinessPartnerCode(),
            bulkProcess.getNotes(), bulkProcess.getStatus(), failedDataList, deletePickupPointStatusImprovement);
    deletePickupPointResponseEventModel.setPpCodeUpdateFilePath(filePath);
    log.info("Publishing event  : {}, payload : {} ", kafkaTopicProperties.getDeletePickupPointResponseEvent(),
        deletePickupPointResponseEventModel);
    kafkaProducer.send(kafkaTopicProperties.getDeletePickupPointResponseEvent(),
        deletePickupPointResponseEventModel.getBusinessPartnerCode(), deletePickupPointResponseEventModel);
  }

  @Override
  public void setFinalStatusAndNotificationForWorkOrderUpload(String storeId,
    BulkProcess bulkProcess, List<BulkProcessData> bulkProcessDataList) throws Exception {
    String errorFileUrl = StringUtils.EMPTY;
    Map<String, String> processTypeErrorFileLocationMap =
      ImmutableMap.of(bulkProcess.getBulkProcessType(), ProcessorUtils.WORK_ORDER_UPLOAD_DIR);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setFileName(bulkProcess.getUploadedFile());
    List<BulkProcessData> failedDataList = bulkProcessDataList.stream()
      .filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus()))
      .collect(Collectors.toList());
    List<BulkProcessData> successData =
      bulkProcessDataList.stream().filter(rowData -> BulkProcessData.STATUS_SUCCESS.equals(rowData.getStatus()))
        .collect(Collectors.toList());
    ConverterUtil.setBulkProcessStatus(bulkProcess, bulkProcessDataList.size(), successData.size());
    setBulkUpdateSuccessAndErrorCount(bulkProcess, successData, failedDataList);
    getDescription(bulkProcess, bulkUpdateQueue, bulkProcess.getSuccessCount(),
      bulkProcess.getTotalCount(), bulkProcess.getInternationalMerchant(),
      getFailedMessageString(bulkProcess, bulkProcess.getInternationalMerchant()));
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList =
      failedDataList.stream().map(this::generateBulkUpdateErrorDTO).collect(Collectors.toList());
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      log.info("Generating Error file for work Order upload for processCode : {} ",
        bulkProcess.getBulkProcessCode());
      errorFileUrl =
        generateErrorSheetByProcessType(bulkProcess, bulkUpdateQueue, successData,
          processTypeErrorFileLocationMap, failedDataList);
    }
    log.info("sending notification for work Order upload for processCode : {} ", bulkProcess.getBulkProcessCode());
    ConverterUtil.setBulkProcessNotesForWorkOrderUpload(bulkUpdateErrorDTOList, storeId,
      bulkProcess);
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
      bulkProcess.getDescription().concat(errorFileUrl), false, false);
  }

  @Override
  public void setFinalStatusAndNotificationOnBasicInfoUpdate(String storeId,
      BulkProcess bulkProcess) throws Exception {
    List<BulkProcessData> bulkProcessDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    Map<String, List<BulkProcessData>> groupedBulkProcessData = bulkProcessDataList.stream()
        .collect(Collectors.groupingBy(BulkProcessData::getStatus));
    List<BulkProcessData> failedDataList = groupedBulkProcessData.getOrDefault(BulkProcessData.STATUS_FAIL, Collections.emptyList());
    List<BulkProcessData> successData = groupedBulkProcessData.getOrDefault(BulkProcessData.STATUS_SUCCESS, Collections.emptyList());
    ConverterUtil.setBulkProcessStatus(bulkProcess, bulkProcessDataList.size(), successData.size());
    setBulkUpdateSuccessAndErrorCount(bulkProcess, successData, failedDataList);

    String errorFileUrl = StringUtils.EMPTY;
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      log.info("Generating Error file for work Order upload for processCode : {} ",
          bulkProcess.getBulkProcessCode());
      errorFileUrl =  generateErrorFileForBasicInfoUpdate(bulkProcess, failedDataList, successData);
    }
    ConverterUtil.setBulkProcessNotesForBulkBasicInfoUpdate(failedDataList, storeId,
        bulkProcess);

    log.info("sending notification for bulk basic info update for processCode : {} ", bulkProcess.getBulkProcessCode());
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
        bulkProcess.getDescription().concat(errorFileUrl), false, false);
  }

  private String generateErrorFileForBasicInfoUpdate(BulkProcess bulkProcess,
      List<BulkProcessData> failedDataList, List<BulkProcessData> successDataList)
      throws Exception {

    Sheet uploadBasicInfoSheet = fileStorageServiceBean.getDataSheetForBasicInfo(bulkProcess.getUploadedFile(),
        bulkProcess);
    Sheet errorBasicInfoSheet = getErrorFileForBasicInfoSheet(failedDataList, uploadBasicInfoSheet);

    List<Map<String, String>> successRowNumberMapList = new ArrayList<>();
    for (BulkProcessData bulkProcessData : successDataList) {
      Map<String, String> rowNumberMap = new HashMap<>();
      // +3 to account for header and description rows
      rowNumberMap.put(Constant.DATA_ROW_NUMBER, String.valueOf(bulkProcessData.getRowNumber() - 1));
      successRowNumberMapList.add(rowNumberMap);
    }

    byte[] deletePassedBasicInfoUpdateData =
        BulkUpdateServiceUtil.deletePassedProductFromExcel(errorBasicInfoSheet, successRowNumberMapList,
            bulkProcess.getBulkProcessCode(), null,
            null);
    BulkUpdateProcessDTO processDTO =
        BulkUpdateProcessDTO.builder().bulkProcessType(BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue())
            .fileContent(deletePassedBasicInfoUpdateData).build();

    fileStoreService.createBulkFile(processDTO, StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
        ProcessorUtils.FILETYPE_XLSX_EXCEL);

    return fileStoreService.getDownloadLink(StringUtils.EMPTY,
       BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue(),
       StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
       ProcessorUtils.FILETYPE_XLSX_EXCEL);
  }

  private Sheet getErrorFileForBasicInfoSheet(List<BulkProcessData> failedDataList,
      Sheet uploadBasicInfoSheet) {
    Row headerRow = uploadBasicInfoSheet.getRow(BASIC_INFO_HEADER_INDEX);
    Set<String> headersInSheet = new HashSet<>();
    for (Cell cell : headerRow) {
      headersInSheet.add(StringUtils.trimToEmpty(cell.getStringCellValue()));
    }
    boolean instoreSeller = headersInSheet.contains(BulkParameters.INSTORE);
    boolean hasErrorColumn = headersInSheet.contains(BulkParameters.ERROR_COLUMN);
    List<String> headers = getBasicInfoHeaderList(instoreSeller, hasErrorColumn);
    int failureReasonColumnIndex = headers.size();
    Workbook workbook = uploadBasicInfoSheet.getWorkbook();
    for (Row row : uploadBasicInfoSheet) {
      Cell cell = row.getCell(instoreSeller ? BULK_BASIC_INFO_INSTORE_SHIPPING_WEIGHT_COLUMN :
          BULK_BASIC_INFO_SHIPPING_WEIGHT_COLUMN);
      if (Objects.nonNull(cell)) {
        cell.setCellFormula(null);
      }
    }
    CellStyle errorHeaderStyle =  bulkUpdateServiceUtil.createErrorHeaderStyle(workbook);
    CellStyle errorMessageStyle = bulkUpdateServiceUtil.createErrorMessageCellStyle(workbook);
    createErrorHeaderAndSetStyle(uploadBasicInfoSheet, failureReasonColumnIndex, errorHeaderStyle, errorMessageStyle);
    populateErrorMessages(failedDataList, uploadBasicInfoSheet, failureReasonColumnIndex);
    return uploadBasicInfoSheet;
  }

  private void createErrorHeaderAndSetStyle(Sheet sheet, int colIndex, CellStyle headerStyle, CellStyle errorDescriptionStyle) {
    // Header row is always at index 0
    Row headerRow = CommonUtils.getOrCreateRow(sheet, 0);
    Cell cell = headerRow.createCell(colIndex);
    cell.setCellValue(BulkParameters.ERROR_COLUMN);
    cell.setCellStyle(headerStyle);
    int firstDataRowIndex = CommonUtils.findNonNullDataRowIndexByStartRow(sheet, 1);
    int secondDataRowIndex =
      CommonUtils.findNonNullDataRowIndexByStartRow(sheet, firstDataRowIndex + 1);
    int thirdRowIndex =  CommonUtils.findNonNullDataRowIndexByStartRow(sheet,
      secondDataRowIndex + 1);
    Row headerRow1 = CommonUtils.getOrCreateRow(sheet, firstDataRowIndex);
    Cell cell1 = headerRow1.createCell(colIndex);
    cell1.setCellStyle(headerStyle);
    Row headerRow2 = CommonUtils.getOrCreateRow(sheet, secondDataRowIndex);
    Cell cell2 = headerRow2.createCell(colIndex);
    cell2.setCellStyle(headerStyle);
    
    // Merge cells from header row (0) to firstDataRowIndex + 1
    sheet.addMergedRegion(new CellRangeAddress(0, secondDataRowIndex, colIndex, colIndex));
    sheet.setColumnWidth(colIndex, bulkUpdateTemplateColumnWidth * 256);
    Row descriptionRow = CommonUtils.getOrCreateRow(sheet, thirdRowIndex);
    Cell cell3  = descriptionRow.createCell(colIndex);
    cell3.setCellValue(BulkParameters.BASIC_INFO_UPDATE_ERROR_MESSAGE);
    cell3.setCellStyle(errorDescriptionStyle);
  }

  private void populateErrorMessages(List<BulkProcessData> failedDataList, Sheet sheet,
      int colIndex) {
    for (BulkProcessData data : failedDataList) {
      int rowNum = data.getRowNumber() - 1;
      Row row = sheet.getRow(rowNum);
      Cell cell = row.createCell(colIndex);
      String message =  StringUtils.defaultIfEmpty(data.getErrorMessage(), SYSTEM_ERROR);
      cell.setCellValue(message);
    }
  }


  private BulkUpdateErrorDTO generateBulkUpdateErrorDTO(BulkProcessData bulkProcessData) {
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    bulkUpdateErrorDTO.setProductSku(bulkProcessData.getParentProduct());
    bulkUpdateErrorDTO.setReason(bulkProcessData.getErrorMessage());
    return bulkUpdateErrorDTO;
  }


  private String generateExcelFileIfPPCodeUpdated(BulkProcess bulkProcess, List<BulkProcessData> allDataList)
      throws Exception {
    List<DeleteUpdatePickUpPointResponse> ppCodeUpdatedResponseList = getPPCodeUpdatedRows(allDataList);
    String filePath = StringUtils.EMPTY;
    if (CollectionUtils.isNotEmpty(ppCodeUpdatedResponseList)) {
      BulkDownloadRequest request = new BulkDownloadRequest();
      request.setBulkProcessEntity(BulkProcessEntity.DELETE_UPDATE_PICKUP_POINTS);
      request.setRequestId(bulkProcess.getRequestId());
      request.setFileType(FileType.XLSX);
      request.setFilename(bulkProcess.getNotes() + Constant.UPDATED_PICKUP_POINTS);
      BulkDataResponse response = new DeleteUpdatePickupPointDataResponse(ppCodeUpdatedResponseList);
      BulkProcessHelper helper = bulkProcessHelperFactory.getHelper(request);
      response.setBusinessPartnerCode(bulkProcess.getBusinessPartnerCode());
      filePath = bulkProcessFileGeneration.generateFileFromResponse(request, response, helper);
      log.info("File path for generated file : {} for bulkProcessCode : {} ", filePath,
          bulkProcess.getBulkProcessCode());
    }
    return filePath;
  }

  private List<DeleteUpdatePickUpPointResponse> getPPCodeUpdatedRows(List<BulkProcessData> allDataList)
      throws JsonProcessingException {
    List<DeleteUpdatePickUpPointResponse> deleteUpdatePickUpPointResponseList = new ArrayList<>();
    for (BulkProcessData processData : allDataList) {
      if (BulkProcessData.STATUS_SUCCESS.equals(processData.getStatus()) && StringUtils.isNotEmpty(
          processData.getNotes())) {
        LinkedHashMap<String, String> ppCodeUpdatedRecords =
            objectMapper.readValue(processData.getNotes(), new TypeReference<LinkedHashMap<String, String>>() {
            });
        DeleteUpdatePickUpPointResponse deleteUpdatePickUpPointResponse = new DeleteUpdatePickUpPointResponse();
        deleteUpdatePickUpPointResponse.setItemSku(ppCodeUpdatedRecords.get(BulkParameters.ITEM_SKU_REQUEST));
        deleteUpdatePickUpPointResponse.setPickupPointCode(
            ppCodeUpdatedRecords.get(BulkParameters.NEW_PICKUP_POINT_CODE_REQUEST));
        deleteUpdatePickUpPointResponseList.add(deleteUpdatePickUpPointResponse);
      }
    }
    return deleteUpdatePickUpPointResponseList;
  }

  private void setVatInfoDescription(BulkProcess bulkProcess, BulkUpdateQueue bulkUpdateQueue, int savedProduct,
      int totalProduct) {
    String failedMessage = getFailedMessageString(bulkProcess, bulkProcess.getInternationalMerchant());
    if (bulkProcess.getInternationalMerchant()) {
      bulkProcess.setDescription(
          new StringBuilder(bulkUpdateQueue.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_SUCCESSFULLY_UPDATED_EN, savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_FAILED_UPDATE_EN, totalProduct - savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL).append(failedMessage).toString());
    } else {
      bulkProcess.setDescription(
          new StringBuilder(bulkUpdateQueue.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_SUCCESSFULLY_UPDATED, savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL)
              .append(String.format(BulkUpdateServiceUtil.VAT_FAILED_UPDATE, totalProduct - savedProduct))
              .append(BulkUpdateServiceUtil.END_SYMBOL).append(failedMessage).toString());
    }
  }

  private BatchVatUpdateResponse toBatchVatUpdateResponseFromJson(BulkProcessData bulkProcessData) {
    BatchVatUpdateResponse batchVatUpdateResponse = new BatchVatUpdateResponse();
    try {
      VatUpdateDto vatUpdateDto = objectMapper.readValue(bulkProcessData.getBulkRequestData(), VatUpdateDto.class);
      batchVatUpdateResponse = BatchVatUpdateResponse.builder().skuCode(vatUpdateDto.getItemCode())
          .vatFlagRequest(vatUpdateDto.getVatApplicable()).errorMessage(bulkProcessData.getErrorMessage()).build();
    } catch (IOException e) {
      log.error("Error while parsing vat update data json. data: {} ", bulkProcessData.getBulkRequestData(), e);
    }
    return batchVatUpdateResponse;
  }

  private BulkProcessData getBulkProcessData(Map<String, BulkProcessData> itemCodeToBulkProcessDataMapping,
      VatUpdateDto vatUpdateDto, String errorMessage) throws JsonProcessingException {
    BulkProcessData bulkProcessData = itemCodeToBulkProcessDataMapping.get(vatUpdateDto.getItemCode());
    if (StringUtils.isNotBlank(errorMessage)) {
      BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
      bulkUpdateErrorDTO.setProductSku(vatUpdateDto.getItemCode());
      bulkUpdateErrorDTO.setReason(errorMessage);
      bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
      bulkProcessData.setInputErrorCount(1);
      bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(bulkUpdateErrorDTO));
    } else {
      bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
    }
    bulkProcessData.setEndDate(new Date());
    return bulkProcessData;
  }


  private void setMandatoryParameters(String storeId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constant.USER_NAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }

  @Override
  public void setFinalStatusAndNotificationOnUpdate(BulkProcess bulkProcess, String storeId) throws Exception {
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    getBulkUpdateServiceUtil()
        .getFailedSuccessDto(bulkUpdateSuccessDTOList, bulkProcess, bulkUpdateErrorDTOList, rowDataList);
    boolean isWSConfig = false;
    try {
      WholeSaleCount wholeSaleCount = getBulkUpdateServiceUtil()
          .updateBulkFinalStatusAndSendNotification(bulkProcess, rowDataList.size(), bulkUpdateErrorDTOList,
              bulkUpdateSuccessDTOList, bulkProcess.getInternationalMerchant());
      String downloadLink =
        createWholesaleErrorWorkbook(bulkProcess.getBulkProcessCode(), bulkUpdateErrorDTOList,
          bulkUpdateSuccessDTOList);
      if (Objects.nonNull(wholeSaleCount)) {
        isWSConfig = true;
        wholeSaleCount.setDownloadFilePath(downloadLink);
      }
      getBulkUpdateServiceUtil()
          .updateBulkProcessNotes(bulkUpdateErrorDTOList, storeId, bulkProcess.getBulkProcessCode(), bulkProcess,
              isWSConfig, wholeSaleCount);
      notificationService
          .sendNotificationWithErrorFileGenerated(bulkProcess, bulkProcess.getDescription() + downloadLink, false, isWSConfig);
      if(BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus()) || BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus())) {
        Map<String, List<BulkProcessData>> productSkuXBulkProcessDataListGrouped =
          bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(storeId,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_SUCCESS);
        notificationService.sendNotificationForSchedulesRemovalForUpsetAndUpdate(productSkuXBulkProcessDataListGrouped, bulkProcess.getBusinessPartnerCode());
      }
      if (CollectionUtils.isNotEmpty(bulkUpdateSuccessDTOList)) {
        sendSecondaryNotificationForDiscountAndPromo(
            checkCampaignAvailability(storeId, bulkProcess.getRequestId(), bulkUpdateSuccessDTOList,
                bulkProcess.getBusinessPartnerCode()),
            bulkUpdateSuccessDTOList.stream().filter(BulkUpdateSuccessDTO::isDiscountAboveMax)
                .collect(Collectors.toList()), bulkUpdateSuccessDTOList, bulkProcess,
            bulkProcess.getInternationalMerchant());
      }
    } catch (Exception ex) {
      LOGGER
          .error("Error while sending notifications for bulk process code : {} ", bulkProcess.getBulkProcessCode(), ex);
    }
  }

  @Override
  public void setFinalStatusAndNotificationOnCampaignUpload(BulkProcess bulkProcess, String storeId) throws Exception {
    String errorFileUrl = StringUtils.EMPTY;
    log.info("Setting final status and sending notification for bulk process : {}", bulkProcess.getBulkProcessCode());
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    getBulkUpdateServiceUtil().getFailedSuccessDto(bulkUpdateSuccessDTOList, bulkProcess, bulkUpdateErrorDTOList,
        rowDataList);
    List<String> descriptions = Lists.newArrayList(BulkUpdateServiceUtil.PRODUCTS_CAMPAIGN_ADD,
        BulkUpdateServiceUtil.PRODUCTS_CAMPAIGN_SUCCESSFULLY_ADDED, BulkUpdateServiceUtil.PRODUCTS_CAMPAIGN_NOT_ADDED);
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    bulkUpdateErrorCounter.setInputErrorCounter(bulkProcess.getInputErrorCount());
    bulkUpdateErrorCounter.setSystemErrorCounter(bulkProcess.getSystemErrorCount());
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue =
        objectMapper.readValue(bulkProcess.getNotes(), BulkAddCampaignProductQueue.class);
    getBulkUpdateServiceUtil().updateBulkProductFinalStatus(bulkProcess, bulkAddCampaignProductQueue,
        bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(), bulkUpdateErrorDTOList, bulkUpdateErrorCounter,
        descriptions);
    List<BulkProcessData> failRowData =
      rowDataList.stream().filter(rowData -> BulkProcessData.STATUS_FAIL.equals(rowData.getStatus()))
        .collect(Collectors.toList());
    if(CollectionUtils.isNotEmpty(failRowData)) {
      log.info("Generating campaign error file for bulkProcessCode : {} ",
        bulkProcess.getBulkProcessCode());
      errorFileUrl = generateErrorSheetForCampaignUpload(bulkProcess, failRowData);
    }
    log.info("Sending notification for bulk process : {}", bulkProcess);
    notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
      String.format(CAMPAIGN_NOTIF_MSG, bulkProcess.getSuccessCount(), bulkProcess.getErrorCount())
        + errorFileUrl, false, false);
  }

  private String generateErrorSheetForCampaignUpload(BulkProcess bulkprocess,
    List<BulkProcessData> failRowData) throws Exception {
    List<List<String>> xlData = new ArrayList<>();
    List<String> headerList =
      bulkCampaignProductProcessHelper.getHeaderList(new BulkDataResponse());
    List<String> headerListWithError = new ArrayList<>(headerList);
    headerListWithError.remove(BulkParameters.KOMENTAR);
    headerListWithError.add(BulkParameters.ALASAN_KEGAGALAN);
    for (BulkProcessData bulkProcessData : failRowData) {
      TypeReference<BulkUpdateErrorDTO> typ = new TypeReference<BulkUpdateErrorDTO>() {
      };
      Map<String, String> row = setBlpInitialData(bulkProcessData);
      BulkUpdateErrorDTO bulkUpdateErrorDTO = getObjectMapper().readValue(String.valueOf(bulkProcessData.getErrorMessage()), typ);

      List<String> cellValue = new ArrayList<>();
      cellValue.add(row.get(BulkParameters.BLIBLI_SKU));
      cellValue.add(row.get(BulkParameters.PICKUP_POINT_CODE));
      cellValue.add(row.get(BulkParameters.PICKUP_POINT_NAME));
      cellValue.add(row.get(BulkParameters.PRODUCT_NAME));
      cellValue.add(row.get(BulkParameters.SKU_NAME));
      cellValue.add(row.get(BulkParameters.HARGA_NORMAL));
      cellValue.add(row.get(BulkParameters.HARGA_JUAL));
      cellValue.add(row.get(BulkParameters.POTONGAN_HARGA));
      cellValue.add(row.get(BulkParameters.PERSENTASE_DISKON));
      cellValue.add(row.get(BulkParameters.STOK_TERSEDIA));
      if (pricingCampaignRecommendationEnabled) {
        cellValue.add(row.get(BulkParameters.HARGA_REKOMENDASI));
      }
      cellValue.add(row.get(BulkParameters.REKOMENDASI));
      cellValue.add(row.get(BulkParameters.HARGA_AKHIR));
      cellValue.add(row.get(BulkParameters.KUOTA));
      cellValue.add(bulkUpdateErrorDTO.getReason());
      xlData.add(cellValue);
    }
    Workbook sheets = helper.callSuperGenerateDataSheet(headerListWithError, xlData, bulkUpdateTemplateColumnWidth);
    byte[] byteDataSheet;
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      sheets.write(outputStream);
      byteDataSheet = outputStream.toByteArray();
    }
    String filePath =
      ProcessorUtils.BULK_CAMPAIGN_ERROR_PRODUCT_CREATE_DIR + bulkprocess.getBulkProcessCode()
        + File.separator + bulkprocess.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL;

    BulkUpdateProcessDTO processDTO =
      BulkUpdateProcessDTO.builder().bulkProcessType(CAMPAIGN_ERROR.getValue())
        .fileContent(byteDataSheet).build();
    fileStoreService
      .createBulkFile(processDTO, StringUtils.left(bulkprocess.getBulkProcessCode(), CAMPAIGN_NOTIFICATION_FILE_LENGTH),
        ProcessorUtils.FILETYPE_XLSX_EXCEL);

    return fileStoreService
      .campaignErrorFilePath(filePath, CAMPAIGN_ERROR.getValue(),
        StringUtils.left(bulkprocess.getBulkProcessCode(), CAMPAIGN_NOTIFICATION_FILE_LENGTH), ProcessorUtils.FILETYPE_XLSX_EXCEL);
  }

  @Override
  @Transactional(readOnly = false)
  public void setFinalStatusAndNotificationOnInstoreUpload(BulkProcess bulkProcess, String storeId) throws Exception {
    String errorFileUrl = StringUtils.EMPTY;
    Map<String, String> processTypeErrorFileLocationMap =
      ImmutableMap.of(bulkProcess.getBulkProcessType(), ProcessorUtils.BULK_UPDATE_DIR);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setFileName(bulkProcess.getDescription());
    List<BulkProcessData> rowDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
    List<BulkProcessData> successData =
        rowDataList.stream().filter(rowData -> BulkProcessData.STATUS_SUCCESS.equals(rowData.getStatus()))
            .collect(Collectors.toList());
    List<BulkProcessData> failureData =
        rowDataList.stream().filter(rowData -> BulkProcessData.STATUS_FAIL.equals(rowData.getStatus()))
            .collect(Collectors.toList());
    checkBulkStatusAndDeleteFile(bulkProcess.getBulkProcessCode(), bulkProcess, rowDataList.size(), successData.size());
    setBulkUpdateSuccessAndErrorCount(bulkProcess, successData, failureData);
    getDescription(bulkProcess, bulkUpdateQueue, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),  bulkProcess.getInternationalMerchant(),
        getFailedMessageString(bulkProcess, bulkProcess.getInternationalMerchant()));
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      errorFileUrl = generateErrorSheetByProcessType(bulkProcess, bulkUpdateQueue, successData, processTypeErrorFileLocationMap,
        failureData);
    }
    if (generateInstoreErrorFile) {
      notificationService.sendNotificationWithErrorFileGenerated(bulkProcess,
        bulkProcess.getDescription().concat(errorFileUrl), false, false);
    } else {
      notificationService.sendNotification(bulkProcess, NotificationType.BULK_ARCHIVED.getValue(),
        StringUtils.equals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcess.getStatus()));
    }

  }

  private String generateErrorSheetByProcessType(BulkProcess bulkProcess,
    BulkUpdateQueue bulkUpdateQueue, List<BulkProcessData> bulkProcessDatas,
    Map<String, String> processTypeErrorFileLocationMap, List<BulkProcessData> failedDataList) throws Exception {
    String filePath = processTypeErrorFileLocationMap.get(bulkProcess.getBulkProcessType())
      + bulkProcess.getBulkProcessCode() + File.separator + bulkProcess.getBulkProcessCode()
      + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    String processType = bulkProcess.getBulkProcessType();
    bulkUpdateQueue.setFileName(bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    List<Map<String, String>> successRowNumberMapList = new ArrayList<>();
    for (BulkProcessData bulkProcessData : bulkProcessDatas) {
      Map<String, String> rowNumberMap = new HashMap<>();
      rowNumberMap.put(Constant.DATA_ROW_NUMBER, String.valueOf(bulkProcessData.getRowNumber() + 1));
      successRowNumberMapList.add(rowNumberMap);
    }

    Sheet sellerUploadedSheet = fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);

    if (workOrderTypes.contains(BulkProcessType.getBulkProcessType(bulkProcess.getBulkProcessType()))) {
      processType = BulkProcessType.WORK_ORDER_ERROR.getValue();
      sellerUploadedSheet = getFileWithFailureReasonForWorkOrderUpload(bulkProcess, failedDataList, sellerUploadedSheet);
    }

    byte[] deletePassedProductData =
        BulkUpdateServiceUtil.deletePassedProductFromExcel(sellerUploadedSheet, successRowNumberMapList,
            bulkProcess.getBulkProcessCode(), filePath,
            processTypeErrorFileLocationMap.get(bulkProcess.getBulkProcessType()));

    BulkUpdateProcessDTO processDTO =
      BulkUpdateProcessDTO.builder().bulkProcessType(processType)
        .fileContent(deletePassedProductData).build();
    fileStoreService.createBulkFile(processDTO, StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
      ProcessorUtils.FILETYPE_XLSX_EXCEL);
    return fileStoreService.getDownloadLink(StringUtils.EMPTY,
      processType, StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize),
      ProcessorUtils.FILETYPE_XLSX_EXCEL);
  }

  private Sheet getFileWithFailureReasonForWorkOrderUpload(BulkProcess bulkProcess,
      List<BulkProcessData> failedDataList, Sheet sheet) throws Exception {
    List<String> headers = getHeaderListByProcessTypeForWorkOrderUploads(bulkProcess.getBulkProcessType());
    List<String> headerWithError = new ArrayList<>(headers);
    headerWithError.add(BulkWorkOrderConstants.FAILURE_REASON);
    Map<Integer, String> errorMap = readErrorDataFromBulkProcessDataForWorkOrder(failedDataList);


    Row headerRow = sheet.getRow(0);
    int failureReasonColumnIndex = headerWithError.indexOf(BulkWorkOrderConstants.FAILURE_REASON);
    // Create or get the error header cell if this is not a header row
    Cell errorHeaderCell = getOrCreateCell(headerRow, failureReasonColumnIndex);
    errorHeaderCell.setCellValue(BulkWorkOrderConstants.FAILURE_REASON);

    // Populate 'Error' column with error messages from failedData list for data rows
    for (BulkProcessData bulkProcessData : failedDataList) {
      int rowNum = bulkProcessData.getRowNumber();
      String errorMessage = errorMap.getOrDefault(rowNum, "");
      rowNum = rowNum + 1;
      Row row = sheet.getRow(rowNum);
      if (row == null) {
        row = sheet.createRow(rowNum);
      }
      Cell errorCell = getOrCreateCell(row, failureReasonColumnIndex);
      errorCell.setCellValue(errorMessage);
    }

    log.info("Error messages added successfully for BulkProcess : {} ", bulkProcess.getBulkProcessCode());
    return sheet;
  }

  private Cell getOrCreateCell(Row row, int cellIndex) {
    Cell cell = row.getCell(cellIndex);
    if (cell == null) {
      cell = row.createCell(cellIndex);
    }
    cell.setCellValue(BulkWorkOrderConstants.FAILURE_REASON);
    return cell;
  }




  private Map<Integer, String> readErrorDataFromBulkProcessDataForWorkOrder(List<BulkProcessData> failedData) {
    Map<Integer, String> errorMap = new HashMap<>();
    for (BulkProcessData bulkProcessData : failedData) {
      errorMap.put(bulkProcessData.getRowNumber(), bulkProcessData.getErrorMessage());
    }
    return errorMap;
  }


  public static List<String> getHeaderListByProcessTypeForWorkOrderUploads(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case ASSEMBLY_REQUEST:
        return new ArrayList<>(BulkWorkOrderConstants.ASSEMBLY_HEADERS);
      case DISASSEMBLY_REQUEST:
        return new ArrayList<>(BulkWorkOrderConstants.DISASSEMBLY_HEADERS);
      default:
        return new ArrayList<>(BulkWorkOrderConstants.TRANSFER_REQUEST_HEADERS);
    }
  }


  private void setBulkUpdateSuccessAndErrorCount(BulkProcess bulkProcess, List<BulkProcessData> successData,
      List<BulkProcessData> failureData) {
    int systemErrorCount =
        (int) failureData.stream().filter(bulkProcessData -> Objects.nonNull(bulkProcessData.getSystemErrorCount()))
            .count();
    bulkProcess.setEndDate(new Date());
    bulkProcess.setErrorCount(failureData.size());
    bulkProcess.setSystemErrorCount(systemErrorCount);
    bulkProcess.setInputErrorCount(failureData.size() - systemErrorCount);
    bulkProcess.setSuccessCount(successData.size());
  }

  private boolean isPPCodeChangedForNonMppMerchant(String oldPPCode, String newPPCode) {
    return !(StringUtils.equalsIgnoreCase(oldPPCode, newPPCode));
  }

  private Set<String> parseColumnsToParseAsDouble() {
    if (StringUtils.isBlank(columnsToParseAsDouble)) {
      return Collections.emptySet();
    }
    return Arrays.stream(columnsToParseAsDouble.split(Constant.SEMI_COLONS)).map(String::trim)
      .filter(StringUtils::isNotBlank).collect(Collectors.toSet());
  }
}
