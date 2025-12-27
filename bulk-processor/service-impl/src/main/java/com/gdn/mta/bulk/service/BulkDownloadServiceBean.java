package com.gdn.mta.bulk.service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.gdn.mta.bulk.util.CommonUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.util.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.BulkDownloadErrorCode;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.UnifiedBulkDownloadException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentDTO;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkDownloadProductDTO;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.CampaignBulkDownloadRequest;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkDownloadQueue;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.service.download.BulkProductDataServiceBean;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.common.collect.ImmutableList;


/**
 * Created by virajjasani on 31/08/16.
 */
@Service
public class BulkDownloadServiceBean implements BulkDownloadService {

  private static final Logger LOG = LoggerFactory.getLogger(BulkDownloadServiceBean.class);
  private static final int MAX_PRODUCT_SIZE = 50;
  private static final String FILE_DESC_PENDING = " pembuatan file tertunda";
  private static final String FILE_DESC_IN_PROGRESS = " penciptaan file sedang berlangsung";
  private static final String FILE_DESC_SUCCESS = " penciptaan file berhasil";
  private static final String FILE_DESC_FAILURE = " penciptaan file gagal";
  private static final String STORE_ID = "10001";
  private static final String MAIL_TEMPLATE_ID = "BULK_PRODUCT_DOWNLOAD";
  private static final String MAIL_SENDER = "no-reply@blibli.com";
  private static final String MAIL_SUBJECT = "Produk Download";
  private static final String INVENTORY_FULFILLMENT_BLIBLI = "BL";

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.BLIBLI_SKU)
          .add(BulkParameters.PRODUCT_NAME).add(BulkParameters.SKU_CODE_HEADER)
          .add(BulkParameters.SELLER_SKU).build();
  public static final String IN_PROGRESS = "IN_PROGRESS";
  public static final String PENDING = "PENDING";
  private static final int INSTORE_FILE_START_INDEX = 8;

  @Autowired
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private POIUtil poiUtil;

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ExcelEditHelperService excelEditHelperService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${blibli.mass.template.location}")
  private String unifiedTemplateDirectory;

  @Value("${sysparam.directory.unified.file}")
  private String unifiedTemplateFile;

  @Value("${sysparam.base.directory.file-path}")
  private String fileStore;

  @Value("${sysparam.directory.unified.file.en}")
  private String unifiedTemplateFileEnglish;

  @Value("${max.bulk.requests.per.merchant:1}")
  private int maxBulkRequests;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${pickup.point.name.concat.switch.enabled}")
  private boolean pickupPointNameConcat;

  @Value("${pickup.point.name.delimiter}")
  private String ppNameDelimiter;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Override
  public void preProcess(String requestId, Map<String, Boolean> privilegedMap,
      String businessPartnerCode, Integer productSize, ProductLevel3SummaryRequest request, String username, BulkDownloadMailRecipient
      bulkDownloadMailRecipient) throws Exception {
    LOG.info(
        "invoking preProcessing of Bulk Products Download. requestId: {}, privilegedMap: {}, "
            + "businessPartnerCode: {}",
        requestId, privilegedMap, businessPartnerCode);
    try {
      String fileName = requestId + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
      bulkDownloadEntity.setBusinessPartnerCode(businessPartnerCode);
      bulkDownloadEntity.setCreatedDate(new Date());
      bulkDownloadEntity.setRequestId(requestId);
      bulkDownloadEntity.setEntityType(BulkDownloadEntity.ENTITY_TYPE_PRODUCTS);
      bulkDownloadEntity.setStatus(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue());
      bulkDownloadEntity.setDescription(fileName + FILE_DESC_PENDING);
      bulkDownloadEntity.setFileName(fileName);
      bulkDownloadEntity.setCreatedBy(username);
      saveBulkDownloadProductInfo(bulkDownloadEntity);
      sendDataToQueue(requestId, privilegedMap, businessPartnerCode, productSize,
          request,bulkDownloadMailRecipient);
    } catch (Exception e) {
      LOG.error(
          "error invoking preProcessing of Bulk Products Download. requestId: {}, privilegedMap: "
              + "{}, businessPartnerCode: {}",
          requestId, privilegedMap, businessPartnerCode, e);
      throw e;
    }
  }

  private void sendDataToQueue(String requestId, Map<String, Boolean> privilegedMap,
      String businessPartnerCode, Integer productSize,ProductLevel3SummaryRequest request,
      BulkDownloadMailRecipient bulkDownloadMailRecipient) throws Exception {
    BulkDownloadQueue bulkDownloadQueue =
        new BulkDownloadQueue(businessPartnerCode, requestId, privilegedMap, productSize, request);
    bulkDownloadQueue.setEmailTo(bulkDownloadMailRecipient.getEmailTo());

    kafkaProducer.send(kafkaTopicProperties.getBulkProductDownloadEvent(),
        bulkDownloadQueue);

    LOG.info(
        "preProcessing done. Sent object for Queue processing. requestId: {}, privilegedMap: {}, "
            + "businessPartnerCode: {}",
        requestId, privilegedMap, businessPartnerCode);
  }

  @Transactional
  public void saveBulkDownloadProductInfo(BulkDownloadEntity bulkDownloadEntity) {
    bulkDownloadAuditRepository.save(bulkDownloadEntity);
  }

  @Override
  public void postProcess(BulkDownloadQueue bulkDownloadQueue) throws Exception {
    LOG.info("invoking postProcessing of Bulk Products Download. bulkDownloadQueue: {}",
        bulkDownloadQueue);
    String requestId = bulkDownloadQueue.getRequestId();
    BulkDownloadEntity bulkDownloadEntity =
        bulkDownloadAuditRepository.findByRequestId(requestId);
    bulkDownloadEntity.setStatus(BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue());
    bulkDownloadEntity.setDescription(bulkDownloadEntity.getFileName() + FILE_DESC_IN_PROGRESS);
    try {
      saveBulkDownloadProductInfo(bulkDownloadEntity);
      ProfileResponse businessPartner = businessPartnerRepository
          .filterByBusinessPartnerCodeV2(STORE_ID,
              bulkDownloadQueue.getBusinessPartnerCode());
      boolean isBlibliFulfillment =
          (INVENTORY_FULFILLMENT_BLIBLI.equals(businessPartner.getCompany()
              .getInventoryFulfillment()));
      Map<String, String> exceptionMap = new HashMap<>();
      Workbook workbook =
          generateWorkbookBulkDownload(businessPartner, bulkDownloadQueue.getPrivilegedMap(),
              bulkDownloadQueue.getProductSize(), bulkDownloadQueue.getRequest(), exceptionMap, isBlibliFulfillment);
      bulkDownloadEntity.setStatus(BulkDownloadEntityStatus.STATUS_SUCCESS.getStatusValue());
      bulkDownloadEntity.setDescription(bulkDownloadEntity.getFileName() + FILE_DESC_SUCCESS);
      saveBulkDownloadProductInfo(bulkDownloadEntity);
      String directory = ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT);
      ProcessorUtils.createDirectories(directory + requestId);
      ProcessorUtils.createXLSXFile(
          directory + requestId + File.separator + requestId
              + ProcessorUtils.FILETYPE_XLSX_EXCEL, workbook);
      Map<String, Object> emailParameters = new HashMap<>();
      String log = BulkProductDataServiceBean.getExceptionMsgString(exceptionMap, bulkDownloadQueue.getProductSize(), "in");
      String userName = getUserName(bulkDownloadEntity.getCreatedBy());
      emailParameters.put("name", userName);
      emailParameters.put("reqId", requestId);
      emailParameters.put("businessPartnerCode", bulkDownloadEntity.getBusinessPartnerCode());
      emailParameters.put("log", log);
      BulkDownloadMailRecipient bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
      bulkDownloadMailRecipient.setEmailTo(bulkDownloadQueue.getEmailTo());
      mailDeliveryService.sendBulkDownloadEmail(MAIL_TEMPLATE_ID, MAIL_SENDER,
          MAIL_SUBJECT, emailParameters, "userName", userName, bulkDownloadMailRecipient);
      notificationService.sendDownloadNotification(bulkDownloadEntity);
      LOG.info(
          "postProcessing for Bulk Products Download successfully completed. bulkDownloadQueue: {}",
          bulkDownloadQueue);
    } catch (Exception e) {
      LOG.error("error invoking postProcessing of Bulk Products Download. bulkDownloadQueue: {}",
          bulkDownloadQueue, e);
      updateFailureStatus(bulkDownloadEntity);
      notificationService.sendDownloadNotification(bulkDownloadEntity);
      throw e;
    }
  }

  public BulkDownloadProductDTO getBulkDownloadProductByRequestId(String requestId)
      throws Exception {
    BulkDownloadEntity bulkDownloadEntity =
        bulkDownloadAuditRepository.findByRequestId(requestId);
    BulkDownloadProductDTO bulkDownloadProductDTO = new BulkDownloadProductDTO();
    BeanUtils.copyProperties(bulkDownloadEntity, bulkDownloadProductDTO);
    return bulkDownloadProductDTO;
  }

  @Override
  public BulkDownloadProductDTO getBulkDownloadProduct(String requestId, String status)
      throws Exception {
    BulkDownloadEntity bulkDownloadEntity =
        bulkDownloadAuditRepository.findByRequestIdAndStatus(requestId, status);
    if (bulkDownloadEntity == null) {
      LOG.error("Bulk Download: No record found for request {} and status {}", requestId, status);
      throw new BulkDownloadException(BulkDownloadErrorCode.RECORD_NOT_FOUND.toString(),
          BulkDownloadErrorCode.RECORD_NOT_FOUND.getErrorMessage());
    }
    BulkDownloadProductDTO bulkDownloadProductDTO = new BulkDownloadProductDTO();
    BeanUtils.copyProperties(bulkDownloadEntity, bulkDownloadProductDTO);
    return bulkDownloadProductDTO;
  }

  private String getUserName(String email) {
    return email.split("@")[0];
  }

  @Override
  public BulkDownloadFileContentDTO getFileContents(String fileId) {
    BulkDownloadFileContentDTO bulkDownloadFileContentDTO = new BulkDownloadFileContentDTO();
    File file = new File(
        ProcessorUtils.BULK_DOWNLOAD_DIR + fileId + File.separator + fileId
            + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    try (FileInputStream fileInputStream = new FileInputStream(file)) {
      bulkDownloadFileContentDTO.setFileContent(IOUtils.toByteArray(fileInputStream));
      bulkDownloadFileContentDTO.setFileAvailable(true);
      bulkDownloadFileContentDTO.setRequestId(fileId);
    } catch (Exception e) {
      LOG.error("error while reading bulk download product file. fileId: {}", fileId, e);
    }
    return bulkDownloadFileContentDTO;
  }

  @Override
  public BulkDownloadFileContentDTO getFileContents(BulkDownloadProductDTO bulkDownloadProductDTO) {
    BulkDownloadFileContentDTO bulkDownloadFileContentDTO = new BulkDownloadFileContentDTO();
    try {
      BulkProcessEntity entity = BulkProcessEntity.valueOf(bulkDownloadProductDTO.getEntityType());
      String directory = ProcessorUtils.ENTITY_DIR_MAP.get(entity);
      String filepath =
          new StringBuilder().append(directory).append(bulkDownloadProductDTO.getRequestId())
              .append(File.separator).append(bulkDownloadProductDTO.getFileName()).toString();
      File file = new File(filepath);
      bulkDownloadFileContentDTO.setRequestId(bulkDownloadProductDTO.getRequestId());
      bulkDownloadFileContentDTO.setFileAvailable(false);
      if (file.exists()) {
        FileInputStream fileInputStream = new FileInputStream(file);
        bulkDownloadFileContentDTO.setFileContent(IOUtils.toByteArray(fileInputStream));
        bulkDownloadFileContentDTO.setFileAvailable(true);
      }
    } catch (IllegalArgumentException iae) {
      LOG.error("No entity found with name {} to download file {} for request {}",
          bulkDownloadProductDTO.getEntityType(), bulkDownloadProductDTO.getFileName(),
          bulkDownloadProductDTO.getRequestId(), iae);
    } catch (Exception e) {
      LOG.error("error while reading bulk download file. fileId: {}",
          bulkDownloadProductDTO.getRequestId(), e);
    }
    return bulkDownloadFileContentDTO;
  }

  private void updateFailureStatus(BulkDownloadEntity bulkDownloadEntity) {
    bulkDownloadEntity.setStatus(BulkDownloadEntityStatus.STATUS_FAILED.getStatusValue());
    bulkDownloadEntity.setDescription(bulkDownloadEntity.getFileName() + FILE_DESC_FAILURE);
    saveBulkDownloadProductInfo(bulkDownloadEntity);
  }

  public Workbook generateWorkbookBulkDownload(ProfileResponse businessPartner,
      Map<String, Boolean> privilegedMap, Integer productSize, ProductLevel3SummaryRequest request, Map<String, String> exceptionMap, Boolean isBlibliFulfillment) throws Exception {
    boolean isBusinessPartnerO2O = businessPartner.getCompany().isOfflineToOnlineFlag();

    List<PickupPointResponse> pickupPointResponseList = this.pickupPointService
      .getPickupPointSummaryFilter(0, PickupPointFilterRequest.builder()
          .businessPartnerCode(businessPartner.getBusinessPartnerCode()).build());

    List<PickupPointDTO> pickupPointDTO = new ArrayList<>();
    copyPickupPointResponseToDto(pickupPointDTO, pickupPointResponseList);

    int pickupPointIndex = 3;
    List<String> headerList = new ArrayList<>(HEADER_LIST);
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PRICE, false)) {
      headerList.add(BulkParameters.PRICE_HEADER);
      headerList.add(BulkParameters.SELLING_PRICE_HEADER);
      pickupPointIndex += 2;
    }
    boolean isOmgSeller = CommonUtils.isOMGSeller(businessPartner);
    int index = checkForStockAndProductType(privilegedMap, headerList, isOmgSeller);
    pickupPointIndex += index;
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false)) {
      headerList.add(BulkParameters.PICKUP_POINT_HEADER);
      pickupPointIndex++;
    }
    checkForProductFlags(privilegedMap, isBusinessPartnerO2O, headerList);
    BulkDownloadProductLevel3Response result =
        getProductsFromProductService(businessPartner.getBusinessPartnerCode(), productSize, request);
    exceptionMap.putAll(result.getExceptionMap());
    List<List<String>> productContentsForXLFile = bulkDownloadServiceBeanUtil
        .getAllProductValues(privilegedMap, isBusinessPartnerO2O, result.getProductLevel3SummaryResponses(), isBlibliFulfillment,
          businessPartner, new ArrayList<>());
    Workbook workbook = poiUtil
        .generateXLFile(headerList, productContentsForXLFile, pickupPointDTO);
    bulkDownloadServiceBeanUtil
      .generateValidationForWorkbook(workbook, pickupPointResponseList.size(), pickupPointIndex,
        privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false), Constant.PICKUP_POINT_SHEET);
    return workbook;
  }

  private void copyPickupPointResponseToDto(List<PickupPointDTO> pickupPointsDto,
    List<PickupPointResponse> pickupPoints) {
    for (PickupPointResponse pickupPointResponse : pickupPoints) {
      PickupPointDTO pickupPointDTO = new PickupPointDTO();
      BeanUtils.copyProperties(pickupPointResponse, pickupPointDTO);
      pickupPointsDto.add(pickupPointDTO);
    }
  }

  public String generateWholeSaleErrorWorkbookBulkDownload(String bulkProcessCode,
      List<List<String>> productContentsForXLFile) throws Exception {
    List<String> headerList = new ArrayList<>();
    headerList.add(BulkParameters.ITEM_SKU);
    headerList.add(BulkParameters.PICKUP_POINT_HEADER);
    headerList.add(BulkParameters.REASON);
    Workbook workbook = poiUtil.generateXLFileForWholesale(headerList, productContentsForXLFile);
    String directory = ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.FAILED_PRODUCT);
    String path = directory + bulkProcessCode + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    byte[] byteDataSheet;
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      workbook.write(outputStream);
      byteDataSheet = outputStream.toByteArray();
    }
    BulkUpdateProcessDTO processDTO = BulkUpdateProcessDTO.builder().fileContent(byteDataSheet)
      .bulkProcessType(BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue()).build();
    fileStorageService.createBulkFile(processDTO, bulkProcessCode, ProcessorUtils.FILETYPE_XLSX_EXCEL);

    return fileStorageService
      .getDownloadLink(path, BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue(), bulkProcessCode,
        ProcessorUtils.FILETYPE_XLSX_EXCEL);
  }


  public String generateEanErrorWorkBookBulkDownload(String bulkProcessCode,
      List<List<String>> productContentsForXLFile) throws Exception {
    List<String> headerList = new ArrayList<>();
    headerList.add(BulkParameters.NAMA_PRODUK);
    headerList.add(BulkParameters.ITEM_SKU);
    headerList.add(BulkParameters.REASON);
    Workbook workbook = poiUtil.generateXLFileForWholesale(headerList, productContentsForXLFile);
    String directory = ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.FAILED_PRODUCT);
    String path = directory + bulkProcessCode + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    byte[] byteDataSheet;
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      workbook.write(outputStream);
      byteDataSheet = outputStream.toByteArray();
    }
    BulkUpdateProcessDTO processDTO = BulkUpdateProcessDTO.builder().fileContent(byteDataSheet)
        .bulkProcessType(BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue()).build();
    fileStorageService.createBulkFile(processDTO, bulkProcessCode, ProcessorUtils.FILETYPE_XLSX_EXCEL);

    return fileStorageService
        .getDownloadLink(path, BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue(), bulkProcessCode,
            ProcessorUtils.FILETYPE_XLSX_EXCEL);
  }

  private BulkDownloadProductLevel3Response getProductsFromProductService(
      String businessPartnerCode, Integer productSize, ProductLevel3SummaryRequest request) {
    List<ProductLevel3SummaryResponse> productSummaryResponseList = new ArrayList<>();
    BulkDownloadProductLevel3Response gdnSkus = new BulkDownloadProductLevel3Response();
    Map<String, String> exceptionMap = new HashMap<>();
    int i = 0;
    while ((i * MAX_PRODUCT_SIZE) < productSize) {
      Pageable pageable = PageRequest.of(i, MAX_PRODUCT_SIZE);
      try {
        BulkDownloadProductLevel3Response gdnSku = productLevel3Repository
            .findSummaryByFilterForBulkDownload(businessPartnerCode, pageable, request);
        if ((gdnSku == null) || (CollectionUtils.isEmpty(gdnSku.getProductLevel3SummaryResponses())
            && org.springframework.util.CollectionUtils.isEmpty(gdnSku.getExceptionMap()))) {
          break;
        }
        productSummaryResponseList.addAll(gdnSku.getProductLevel3SummaryResponses());
        exceptionMap.putAll(gdnSku.getExceptionMap());
      } catch (Exception e) {
        LOG.error(
            "error invoking product summary from product service client. businessPartnerCode: {},"
                + " page: {}, size: {}",
            businessPartnerCode, i, MAX_PRODUCT_SIZE, e);
      }
      i++;
    }
    gdnSkus.setProductLevel3SummaryResponses(productSummaryResponseList);
    gdnSkus.setExceptionMap(exceptionMap);
    return gdnSkus;
  }


  public static void checkForProductFlags(Map<String, Boolean> privilegedMap,
      boolean isBusinessPartnerO2O, List<String> headerList) {
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, false)) {
      headerList.add(BulkParameters.PUBLISHED_HEADER);
      headerList.add(BulkParameters.PURCHASED_HEADER);
    }
    if (isBusinessPartnerO2O && privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_O2O, false)) {
      headerList.add(BulkParameters.OFFLINE_TO_ONLINE_HEADER);
    }
  }

  public int checkForStockAndProductType(Map<String, Boolean> privilegedMap,
      List<String> headerList, boolean isOmgSeller) {
    int index = 0;
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_STOCK, false)) {
      headerList.add(BulkParameters.STOCK_HEADER);
      index++;
      if (isOmgSeller && preOrderQuotaFeatureSwitch) {
        headerList.add(BulkParameters.PO_QUOTA);
        index++;
      }
    }
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PRODUCT_TYPE, false)) {
      headerList.add(BulkParameters.TYPE_HANDLING_HEADER);
      index++;
    }
    return index;
  }
  
  public BulkDownloadFileContentDTO getBulkProcessProductFile(String storeId, String bulkProcessCode) throws Exception{
    BulkDownloadFileContentDTO bulkDownloadFileContentDTO = new BulkDownloadFileContentDTO();
    bulkDownloadFileContentDTO.setFileAvailable(false);
    bulkDownloadFileContentDTO.setRequestId(bulkProcessCode);

    BulkProcess bulkProcess = bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId,
                    bulkProcessCode);
    
    if(bulkProcess == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Bulk process is not found for bulkProcessCode: " 
          + bulkProcessCode);
    }

    String directoryPath;
    if (bulkProcess.getBulkUpdate()) {
      directoryPath = ProcessorUtils.BULK_UPDATE_DIR;
    } else {
      directoryPath = ProcessorUtils.DATA_BASE_DIR;
    }
    File file = new File(directoryPath + bulkProcessCode + File.separator + bulkProcessCode
            + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    if(file.exists()){
      try (FileInputStream fileInputStream = new FileInputStream(file)) {
        bulkDownloadFileContentDTO.setFileContent(IOUtils.toByteArray(fileInputStream));
        bulkDownloadFileContentDTO.setFileAvailable(true);
        bulkDownloadFileContentDTO.setRequestId(bulkProcessCode);
      } catch (Exception e) {
        LOG.error("error while reading bulk download product file. bulkProcessCode: {}", bulkProcessCode, e);
      }
    }
    return bulkDownloadFileContentDTO;
  }

  @Override
  public UnifiedBulkDownloadDTO downloadProductUnifiedTemplate(String storeId, String requestId,
      String businessPartnerCode, Set<String> pickupPointCodes) throws Exception {
    UnifiedBulkDownloadEvent lastDownloadStatus =
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(storeId, businessPartnerCode);
    if (Objects.nonNull(lastDownloadStatus) && StringUtils
        .equalsIgnoreCase(lastDownloadStatus.getDownloadStatus(), Constant.UNIFIED_DOWNLOAD_IN_PROGRESS)) {
      LOG.error("File Download in progress for requestId : {} businessPartnerCode : {}", requestId,
          businessPartnerCode);
      throw new UnifiedBulkDownloadException(BulkDownloadErrorCode.DOWNLOAD_IN_PROGRESS.toString(),
          BulkDownloadErrorCode.DOWNLOAD_IN_PROGRESS.getErrorMessage());
    }
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO;
    try {
      unifiedBulkDownloadService
          .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_IN_PROGRESS, businessPartnerCode, pickupPointCodes);
      ProfileResponse profileResponse = businessPartnerRepository.filterByBusinessPartnerCodeV2(storeId, businessPartnerCode);
      ShippingTypeEligibility shippingTypeEligibility =
        ConverterUtil.getShippingEligibility(profileResponse, bpBopisRestrictionEnabled);

      BulkInternalUploadRequestDTO requestDTO = BulkInternalUploadRequestDTO.builder()
          .bulkInternalProcessType(BulkInternalProcessType.GENERIC_FILE_GENERATION)
          .businessPartnerCode(profileResponse.getBusinessPartnerCode()).instore(
              CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, profileResponse))
          .build();
      Boolean isFileGenerationNeeded = fileStorageService.isFileExists(requestDTO);

      boolean isGlobalFlag = checkForGlobalFieldAndFileExists(storeId, isFileGenerationNeeded, lastDownloadStatus);
      SystemParameterConfig brandSheetAppendInReviewBrandConfig = systemParameterConfigService
          .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG);
      boolean isAppendInReviewBrand = Boolean.parseBoolean(brandSheetAppendInReviewBrandConfig.getValue());
      List<PredefinedAllowedAttributeValueResponse> inReviewBrands = new ArrayList<>();
      if(isAppendInReviewBrand) {
        inReviewBrands= getInReviewBrands(businessPartnerCode);
      }
      LOG.debug("Start excel file operation");
      MerchantStatusType merchantStatusType = setMerchantType(profileResponse);
      boolean instoreSeller = CommonUtils.isInstoreEligibleSeller(profileResponse);
      String unifiedTemplateFileEn;
      String unifiedTemplateFileIn;
      String merchantType =  profileResponse.getCompany().getMerchantType();
      int index =
          getIndexByMerchantTypeAndEligibilityForBundling(merchantStatusType, instoreSeller, merchantType);
      unifiedTemplateFileEn = Arrays.asList(unifiedTemplateFileEnglish.split(Constant.COMMA)).get(index-1);
      unifiedTemplateFileIn = Arrays.asList(unifiedTemplateFile.split(Constant.COMMA)).get(index-1);
      if (profileResponse.getCompany().isInternationalFlag()) {
        unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse, inReviewBrands,
          unifiedTemplateFileEn, isGlobalFlag, lastDownloadStatus, isAppendInReviewBrand,
          pickupPointNameConcat, ppNameDelimiter, shippingTypeEligibility,
          requestDTO.getBulkInternalProcessType(), pickupPointCodes);
      } else {
        unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse, inReviewBrands,
          unifiedTemplateFileIn, isGlobalFlag, lastDownloadStatus, isAppendInReviewBrand,
          pickupPointNameConcat, ppNameDelimiter, shippingTypeEligibility,
          requestDTO.getBulkInternalProcessType(), pickupPointCodes);
      }
      LOG.debug("Done excel file operation");
      unifiedBulkDownloadService
          .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_COMPLETE, businessPartnerCode, pickupPointCodes);
    } catch (Exception exception) {
      LOG.error("Error while downloading unified product template requestId: {}, BusinessPartnerCode {}", requestId,
          businessPartnerCode, exception);
      unifiedBulkDownloadService
          .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_ABORT, businessPartnerCode, pickupPointCodes);
      throw exception;
    }
    return unifiedBulkDownloadDTO;
  }

  private int getIndexByMerchantTypeAndEligibilityForBundling(MerchantStatusType sellerType, boolean instoreSeller,
      String merchantType) {
    int index = sellerType.getType();
    if (instoreNewFlowEnabled && instoreSeller) {
      index = INSTORE_FILE_START_INDEX + index;
    }
    if (productBundlingEnabled && Arrays.asList(productBundlingEligibleMerchantTypes.split(Constant.COMMA))
        .contains(merchantType)) {
      index = index + MerchantStatusType.values().length;
    }
    return index;
  }

  private static List<String> getSalesChannelFromProfileResponse(ProfileResponse profileResponse) {
    List<String> salesChannel = new ArrayList<>();
    if (Objects.nonNull(profileResponse.getCompany()) && CollectionUtils.isNotEmpty(
        profileResponse.getCompany().getSalesChannel())) {
      salesChannel = profileResponse.getCompany().getSalesChannel();
    }
    return salesChannel;
  }


  public static MerchantStatusType setMerchantType(ProfileResponse profileResponse) {
    List<String> salesChannel = getSalesChannelFromProfileResponse(profileResponse);
    boolean isCncSeller = Objects.nonNull(profileResponse.getCompany()) && profileResponse.getCompany().isCncActivated()
        && !salesChannel.contains(BulkProcessConstant.B2B_SELLER_CHANNEL);
    boolean isBfbAndCncSeller =
        Objects.nonNull(profileResponse.getCompany()) && profileResponse.getCompany().isCncActivated()
            && salesChannel.contains(BulkProcessConstant.B2B_SELLER_CHANNEL);
    boolean isBfbSeller =
        Objects.nonNull(profileResponse.getCompany()) && !profileResponse.getCompany().isCncActivated()
            && salesChannel.contains(BulkProcessConstant.B2B_SELLER_CHANNEL);
    if (isCncSeller) {
      return MerchantStatusType.DELIVERY_AND_CNC;
    } else if (isBfbAndCncSeller) {
      return MerchantStatusType.BFB_AND_CNC;
    } else if (isBfbSeller) {
      return MerchantStatusType.BFB;
    } else
      return MerchantStatusType.PURE_DELIVERY;
  }

  private boolean checkForGlobalFieldAndFileExists(String storeId, Boolean destinationFileExists,
      UnifiedBulkDownloadEvent lastDownloadStatus) {
    if (!destinationFileExists || Objects.isNull(lastDownloadStatus)) {
      return true;
    }
    SystemParameterConfig categorySheetConfig = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    SystemParameterConfig brandSheetConfig = systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    if ((lastDownloadStatus.getLastDownloadedTime().compareTo(categorySheetConfig.getUpdatedDate()) < 0
        || lastDownloadStatus.getLastDownloadedTime().compareTo(brandSheetConfig.getUpdatedDate()) < 0)) {
      return true;
    }
    return false;
  }

  private List<PredefinedAllowedAttributeValueResponse> getInReviewBrands(String businessPartnerCode) {
    List<PredefinedAllowedAttributeValueResponse> response =
        pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, businessPartnerCode, true, true);
    if (CollectionUtils.isEmpty(response)) {
      return new ArrayList<>();
    } else {
      response.sort(Comparator
          .comparing(predefinedAllowedAttributeValueResponse -> predefinedAllowedAttributeValueResponse.getValue()));
      return response;
    }
  }

  @Override
  public long countNumberOfCampaignDownloads(String storeId, String businessPartnerCode,
      CampaignBulkDownloadRequest request) throws Exception {
    List<BulkDownloadEntity> bulkDownloadEntityList = bulkDownloadAuditRepository
        .findByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.CAMPAIGN_PRODUCT.name(),
            businessPartnerCode, Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    long count = 0;
    for (BulkDownloadEntity bulkDownloadEntity : bulkDownloadEntityList) {
      if (StringUtils.isNotEmpty(bulkDownloadEntity.getDescription())
          && Constant.OPEN_CURLY_BRACES == bulkDownloadEntity.getDescription().charAt(0)
          && Constant.CLOSE_CURLY_BRACES == bulkDownloadEntity.getDescription()
          .charAt(bulkDownloadEntity.getDescription().length() - 1)) {
        BulkDownloadRequest bulkDownloadRequest =
            objectMapper.readValue(bulkDownloadEntity.getDescription(), new TypeReference<BulkDownloadRequest>() {
            });
        CampaignProductDownloadRequest campaignDownloadRequest = (CampaignProductDownloadRequest) bulkDownloadRequest;
        if ((Objects.nonNull(campaignDownloadRequest)) && request.getPage() == campaignDownloadRequest.getPage()
            && request.getCampaignCode().equals(campaignDownloadRequest.getCampaignCode())) {
          count++;
        }
      }
    }
    return count;
  }

  @Override
  public BulkInternalPendingRequestResponse countPendingRequestsByUsernameAndDownloadType(
      String businessPartnerCode, String username, String downloadType) throws Exception {
    BulkInternalPendingRequestResponse bulkInternalPendingRequestResponse = new BulkInternalPendingRequestResponse();
    Integer count =
        bulkDownloadAuditRepository.countByEntityTypeAndBusinessPartnerCodeAndStatusInAndCreatedByAndMarkForDeleteFalse(
            downloadType, businessPartnerCode, Arrays.asList(PENDING, IN_PROGRESS), username);
    bulkInternalPendingRequestResponse.setPendingRequestsCount(count);
    if (count < maxBulkRequests) {
      bulkInternalPendingRequestResponse.setBulkInternalStatusFlag(true);
    } else {
      bulkInternalPendingRequestResponse.setBulkInternalStatusFlag(false);
    }
    return bulkInternalPendingRequestResponse;
  }

  @Async
  @Transactional(readOnly = false)
  @Override
  public void clearInProgressDownloads(String storeId, String entityType, String status) {
    Integer abortTimeInMinute = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.STORE_COPY_ABORT_IN_PROGRESS_DOWNLOADS_BULK_DOWNLOAD_ENTITY).getValue());
    Date pendingToAbortDate = DateUtils.addMinutes(new Date(), -abortTimeInMinute);
    LOG.info("Aborting all in progress downloads for storeId {} before {}", storeId, pendingToAbortDate);
    bulkDownloadAuditRepository.updateStatusInProgressBulkDownloadEntityToAborted(pendingToAbortDate, status,
        entityType);
  }
}
