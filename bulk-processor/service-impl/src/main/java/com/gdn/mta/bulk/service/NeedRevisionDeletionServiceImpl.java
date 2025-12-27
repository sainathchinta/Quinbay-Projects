package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.ProductCodeAndNameResponseList;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.NeedRevisionDeletionEventModel;
import com.gdn.mta.bulk.repository.BulkNeedRevisionDeletionDataRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.NeedRevisionDeletionRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Service
public class NeedRevisionDeletionServiceImpl implements NeedRevisionDeletionService {

  public static final String NEED_REVISION_DELETION_TYPE = "NEED_REVISION_DELETION";
  public static final String DELETED_PRODUCTS = "Deleted Products";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private NeedRevisionDeletionRepository needRevisionDeletionRepository;

  @Autowired
  private BulkNeedRevisionDeletionDataRepository needRevisionDeletionDataRepository;

  @Autowired
  private BulkNeedRevisionDeletionDataRepository bulkNeedRevisionDeletionDataRepository;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${batch.size.for.fetching.nr.deletion}")
  private int batchSizeForFetchingNrDeletion;

  @Value("${gcs.need.revision.deletion.upload.file.path}")
  private String gcsNeedRevisionDeletionUploadFilePath;

  @Value("${auto.need.revision.deletion.merchant.types}")
  private String autoNeedRevisionDeletionMerchantTypes;

  @Value("${fetch.published.count.for.need.revision.deletion}")
  private int fetchPublishedCountForNeedRevisionDeletion;

  @Value("${batch.size.to.publish.data.entries}")
  private int batchSizeToPublishDataEntries;

  @Override
  public void populateNRBusinessPartnerCodes(String storeId, String requestId) {
    List<String> businessPartnerCodeList = productRepository.fetchNRBusinessPartnerCodes(storeId, requestId);
    if (CollectionUtils.isNotEmpty(businessPartnerCodeList)) {
      List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
          setBulkNeedRevisionDeletionEntities(storeId, businessPartnerCodeList);
      needRevisionDeletionRepository.saveAll(bulkNeedRevisionDeletionList);
    }
  }

  private static List<NeedRevisionEligibilityRequest> getNeedRevisionEligibilityRequestList(
    List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDataList,
    String businessPartnerCode) {
    List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequestList = new ArrayList<>();
    for (BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData : bulkNeedRevisionDeletionDataList) {
      NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
        NeedRevisionEligibilityRequest.builder().businessPartnerCode(businessPartnerCode)
          .productCode(bulkNeedRevisionDeletionData.getProductCode()).build();
      needRevisionEligibilityRequestList.add(needRevisionEligibilityRequest);

    }
    return needRevisionEligibilityRequestList;
  }

  @Override
  public void processNeedRevisionDeletion(String storeId, Integer fetchProcessCountForDeletion) {
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionInProgressList =
        needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
            storeId, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
            PageRequest.of(0, fetchProcessCountForDeletion));

    List<String> businessPartnerCodes = bulkNeedRevisionDeletionInProgressList.stream()
        .map(BulkNeedRevisionDeletion::getBusinessPartnerCode).collect(Collectors.toList());
    log.info("Processing need revision deletion for business partners {} ", businessPartnerCodes);
    for (BulkNeedRevisionDeletion bulkNeedRevisionDeletion :
        bulkNeedRevisionDeletionInProgressList) {
      bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
      bulkNeedRevisionDeletion = needRevisionDeletionRepository.save(bulkNeedRevisionDeletion);
      List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDataList =
          needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(storeId,
              bulkNeedRevisionDeletion.getDeletionProcessCode());
      List<List<BulkNeedRevisionDeletionData>> bulkNeedRevisionDeletionDataBatches =
          Lists.partition(bulkNeedRevisionDeletionDataList, batchSizeToPublishDataEntries);
      BulkNeedRevisionDeletion finalBulkNeedRevisionDeletion = bulkNeedRevisionDeletion;
      bulkNeedRevisionDeletionDataBatches.forEach(
          bulkNeedRevisionDeletionDataBatch -> publishNeedRevisionDeletionEventsForDataInBatches(
              finalBulkNeedRevisionDeletion, bulkNeedRevisionDeletionDataBatch));
    }
  }

  @Override
  public void performEligibilityCheckAndProcessDataDeletion(String storeId,
      List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDataList,
      String businessPartnerCode) throws ApplicationException {
    List<NeedRevisionEligibilityRequest> eligibilityRequests =
        getNeedRevisionEligibilityRequestList(bulkNeedRevisionDeletionDataList,
            businessPartnerCode);

    // Fetch eligibility responses in a single batch
    log.info("Calling PBP to fetch Eligibility for products belonging to Seller : {} , total "
            + "count of products : {} ", businessPartnerCode,
        eligibilityRequests.stream().map(NeedRevisionEligibilityRequest::getProductCode).distinct()
            .count());

    Map<String, NeedRevisionEligibilityResponse> eligibilityResponseMap =
        productRepository.getEligibilityForNeedRevisionDeletion(storeId, eligibilityRequests)
            .stream().collect(
                Collectors.toMap(NeedRevisionEligibilityResponse::getProductCode,
                    Function.identity()));
    // Process each deletion data and update status
    processDataDeletion(storeId, bulkNeedRevisionDeletionDataList, eligibilityResponseMap,
        businessPartnerCode);
    needRevisionDeletionDataRepository.saveAll(bulkNeedRevisionDeletionDataList);
  }

  @Override
  public void saveBulkNeedRevisionDeletion(BulkNeedRevisionDeletion bulkNeedRevisionDeletion) {
    needRevisionDeletionRepository.save(bulkNeedRevisionDeletion);
  }

  @Override
  public List<BulkNeedRevisionDeletionData> saveBulkNeedRevisionDeletionData(
      List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionData) {
    return needRevisionDeletionDataRepository.saveAll(bulkNeedRevisionDeletionData);
  }

  private void publishNeedRevisionDeletionEventsForDataInBatches(
      BulkNeedRevisionDeletion bulkNeedRevisionDeletion,
      List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionData) {
    NeedRevisionDeletionEventModel needRevisionDeletionEventModel =
        new NeedRevisionDeletionEventModel();
    needRevisionDeletionEventModel.setBusinessPartnerCode(
        bulkNeedRevisionDeletion.getBusinessPartnerCode());
    needRevisionDeletionEventModel.setDeletionProcessCode(
        bulkNeedRevisionDeletion.getDeletionProcessCode());
    needRevisionDeletionEventModel.setStoreId(bulkNeedRevisionDeletion.getStoreId());
    needRevisionDeletionEventModel.setNeedRevisionDeletionDataIds(
        bulkNeedRevisionDeletionData.stream().map(BulkNeedRevisionDeletionData::getId)
            .collect(Collectors.toList()));
    String needRevisionDeletionEvent = kafkaTopicProperties.getNeedRevisionDeletionEvent();
    kafkaProducer.send(needRevisionDeletionEvent,
        needRevisionDeletionEventModel.getBusinessPartnerCode(), needRevisionDeletionEventModel);
    log.info("Published Need Revision Deletion event {} for process code {} ",
        needRevisionDeletionEvent, bulkNeedRevisionDeletion.getDeletionProcessCode());
  }

  private void processDataDeletion(String storeId,
    List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDataList,
    Map<String, NeedRevisionEligibilityResponse> eligibilityResponseMap, String businessPartnerCode) {

    log.info("Total calls to PBP for Eligible deletion for seller : {} is {} ",
      businessPartnerCode, eligibilityResponseMap.values().stream().filter(
        needRevisionEligibilityResponse -> Boolean.TRUE.equals(
          needRevisionEligibilityResponse.getEligibleForDeletion())).count());

    for (BulkNeedRevisionDeletionData deletionData : bulkNeedRevisionDeletionDataList) {
      NeedRevisionEligibilityResponse eligibilityResponse =
        eligibilityResponseMap.get(deletionData.getProductCode());

      if (Objects.nonNull(eligibilityResponse)) {
        processEligibilityResponse(storeId, deletionData, eligibilityResponse);
      } else {
        // Default to 'Aborted' if eligibility response is null
        setAbortedStatus(deletionData);
      }
    }
  }

  private void processEligibilityResponse(String storeId, BulkNeedRevisionDeletionData deletionData,
      NeedRevisionEligibilityResponse eligibilityResponse) {
    try {
      if (Boolean.TRUE.equals(eligibilityResponse.getEligibleForDeletion())) {
        // Perform product deletion if eligible
        DeleteProductRequest deleteRequest = new DeleteProductRequest();
        deleteRequest.setProductCode(eligibilityResponse.getProductCode());
        deleteRequest.setProductName(eligibilityResponse.getProductName());
        deleteRequest.setNotes(Constant.BULK_NEED_REVISION_POST_LIVE_DELETION);

        boolean deleted = productRepository.deleteProductCollection(storeId, deleteRequest, false);
        String status = deleted ?
            BulkNeedRevisionDeletion.STATUS_FINISHED :
            BulkNeedRevisionDeletion.STATUS_FAIL;
        deletionData.setEligibleForDeletion(deleted);
        deletionData.setStatus(status);
      } else {
        // Mark as fail if not eligible for deletion i.e active campaign or order history
        // [eligible for deletion false/null]
        deletionData.setEligibleForDeletion(eligibilityResponse.getEligibleForDeletion());
        deletionData.setStatus(BulkNeedRevisionDeletion.STATUS_FAIL);
      }
      deletionData.setUpdatedDate(new Date());
    } catch (Exception exception) {
      deletionData.setUpdatedDate(new Date());
      deletionData.setEligibleForDeletion(false);
      deletionData.setStatus(BulkNeedRevisionDeletion.STATUS_FAIL);
    }
  }

  private void setAbortedStatus(BulkNeedRevisionDeletionData deletionData) {
    deletionData.setEligibleForDeletion(null);
    deletionData.setStatus(BulkNeedRevisionDeletion.STATUS_ABORTED);
    deletionData.setUpdatedDate(new Date());
  }

  @Override
  public void sendNotificationForNeedRevisionDeletion(String storeId, String requestId) throws Exception {
    if (checkAndUpdatePublishedToProcessedForNeedRevisionDeletion(storeId)) {
      Page<BulkNeedRevisionDeletion> needRevisionDeletionsPage =
          needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(storeId,
              BulkNeedRevisionDeletion.STATUS_PROCESSED, PageRequest.of(Constant.ZERO, batchSizeForFetchingNrDeletion));
      List<BulkNeedRevisionDeletion> needRevisionDeletionList =
          Optional.ofNullable(needRevisionDeletionsPage).map(Page::getContent).orElse(Collections.emptyList());
      List<BulkNeedRevisionDeletion> statusUpdateList = new ArrayList<>();
      for (BulkNeedRevisionDeletion needRevisionDeletion : needRevisionDeletionList) {
        List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionData =
            bulkNeedRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(storeId,
                needRevisionDeletion.getDeletionProcessCode());
        List<BulkNeedRevisionDeletionData> eligibleForDeletionList =
            bulkNeedRevisionDeletionData.stream().filter(Objects::nonNull)
                .filter(bulkData -> StringUtils.equals(bulkData.getStatus(), BulkNeedRevisionDeletion.STATUS_FINISHED))
                .collect(Collectors.toList());
        List<BulkNeedRevisionDeletionData> eligibleForNextDeletionList =
            bulkNeedRevisionDeletionData.stream().filter(Objects::nonNull)
                .filter(bulkData -> Boolean.FALSE.equals(bulkData.getEligibleForDeletion()))
                .collect(Collectors.toList());
        try (SXSSFWorkbook eligibleForDeletionFile = POIUtil.generateExcelFileForNotification(
            eligibleForDeletionList)) {
          if (eligibleForDeletionFile.getSheetAt(0).getPhysicalNumberOfRows() > 1) {
            byte[] bytes = POIUtil.getByteContentFromExcel(eligibleForDeletionFile.getSheet(DELETED_PRODUCTS));
            uploadFileToGcs(needRevisionDeletion, bytes);
          }
        }
        sendNotification(needRevisionDeletion, eligibleForDeletionList,
            Optional.of(eligibleForNextDeletionList).orElse(new ArrayList<>()).size());
        updateStatusInNeedRevisionDeletionTable(needRevisionDeletion, bulkNeedRevisionDeletionData, statusUpdateList);
      }
      needRevisionDeletionRepository.saveAll(statusUpdateList);
    } else {
      log.info("No Processed entries in need revision deletion table to send notification : {} ", requestId);
    }
  }

  private boolean checkAndUpdatePublishedToProcessedForNeedRevisionDeletion(String storeId) {
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionPublishedList =
        needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(storeId,
            BulkNeedRevisionDeletion.STATUS_PUBLISHED, PageRequest.of(0, fetchPublishedCountForNeedRevisionDeletion));
    Map<String, BulkNeedRevisionDeletion> bulkNeedRevisionDeletionMap = bulkNeedRevisionDeletionPublishedList.stream()
        .collect(Collectors.toMap(BulkNeedRevisionDeletion::getDeletionProcessCode, Function.identity()));
    List<String> bulkNeedRevisionDeletionDataPendingList =
        getBulkNeedRevisionDeletionDataPendingList(storeId, bulkNeedRevisionDeletionMap);
    needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(
        storeId, new ArrayList<>(bulkNeedRevisionDeletionMap.keySet()),
        Arrays.asList(BulkNeedRevisionDeletion.STATUS_PENDING, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS));
    if (CollectionUtils.isNotEmpty(bulkNeedRevisionDeletionDataPendingList)) {
      for (String deleteProcessCode : bulkNeedRevisionDeletionDataPendingList) {
        bulkNeedRevisionDeletionMap.remove(deleteProcessCode);
      }
    }
    if (MapUtils.isEmpty(bulkNeedRevisionDeletionMap)) {
      return false;
    }
    List<BulkNeedRevisionDeletion> updateBulkNeedRevisionProcessList = new ArrayList<>();
    for (Map.Entry<String, BulkNeedRevisionDeletion> entry : bulkNeedRevisionDeletionMap.entrySet()) {
      setBulkNRDeletionListToProcessed(entry, updateBulkNeedRevisionProcessList);
    }
    needRevisionDeletionRepository.saveAll(updateBulkNeedRevisionProcessList);
    return true;
  }

  private List<String> getBulkNeedRevisionDeletionDataPendingList(String storeId, Map<String, BulkNeedRevisionDeletion> bulkNeedRevisionDeletionMap) {
    return needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(
            storeId, new ArrayList<>(bulkNeedRevisionDeletionMap.keySet()),
            Arrays.asList(BulkNeedRevisionDeletion.STATUS_PENDING, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS)).stream()
        .map(BulkNeedRevisionDeletionData::getDeletionProcessCode).collect(Collectors.toList());
  }

  private static void setBulkNRDeletionListToProcessed(Map.Entry<String, BulkNeedRevisionDeletion> entry,
      List<BulkNeedRevisionDeletion> updateBulkNeedRevisionProcessList) {
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = entry.getValue();
    bulkNeedRevisionDeletion.setStatus(BulkProcess.STATUS_PROCESSED);
    updateBulkNeedRevisionProcessList.add(bulkNeedRevisionDeletion);
  }

  private void uploadFileToGcs(BulkNeedRevisionDeletion needRevisionDeletion, byte[] bytes) throws Exception {
    fileStorageService.uploadFileToBulkBucket(
        gcsNeedRevisionDeletionUploadFilePath + Constant.SLASH + needRevisionDeletion.getDeletionProcessCode()
            + Constant.SLASH + needRevisionDeletion.getDeletionProcessCode() + Constant.DOT + Constant.FILE_TYPE_XLSX,
        bytes);
  }

  private void sendNotification(BulkNeedRevisionDeletion needRevisionDeletion,
      List<BulkNeedRevisionDeletionData> eligibleForDeletionList, int nextWeekDeletionSize) {
    String fileURL = fileStorageService.getDownloadLinkForNeedRevisionDeletion(gcsNeedRevisionDeletionUploadFilePath,
        needRevisionDeletion.getDeletionProcessCode());
    notificationService.sendNeedRevisionDeletionNotification(needRevisionDeletion, fileURL,
        eligibleForDeletionList.size(), nextWeekDeletionSize);
  }

  private static void updateStatusInNeedRevisionDeletionTable(BulkNeedRevisionDeletion needRevisionDeletion,
      List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionData,
      List<BulkNeedRevisionDeletion> statusUpdateList) {
    List<BulkNeedRevisionDeletionData> failedRowData = new ArrayList<>();
    failedRowData = bulkNeedRevisionDeletionData.stream().filter(
        bulkNeedRevisionDeletionData1 -> bulkNeedRevisionDeletionData1.getStatus()
            .equals(BulkNeedRevisionDeletion.STATUS_FAIL)).collect(Collectors.toList());
    if (CollectionUtils.isEmpty(failedRowData)) {
      needRevisionDeletion.setStatus(BulkProcess.STATUS_FINISHED);
    } else if (failedRowData.size() == needRevisionDeletion.getTotalCount()) {
      needRevisionDeletion.setStatus(BulkProcess.STATUS_ABORTED);
    } else {
      needRevisionDeletion.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    }
    needRevisionDeletion.setUpdatedDate(new Date());
    statusUpdateList.add(needRevisionDeletion);
  }

  private List<BulkNeedRevisionDeletion> setBulkNeedRevisionDeletionEntities(String storeId,
      List<String> businessPartnerCodeList) {
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList = new ArrayList<>();
    for (String businessPartnerCode : businessPartnerCodeList) {
      BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
      bulkNeedRevisionDeletion.setStoreId(storeId);
      bulkNeedRevisionDeletion.setBusinessPartnerCode(businessPartnerCode);
      bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
      bulkNeedRevisionDeletion.setDeletionProcessCode(GdnUUIDHelper.generateUUID());
      bulkNeedRevisionDeletion.setProcessType(NEED_REVISION_DELETION_TYPE);
      bulkNeedRevisionDeletionList.add(bulkNeedRevisionDeletion);
    }
    return bulkNeedRevisionDeletionList;
  }

  @Override
  public void fetchProductsOfABusinessPartner(String storeId, String requestId) throws Exception {
    List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDatasToBeSaved = new ArrayList<>();
    SystemParameterConfig systemParameterConfig =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            SystemParameterConfigNames.NR_DELETION_FETCH_PRODUCTS_WITH_BP_CODES_SIZE);
    String businessPartnerBatchSize =
        Optional.ofNullable(systemParameterConfig).orElse(new SystemParameterConfig()).getValue();
    Page<BulkNeedRevisionDeletion> needRevisionDeletionPage =
        needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
            storeId, BulkNeedRevisionDeletion.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(businessPartnerBatchSize)));
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
        updateBulkNeedRevisionStatus(BulkNeedRevisionDeletion.STATUS_PICKED,
            needRevisionDeletionPage.getContent());
    bulkNeedRevisionDeletionList = filterBusinessPartnersToFetchProducts(bulkNeedRevisionDeletionList, businessPartnerBatchSize);
    for (BulkNeedRevisionDeletion bulkNeedRevisionDeletion : bulkNeedRevisionDeletionList) {
      try {
        ProductCodeAndNameResponseList productCodeAndNameResponseList =
            pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(storeId, requestId,
                bulkNeedRevisionDeletion.getBusinessPartnerCode());
        if (Objects.nonNull(productCodeAndNameResponseList) && CollectionUtils.isNotEmpty(
            productCodeAndNameResponseList.getProducts())) {
          bulkNeedRevisionDeletion.setTotalCount(
              productCodeAndNameResponseList.getProducts().size());
          bulkNeedRevisionDeletionDatasToBeSaved.addAll(
              prepareBulkNeedRevisionDataToSave(productCodeAndNameResponseList,
                  bulkNeedRevisionDeletion));
        }
      } catch (Exception exception) {
        log.error("Error while fetching products for NR Deletion for bpCode {}",
            bulkNeedRevisionDeletion.getBusinessPartnerCode(), exception);
      }
    }
    if (CollectionUtils.isNotEmpty(bulkNeedRevisionDeletionDatasToBeSaved)) {
      bulkNeedRevisionDeletionDataRepository.saveAll(bulkNeedRevisionDeletionDatasToBeSaved);
    }
    updateBulkNeedRevisionStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS,
        bulkNeedRevisionDeletionList);
  }

  @Override
  public BulkNeedRevisionDeletion fetchNeedRevisionDeletionByDeletionProcessCode(String storeId,
      String deletionBulkProcessCode) {
    return needRevisionDeletionRepository.findByStoreIdAndDeletionProcessCode(storeId,
        deletionBulkProcessCode);
  }

  @Override
  public List<BulkNeedRevisionDeletionData> fetchNeedRevisionDeletionDataByDeletionProcessCodeAndIds(
      String storeId, String needRevisionDeletionProcessCode,
      List<String> needRevisionDeletionDataIds) {
    return bulkNeedRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCodeAndIdIn(
        storeId, needRevisionDeletionProcessCode, needRevisionDeletionDataIds);
  }

  private List<BulkNeedRevisionDeletion> filterBusinessPartnersToFetchProducts(
      List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList, String businessPartnerBatchSize)
      throws Exception {
    List<String> allowedMerchantTypes =
        Arrays.asList(autoNeedRevisionDeletionMerchantTypes.split(Constant.COMMA));
    BusinessPartnerFilterRequest businessPartnerFilterRequest =
        getBusinessPartnerFilterRequest(bulkNeedRevisionDeletionList);
    businessPartnerFilterRequest.setMerchantTypes(allowedMerchantTypes);
    List<ProfileResponse> profileResponses =
        businessPartnerRepository.filterByBusinessPartnerCodeList(businessPartnerFilterRequest, 0,
            Integer.parseInt(businessPartnerBatchSize));
    List<String> businessPartnerCodesToFetchProducts =
        Optional.ofNullable(profileResponses).orElseGet(ArrayList::new).stream()
            .filter(Objects::nonNull).filter(profileResponse -> allowedMerchantTypes.contains(
                profileResponse.getCompany().getMerchantType()))
            .map(ProfileResponse::getBusinessPartnerCode).collect(Collectors.toList());
    List<BulkNeedRevisionDeletion> processToBeFetched = new ArrayList<>();
    List<BulkNeedRevisionDeletion> processesToBeFailed = new ArrayList<>();

    Map<Boolean, List<BulkNeedRevisionDeletion>> combinedList =
        bulkNeedRevisionDeletionList.stream().collect(Collectors.partitioningBy(
            bulkNeedRevisionDeletion -> businessPartnerCodesToFetchProducts.contains(
                bulkNeedRevisionDeletion.getBusinessPartnerCode())));
    processToBeFetched = combinedList.get(true);
    processesToBeFailed = combinedList.get(false);
    updateBulkNeedRevisionStatus(BulkNeedRevisionDeletion.ABORTED_SELLER_NOT_ELIGIBLE,
        processesToBeFailed);
    return processToBeFetched;
  }

  private BusinessPartnerFilterRequest getBusinessPartnerFilterRequest(
      List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList) {
    BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
    businessPartnerFilterRequest.setBusinessPartnerCodes(
        bulkNeedRevisionDeletionList.stream().map(BulkNeedRevisionDeletion::getBusinessPartnerCode)
            .collect(Collectors.toSet()));
    return businessPartnerFilterRequest;
  }

  private List<BulkNeedRevisionDeletion> updateBulkNeedRevisionStatus(String status,
      List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList) {
    for (BulkNeedRevisionDeletion bulkNeedRevisionDeletion : bulkNeedRevisionDeletionList) {
      bulkNeedRevisionDeletion.setStatus(status);
      if (status.equals(BulkNeedRevisionDeletion.STATUS_PICKED)) {
        bulkNeedRevisionDeletion.setStartDate(new Date());
      }
      bulkNeedRevisionDeletion.setUpdatedDate(new Date());
    }
    return needRevisionDeletionRepository.saveAll(bulkNeedRevisionDeletionList);
  }

  private List<BulkNeedRevisionDeletionData> prepareBulkNeedRevisionDataToSave(
      ProductCodeAndNameResponseList productCodeAndNameResponseList,
      BulkNeedRevisionDeletion bulkNeedRevisionDeletion) throws JsonProcessingException {
    List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDatas = new ArrayList<>();
    for (ProductCodeAndNameDetails productCodeAndNameDetails :
        productCodeAndNameResponseList.getProducts()) {
      BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData =
          new BulkNeedRevisionDeletionData();
      bulkNeedRevisionDeletionData.setCreatedBy(bulkNeedRevisionDeletion.getCreatedBy());
      bulkNeedRevisionDeletionData.setDeletionProcessCode(
          bulkNeedRevisionDeletion.getDeletionProcessCode());
      bulkNeedRevisionDeletionData.setProductCode(productCodeAndNameDetails.getProductCode());
      bulkNeedRevisionDeletionData.setStoreId(bulkNeedRevisionDeletion.getStoreId());
      bulkNeedRevisionDeletionData.setProductData(
          objectMapper.writeValueAsString(productCodeAndNameDetails));
      bulkNeedRevisionDeletionData.setCreatedDate(new Date());
      bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
      bulkNeedRevisionDeletionDatas.add(bulkNeedRevisionDeletionData);
    }
    return bulkNeedRevisionDeletionDatas;
  }


}
