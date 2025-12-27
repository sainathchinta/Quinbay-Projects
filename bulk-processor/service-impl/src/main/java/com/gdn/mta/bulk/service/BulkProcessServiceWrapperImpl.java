package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.BulkProcessType.ASSEMBLY_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD;
import static com.gdn.mta.bulk.dto.BulkProcessType.DELETE_PICKUP_POINT;
import static com.gdn.mta.bulk.dto.BulkProcessType.DISASSEMBLY_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.EXTERNAL_CREATION_UPLOAD;
import static com.gdn.mta.bulk.dto.BulkProcessType.PRODUCT_CREATION_UPLOAD;
import static com.gdn.mta.bulk.dto.BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1;
import static com.gdn.mta.bulk.dto.BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2;
import static com.gdn.mta.bulk.dto.BulkProcessType.TRANSFER_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.getBulkProcessType;
import static com.gdn.mta.bulk.util.BulkCreationCommonUtil.getMerchantType;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.x.businesspartner.dto.CompanyDTO;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.repository.BulkProcessCustomRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableSet;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkProcessServiceWrapperImpl implements BulkProcessServiceWrapper {

  @Value("${default.internal.activation.period:120}")
  private int internalActivationPeriod;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkProcessCustomRepository bulkProcessCustomRepository;

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;


  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Autowired
  private BulkUpdateService bulkUpdateService;

  @Autowired
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private BulkArchiveService bulkArchiveService;

  @Autowired
  private BulkDeleteService bulkDeleteService;

  @Autowired
  private BulkUpsertService bulkUpsertService;

  @Autowired
  private QrCodeFinalizeService qrCodeFinalizeService;

  @Autowired
  private NotificationService notificationService;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  private static final Set<BulkProcessType> BULK_CREATION_PROCESS_TYPES =
      ImmutableSet.of(PRODUCT_CREATION_UPLOAD, PRODUCT_CREATION_UPLOAD_PRIORITY_1,
        PRODUCT_CREATION_UPLOAD_PRIORITY_2, CONVERTED_PRODUCT_CREATION_UPLOAD, EXTERNAL_CREATION_UPLOAD);

  private static final Set<BulkProcessType> BULK_WORK_ORDER_UPLOAD_TYPE =
    ImmutableSet.of(DISASSEMBLY_REQUEST, ASSEMBLY_REQUEST, TRANSFER_REQUEST);

  @Async
  @Override
  @Trace(dispatcher = true)
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void processReadyToProcessData(String storeId, String bulkProcessType) throws Exception {
    int batchSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        getReadyToProcessParamNameByProcessType(bulkProcessType)).getValue());
    if (batchSize == Constant.ZERO) {
      return;
    }
    boolean orderByCount = Boolean.parseBoolean(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY).getValue());
    List<BulkProcess> bulkProcessList = bulkProcessCustomRepository.findBlpToProcess(storeId, orderByCount, batchSize,
        BulkProcess.STATUS_READY_TO_PROCESS, bulkProcessType);
    log.info("Processing start for RTP total blps : {}, for limit {} and orderBy {}", bulkProcessList.size(), batchSize,
        orderByCount);
    List<BulkProcess> updatedList = new ArrayList<>();
    for (BulkProcess bulkProcess : bulkProcessList) {
      switch (getBulkProcessType(bulkProcessType)) {
        case PRODUCT_CREATION_UPLOAD:
        case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
        case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
        case CONVERTED_PRODUCT_CREATION_UPLOAD:
        case EXTERNAL_CREATION_UPLOAD:
          bulkProcessService.publishBulkProductCreationEvent(storeId, bulkProcess, bulkProcessType);
          break;
        case PRODUCT_LEVEL_3:
        case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
        case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2:
          if (bulkProcess.getBulkUpdate()) {
            bulkProcessService.publishBulkUpdateEvent(storeId, bulkProcess);
          }
          break;
        case EAN_PRODUCT_LEVEL_4:
          if (bulkProcess.getBulkUpdate()) {
            bulkProcessService.publishBulkUpdateEANEvent(storeId, bulkProcess);
          }
          break;
        case CAMPAIGN:
          bulkProcessService.publishCampaignUploadEvent(storeId, bulkProcess);
          break;
        case INSTANT_PICKUP_PRODUCT_UPSERT:
          bulkProcessService.publishBulkInstantPickupItemUpsertEvent(storeId, bulkProcess);
          break;
        case INSTANT_PICKUP_PRODUCT_DELETE:
          bulkProcessService.publishBulkInstantPickupItemDeleteEvent(storeId, bulkProcess);
          break;
        case IN_STORE:
          bulkProcessService.publishInstoreUpdateEvent(storeId, bulkProcess);
          break;
        case ARCHIVE:
          bulkProcessService.publishArchiveProductEvent(storeId, bulkProcess);
          break;
        case SUBJECT_TO_VAT:
          bulkProcessService.publishVatUpdateEvent(storeId, bulkProcess);
          break;
        case DELETE_PICKUP_POINT:
          bulkProcessService.publishBulkDeleteItemPickupPointEvent(storeId, bulkProcess);
          break;
        case QR_GENERATION:
          bulkProcessService.publishForQRCodeGenerationEvent(storeId, bulkProcess);
          break;
        case ASSEMBLY_REQUEST:
        case DISASSEMBLY_REQUEST:
        case TRANSFER_REQUEST:
          bulkProcessService.publishBulkWorkOrderUploadEvent(storeId, bulkProcess);
        break;
        case PRODUCT_BASIC_INFO:
        case PRODUCT_BASIC_INFO_PRIORITY_1:
        case PRODUCT_BASIC_INFO_PRIORITY_2:
          bulkProcessService.publishBasicInfoUpdateEvent(storeId, bulkProcess);
          break;
        default:
          break;
      }
      bulkProcess.setStatus(BulkProcess.STATUS_PUBLISHED);
      updatedList.add(bulkProcess);
      log.info("Save published blp : {}", bulkProcess.getBulkProcessCode());
    }
    if (CollectionUtils.isNotEmpty(updatedList)) {
      bulkProcessRepository.saveAll(updatedList);
    }
  }

  private String getReadyToProcessParamNameByProcessType(String bulkProcessType) {
    String variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE;
    switch (getBulkProcessType(bulkProcessType)) {
      case PRODUCT_CREATION_UPLOAD:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE;
        break;
      case PRODUCT_LEVEL_3:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_BATCH_SIZE;
        break;
      case CAMPAIGN:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_CAMPAIGN_BATCH_SIZE;
        break;
      case INSTANT_PICKUP_PRODUCT_UPSERT:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPSERT_BATCH_SIZE;
        break;
      case INSTANT_PICKUP_PRODUCT_DELETE:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_DELETE_BATCH_SIZE;
        break;
      case IN_STORE:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_INSTORE_BATCH_SIZE;
        break;
      case ARCHIVE:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_ARCHIVE_BATCH_SIZE;
        break;
      case SUBJECT_TO_VAT:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_VAT_UPDATE_BATCH_SIZE;
        break;
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_1;
        break;
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_2;
        break;
      case CONVERTED_PRODUCT_CREATION_UPLOAD:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_CONVERTED_UPLOAD;
        break;
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_PRIORITY_1_BATCH_SIZE;
        break;
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_PRIORITY_2_BATCH_SIZE;
        break;
      case ASSEMBLY_REQUEST:
      case DISASSEMBLY_REQUEST:
      case TRANSFER_REQUEST:
        variableName = SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE;
      break;
      case DELETE_PICKUP_POINT:
        variableName = SystemParameterConfigNames.BULK_DELETE_PICKUP_POINT_READY_TO_PROCESS_BATCH_SIZE;
        break;
      case PRODUCT_BASIC_INFO:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BASIC_INFO_BATCH_SIZE;
        break;
      case PRODUCT_BASIC_INFO_PRIORITY_1:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BASIC_INFO_PRIORITY_1_BATCH_SIZE;
        break;
      case PRODUCT_BASIC_INFO_PRIORITY_2:
        variableName = SystemParameterConfigNames.BULK_READY_TO_PROCESS_BASIC_INFO_PRIORITY_2_BATCH_SIZE;
        break;
      case EXTERNAL_CREATION_UPLOAD:
        variableName =
          SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_EXTERNAL_UPLOAD;
        break;
      default:
        break;
    }
    return variableName;
  }

  @Async
  @Override
  @Trace(dispatcher = true)
  public void processProcessedData(String storeId, String bulkProcessType) throws Exception {
    int batchSize = Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE).getValue());
    if (batchSize == Constant.ZERO) {
      return;
    }
    ProfileResponse profileResponse = null;
    boolean orderByCount = Boolean.parseBoolean(systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY).getValue());
    List<BulkProcess> bulkProcessList =
        bulkProcessCustomRepository.findBlpToProcess(storeId, orderByCount, batchSize, BulkProcess.STATUS_PROCESSED, bulkProcessType);
    log.info("Processing start for processed total blps : {}, for limit {} and orderBy {}", bulkProcessList.size(),
        batchSize, orderByCount);
    List<BulkProcess> statusUpdateList = new ArrayList<>();
    List<BulkProcess> updatedList = new ArrayList<>();
    Map<String, List<BulkProcessData>> failedRowDataMap = new HashMap<>();
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    for (BulkProcess bulkProcess : bulkProcessList) {
      if (StringUtils.isBlank(bulkProcess.getBulkProcessType())) {
        continue;
      }
      BulkProcessType processType = getBulkProcessType(bulkProcessType);
      List<BulkProcessData> failedRowData = new ArrayList<>();
      if (DELETE_PICKUP_POINT.equals(processType) || BULK_WORK_ORDER_UPLOAD_TYPE.contains(processType)) {
        bulkProcessDataList = bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess);
        failedRowData = bulkProcessDataList.stream()
          .filter(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus()))
          .collect(Collectors.toList());
      } else {
        failedRowData =
            bulkProcessDataService.getFailedDataForProcessedFile(storeId, bulkProcess.getBulkProcessCode());
      }
      if (BULK_CREATION_PROCESS_TYPES.contains(processType) ) {
        failedRowDataMap.put(bulkProcess.getBulkProcessCode(), failedRowData);
      }
      if (CollectionUtils.isEmpty(failedRowData)) {
        bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
      } else if (failedRowData.size() == bulkProcess.getTotalCount()) {
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      } else {
        bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      }
      statusUpdateList.add(bulkProcess);
    }
    statusUpdateList = bulkProcessService.saveBulkProcessList(statusUpdateList);

    for (BulkProcess bulkProcess : statusUpdateList) {
      profileResponse=this.businessPartnerRepository
        .filterByBusinessPartnerCodeV2(bulkProcess.getStoreId(),
          bulkProcess.getBusinessPartnerCode());
      MerchantStatusType merchantStatusType = getMerchantType(profileResponse);
      String merchantType =
          Optional.ofNullable(profileResponse.getCompany()).map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
      boolean instoreSeller = CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, profileResponse);
      switch (getBulkProcessType(bulkProcessType)) {
        case PRODUCT_CREATION_UPLOAD:
        case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
        case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
        case CONVERTED_PRODUCT_CREATION_UPLOAD: {
          bulkFailedProductFileService.createFileAndSendNotification(bulkProcess, updateBlpFinalStatus(bulkProcess,
                  Optional.ofNullable(failedRowDataMap.get(bulkProcess.getBulkProcessCode())).orElse(new ArrayList<>())),
              merchantStatusType, merchantType, instoreSeller);
          break;
        }
        case EXTERNAL_CREATION_UPLOAD: {
          bulkFailedProductFileService.createErrorFileForConvertedUploadProcess(
            internalActivationPeriod, bulkProcess,
            Optional.ofNullable(failedRowDataMap.get(bulkProcess.getBulkProcessCode()))
              .orElse(new ArrayList<>()), profileResponse.getCompany().getMerchantType());
          break;
        }
        case PRODUCT_LEVEL_3:
        case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
        case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2: {
          bulkUpdateService.setFinalStatusAndNotificationOnUpdate(bulkProcess, storeId);
          break;
        }
        case EAN_PRODUCT_LEVEL_4: {
          eanProductLevel4BulkUpdateService.setFinalStatusAndNotificationOnEANUpdate(storeId, bulkProcess);
          break;
        }
        case CAMPAIGN: {
          bulkUpdateService.setFinalStatusAndNotificationOnCampaignUpload(bulkProcess, storeId);
          break;
        }
        case IN_STORE: {
          bulkUpdateService.setFinalStatusAndNotificationOnInstoreUpload(bulkProcess, storeId);
          break;
        }
        case ARCHIVE: {
          bulkArchiveService.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess, storeId);
          break;
        }
        case INSTANT_PICKUP_PRODUCT_DELETE: {
          bulkDeleteService.setFinalStatusAndNotificationOnInstantPickupDelete(bulkProcess, storeId);
          break;
        }
        case INSTANT_PICKUP_PRODUCT_UPSERT: {
          bulkUpsertService.setFinalStatusAndNotificationOnInstantPickupUpsert(bulkProcess, storeId);
          break;
        }
        case SUBJECT_TO_VAT: {
          bulkUpdateService.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, storeId);
          break;
        }
        case DELETE_PICKUP_POINT: {
          bulkUpdateService.sendFinalStatusEventForPickupPointDelete(storeId, bulkProcess,
            Optional.ofNullable(bulkProcessDataList).orElse(new ArrayList<>()));
          break;
        }
        case QR_GENERATION:
          qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(storeId, bulkProcess);
          break;
        case ASSEMBLY_REQUEST:
        case DISASSEMBLY_REQUEST:
        case TRANSFER_REQUEST:
             {
               bulkUpdateService.setFinalStatusAndNotificationForWorkOrderUpload(storeId,
                 bulkProcess, bulkProcessDataList);
               break;
             }
        case PRODUCT_BASIC_INFO:
        case PRODUCT_BASIC_INFO_PRIORITY_1:
        case PRODUCT_BASIC_INFO_PRIORITY_2: {
          bulkUpdateService.setFinalStatusAndNotificationOnBasicInfoUpdate(storeId,
              bulkProcess);
          break;
        }
        default:
          break;
      }
      updatedList.add(bulkProcess);
    }
    if (CollectionUtils.isNotEmpty(updatedList)) {
      bulkProcessService.saveBulkProcessList(updatedList);
    }
  }

  @Override
  public void abortStruckProcessesByProcessTypes(String storeId, List<String> bulkProcessTypes) {
    Map<String, Integer> processTypeXAbortTimeMap =  fetchProcessTypeXAbortTimeMap(storeId,
      bulkProcessTypes);
    bulkProcessService.abortStruckProcessesByProcessTypes(storeId, processTypeXAbortTimeMap);
  }

  private Map<String, Integer> fetchProcessTypeXAbortTimeMap(String storeId, List<String> bulkProcessTypes) {
    Map<String, Integer> processTypeXAbortTimeMap = new HashMap<>();
    bulkProcessTypes.forEach(bulkProcessType -> {
      processTypeXAbortTimeMap.put(bulkProcessType,
        Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId
        , ConverterUtil.getVariableValueByProcessTypeForAbortion(bulkProcessType)).getValue()));
    });
    return processTypeXAbortTimeMap;
  }

  private List<List<Object>> updateBlpFinalStatus(BulkProcess bulkProcess, List<BulkProcessData> failedRowData) throws IOException {
    List<List<Object>> userInputRows = BulkCreationCommonUtil.setStateAndFetchInvalidRows(bulkProcess, failedRowData);
    if (Constant.GENERIC.equals(bulkProcess.getNotes())) {
      BulkCreationCommonUtil.setDescriptionForGeneric(bulkProcess, internalActivationPeriod, failedRowData.size());
    } else {
      this.getReviewConfigStatusAndSetDescription(bulkProcess);
    }
    log.info("Save {} blp : {}", bulkProcess.getStatus(), bulkProcess.getBulkProcessCode());
    return userInputRows;
  }

  private void getReviewConfigStatusAndSetDescription(BulkProcess bulkProcess) {
    String status = Constant.POST_LIVE;
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(bulkProcess.getBusinessPartnerCode())
            .categoryCode(bulkProcess.getNotes()).build();
    GdnRestListResponse<ConfigurationStatusResponse> configStatus = pcbOutboundService
        .getconfigurationstatus(bulkProcess.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcess.getBulkProcessCode(), bulkProcess.getCreatedBy(), Arrays.asList(configurationStatusRequest));
    if (Objects.isNull(configStatus) || !configStatus.isSuccess()) {
      log.error("PCB error : {} for fetching configuration status for merchantCode : {} and categoryCode : {} :",
          configStatus, bulkProcess.getBusinessPartnerCode(), bulkProcess.getNotes());
    } else {
      status = Optional.of(configStatus.getContent().get(0).getReviewConfig()).get();
    }
    int internalActivationInterval = internalActivationPeriod;
    try {
      List<CategoryResponse> categoryResponses = categoryRepository
          .filterCategoryHierarchyByCategoryCode(bulkProcess.getStoreId(), bulkProcess.getNotes());
      Integer updatedInternalActivationInterval =
          categoryResponses.stream().filter(categoryResponse -> Objects.isNull(categoryResponse.getParentCategoryId()))
              .findFirst().orElse(new CategoryResponse()).getInternalActivationInterval();
      if (Objects.nonNull(updatedInternalActivationInterval)) {
        internalActivationInterval = updatedInternalActivationInterval;
      }
    } catch (Exception e) {
      log.error("failed to fetch SLA message for category : {} and bulkProcessCode : {}", bulkProcess.getNotes(),
          bulkProcess.getBulkProcessCode());
    }
    String slaNotificationMsg = BulkCreationCommonUtil
        .getSLANotification(bulkProcess.getInternationalMerchant(), internalActivationInterval, status);
    BulkCreationCommonUtil.setDescriptionForCn(bulkProcess, status, slaNotificationMsg);
  }
}
