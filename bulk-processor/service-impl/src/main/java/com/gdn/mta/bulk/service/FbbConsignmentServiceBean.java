package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.BulkInternalProcessType.FBB_L5_CREATE;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.FbbStatusEventModel;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.FbbFailedItems;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.FbbConsignmentEventModel;
import com.gdn.mta.bulk.entity.FbbL5ItemEventModel;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class FbbConsignmentServiceBean implements FbbConsignmentService {

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${fbb.creation.minimum.price.validation}")
  boolean fbbCreationMinimumPriceValidation;

  private static final String ITEM_SKU_KEY = "itemSku";
  private static final String SUCCESS = "SUCCESS";
  private static final String FAILED = "FAILED";
  private static final String PARTIALLY_COMPLETED = "PARTIALLY_COMPLETED";
  private static final String HYPHEN = "-";

  @Override
  public void preProcessFbbConsignmentCreation(FbbConsignmentEventModel eventModel) {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String sellerCode = eventModel.getBusinessPartnerCode();
    final String consignmentId = eventModel.getConsignmentId();
    log.info("Invoking pre process for fbb consignment id {} and bp code {} ", consignmentId,
      sellerCode);
    BulkInternalProcessUploadRequest uploadRequest =
      BulkInternalProcessUploadRequest.builder().internalProcessRequestCode(bulkProcessCode)
        .sellerCode(sellerCode).notes(consignmentId).processType(FBB_L5_CREATE.name()).build();
    BulkInternalProcess existingProcess =
      internalProcessService.checkPendingFbbL5Process(Constant.STORE_ID, sellerCode, consignmentId);

    if (Objects.nonNull(existingProcess)) {
      log.error("Processing of consignment id {} for bp code {} is already in progress.",
        consignmentId, sellerCode);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
        "Bulk consignment creation with bulk process code : " + existingProcess
          .getInternalProcessRequestCode() + " is " + "being processed");
    }

    //we will save consignment id in notes
    BulkInternalProcess savedInternalProcess = internalProcessService.saveInternalProcess(
      RequestHelper.getFbbBulkInternalProcess(Constant.STORE_ID, uploadRequest));

    eventModel.getFbbItems().forEach(item -> {
      FbbL5ItemEventModel fbbL5ItemEvent = new FbbL5ItemEventModel();
      BeanUtils.copyProperties(item, fbbL5ItemEvent);
      fbbL5ItemEvent.setBusinessPartnerCode(eventModel.getBusinessPartnerCode());
      fbbL5ItemEvent.setInternalProcessCode(savedInternalProcess.getInternalProcessRequestCode());
      log.info("Publishing event : {} itemSku : {} ", kafkaTopicProperties.getFbbCreateL5(),
        item.getItemSku());
      kafkaProducer
        .send(kafkaTopicProperties.getFbbCreateL5(), fbbL5ItemEvent);
    });
  }

  @Override
  public void processL5CreationEvent(String internalProcessCode, FbbL5CreateDTO fbbL5CreateDTO)
    throws JsonProcessingException {
    BulkInternalProcess bulkInternalProcess =
      internalProcessService.findByInternalProcessRequestCode(Constant.STORE_ID,
        internalProcessCode);
    internalProcessService.saveBulkInternalProcessData(
      RequestHelper.getFbbL5InternalDataProcess(Constant.STORE_ID, bulkInternalProcess,
        fbbL5CreateDTO));
  }

  @Override
  public void publishL5CreateRows(String storeId, String requestId,
    Page<BulkInternalProcess> internalProcesses) {
    for (BulkInternalProcess bulkProcess : internalProcesses) {
      List<BulkInternalProcessData> bulkInternalProcessData = internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(storeId,
          bulkProcess.getId(), BulkProcessData.STATUS_PENDING);
      log.info("Publishing event : {}, blpCode : {} ", kafkaTopicProperties.getFbbL5UpdateRows(),
        bulkProcess.getInternalProcessRequestCode());
      Optional.ofNullable(bulkInternalProcessData).orElse(new ArrayList<>()).stream().forEach(
        internalProcessData -> {
          try {
            publishPendingFbbL5CreateRows(storeId, internalProcessData);
          } catch (Exception ex) {
            internalProcessData.setStatus(ProcessStatus.FAILED.name());
            internalProcessData.setErrorMessage(Constant.SYSTEM_ERROR);
            internalProcessService.saveBulkInternalProcessData(internalProcessData);
            log.error("Error publishing fbb for l5 create ", ex);
          }
        });
    }
  }

  @Override
  public void processFbbL4RowEvent(String storeId, String id) {
    BulkInternalProcessData bulkInternalProcessData = internalProcessService
      .bulkInternalProcessDataByIdAndStatus(storeId, id,
        ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData = internalProcessService
          .saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)).get(0);
        FbbCreatePickupPointRequest fbbCreateRequest =
          RequestHelper.toFbbCreateRequestFromJson(bulkInternalProcessData.getData());
        fbbCreateRequest.setDefaultWarehouse(true);

          FbbCreatePickupPointResponse fbbL5CreateResponse = pbpOutboundService.createDefaultL5Fbb(fbbCreateRequest);
          String status = StringUtils.isBlank(fbbL5CreateResponse.getErrorCode()) ?
              ProcessStatus.COMPLETED.name() :
              ProcessStatus.FAILED.name();
          RequestHelper.updateInternalProcessDataStatusAndErrorMessageForFbb(bulkInternalProcessData, status,
              fbbL5CreateResponse.getReason(), fbbL5CreateResponse.getErrorCode());
      } catch (Exception ex) {
        log.error(
          "Exception while processing fbb crete for internalProcessDataRequestId : {} ",
          id, ex);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    } else {
      log.error("No in-progress row found for request id : {},", id);
    }
  }

  @Override
  public void setFinalStatusForDefaultFbbL5Creation(BulkInternalProcess bulkInternalProcess,
    String storeId) throws Exception {
    String status = StringUtils.EMPTY;
    List<BulkInternalProcessData> totalData = internalProcessService
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
        bulkInternalProcess.getId());
    List<String> bulkInternalProcessDataStatusList =
      Optional.ofNullable(totalData).orElse(new ArrayList<>()).stream().map(BulkInternalProcessData::getStatus).distinct()
        .collect(Collectors.toList());
    if(CollectionUtils.isEmpty(bulkInternalProcessDataStatusList)){
      log.info("No Entry in Data Table found for process : {} and request-code : {} ",
        bulkInternalProcess.getProcessType(), bulkInternalProcess.getInternalProcessRequestCode());
      return;
    }
    else if (bulkInternalProcessDataStatusList.contains(ProcessStatus.PENDING.name())
      || bulkInternalProcessDataStatusList.contains(ProcessStatus.IN_PROGRESS.name())) {
      log.info("No Status update since process : {} is still in progress  for "
          + "internalProcessRequestId : {}", bulkInternalProcess.getProcessType(),
        bulkInternalProcess.getId());
      return;
    }
    List<BulkInternalProcessData> failedInternalProcessData =
      totalData.stream().filter(data -> data.getStatus().equals(ProcessStatus.FAILED.name()))
        .collect(Collectors.toList());
    List<FbbFailedItems> failedItems = new ArrayList<>();
    for (BulkInternalProcessData failedProcessData : failedInternalProcessData) {
      LinkedHashMap<String, String> rowDataJson = new ObjectMapper()
        .readValue(failedProcessData.getData(), new TypeReference<LinkedHashMap<String, String>>() {
        });
      failedItems.add(FbbFailedItems.builder().itemSku(rowDataJson.get(ITEM_SKU_KEY))
        .errorCode(failedProcessData.getNotes()).errorMessage(failedProcessData.getErrorMessage())
        .consignmentId(bulkInternalProcess.getNotes()).build());
    }

    int successSize = totalData.size() - failedInternalProcessData.size();
    if (successSize == totalData.size()) {
      bulkInternalProcess.setStatus(BulkProcess.STATUS_FINISHED);
      status = SUCCESS;
    } else {
      if (successSize != Constant.ZERO) {
        bulkInternalProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
        status = PARTIALLY_COMPLETED;
      } else {
        bulkInternalProcess.setStatus(BulkProcess.STATUS_ABORTED);
        status = FAILED;
      }
    }

    log.info("Updating the final status of fbb l5 creation with bulk process status {} ",
      bulkInternalProcess.getStatus());

    bulkInternalProcess.setEndTime(Calendar.getInstance().getTime());
    bulkInternalProcess.setSuccessCount(successSize);
    bulkInternalProcess.setErrorCount(totalData.size() - successSize);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    publishFinalStatusEventFbb(bulkInternalProcess, failedItems, status);
  }

  private void publishFinalStatusEventFbb(BulkInternalProcess bulkInternalProcess,
    List<FbbFailedItems> failedItems, String status) {
    FbbStatusEventModel fbbStatusEventModel =
      RequestHelper.toFbbUpdateStatusEvent(bulkInternalProcess, failedItems, status);
    log.info("Publishing event : {} payload : {} ", kafkaTopicProperties.getFbbCreateConsignmentResult(),
      fbbStatusEventModel);
    kafkaProducer.send(kafkaTopicProperties.getFbbCreateConsignmentResult(), fbbStatusEventModel);
  }

  private void publishPendingFbbL5CreateRows(String storeId,
    BulkInternalProcessData bulkInternalProcessData) throws IOException {
    String productSku = extractProductSkuForDomainEventKey(bulkInternalProcessData);
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    log.info("Saving data for published entries : {}", bulkInternalProcessData.getInternalProcessRequestCode());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    InternalProcessDataDomainEventModel eventModel =
        RequestHelper.toInternalProcessDataDomainEventModel(bulkInternalProcessData, storeId);
    eventModel.setInternalProcessRequestId(bulkInternalProcessData.getId());
    kafkaProducer.send(kafkaTopicProperties.getFbbL5UpdateRows(), productSku, eventModel);
  }

  private String extractProductSkuForDomainEventKey(BulkInternalProcessData bulkInternalProcessData)
    throws IOException {
    FbbCreatePickupPointRequest fbbCreateRequest =
      RequestHelper.toFbbCreateRequestFromJson(bulkInternalProcessData.getData());
    return fbbCreateRequest.getItemSku()
      .substring(0, StringUtils.ordinalIndexOf(fbbCreateRequest.getItemSku(), HYPHEN, 3));
  }
}
