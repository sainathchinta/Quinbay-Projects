package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.DeletePickupPointSummary;
import com.gdn.mta.bulk.dto.PickupPointDeleteProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
public class PickupPointDeleteServiceBean implements PickupPointDeleteService {

  @Autowired
  @Lazy
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Value("${fetch.in.progress.pickup.point.size}")
  private int fetchInProgressPickupPointSize;

  @Value("${fetch.active.pickup.point.size}")
  private int fetchActivePickupPointSize;

  @Value("${delete.pickup.point.error.status}")
  private String deletePickupPointErrorStatus;

  @Value("${update.process.status.as.picked}")
  private boolean updateProcessStatusAsPicked;

  private static final String DESCRIPTION = "Delete Pickup point";

  @Override
  @Trace(dispatcher = true)
  public void processDeletePickupPointEvent(
    PickupPointDeleteProcessDTO pickupPointDeleteProcessDTO) throws Exception {
    final String bulkProcessCode = UUID.randomUUID().toString();
    final String storeId = pickupPointDeleteProcessDTO.getStoreId();
    final String businessPartnerCode = pickupPointDeleteProcessDTO.getBusinessPartnerCode();
    final String pickupPointCode = pickupPointDeleteProcessDTO.getPickupPointCode();

    log.info("Invoking pre process for pp delete for pp code {} and bp code {} ",
      pickupPointDeleteProcessDTO.getPickupPointCode(),
      pickupPointDeleteProcessDTO.getBusinessPartnerCode());
    List<String> errorStatus =
        Stream.of(StringUtils.split(deletePickupPointErrorStatus, ",")).collect(Collectors.toList());
    List<BulkProcess> existingBulkProcesses = bulkProcessService
      .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(storeId, businessPartnerCode,
        BulkProcessType.DELETE_PICKUP_POINT.getValue(), pickupPointCode);
    if (CollectionUtils.isNotEmpty(existingBulkProcesses)) {
      for (BulkProcess existingBulkProcess : existingBulkProcesses) {
        if (errorStatus.contains(existingBulkProcess.getStatus())) {
          log.error("Processing of pp code {} delete for bp code {} is already in progress.", pickupPointCode,
              businessPartnerCode);
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
              "Bulk delete pp code " + bulkProcessCode + "with bulk process code : "
                  + existingBulkProcess.getBulkProcessCode() + " is already processed  or being processed");
        }
      }
    }
    BulkUpdateProcessDTO bulkUpdateProcessDTO =
      BulkUpdateProcessDTO.builder().businessPartnerCode(businessPartnerCode)
        .bulkProcessType(BulkProcessType.DELETE_PICKUP_POINT.getValue()).build();
    BulkProcess bulkProcess = bulkUpdateServiceUtil
        .getBulkProcess(storeId, bulkProcessCode, bulkProcessCode, bulkUpdateProcessDTO, 0, 0, false, false);
    //We will keep pp code in notes
    bulkProcess.setNotes(pickupPointCode);
    bulkProcess.setDescription(DESCRIPTION);
    bulkProcess.setCreatedBy(pickupPointDeleteProcessDTO.getCreatedBy());
    bulkProcess.setUpdatedBy(pickupPointDeleteProcessDTO.getUpdatedBy());
    bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess);
  }

  @Override
  @Trace(dispatcher = true)
  public void processDeleteItemPickupPointEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    log.info("Bulk DeleteItemPickupPoint process in progress, bulk process code : {} , rowNumbers : {} ",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
    BulkProcess bulkProcess = bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
        bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      log.warn("No rows found in pending state for the bulk process code : {}, row numbers : {}",
          bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
      return;
    }
    String pickupPointCode = bulkProcess.getNotes();
    String productSku =
        bulkProcessDataList.stream().map(BulkProcessData::getParentProduct).findFirst().orElse(StringUtils.EMPTY);
    Map<String, BulkProcessData> updatedBulkDataMap = new HashMap<>();
    Set<String> updatedItemSkusSet = new HashSet<>();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      Map<String, String> row = setBlpInitialData(bulkProcessData);
      updatedItemSkusSet.add(row.get(BulkParameters.ITEM_SKU_REQUEST));
      updatedBulkDataMap.put(row.get(BulkParameters.ITEM_SKU_REQUEST).concat(row.get(BulkParameters.STATE_REQUEST)),
          bulkProcessDataService.saveOperation(bulkProcessData));
    }
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
        ItemSkuPickupPointRequest.builder().productSku(productSku).pickupPointCode(pickupPointCode)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).itemSkuList(new ArrayList<>()).build();
    List<DeleteInProgressL5Response> deleteInProgressL5ResponseList = new ArrayList<>();
    try {
      getDeleteInputData(itemSkuPickupPointRequest, updatedItemSkusSet);
      deleteInProgressL5ResponseList =
          callPBPToDelete(bulkProcess, itemSkuPickupPointRequest, deleteInProgressL5ResponseList);
      List<BulkProcessData> failedBulkData = new ArrayList<>();
      List<BulkProcessData> successData = new ArrayList<>();
      setErrorMessageForFailedData(updatedBulkDataMap, deleteInProgressL5ResponseList, failedBulkData);
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
      retryBulkProcessDataIfEligible(bulkUpdateEventModel, bulkProcess, bulkProcessDataList, pickupPointCode, updatedBulkDataMap, e);
    }
  }

  private void setErrorMessageForFailedData(Map<String, BulkProcessData> updatedBulkDataMap,
      List<DeleteInProgressL5Response> deleteInProgressL5ResponseList, List<BulkProcessData> failedBulkData)
      throws JsonProcessingException {
    if (CollectionUtils.isNotEmpty(deleteInProgressL5ResponseList)) {
      for (DeleteInProgressL5Response deleteInProgressL5Response : deleteInProgressL5ResponseList) {
        if (Objects.nonNull(
            updatedBulkDataMap.get(deleteInProgressL5Response.getItemSku().concat(BulkParameters.IN_PROGRESS)))) {
          getFailedBulkProcessDataList(deleteInProgressL5Response.getItemSku().concat(BulkParameters.IN_PROGRESS),
              updatedBulkDataMap, deleteInProgressL5Response, failedBulkData);
        }
        if (Objects.nonNull(
            updatedBulkDataMap.get(deleteInProgressL5Response.getItemSku().concat(BulkParameters.ACTIVE)))) {
          getFailedBulkProcessDataList(deleteInProgressL5Response.getItemSku().concat(BulkParameters.ACTIVE),
              updatedBulkDataMap, deleteInProgressL5Response, failedBulkData);
        }
      }
    }
  }

  private static void getDeleteInputData(ItemSkuPickupPointRequest itemSkuPickupPointRequest,
      Set<String> inProgressItemSkusSet) {
    itemSkuPickupPointRequest.setItemSkuList(new ArrayList<>(inProgressItemSkusSet));
  }

  private List<DeleteItemPickupPointResponse> callXproductToDelete(BulkProcess bulkProcess,
      DeleteItemPickupPointRequest deleteItemPickupPointRequest,
      List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList) {
    if (CollectionUtils.isNotEmpty(deleteItemPickupPointRequest.getItemSkus())) {
      log.info(
          "Calling x-product to delete active L5's with DeleteItemPickupPointRequest = {} and bulkProcessCode = {} ",
          deleteItemPickupPointRequest, bulkProcess.getBulkProcessCode());
      deleteItemPickupPointResponseList =
          xProductOutboundService.deleteItemPickupPointByPickupPointCode(deleteItemPickupPointRequest);
    }
    return deleteItemPickupPointResponseList;
  }

  private List<DeleteInProgressL5Response> callPBPToDelete(BulkProcess bulkProcess,
      ItemSkuPickupPointRequest itemSkuPickupPointRequest,
      List<DeleteInProgressL5Response> deleteInProgressL5ResponseList) {
    if (CollectionUtils.isNotEmpty(itemSkuPickupPointRequest.getItemSkuList())) {
      log.info("Calling PBP to delete in progress L5's with ItemSkuPickupPointRequest = {} and bulkProcessCode = {} ",
          itemSkuPickupPointRequest, bulkProcess.getBulkProcessCode());
      deleteInProgressL5ResponseList = pbpOutboundService.deleteL5ByPickupPointCode(itemSkuPickupPointRequest);
    }
    return deleteInProgressL5ResponseList;
  }

  private void retryBulkProcessDataIfEligible(BulkUpdateEventModel bulkUpdateEventModel, BulkProcess bulkProcess,
      List<BulkProcessData> bulkProcessDataList, String pickupPointCode,
      Map<String, BulkProcessData> updatedBulkDataMap, Exception e) throws JsonProcessingException {
    log.error("Bulk DeleteItemPickupPoint failed, bulk process code : {} , rowNumbers : {} , error",
        bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(), e);
    if (Objects.isNull(bulkProcess.getSystemErrorCount()) || bulkProcess.getSystemErrorCount() == 0) {
      for (Map.Entry<String, BulkProcessData> entry : updatedBulkDataMap.entrySet()) {
        BulkProcessData bulkProcessData = entry.getValue();
        bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
        bulkProcessData.setEndDate(new Date());
        bulkProcessDataService.saveOperation(bulkProcessData);
      }
      bulkProcess.setSystemErrorCount(bulkProcessDataList.size());
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
      bulkProcessService.saveOperation(bulkProcess);
    } else {
      for (Map.Entry<String, BulkProcessData> entry : updatedBulkDataMap.entrySet()) {
        BulkProcessData bulkProcessData = entry.getValue();
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        bulkProcessData.setEndDate(new Date());
        bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(
            new DeleteItemPickupPointResponse(entry.getKey(), pickupPointCode, BulkParameters.SYSTEM_ERROR,
                StringUtils.EMPTY)));
        bulkProcessDataService.saveOperation(bulkProcessData);
      }
    }
  }

  private void getFailedBulkProcessDataList(String key, Map<String, BulkProcessData> updatedBulkDataMap,
      DeleteInProgressL5Response deleteInProgressL5Response, List<BulkProcessData> failedBulkData)
      throws JsonProcessingException {
    BulkProcessData bulkProcessData = updatedBulkDataMap.get(key);
    bulkProcessData.setEndDate(new Date());
    String status = StringUtils.isEmpty(deleteInProgressL5Response.getReason()) ?
        BulkProcessData.STATUS_SUCCESS :
        BulkProcessData.STATUS_FAIL;
    bulkProcessData.setStatus(status);
    if (!StringUtils.isEmpty(deleteInProgressL5Response.getNewPickupPointCode())) {
      bulkProcessData.setNotes(objectMapper.writeValueAsString(deleteInProgressL5Response));
    }
    if (BulkProcessData.STATUS_FAIL.equals(status)) {
      bulkProcessData.setErrorMessage(objectMapper.writeValueAsString(deleteInProgressL5Response));
    }
    failedBulkData.add(bulkProcessData);
    updatedBulkDataMap.remove(key);
  }

  private Map<String, String> setBlpInitialData(BulkProcessData bulkProcessData) throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    LinkedHashMap<String, String> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(),
        new TypeReference<LinkedHashMap<String, String>>() {
        });
    return rowDataJson;
  }

  @Override
  public void processPendingDeletePickupPointEvent(String storeId, String bulkProcessType) {
    int fetchBatchSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE).getValue());
    List<BulkProcess> bulkProcessList =
        bulkProcessService.findByBulkProcessTypeAndStatus(storeId, bulkProcessType, BulkProcess.STATUS_PENDING, PageRequest.of(0, fetchBatchSize));
    for (BulkProcess bulkProcess : bulkProcessList) {
      String businessPartnerCode = bulkProcess.getBusinessPartnerCode();
      String pickupPointCode = bulkProcess.getNotes();
      try {
        bulkProcess = updatedProcessStatusAsPicked(bulkProcess);
        log.info("fetching in-progress products from PBP by pickup point code = {} ", pickupPointCode);
        List<DeletePickupPointSummary> deletePickupPointSummaryList =
            new ArrayList<>(fetchInProgressL5ByPickupPointCode(businessPartnerCode, pickupPointCode));
        log.info("fetching active products from x-product by pickup point code = {} ", pickupPointCode);
        deletePickupPointSummaryList.addAll(fetchActiveL5ByPickupPointCode(businessPartnerCode, pickupPointCode));
        saveBulkProcessData(bulkProcess, deletePickupPointSummaryList);
      } catch (Exception e) {
        log.info("Error while fetching products with pickup point code = {} and bulk process code = {} ",
            pickupPointCode, bulkProcess.getBulkProcessCode(), e);
        if (Objects.nonNull(bulkProcess.getInputErrorCount()) && bulkProcess.getInputErrorCount() != 0) {
          //when we have already tried fetching data and got a system error we are marking that bulk process as aborted
          bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
          bulkProcess.setInputErrorCount(bulkProcess.getInputErrorCount() + 1);
          bulkProcess.setDescription(BulkParameters.SYSTEM_ERROR);
          bulkProcess.setEndDate(new Date());
        } else {
          bulkProcess.setInputErrorCount(1);
        }
        bulkProcessService.saveOperation(bulkProcess);
      }
    }
  }

  private BulkProcess updatedProcessStatusAsPicked(BulkProcess bulkProcess) {
    if(updateProcessStatusAsPicked) {
      bulkProcess.setStatus(BulkProcess.STATUS_PICKED);
      return bulkProcessService.saveOperation(bulkProcess);
    }
    return bulkProcess;
  }

  private List<DeletePickupPointSummary> fetchInProgressL5ByPickupPointCode(String businessPartnerCode,
      String pickupPointCode) throws Exception {
    int page = 0;
    Page<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponsePage = null;
    List<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeResponseList =
        new ArrayList<>();
    do {
      inProgressProductsByPickupPointCodeResponsePage =
          this.pbpOutboundService.getInProgressProductsByPickupPointCode(businessPartnerCode, pickupPointCode, page,
              fetchInProgressPickupPointSize);
      inProgressProductsByPickupPointCodeResponseList.addAll(
          inProgressProductsByPickupPointCodeResponsePage.getContent());
      page++;
    } while (inProgressProductsByPickupPointCodeResponsePage.hasNext());
    return inProgressProductsByPickupPointCodeResponseList.stream().map(
            inProgressProductsByPickupPointCodeResponse -> new DeletePickupPointSummary(
                inProgressProductsByPickupPointCodeResponse.getProductSku(),
                inProgressProductsByPickupPointCodeResponse.getItemSku(),
                inProgressProductsByPickupPointCodeResponse.getPickupPointCode(), BulkParameters.IN_PROGRESS))
        .collect(Collectors.toList());
  }

  private List<DeletePickupPointSummary> fetchActiveL5ByPickupPointCode(String businessPartnerCode,
      String pickupPointCode) throws Exception {
    int page = 0;
    Page<ProductSkuPickupPointResponseV2> productSkuPickupPointResponsePage = null;
    List<ProductSkuPickupPointResponseV2> productSkuPickupPointResponseList = new ArrayList<>();
    do {
      productSkuPickupPointResponsePage =
          this.xProductOutboundService.getActiveProductsByPickupPointCode(businessPartnerCode, pickupPointCode, page,
              fetchActivePickupPointSize);
      productSkuPickupPointResponseList.addAll(productSkuPickupPointResponsePage.getContent());
      page++;
    } while (productSkuPickupPointResponsePage.hasNext());
    return productSkuPickupPointResponseList.stream().map(
            inProgressProductsByPickupPointCodeResponse -> new DeletePickupPointSummary(
                inProgressProductsByPickupPointCodeResponse.getProductSku(),
                inProgressProductsByPickupPointCodeResponse.getItemSku(),
                inProgressProductsByPickupPointCodeResponse.getPickupPointCode(), BulkParameters.ACTIVE))
        .collect(Collectors.toList());
  }

  private void saveBulkProcessData(BulkProcess bulkProcess, List<DeletePickupPointSummary> deletePickupPointSummaryList)
      throws Exception {
    log.info("Inserting data rows for bulk process code = {} ", bulkProcess.getBulkProcessCode());
    List<BulkProcessData> requestData = new ArrayList<>();
    int rowNumber = 1;
    for (DeletePickupPointSummary deletePickupPointSummary : deletePickupPointSummaryList) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setParentProduct(deletePickupPointSummary.getProductSku());
      bulkProcessData.setRowNumber(rowNumber++);
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(deletePickupPointSummary));
      bulkProcessData.setStartDate(new Date());
      requestData.add(bulkProcessData);
    }
    bulkProcessDataService.saveOperationBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setTotalCount(rowNumber - 1);
    bulkProcessService.saveOperation(bulkProcess);
    log.info("Rows are successfully inserted for bulk process code = {} ", bulkProcess.getBulkProcessCode());
  }
}
