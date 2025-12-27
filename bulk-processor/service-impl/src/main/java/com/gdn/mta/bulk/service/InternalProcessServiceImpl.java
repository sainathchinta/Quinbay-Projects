package com.gdn.mta.bulk.service;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.FailedReasonResponse;
import com.gdn.mta.bulk.util.RequestHelper;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.repository.BulkInternalProcessDataRepository;
import com.gdn.mta.bulk.repository.BulkInternalProcessRepository;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class InternalProcessServiceImpl implements InternalProcessService {
  private static final String CANCELLED_BY_USER = "Cancelled by User - ";
  private static final List<String> INCOMPLETE_STATUS = Arrays
          .asList(ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name(),
            ProcessStatus.PUBLISHED.name(), ProcessStatus.PICKED.name());

  @Autowired
  private BulkInternalProcessRepository bulkInternalProcessRepository;

  @Autowired
  private BulkInternalProcessDataRepository bulkInternalProcessDataRepository;

  @Autowired
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @Value("${concurrent.vendor.action.file.count}")
  private int concurrentVendorActionFileCount;
  private static final String YES = "YES";

  @Override
  public Page<BulkInternalProcess> getAllBulkInternalProcessByStatus(String storeId, String status,
      Pageable pageRequest, String processType) {
    return bulkInternalProcessRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(storeId, status,
        processType, pageRequest);
  }

  @Override
  @Transactional(readOnly = false)
  public BulkInternalProcess saveInternalProcess(BulkInternalProcess bulkInternalProcess) {
    return bulkInternalProcessRepository.saveAndFlush(bulkInternalProcess);
  }

  @Override
  @Transactional( readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public List<BulkInternalProcessData> saveInternalProcessData(List<BulkInternalProcessData> bulkInternalProcessData) {
   return bulkInternalProcessDataRepository.saveAll(bulkInternalProcessData);
  }

  @Override
  public Map<String, List<BulkInternalProcessPendingDataDTO>> getAllBulkInternalProcessDataByStatusAndBatchSize(
      String status, Integer fetchBatchSize, String processType, String storeId,
      List<BulkInternalProcess> bulkInternalProcessList) {
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessPendingDataDTOMap = new HashMap<>();
    List<String> internalProcessRequestIdList =
        bulkInternalProcessList.stream().map(bulkInternalProcess -> bulkInternalProcess.getId())
            .collect(Collectors.toList());
    List<BulkInternalProcessPendingDataDTO> bulkInternalProcessPendingDataDTOList = new ArrayList<>();
    log.debug("getAllBulkInternalProcessDataByStatusAndBatchSize : storeId : {}, status : {}, processType : {}, "
            + "internalProcessRequestIdList : {}, fetchBatchSize : {}", storeId, status, processType,
        internalProcessRequestIdList, fetchBatchSize);
    List<Object[]> bulkInternalDistinctData =
        bulkInternalProcessDataRepository.getDistinctParentCodeByStatusAndProcessType(storeId, status, processType,
            internalProcessRequestIdList, fetchBatchSize);
    log.debug("getDistinctParentCodeByStatusAndProcessType : {}", bulkInternalDistinctData);
    for (Object[] objects : bulkInternalDistinctData) {
      bulkInternalProcessPendingDataDTOList.add(
          BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(String.valueOf(objects[0]))
              .parentCode(String.valueOf(objects[1])).processType(String.valueOf(objects[2])).build());
    }
    log.debug("bulkInternalProcessPendingDataDTOList : {}", bulkInternalProcessPendingDataDTOList);
    for (BulkInternalProcessPendingDataDTO bulkInternalProcessPendingDataDTO : bulkInternalProcessPendingDataDTOList) {
      if (bulkInternalProcessPendingDataDTOMap.containsKey(
          bulkInternalProcessPendingDataDTO.getInternalProcessRequestId())) {
        List<BulkInternalProcessPendingDataDTO> existingBulkInternalProcessPendingDataDTOList =
            bulkInternalProcessPendingDataDTOMap.get(bulkInternalProcessPendingDataDTO.getInternalProcessRequestId());
        existingBulkInternalProcessPendingDataDTOList.add(bulkInternalProcessPendingDataDTO);
        bulkInternalProcessPendingDataDTOMap.put(bulkInternalProcessPendingDataDTO.getInternalProcessRequestId(),
            existingBulkInternalProcessPendingDataDTOList);
      } else {
        List<BulkInternalProcessPendingDataDTO> newBulkInternalProcessPendingDataDTOList = new ArrayList<>();
        newBulkInternalProcessPendingDataDTOList.add(bulkInternalProcessPendingDataDTO);
        bulkInternalProcessPendingDataDTOMap.put(bulkInternalProcessPendingDataDTO.getInternalProcessRequestId(),
            newBulkInternalProcessPendingDataDTOList);
      }
    }
    log.debug("bulkInternalProcessPendingDataDTOMap : {}", bulkInternalProcessPendingDataDTOMap);
    return bulkInternalProcessPendingDataDTOMap;
  }

  @Override
  public List<BulkInternalProcessData> getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(
      String parentCode, String processType, String internalProcessRequestId) {
    return bulkInternalProcessDataRepository.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndAndMarkForDeleteFalse(
        parentCode, processType, internalProcessRequestId);
  }


  @Override
  public List<BulkInternalProcessData> getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(
    String parentCode, String processType, String processStatus,
    String internalProcessRequestId) {
    return bulkInternalProcessDataRepository.findByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestIdAndAndMarkForDeleteFalse(
      parentCode, processType, processStatus, internalProcessRequestId);
  }

  @Override
  public List<BulkInternalProcessData> findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
      String parentCode, String processType, String internalProcessRequestId, String status) {
    return bulkInternalProcessDataRepository.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
        parentCode, processType, internalProcessRequestId, status);
  }

  @Override
  @Transactional(readOnly = false)
  public List<BulkInternalProcess> saveInternalProcesses(List<BulkInternalProcess> bulkInternalProcessList) {
   return bulkInternalProcessRepository.saveAll(bulkInternalProcessList);
  }

  @Override
  public int getCountByStoreIdAndStatusAndInternalProcessRequestId(String storeId, String status,
      String internalProcessRequestId) {
    return bulkInternalProcessDataRepository.getCountByStoreIdAndStatusAndInternalProcessRequestId(storeId, status,
        internalProcessRequestId);
  }

  @Override
  public Page<BulkInternalProcess> getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(String storeId,
      String processType, Date date, List<String> status, Pageable pageable) {
    return bulkInternalProcessRepository
        .findByStoreIdAndProcessTypeAndCreatedDateLessThanAndStatusIn(storeId, processType, date, status, pageable);
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteBulkInternalProcess(List<BulkInternalProcess> bulkInternalProcessList) {
    bulkInternalProcessRepository.deleteAll(bulkInternalProcessList);
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteBulkInternalProcessDataByInternalRequestId(String internalProcessRequestId) {
    bulkInternalProcessDataRepository.deleteByInternalProcessRequestId(internalProcessRequestId);
  }

  @Override
  public List<BulkInternalProcessData> getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(String storeId,
      String internalProcessRequestId) {
    return bulkInternalProcessDataRepository.findByStoreIdAndInternalProcessRequestId(storeId, internalProcessRequestId);
  }

  @Override
  public List<BulkInternalProcessData> getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(
      String storeId, String internalProcessRequestCode, String status) {
    return bulkInternalProcessDataRepository.findByStoreIdAndInternalProcessRequestCodeAndStatus(storeId,
        internalProcessRequestCode, status);
  }

  @Override
  public List<BulkInternalProcessData> getBulkInternalProcessDataByRequestIds(String storeId,
      List<String> internalRequestIds, int size) {
    return bulkInternalProcessDataRepository.findByStoreIdAndMarkForDeleteFalseAndInternalProcessRequestIdIn(storeId, internalRequestIds, size);
  }

  @Override
  public List<BulkInternalProcessData> getBulkInternalProcessDataByRequestIdsAndStatus(String storeId,
      List<String> internalRequestIds, String status, int size) {
    Pageable pageRequest = PageRequest.of(0, size, Sort.by(Sort.Direction.ASC, Constant.CREATED_DATE));
    return bulkInternalProcessDataRepository.findByStoreIdAndInternalProcessRequestIdInAndStatusAndMarkForDeleteFalse(
        storeId, internalRequestIds, status, pageRequest);
  }

  @Override
  public BulkInternalProcessData getBulkInternalProcessDataById(String storeId, String id) {
    return bulkInternalProcessDataRepository.findByStoreIdAndId(storeId, id);
  }

  @Override
  @Transactional
  public BulkInternalProcessData saveBulkInternalProcessData(BulkInternalProcessData bulkInternalProcessData) {
    return bulkInternalProcessDataRepository.save(bulkInternalProcessData);
  }

  @Override
  public BulkInternalProcess findByInternalProcessRequestCode(String storeId, String internalProcessRequestCode) {
    return bulkInternalProcessRepository
        .findByStoreIdAndInternalProcessRequestCode(storeId, internalProcessRequestCode);
  }

  @Override
  public List<BulkInternalProcessData> findByStoreIdAndBulkProcessCodeAndRowNumberIn(String storeId,
      String internalProcessRequestCode, List<String> parentCode) throws Exception {
    return bulkInternalProcessDataRepository
        .findByStoreIdAndInternalProcessRequestCodeAndParentCodeIn(storeId, internalProcessRequestCode, parentCode);
  }

  @Override
  @Transactional
  public void abortPendingBulkInternalProcess(String storeId, Date pendingToAbortDate, String processType) {
    bulkInternalProcessRepository.updateStatusInProgressBulkInternalProcessToAborted(pendingToAbortDate, processType);
  }

  @Override
  @Transactional
  public void failPendingBulkInternalProcessData(String storeId, Date pendingToAbortDate, String processType) {
    bulkInternalProcessDataRepository.updateStatusInProgressBulkInternalProcessToFailed(pendingToAbortDate, processType);
  }

  @Override
  public Page<BulkInternalProcess> bulkInternalProcessSummary(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, int page, int size) {
    return bulkInternalProcessRepository
        .bulkInternalProcessSummary(storeId, bulkInternalProcessSummaryRequest, PageRequest.of(page, size));
  }

  @Override
  @Transactional( readOnly = false, rollbackFor = Exception.class)
  public void bulkInternalProcessCancelRequest(String storeId, String username, String internalProcessRequestCode) {
    String notes = CANCELLED_BY_USER + username;
    this.bulkInternalProcessRepository
        .bulkInternalProcessCancelRequest(storeId, username, internalProcessRequestCode, notes);
  }

  @Override
  public InternalProcessPendingFilesResponse checkPendingFiles(String storeId, String username,
    String processType) {
    InternalProcessPendingFilesResponse internalProcessPendingFilesResponse =
      new InternalProcessPendingFilesResponse(true, processType, username);
    List<BulkInternalProcess> countPendingStatus =
      bulkInternalProcessRepository.findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(storeId,
        username, processType, INCOMPLETE_STATUS);
    if (processType.equalsIgnoreCase(BulkInternalProcessType.BULK_APPROVAL.name())
      || processType.equalsIgnoreCase(BulkInternalProcessType.BULK_REJECTION.name())) {
      internalProcessPendingFilesResponse.setIsFilesPending(
        countPendingStatus.size() >= concurrentVendorActionFileCount);
    } else if (CollectionUtils.isEmpty(countPendingStatus)) {
      internalProcessPendingFilesResponse.setIsFilesPending(false);
    }
    return internalProcessPendingFilesResponse;
  }

  @Override
  public BulkInternalProcessData bulkInternalProcessDataByIdAndStatus(String storeId, String id, String status) {
    return bulkInternalProcessDataRepository.findByStoreIdAndIdAndStatus(storeId, id, status);
  }

  @Override
  public List<BulkInternalProcessData> bulkInternalProcessDataByIdInAndStatus(String storeId, List<String> ids,
      String status) {
    return bulkInternalProcessDataRepository.findByStoreIdAndIdInAndStatus(storeId, ids, status);
  }

  @Override
  public BulkInternalProcess checkPendingFbbL5Process(String storeId, String sellerCode,
    String notes) {
    return bulkInternalProcessRepository
      .findFirstByStoreIdAndSellerCodeAndNotesAndStatusIn(storeId, sellerCode, notes, INCOMPLETE_STATUS);
  }

  @Override
  public List<BulkInternalProcessData> getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(
    String storeId, String internalProcessRequestId, String status) {
    return bulkInternalProcessDataRepository.findByStoreIdAndInternalProcessRequestIdAndStatus(storeId,
      internalProcessRequestId, status);
  }

  @Override
  public Page<BulkInternalProcess> getAllBulkInternalProcessByStatusOrderByDateAsc(String storeId,
    String status, Pageable pageRequest, String processType) {
    return bulkInternalProcessRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalseOrderByCreatedDateAsc(
        storeId, status,
        processType, pageRequest);
    }

  @Override
  public long countByStoreIdAndProcessTypeAndStatusIn(String storeId, String processType,
    List<String> status) {
    return bulkInternalProcessRepository.countByStoreIdAndProcessTypeAndStatusIn(storeId,
      processType, status);
  }

  @Override
  public BulkInternalProcess findFirstByStoreIdAndProcessTypeAndStatusIn(String storeId,
    String processType,
    List<String> status) {
    return bulkInternalProcessRepository.findFirstByStoreIdAndProcessTypeAndStatusIn(storeId,
      processType, status);
  }

  @Override
  public Set<String> getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(String storeId,
      String internalBulkRequestId) {
    return bulkInternalProcessDataRepository.findParentCodeByStoreIdAndInternalProcessRequestIdAndStatus(storeId, internalBulkRequestId);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void bulkInterUpdatedStatusForRestrictedKeywordBulkUpdate(String status, String updatedBy, String storeId,
      String internalProcessRequestId, Set<String> parentCode) {
    this.bulkInternalProcessDataRepository.updateStatusByParentCodeAndStoreIdAndInternalProcessRequestIdAndStatus(
        status, updatedBy, storeId, internalProcessRequestId, parentCode);
  }

  @Override
  public Map<String, String> updateRemoveBulkProductTypeTagging(
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests,
    String updatedBy) {
    Map<String, String> rowXErrorMessage = new HashMap<>();

    Map<Boolean, List<BulkPriceProductTypeTaggingRequest>> partitionedRequests =
      bulkPriceProductTypeTaggingRequests.stream()
        .filter(Objects::nonNull)
        .collect(Collectors.partitioningBy(request -> request.getDeleteProductTypeTagging().equalsIgnoreCase(YES)));

    // Process requests for update
    List<BulkPriceProductTypeTaggingRequest> productTypeTaggingRequestsForUpdate =
      partitionedRequests.getOrDefault(false, Collections.emptyList());
    if (CollectionUtils.isNotEmpty(productTypeTaggingRequestsForUpdate)) {
      List<FailedReasonResponse> failedReasonResponsesForUpdate =
        priceAnalyticsOutboundService.updateTagging(updatedBy,
          RequestHelper.toUpdateRemoveProductTaggingRequest(productTypeTaggingRequestsForUpdate));

      populateErrorMessageMap(productTypeTaggingRequestsForUpdate, failedReasonResponsesForUpdate, rowXErrorMessage);
    }

    // Process requests for removal
    List<BulkPriceProductTypeTaggingRequest> productTypeTaggingRequestsForRemoval =
      partitionedRequests.getOrDefault(true, Collections.emptyList());
    if (CollectionUtils.isNotEmpty(productTypeTaggingRequestsForRemoval)) {
      List<FailedReasonResponse> failedReasonResponsesForRemoval =
        priceAnalyticsOutboundService.removeTagging(updatedBy,
          RequestHelper.toUpdateRemoveProductTaggingRequest(productTypeTaggingRequestsForRemoval));

      populateErrorMessageMap(productTypeTaggingRequestsForRemoval, failedReasonResponsesForRemoval, rowXErrorMessage);
    }
    log.info("populated map after BulkProductTypeTagging update is : {} ", rowXErrorMessage);
    return rowXErrorMessage;
  }

  private void populateErrorMessageMap(List<BulkPriceProductTypeTaggingRequest> requests,
    List<FailedReasonResponse> responses,
    Map<String, String> errorMessageMap) {
    if(CollectionUtils.isNotEmpty(responses)) {
      Map<String, String> responseMap = responses.stream().collect(
        Collectors.toMap(FailedReasonResponse::getItemPickUpPointId, FailedReasonResponse::getFailedReason));
      for (BulkPriceProductTypeTaggingRequest request : requests) {
        String itemPickUpPointId = request.getItemSku().concat(Constant.HYPHEN).concat(request.getPickupPointCode());
        String failedReason = responseMap.getOrDefault(itemPickUpPointId, StringUtils.EMPTY);
        errorMessageMap.put(request.getId(), failedReason);
      }
    }
  }

  @Override
  public long countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(String storeId,
      String processType, List<String> stateList, String userName) {
    if(StringUtils.isNotEmpty(processType)) {
      return bulkInternalProcessRepository.countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(
          storeId, processType, stateList, userName);
    } else {
      return bulkInternalProcessRepository.countByStoreIdAndStatusInAndCreatedByAndMarkForDeleteFalse(
          storeId, stateList, userName);
    }
  }

}
