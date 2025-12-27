package com.gdn.mta.bulk.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.VendorBulkAssignErrorCounter;
import com.gdn.mta.bulk.models.VendorBulkAssignmentRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.MasterDataBulkParameters;
import com.gdn.partners.bulk.util.VendorProductDataBulkParameters;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkScreeningProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductActionsResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class VendorProductBulkAssignServiceBean implements VendorProductBulkAssignService {

  private static final String TYPE_CONTENT_REVIEW = "content";
  private static final String REVIEWERS = "REVIEWERS";
  private static final String ASSIGN = "assign";

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private InternalProcessService internalProcessService;

  private static final Logger LOGGER = LoggerFactory.getLogger(VendorProductBulkAssignServiceBean.class);

  @Override
  public void processBulkUpdate(BulkVendorProductAssignRequest bulkVendorProductAssignRequest) throws Exception {
    LOGGER.info(Constant.VENDOR_PRODUCT_BULK_ASSIGN_LOG_MESSAGE, bulkVendorProductAssignRequest.getStoreId(),
        bulkVendorProductAssignRequest.getBulkProcessCode(), bulkVendorProductAssignRequest.getUpdatedBy());
    BulkProcess existingBulkProcess = bulkProcessService
        .findByBulkProcessCode(bulkVendorProductAssignRequest.getStoreId(),
            bulkVendorProductAssignRequest.getBulkProcessCode());
    if (Objects.isNull(existingBulkProcess)) {
      BulkProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
      BulkProcess savedBulkProcess = bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess);
      LOGGER.info("Starting vendor bulk assign process. bulkVendorProductAssignRequest: {}, bulkProcess: {}",
          bulkVendorProductAssignRequest, bulkProcess);
      process(bulkVendorProductAssignRequest, savedBulkProcess);
    }
  }

  private void process(BulkVendorProductAssignRequest bulkVendorProductAssignRequest, BulkProcess bulkProcess)
      throws Exception {
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessService.saveOperation(bulkProcess);
    Sheet excelSheetData = getExcelSheetData(bulkVendorProductAssignRequest.getFilePath());
    List<Map<String, String>> productDataFromExcel = POIUtil.readFromExcelForBulkUpdate(excelSheetData, 1, 0, 0,
        new HashMap<>());
    int totalProduct = productDataFromExcel.size();
    bulkProcess.setTotalCount(totalProduct);
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    VendorBulkAssignErrorCounter counter = new VendorBulkAssignErrorCounter();
    validateExcelData(productDataFromExcel, successData, failureData, counter,
        bulkVendorProductAssignRequest.getAssignmentType(), bulkVendorProductAssignRequest.getValidUserRoleList());
    List<VendorProductActionsResponse> responses =
        assignVendorProductsFromSuccessData(successData, bulkVendorProductAssignRequest);
    updateFailedDataAfterUpdate(responses, counter);
    if (successData.size() == totalProduct) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      if (CollectionUtils.isNotEmpty(successData)) {
        bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      } else {
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      }
    }
    if (BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      deleteFile(bulkVendorProductAssignRequest.getFilePath());
    }
    updateBulkFinalStatus(bulkProcess, bulkVendorProductAssignRequest, successData.size(), totalProduct, counter);
  }

  private void updateBulkFinalStatus(BulkProcess bulkProcess, BulkVendorProductAssignRequest bulkUpdateQueue,
      int successProductCount, int totalProductCount, VendorBulkAssignErrorCounter counter) throws Exception {
    bulkProcess.setEndDate(Calendar.getInstance().getTime());
    bulkProcess.setDescription(new StringBuilder(bulkUpdateQueue.getFilePath()).append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.PRODUCTS_SUCCESSFULLY_UPDATED).append(successProductCount)
        .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.PRODUCTS_NOT_UPDATED)
        .append(totalProductCount - successProductCount).toString());
    bulkProcess.setSuccessCount(successProductCount);
    bulkProcess.setErrorCount(totalProductCount - successProductCount);
    bulkProcess.setInputErrorCount(counter.getAssigneeErrorCount());
    bulkProcess.setSystemErrorCount(counter.getSystemErrorCount());
    bulkProcessService.saveOperation(bulkProcess);
    notificationService.sendBulkAssignNotification(bulkProcess, bulkProcess.getSuccessCount(),
      bulkProcess.getTotalCount(), bulkUpdateQueue.getVendorCode());
    LOGGER.info(Constant.MASTER_DATA_BULK_UPDATE_COMPLETED_MESSAGE, bulkUpdateQueue);
  }

  private void deleteFile(String filePath) {
    try {
      ProcessorUtils.deleteFile(filePath);
    } catch (Exception e) {
      LOGGER.error(Constant.ERROR_IN_FILE_DELETION, filePath, e);
    }
  }

  private void updateFailedDataAfterUpdate(List<VendorProductActionsResponse> responses,
      VendorBulkAssignErrorCounter counter) {
    for (VendorProductActionsResponse response : responses) {
      if (CollectionUtils.isNotEmpty(response.getProductCode())) {
        counter.incrementSystemErrorCount(response.getProductCode().size());
      }
    }
  }

  private void validateExcelData(List<Map<String, String>> excelData, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, VendorBulkAssignErrorCounter counter, String actionType,
      Map<String, List<String>> validUserRoleList) {
    List<String> validUserList = validUserRoleList.get(REVIEWERS);
    for (Map<String, String> row : excelData) {
      String error = validateRowAndGetError(row, actionType, validUserList);
      if (StringUtils.isBlank(error)) {
        successData.add(row);
      } else {
        row.put(MasterDataBulkParameters.FAILURE_REASON, error);
        failureData.add(row);
        counter.incrementAssigneeErrorCount();
      }
    }
  }

  private List<VendorProductActionsResponse> assignVendorProductsFromSuccessData(List<Map<String, String>> successData,
      BulkVendorProductAssignRequest bulkVendorProductAssignRequest) {
    int bulkUpdateBatchSize = systemParameter.getVendorProductAssigneeBulkUpdateBatchSize();
    return Lists.partition(successData, bulkUpdateBatchSize).stream()
        .map(listData -> getBulkVendorProductActionsRequest(listData, bulkVendorProductAssignRequest))
        .map(this::assignVendorProducts).map(BulkVendorProductActionsResponse::getVendorProductActionsResponses)
        .flatMap(List::stream).collect(Collectors.toList());
  }

  private BulkVendorProductActionsResponse assignVendorProducts(BulkVendorProductActionsRequest request) {
    try {
      return productDistributionTaskRepository.bulkVendorProductActions(request);
    } catch (Exception e) {
      LOGGER.error(e.getMessage(), e);
      List<VendorProductActionsResponse> vendorProductActionsResponses =
          request.getBulkScreeningProductActionsRequests().stream()
              .map(req -> getBulkVendorProductActionsResponseInCaseOfException(e, req)).collect(Collectors.toList());
      return new BulkVendorProductActionsResponse(vendorProductActionsResponses);
    }
  }

  private VendorProductActionsResponse getBulkVendorProductActionsResponseInCaseOfException(Exception e,
      BulkScreeningProductActionsRequest request) {
    return new VendorProductActionsResponse(request.getProductCodes(), false, e.getMessage());
  }

  private BulkVendorProductActionsRequest getBulkVendorProductActionsRequest(List<Map<String, String>> data,
      BulkVendorProductAssignRequest bulkVendorProductAssignRequest) {
    Map<String, List<String>> assigneeAndProductCodesMap =
        getAssigneeAndProductCodesMap(data, bulkVendorProductAssignRequest.getAssignmentType());
    List<BulkScreeningProductActionsRequest> bulkScreeningProductActionsRequests = new ArrayList<>();
    for (Map.Entry<String, List<String>> entry : assigneeAndProductCodesMap.entrySet()) {
      bulkScreeningProductActionsRequests.add(
          getBulkScreeningProductActionsRequest(entry.getKey(), bulkVendorProductAssignRequest.getUpdatedBy(),
              entry.getValue()));
    }
    return new BulkVendorProductActionsRequest(bulkVendorProductAssignRequest.getStoreId(),
        bulkVendorProductAssignRequest.getRequestId(), bulkVendorProductAssignRequest.getUpdatedBy(), ASSIGN,
        bulkVendorProductAssignRequest.getAssignmentType(), bulkScreeningProductActionsRequests);
  }

  private Map<String, List<String>> getAssigneeAndProductCodesMap(List<Map<String, String>> successData, String type) {
    Map<String, List<String>> assigneeAndProductCodesMap = new HashMap<>();
    for (Map<String, String> row : successData) {
      String assignee;
      if (TYPE_CONTENT_REVIEW.equals(type)) {
        assignee = row.get(VendorProductDataBulkParameters.CONTENT_ASSIGNEE);
      } else {
        assignee = row.get(VendorProductDataBulkParameters.IMAGE_ASSIGNEE);
      }
      String productCode = row.get(VendorProductDataBulkParameters.PRODUCT_CODE);
      if (!assigneeAndProductCodesMap.containsKey(assignee)) {
        assigneeAndProductCodesMap.put(assignee, Collections.singletonList(productCode));
      } else {
        List<String> productCodes = new ArrayList<>(assigneeAndProductCodesMap.get(assignee));
        productCodes.add(productCode);
        assigneeAndProductCodesMap.put(assignee, productCodes);
      }
    }
    return assigneeAndProductCodesMap;
  }

  private BulkScreeningProductActionsRequest getBulkScreeningProductActionsRequest(String assignedTo, String assignedBy,
      List<String> productCodes) {
    return BulkScreeningProductActionsRequest.builder().assignTo(assignedTo).assignedBy(assignedBy)
        .productCodes(productCodes).build();
  }

  private String validateRowAndGetError(Map<String, String> row, String actionType, List<String> validUserRoleList) {
    StringBuilder stringBuilder = new StringBuilder();
    if (!validateProductCode(row)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_PRODUCT_CODE);
    }
    if (TYPE_CONTENT_REVIEW.equals(actionType)) {
      if (!validateContentAssignee(row, validUserRoleList)) {
        stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_CONTENT_ASSIGNEE);
      }
    } else {
      if (!validateImageAssignee(row, validUserRoleList)) {
        stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_IMAGE_ASSIGNEE);
      }
    }
    return stringBuilder.toString();
  }

  private boolean validateProductCode(Map<String, String> row) {
    if (StringUtils.isBlank(row.get(VendorProductDataBulkParameters.PRODUCT_CODE))) {
      LOGGER.error(Constant.PRODUCT_CODE_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateContentAssignee(Map<String, String> row, List<String> validUserRoleList) {
    String assignee = row.get(VendorProductDataBulkParameters.CONTENT_ASSIGNEE);
    if (StringUtils.isBlank(assignee)) {
      LOGGER.error(Constant.CONTENT_ASSIGNEE_EMPTY, row);
      return false;
    }
    if (!validUserRoleList.contains(assignee)) {
      LOGGER.error(Constant.CONTENT_ASSIGNEE_INVALID, row);
      return false;
    }
    return true;
  }

  private boolean validateImageAssignee(Map<String, String> row, List<String> validUserRoleList) {
    String assignee = row.get(VendorProductDataBulkParameters.IMAGE_ASSIGNEE);
    if (StringUtils.isBlank(assignee)) {
      LOGGER.error(Constant.IMAGE_ASSIGNEE_EMPTY, row);
      return false;
    }
    if (!validUserRoleList.contains(assignee)) {
      LOGGER.error(Constant.IMAGE_ASSIGNEE_INVALID, row);
      return false;
    }
    return true;
  }


  private Sheet getExcelSheetData(String filePath) {
    try (InputStream fileInputStream = new FileInputStream(new File(filePath))) {
      return POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    } catch (IOException e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess)
      throws IOException {
    List<BulkInternalProcessData> internalProcessData = internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId, bulkInternalProcess.getId());
    List<String> bulkInternalProcessDataStatusList =
      Optional.ofNullable(internalProcessData).orElse(new ArrayList<>()).stream().map(BulkInternalProcessData::getStatus).distinct()
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
      List<Map<String, String>> successData;
      if (bulkInternalProcess.getProcessType()
        .equals(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name())) {
        successData = getSuccessRowDataNumberForAutoAssignments(internalProcessData);
      } else {
        successData = getSuccessRowDataNumber(internalProcessData);
      }
      updateFinalStatusForBulkVendorAssignment(bulkInternalProcess, internalProcessData, successData);
      if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())) {
        notificationService.sendBulkAssignNotification(new BulkProcess(), bulkInternalProcess.getSuccessCount(),
          bulkInternalProcess.getTotalCount(), bulkInternalProcess.getSellerCode());
      } else {
        notificationService.sendBulkAssignNotification(new BulkProcess(), successData.size(),
          internalProcessData.size(), bulkInternalProcess.getSellerCode());
      }
  }

  private List<Map<String, String>> getSuccessRowDataNumber(List<BulkInternalProcessData> internalProcessData)
      throws IOException {
    List<BulkInternalProcessData> successInternalProcessData =
        internalProcessData.stream().filter(data -> StringUtils.isBlank(data.getErrorMessage()))
            .collect(Collectors.toList());
    List<Map<String, String>> successData = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : successInternalProcessData) {
      Map<String, String> successRowData = new HashMap<>();
      VendorBulkAssignmentRequest vendorBulkAssignmentRequest =
          objectMapper.readValue(bulkInternalProcessData.getData(), VendorBulkAssignmentRequest.class);
      successRowData
          .put(VendorProductDataBulkParameters.ROW_NUMBER, String.valueOf(vendorBulkAssignmentRequest.getRowNumber()));
      successData.add(successRowData);
    }
    return successData;
  }

  private List<Map<String, String>> getSuccessRowDataNumberForAutoAssignments(List<BulkInternalProcessData> internalProcessData)
    throws IOException {
    List<BulkInternalProcessData> successInternalProcessData =
      internalProcessData.stream().filter(data -> StringUtils.isBlank(data.getErrorMessage()))
        .collect(Collectors.toList());
    List<Map<String, String>> successData = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : successInternalProcessData) {
      Map<String, String> successRowData = new HashMap<>();
      VendorBulkAssignmentRequest vendorBulkAssignmentRequest =
        objectMapper.readValue(bulkInternalProcessData.getData(), VendorBulkAssignmentRequest.class);
      successRowData
        .put(VendorProductDataBulkParameters.PRODUCT_CODE,
          String.valueOf(vendorBulkAssignmentRequest.getProductCode()));
      successData.add(successRowData);
    }
    return successData;
  }

  private void updateFinalStatusForBulkVendorAssignment(BulkInternalProcess bulkInternalProcess,
      List<BulkInternalProcessData> internalProcessData, List<Map<String, String>> successData) {
    bulkInternalProcess.setEndTime(Calendar.getInstance().getTime());
    if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())) {
      long completedAssignments = internalProcessData.stream()
        .map(BulkInternalProcessData::getStatus)
        .filter(status -> StringUtils.equals(status, ProcessStatus.COMPLETED.name()))
        .count();
      if(internalProcessData.size() == completedAssignments){
        bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
      }
      else {
        if (successData.size() != Constant.ZERO) {
          bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
        }
        else {
          bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
        }
      }
      bulkInternalProcess.setSuccessCount(successData.size());
      bulkInternalProcess.setErrorCount(bulkInternalProcess.getTotalCount() - successData.size());
      bulkInternalProcess.setErrorFilePath(null);
      internalProcessService.saveInternalProcess(bulkInternalProcess);
      return;
    }
    if (successData.size() == internalProcessData.size()) {
      bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
    }
    else {
      if (successData.size() != Constant.ZERO) {
        bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
      } else {
        bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      }
    }
      bulkInternalProcess.setErrorFilePath(
          ProcessorUtils.BULK_VENDOR_ASSIGN + bulkInternalProcess.getInternalProcessRequestCode() + File.separator
              + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils.getFileFormat(
              bulkInternalProcess.getFileName()));
    bulkInternalProcess.setSuccessCount(successData.size());
    bulkInternalProcess.setErrorCount(internalProcessData.size() - successData.size());
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  @Override
  public void processVendorBulkAssignment(String storeId, String updatedBy, String processType,
      String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.getBulkInternalProcessDataById(storeId, internalProcessDataRequestId);
    if (ProcessStatus.IN_PROGRESS.name().equals(bulkInternalProcessData.getStatus())) {
      bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
      bulkInternalProcessData = internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
      try {
        VendorBulkAssignmentRequest vendorBulkAssignmentRequest =
            RequestHelper.toVendorBulkAssignmentRequestFromJson(bulkInternalProcessData.getData());
        BulkVendorProductActionsRequest bulkVendorProductActionsRequest =
            RequestHelper.toBulkVendorProductActionsRequest(vendorBulkAssignmentRequest, bulkInternalProcessData);
        assignProductsToReviewers(bulkInternalProcessData, bulkVendorProductActionsRequest);
      } catch (Exception e) {
        log.error("Exception caught when processing internal bulk assignment for internalProcessDataRequestId {} ",
            internalProcessDataRequestId, e);
        RequestHelper
            .updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.FAILED.name(),
                Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    } else {
      log.error("Vendor bulk assignment already processed for ID: {}", internalProcessDataRequestId);
    }
  }

  @Override
  public void processVendorAutoAssignment(VendorAutoAssignmentRequest vendorAutoAssignmentRequest)
      throws JsonProcessingException {
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.toBulkInternalProcessForAutoAssignment(vendorAutoAssignmentRequest);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    log.info("Vendor bulk auto assignment request saved in Internal table request : {}", bulkInternalProcess);
  }

  private void assignProductsToReviewers(BulkInternalProcessData bulkInternalProcessData,
      BulkVendorProductActionsRequest bulkVendorProductActionsRequest) throws Exception {
    BulkVendorProductActionsResponse bulkVendorProductActionsResponse =
        productDistributionTaskRepository.bulkVendorProductActions(bulkVendorProductActionsRequest);
    if (Objects.nonNull(bulkVendorProductActionsResponse) && CollectionUtils
        .isNotEmpty(bulkVendorProductActionsResponse.getVendorProductActionsResponses()) && CollectionUtils
        .isNotEmpty(bulkVendorProductActionsResponse.getVendorProductActionsResponses().get(0).getProductCode())) {
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.FAILED.name(),
          bulkVendorProductActionsResponse.getVendorProductActionsResponses().get(0).getReasonOfFailure());
    } else {
      RequestHelper
          .updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.COMPLETED.name(),
              StringUtils.EMPTY);
    }
  }
}