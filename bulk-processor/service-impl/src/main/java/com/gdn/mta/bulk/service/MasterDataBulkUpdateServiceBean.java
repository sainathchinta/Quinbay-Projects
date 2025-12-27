package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_BULK_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_UPDATE;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.InternalBulkUploadRequest;
import com.gdn.mta.bulk.models.MasterDataBulkUpdateErrorCounter;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.BulkProductSuspensionParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.MasterDataBulkParameters;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.google.common.collect.Lists;

@Service
public class MasterDataBulkUpdateServiceBean implements MasterDataBulkUpdateService {

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private FileStorageService fileStorageService;

  private static final Logger LOGGER = LoggerFactory.getLogger(MasterDataBulkUpdateServiceBean.class);

  @Override
  public void processBulkUpdate(MasterDataBulkUpdateRequest masterDataBulkUpdateRequest)
    throws IOException {
    LOGGER.info(Constant.MASTER_BULK_PROCESS_LOG_MESSAGE, masterDataBulkUpdateRequest.getStoreId(),
        masterDataBulkUpdateRequest.getBulkProcessCode(), masterDataBulkUpdateRequest.getUpdatedBy());
    BulkProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(
        masterDataBulkUpdateRequest, 0, 0);
    bulkProcess.setDescription(getBulkProcessDescription(masterDataBulkUpdateRequest));
   BulkProcess savedBulkProcess = bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess);
   process(masterDataBulkUpdateRequest, savedBulkProcess);
  }

  private void process(MasterDataBulkUpdateRequest masterDataBulkUpdateRequest, BulkProcess bulkProcess)
    throws IOException {
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessService.saveOperation(bulkProcess);
    Sheet excelSheetData = fileStorageService.getFileDataWithInternalUploadRequest(
      BulkInternalUploadRequestDTO.builder().relativePath(masterDataBulkUpdateRequest.getFilePath())
        .bulkInternalProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD).build());
    List<Map<String, String>> productDataFromExcel = POIUtil.readFromExcelForBulkUpdate(excelSheetData, 1, 0, 0,
        new HashMap<>());
    int totalProduct = productDataFromExcel.size();
    bulkProcess.setTotalCount(totalProduct);
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    MasterDataBulkUpdateErrorCounter counter = new MasterDataBulkUpdateErrorCounter();
    validateExcelData(productDataFromExcel, successData, failureData, counter);
    List<SimpleMasterProductUpdateResponse> responses =
        updateMasterDataFromSuccessData(successData, masterDataBulkUpdateRequest);
    updateFailedDataAfterUpdate(responses, successData, failureData, counter);
    removeFailedDataFromSuccessData(successData, failureData);
    if(CollectionUtils.isNotEmpty(failureData)) {
      try {
        ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR +
            bulkProcess.getBulkProcessCode() + File.separator + bulkProcess.getBulkProcessCode());
        BulkUpdateServiceUtil.deletePassedDataFromExcel(excelSheetData, successData,
            bulkProcess.getBulkProcessCode(), masterDataBulkUpdateRequest.getFilePath(), ProcessorUtils.BULK_UPDATE_DIR);
      } catch (Exception e) {
        LOGGER.error(Constant.FAILED_PRODUCT_SAVE_ERROR, bulkProcess.getBulkProcessCode(), e);
      }
      IntStream.range(0, failureData.size()).forEach(i ->
          this.trackerService.sendTracker(MASTER_PRODUCT_BULK_UPDATE, MASTER_PRODUCT_UPDATE,
            HYPHEN, FAILED, masterDataBulkUpdateRequest.getUpdatedBy()));
    }
    if (successData.size() == totalProduct) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      if (CollectionUtils.isNotEmpty(successData)) {
        bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      } else {
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      }
    }
    if(BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      deleteFile(masterDataBulkUpdateRequest.getFilePath());
    }
    updateBulkFinalStatus(bulkProcess, masterDataBulkUpdateRequest, successData.size(), totalProduct, counter);
  }

  @Override
  public void processInternalBulkUploadEvent(String storeId,
      String updatedBy, String processType, String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.getBulkInternalProcessDataById(storeId, internalProcessDataRequestId);
    bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
    bulkInternalProcessData =
        internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)).get(0);
    try {
      InternalBulkUploadRequest internalBulkUploadRequest =
          RequestHelper.toInternalBulkUploadRequestFromJson(bulkInternalProcessData.getData());
      SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest =
          RequestHelper.toSimpleMasterProductUpdateRequest(internalBulkUploadRequest);
      BulkMasterProductUpdateRequest bulkMasterProductUpdateRequest =
          RequestHelper.toBulkMasterProductUpdateRequest(simpleMasterProductUpdateRequest, storeId, updatedBy);
      productRepository.updateMasterProducts(bulkMasterProductUpdateRequest);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.COMPLETED.name(), StringUtils.EMPTY);
    } catch (Exception e) {
      LOGGER.info("Error caught while processing internal bulk upload. ", e);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      this.trackerService.sendTracker(TrackerConstants.MASTER_PRODUCT_BULK_UPDATE,
          TrackerConstants.MASTER_PRODUCT_UPDATE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, updatedBy);
    }
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
  }

  @Override
  public void setFinalStatusAndGenerateFailedExcel(BulkInternalProcess bulkInternalProcess, String storeId)
      throws Exception {
    List<BulkInternalProcessData> internalProcessData = internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId, bulkInternalProcess.getId());
    List<String> bulkInternalProcessDataStatusList =
      Optional.ofNullable(internalProcessData).orElse(new ArrayList<>()).stream().map(BulkInternalProcessData::getStatus).distinct()
        .collect(Collectors.toList());
    if(CollectionUtils.isEmpty(bulkInternalProcessDataStatusList)){
      LOGGER.info("No Entry in Data Table found for process : {} and request-code : {} ",
        bulkInternalProcess.getProcessType(), bulkInternalProcess.getInternalProcessRequestCode());
      return;
    }
    else if (bulkInternalProcessDataStatusList.contains(ProcessStatus.PENDING.name())
      || bulkInternalProcessDataStatusList.contains(ProcessStatus.IN_PROGRESS.name())) {
      LOGGER.info("No Status update since process : {} is still in progress  for "
          + "internalProcessRequestId : {}", bulkInternalProcess.getProcessType(),
        bulkInternalProcess.getId());
      return;
    }
    List<BulkInternalProcessData> failedInternalProcessData =
        internalProcessData.stream().filter(data -> StringUtils.isNotEmpty(data.getErrorMessage()))
            .collect(Collectors.toList());
    List<HashMap<String, String>> failedData = new ArrayList<>();
    int successSize = internalProcessData.size() - failedInternalProcessData.size();
    for (BulkInternalProcessData failedProcessData : failedInternalProcessData) {
      LinkedHashMap<String, String> rowDataJson =
          new ObjectMapper().readValue(failedProcessData.getData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
      failedData.add(rowDataJson);
    }
    updateInternalBulkUploadStatus(failedData, bulkInternalProcess, successSize, internalProcessData.size());
  }

  private void updateInternalBulkUploadStatus(List<HashMap<String, String>> failedData,
      BulkInternalProcess bulkInternalProcess, int successSize, int totalSize) throws Exception {
    if (CollectionUtils.isNotEmpty(failedData)) {
      try {
        SXSSFWorkbook workbook = POIUtil.generateXLFileForSuspension(failedData);
        byte[] bytes =
            POIUtil.getByteContentFromExcel(workbook.getSheet(BulkProductSuspensionParameters.PRODUCT_DATA));
        BulkUpdateProcessDTO bulkUpdateProcessDTO =
            BulkUpdateProcessDTO.builder().bulkProcessType(BulkProcessType.INTERNAL_UPLOAD.getValue())
                .fileContent(bytes).build();
        fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkInternalProcess.getInternalProcessRequestCode(),
            bulkInternalProcess.getFileName());
      } catch (Exception e) {
        LOGGER.error(Constant.FAILED_INTERNAL_BULK_UPLOAD_ERROR, bulkInternalProcess.getInternalProcessRequestCode(), e);
      }
    }
    if (successSize == totalSize)
      bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
    else {
      if (successSize != Constant.ZERO) {
        bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
      } else {
        bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      }
    }
    bulkInternalProcess.setEndTime(Calendar.getInstance().getTime());
    bulkInternalProcess.setErrorFilePath(new StringBuilder(bulkInternalProcess.getFileName()).append(
        BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.PRODUCTS_SUCCESSFULLY_INTERNAL_BULK_UPLOAD)
        .append(successSize).append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.PRODUCTS_NOT_UPDATE).append(totalSize - successSize).toString());
    bulkInternalProcess.setSuccessCount(successSize);
    bulkInternalProcess.setErrorCount(totalSize - successSize);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  private void updateBulkFinalStatus(BulkProcess bulkProcess, MasterDataBulkUpdateRequest bulkUpdateQueue,
    int successProductCount, int totalProductCount, MasterDataBulkUpdateErrorCounter counter) {
    bulkProcess.setEndDate(new Date());
    bulkProcess.setDescription(new StringBuilder(bulkUpdateQueue.getFilePath())
        .append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.PRODUCTS_SUCCESSFULLY_UPDATED).append(successProductCount)
        .append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.PRODUCTS_NOT_UPDATED).append(totalProductCount - successProductCount)
        .toString());
    bulkProcess.setSuccessCount(successProductCount);
    bulkProcess.setErrorCount(totalProductCount - successProductCount);
    bulkProcess.setInputErrorCount(counter.getInputErrorCount());
    bulkProcess.setSystemErrorCount(counter.getSystemErrorCount());
    bulkProcessService.saveOperation(bulkProcess);
    LOGGER.info(Constant.MASTER_DATA_BULK_UPDATE_COMPLETED_MESSAGE, bulkUpdateQueue);
  }

  private void deleteFile(String filePath){
    try{
      ProcessorUtils.deleteFile(filePath);
    }catch (Exception e){
      LOGGER.error(Constant.ERROR_IN_FILE_DELETION, filePath, e);
    }
  }

  private void removeFailedDataFromSuccessData(
    List<Map<String, String>> successData, List<Map<String, String>> failureData){
    List<String> invalidRowNumbers = failureData.stream().map(Map::entrySet).flatMap(Collection::stream)
        .filter(entry -> "RowNumber".equals(entry.getKey()))
        .map(Map.Entry::getValue)
        .collect(Collectors.toList());
    successData.removeIf(map -> invalidRowNumbers.contains(map.get("RowNumber")));
  }

  private void updateFailedDataAfterUpdate(List<SimpleMasterProductUpdateResponse> responses,
    List<Map<String, String>> successData, List<Map<String, String>> failureData,
    MasterDataBulkUpdateErrorCounter counter){
    List<String> failedProductCodes = new ArrayList<>();
    Map<String, String> failedProductCodeAndReasonMap = new HashMap<>();
    for (SimpleMasterProductUpdateResponse response: responses) {
      if(!response.getUpdateSuccess()){
        failedProductCodes.add(response.getProductCode());
        failedProductCodeAndReasonMap.put(response.getProductCode(), response.getReasonOfFailure());
      }
    }
    List<Map<String, String>> failedData = successData.stream().map(Map::entrySet)
        .map(entries -> Pair.of(entries.stream()
            .filter(entry -> MasterDataBulkParameters.PRODUCT_CODE.equals(entry.getKey()))
            .findFirst().map(Map.Entry::getValue).orElse(null), entries))
        .filter(pair -> failedProductCodes.contains(pair.getLeft()))
        .map(pair -> Pair.of(pair.getLeft(),
            pair.getRight().stream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue))))
        .map(pair -> {
          pair.getRight().put(MasterDataBulkParameters.FAILURE_REASON, failedProductCodeAndReasonMap.get(pair.getLeft()));
          return pair.getRight();})
        .collect(Collectors.toList());

    failureData.addAll(failedData);
    counter.setSystemErrorCount(failedData.size());
  }

  private void validateExcelData(List<Map<String, String>> excelData, List<Map<String, String>> successData,
    List<Map<String, String>> failureData, MasterDataBulkUpdateErrorCounter counter){
    for (Map<String, String> row: excelData) {
      String error = validateRowAndGetError(row, counter);
      if(StringUtils.isBlank(error)){
        successData.add(row);
      }else {
        row.put(MasterDataBulkParameters.FAILURE_REASON, error);
        failureData.add(row);
        counter.incrementInputErrorCount();
      }
    }
  }

  private List<SimpleMasterProductUpdateResponse> updateMasterDataFromSuccessData(
    List<Map<String, String>> successData, MasterDataBulkUpdateRequest masterDataBulkUpdateRequest) {
    int bulkUpdateBatchSize = systemParameter.getMasterDataBulkUpdateBatchSize();
    return Lists.partition(successData, bulkUpdateBatchSize).stream()
        .map(listData -> getBulkMasterProductUpdateRequest(listData, masterDataBulkUpdateRequest))
        .map(this::updateMasterProducts)
        .map(BulkMasterProductUpdateResponse::getSimpleMasterProductUpdateResponses)
        .flatMap(List::stream)
        .collect(Collectors.toList());
  }

  private BulkMasterProductUpdateResponse updateMasterProducts(BulkMasterProductUpdateRequest request){
    try{
      return productRepository.updateMasterProducts(request);
    }catch (Exception e){
      LOGGER.error(e.getMessage(), e);
      List<SimpleMasterProductUpdateResponse> simpleMasterProductUpdateResponses =
          request.getSimpleMasterProductUpdateRequests().stream()
              .map(req -> getSimpleMasterUpdateResponseInCaseOfException(e, req))
              .collect(Collectors.toList());
      return new BulkMasterProductUpdateResponse(simpleMasterProductUpdateResponses);
    }
  }

  private SimpleMasterProductUpdateResponse getSimpleMasterUpdateResponseInCaseOfException(
    Exception e, SimpleMasterProductUpdateRequest request) {
    return new SimpleMasterProductUpdateResponse.Builder()
        .productCode(request.getProductCode()).updateSuccess(false).reasonOfFailure(e.getMessage()).build();
  }

  private BulkMasterProductUpdateRequest getBulkMasterProductUpdateRequest(
      List<Map<String, String>> data, MasterDataBulkUpdateRequest masterDataBulkUpdateRequest){
    List<SimpleMasterProductUpdateRequest> simpleMasterProductUpdateRequests = data.stream()
        .map(this::getSimpleMasterProductUpdateRequest)
        .collect(Collectors.toList());
    BulkMasterProductUpdateRequest request =  new BulkMasterProductUpdateRequest(simpleMasterProductUpdateRequests);
    request.setUpdatedBy(masterDataBulkUpdateRequest.getUpdatedBy());
    request.setStoreId(masterDataBulkUpdateRequest.getStoreId());
    request.setUpdatedDate(Calendar.getInstance().getTime());
    return request;
  }

  private SimpleMasterProductUpdateRequest getSimpleMasterProductUpdateRequest(Map<String, String> row) {
    return new SimpleMasterProductUpdateRequest.Builder()
        .productCode(row.get(MasterDataBulkParameters.PRODUCT_CODE))
        .name(row.get(MasterDataBulkParameters.PRODUCT_NAME))
        .brand(row.get(MasterDataBulkParameters.BRAND))
        .length(Double.parseDouble(row.get(MasterDataBulkParameters.LENGTH)))
        .width(Double.parseDouble(row.get(MasterDataBulkParameters.WIDTH)))
        .weight(Double.parseDouble(row.get(MasterDataBulkParameters.WEIGHT)))
        .height(Double.parseDouble(row.get(MasterDataBulkParameters.HEIGHT)))
        .dangerousGoodsLevel(
            (int) Double.parseDouble(row.get(MasterDataBulkParameters.DANGEROUS_GOOD_LEVEL)))
        .build();
  }

  private String validateRowAndGetError(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    StringBuilder stringBuilder = new StringBuilder();
    if(!validateProductCode(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_PRODUCT_CODE);
    }
    if(!validateProductName(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_PRODUCT_NAME);
    }
    if(!validateBrand(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_BRAND);
    }
    if(!validateLength(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_LENGTH);
    }
    if(!validateWidth(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_WIDTH);
    }
    if(!validateHeight(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_HEIGHT);
    }
    if(!validateWeight(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_WEIGHT);
    }
    if(!validateDangerousGoodsLevel(row, counter)){
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.ERROR_IN_DG_LEVEL);
    }
    return stringBuilder.toString();
  }

  private boolean validateProductCode(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.PRODUCT_CODE))){
      counter.incrementProductCodeErrorCount();
      LOGGER.error(Constant.PRODUCT_CODE_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateProductName(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.PRODUCT_NAME))){
      counter.incrementProductNameErrorCount();
      LOGGER.error(Constant.PRODUCT_NAME_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateBrand(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.BRAND))){
      counter.incrementBrandErrorCount();
      LOGGER.error(Constant.BRAND_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateLength(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.LENGTH))){
      counter.incrementLengthErrorCount();
      LOGGER.error(Constant.LENGTH_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateWidth(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.WIDTH))){
      counter.incrementWidthErrorCount();
      LOGGER.error(Constant.WIDTH_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateHeight(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.HEIGHT))){
      counter.incrementHeightErrorCount();
      LOGGER.error(Constant.HEIGHT_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateWeight(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.WEIGHT))){
      counter.incrementWeightErrorCount();
      LOGGER.error(Constant.WEIGHT_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateDangerousGoodsLevel(Map<String, String> row, MasterDataBulkUpdateErrorCounter counter){
    if(StringUtils.isBlank(row.get(MasterDataBulkParameters.DANGEROUS_GOOD_LEVEL))){
      counter.incrementDangerousGoodsLevelErrorCount();
      LOGGER.error(Constant.DANGEROUS_GOOD_LEVEL_EMPTY_ERROR, row);
      return false;
    }
    return true;
  }

  private Sheet getExcelSheetData(String filePath) {
    try(InputStream fileInputStream = new FileInputStream(new File(filePath))) {
      return POIUtil.getSheetForInputStream(fileInputStream,
          filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    }catch (IOException e){
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }

  private String getBulkProcessDescription(MasterDataBulkUpdateRequest masterDataBulkUpdateRequest) {
    return new StringBuilder(masterDataBulkUpdateRequest.getFilePath())
        .append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING).toString();
  }
}
