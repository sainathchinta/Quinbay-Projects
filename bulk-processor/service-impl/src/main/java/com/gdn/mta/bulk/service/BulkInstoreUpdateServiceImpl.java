package com.gdn.mta.bulk.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.models.InstoreUpdateModel;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional(readOnly = true)
public class BulkInstoreUpdateServiceImpl implements  BulkInstoreUpdateService{

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Override
  @Transactional(readOnly = false)
  public void processInstoreUpdateEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception {
    BulkProcess bulkProcess = bulkProcessService
        .findByBulkProcessCode(bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList = bulkProcessDataService
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers(),
            BulkProcessData.STATUS_PENDING);
    try {
      if (CollectionUtils.isEmpty(bulkProcessDataList)) {
        log.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
            bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getRowNumbers());
        return;
      }
      Map<String, BulkProcessData> bulkProcessDataMap = new HashMap<>();
      List<BulkProcessData> bulkProcessDataUpdateList = new ArrayList<>();
      List<InstoreUpdateModel> instoreUpdateModelSucessList = new ArrayList<>();
      bulkProcessDataList.forEach(bulkProcessData -> bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS));
      bulkProcessDataList = bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
      processInstoreBulkUpdateDataRequest(bulkProcessDataList, bulkProcessDataMap, bulkProcessDataUpdateList,
          instoreUpdateModelSucessList);
      bulkUpdatInstoreInXproduct(bulkProcess, bulkProcessDataMap, bulkProcessDataUpdateList,
          instoreUpdateModelSucessList);
      bulkProcessDataService.saveBulkProcessData(bulkProcessDataUpdateList);
    } catch (Exception e) {
      log.error("Error while processing intore update event listener. bulkUpdateEventModel : {} ", bulkUpdateEventModel,
          e);
      //Fail the records
      bulkProcessDataList.forEach(
          bulkProcessData -> setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
              Constant.SYSTEM_ERROR, 1, null));
      bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
    }
  }

  private void bulkUpdatInstoreInXproduct(BulkProcess bulkProcess, Map<String, BulkProcessData> bulkProcessDataMap,
      List<BulkProcessData> bulkProcessDataUpdateList, List<InstoreUpdateModel> instoreUpdateModelSucessList) {
    if (CollectionUtils.isNotEmpty(instoreUpdateModelSucessList)) {
      List<String> failedProductSkus = new ArrayList<>();
      int bulkOff2OnUpdateBatchSize = Integer.parseInt(systemParameterConfigService
          .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)
          .getValue());
      for (List<InstoreUpdateModel> listData : Lists
          .partition(instoreUpdateModelSucessList, bulkOff2OnUpdateBatchSize)) {
        failedProductSkus.addAll(bulkProcessService.bulkUpdateOff2On(listData.stream().collect(Collectors
            .toMap(instoreUpdateModel -> instoreUpdateModel.getProductSku(),
                instoreUpdateModel -> Constant.ONE_DECIMAL_STRING.equals(instoreUpdateModel.getOff2OnFlag()),
                (a, b) -> b)), bulkProcess.getRequestId(), bulkProcess.getCreatedBy()));
      }
      processFailedProductSkusForInstoreUpdate(bulkProcessDataMap, failedProductSkus, bulkProcessDataUpdateList,
          instoreUpdateModelSucessList);
    }
  }

  private void processFailedProductSkusForInstoreUpdate(Map<String, BulkProcessData> bulkProcessDataMap,
      List<String> failedProductSkus, List<BulkProcessData> bulkProcessDataUpdateList,
      List<InstoreUpdateModel> instoreUpdateModelSucessList) {
    for (InstoreUpdateModel instoreUpdateModel : instoreUpdateModelSucessList) {
      BulkProcessData bulkProcessData = bulkProcessDataMap.get(instoreUpdateModel.getProductSku());
      if (failedProductSkus.contains(instoreUpdateModel.getProductSku())) {
        setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
            String.format(Constant.ERROR_OFF_2_ON_UPDATE, instoreUpdateModel.getProductSku()), 1, null);
      } else {
        setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_SUCCESS, StringUtils.EMPTY,
            null, null);
      }
      bulkProcessDataUpdateList.add(bulkProcessData);
    }
  }

  private void processInstoreBulkUpdateDataRequest(List<BulkProcessData> bulkProcessDataList,
      Map<String, BulkProcessData> bulkProcessDataMap, List<BulkProcessData> bulkProcessDataUpdateList,
      List<InstoreUpdateModel> instoreUpdateModelSucessList) {
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      try {
        InstoreUpdateModel instoreUpdateModel =
            RequestHelper.toInstoreUpdateModelFromJson(bulkProcessData.getBulkRequestData());
        bulkProcessDataMap.put(instoreUpdateModel.getProductSku(), bulkProcessData);
        if (validateInstoreUpdateData(instoreUpdateModel)) {
          setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
              BulkProcessValidationErrorMessages.INSTORE_INPUT_VALIDATION_ERROR, null, 1);
          bulkProcessDataUpdateList.add(bulkProcessData);
        } else {
          instoreUpdateModelSucessList.add(instoreUpdateModel);
        }
      } catch (IOException e) {
        log.error("error count while processing instore update event: {} ", bulkProcessData, e);
        setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL, Constant.SYSTEM_ERROR, 1,
            null);
        bulkProcessDataUpdateList.add(bulkProcessData);
      }
    }
  }

  private boolean validateInstoreUpdateData(InstoreUpdateModel instoreUpdateModel) {
    return StringUtils.isEmpty(instoreUpdateModel.getProductSku()) || (
        !Constant.ONE_DECIMAL_STRING.equals(instoreUpdateModel.getOff2OnFlag()) && !Constant.ZERO_DECIMAL_STRING
            .equals(instoreUpdateModel.getOff2OnFlag()));
  }

  private void setBulkProcessDataStatusAndErrorMessage(BulkProcessData bulkProcessData, String status,
      String errorMessage, Integer systemErrorCount, Integer inputErrorCount) {
    bulkProcessData.setStatus(status);
    bulkProcessData.setInputErrorCount(inputErrorCount);
    bulkProcessData.setSystemErrorCount(systemErrorCount);
    bulkProcessData.setEndDate(new Date());
    bulkProcessData.setErrorMessage(errorMessage);
  }
}
