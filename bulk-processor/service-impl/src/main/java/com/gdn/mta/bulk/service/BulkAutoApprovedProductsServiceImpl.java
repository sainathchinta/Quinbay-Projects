package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Service
public class BulkAutoApprovedProductsServiceImpl implements BulkAutoApprovedProductsService {

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private NotificationService notificationService;

  @Override
  public void setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(String storeId,
    BulkInternalProcess bulkInternalProcess) throws IOException {
    log.info("Setting final status and generating notification for process : {} and request-code "
        + ": {}", bulkInternalProcess.getProcessType(),
      bulkInternalProcess.getInternalProcessRequestCode());
    List<BulkInternalProcessData> internalProcessData =
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        storeId, bulkInternalProcess.getId());
    List<String> bulkInternalProcessDataStatusList =
      Optional.ofNullable(internalProcessData).orElse(new ArrayList<>()).stream()
        .map(BulkInternalProcessData::getStatus).distinct().collect(Collectors.toList());
    if (CollectionUtils.isEmpty(bulkInternalProcessDataStatusList)) {
      log.info("No Entry in Data Table found for process : {} and request-code : {} ",
        bulkInternalProcess.getProcessType(), bulkInternalProcess.getInternalProcessRequestCode());
      return;
    } else if (bulkInternalProcessDataStatusList.contains(ProcessStatus.PENDING.name())
      || bulkInternalProcessDataStatusList.contains(ProcessStatus.IN_PROGRESS.name())) {
      log.info("No Status update since process : {} is still in progress  for "
          + "internalProcessRequestId : {}", bulkInternalProcess.getProcessType(),
        bulkInternalProcess.getId());
      return;
    }
    int successDataNumber = (int) internalProcessData.stream()
      .filter(data -> !ProcessStatus.FAILED.name().equals(data.getStatus())).count();
    updateFinalStatusForBulkAutoApproveAssignment(bulkInternalProcess, internalProcessData,
      successDataNumber);
    notificationService.sendBulkAssignNotification(new BulkProcess(), successDataNumber,
      internalProcessData.size(), bulkInternalProcess.getSellerCode());
  }

  private void updateFinalStatusForBulkAutoApproveAssignment(
    BulkInternalProcess bulkInternalProcess, List<BulkInternalProcessData> internalProcessData,
    int successDataNumber) {
    if (successDataNumber == internalProcessData.size()) {
      bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
    } else {
      if (successDataNumber != Constant.ZERO) {
        bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
      } else {
        bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      }
    }
    bulkInternalProcess.setSuccessCount(successDataNumber);
    bulkInternalProcess.setErrorCount(internalProcessData.size() - successDataNumber);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }
}
