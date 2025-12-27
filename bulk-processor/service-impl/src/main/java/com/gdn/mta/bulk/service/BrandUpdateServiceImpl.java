package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.ProductBrandUpdateRequest;
import com.gdn.mta.bulk.util.RequestHelper;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BrandUpdateServiceImpl implements BrandUpdateService {

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public void publishBrandUpdateEvent(String storeId, List<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize) {
    List<BulkInternalProcessData> bulkInternalProcessDatas =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name(), fetchBatchSize);
    for (BulkInternalProcessData bulkInternalProcessData : Optional.ofNullable(bulkInternalProcessDatas)
        .orElse(new ArrayList<>())) {
      bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
      kafkaProducer.send(kafkaTopicProperties.getBrandUpdateEvent(),
          RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData,
              storeId, bulkInternalProcessData.getUpdatedBy()));
    }
  }

  @Override
  public void processBrandUpdateEvent(String storeId, String processType,
      String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.getBulkInternalProcessDataById(storeId, internalProcessDataRequestId);
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        ProductBrandUpdateRequest productBrandUpdateRequest =
            RequestHelper.toProductBrandUpdateRequestFromJson(bulkInternalProcessData.getData());
        pbpOutboundService.updateBrandOfProduct(storeId, productBrandUpdateRequest);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.COMPLETED.name(), null);
      } catch (Exception e) {
        log.error("Exception while updating product brand internalProcessDataRequestId : {} ",
            internalProcessDataRequestId, e);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), e.getMessage());
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }
}
