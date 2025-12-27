package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;
import com.gdn.mta.bulk.models.NeedRevisionDeletionEventModel;
import com.gdn.mta.bulk.service.NeedRevisionDeletionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class BulkNeedRevisionDeletionListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private NeedRevisionDeletionService needRevisionDeletionService;

  @Value("${batch.size.to.publish.data.entries}")
  private int batchSizeToPublishDataEntries;

  @KafkaListener(topics = "#{kafkaTopicProperties.getNeedRevisionDeletionEvent()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void processBulkNeedRevisionDeletion(String message) throws Exception {
    try {
      NeedRevisionDeletionEventModel needRevisionDeletionEventModel =
          objectMapper.readValue(message, NeedRevisionDeletionEventModel.class);
      log.info("Consumed Need revision Deletion event {} data with ids {} ",
          kafkaTopicProperties.getNeedRevisionDeletionEvent(),
          needRevisionDeletionEventModel.getNeedRevisionDeletionDataIds());
      List<BulkNeedRevisionDeletionData> needRevisionDeletionDataList =
          needRevisionDeletionService.fetchNeedRevisionDeletionDataByDeletionProcessCodeAndIds(
              needRevisionDeletionEventModel.getStoreId(),
              needRevisionDeletionEventModel.getDeletionProcessCode(),
              needRevisionDeletionEventModel.getNeedRevisionDeletionDataIds());
      needRevisionDeletionDataList.forEach(
          needRevisionDeletionData -> needRevisionDeletionData.setStatus(
              BulkNeedRevisionDeletion.STATUS_IN_PROGRESS));
      needRevisionDeletionDataList = needRevisionDeletionService.saveBulkNeedRevisionDeletionData(
          needRevisionDeletionDataList);
      needRevisionDeletionService.performEligibilityCheckAndProcessDataDeletion(
          needRevisionDeletionEventModel.getStoreId(), needRevisionDeletionDataList,
          needRevisionDeletionEventModel.getBusinessPartnerCode());
    } catch (Exception exception) {
      log.error("Error while processing Need Revision deletion event ", exception);
    }
  }

}
