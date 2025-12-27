package com.gdn.mta.bulk.listener.kafka;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_BULK_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.mta.bulk.service.MasterDataBulkUpdateService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

@Service
public class MasterDataBulkUploadListener {

  private static final Logger LOG = LoggerFactory.getLogger(MasterDataBulkUploadListener.class);

  @Autowired
  private MasterDataBulkUpdateService masterDataBulkUpdateService;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getInternalUserBulkUploadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {}", kafkaTopicProperties.getInternalUserBulkUploadEvent(), message);
    MasterDataBulkUpdateRequest bulkUpdateQueue = objectMapper.readValue(message, MasterDataBulkUpdateRequest.class);
    LoggerAttributeModel loggerModel = null;
    String userName = StringUtils.EMPTY;
    try {
      userName = bulkUpdateQueue.getUpdatedBy();
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed",
          null, userName, bulkUpdateQueue.getRequestId(), bulkUpdateQueue.getStoreId(), null,
          LoggerChannel.KAFKA.getValue(), bulkUpdateQueue.getFilePath(), String.valueOf(bulkUpdateQueue));
      setMandatoryParameters(bulkUpdateQueue);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      SystemParameterConfig internalUploadParameter =
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.INTERNAL_UPLOAD_SWITCH);
      boolean internalUploadSwitch = false;
      if (Objects.nonNull(internalUploadParameter)) {
        internalUploadSwitch = Boolean.valueOf(internalUploadParameter.getValue());
      }
      if(internalUploadSwitch){
        internalProcessServiceWrapper.uploadInternalBulkUploadToBulkInternalProcess(bulkUpdateQueue.getStoreId(), bulkUpdateQueue);
      } else {
        masterDataBulkUpdateService.processBulkUpdate(bulkUpdateQueue);
      }
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel,
          e, bulkUpdateQueue), e);
      this.trackerService.sendTracker(PRODUCT_UPDATE_EVENT, MASTER_PRODUCT_BULK_UPDATE,
          HYPHEN, FAILED, userName);
    }
  }

  private void setMandatoryParameters(MasterDataBulkUpdateRequest masterDataBulkUpdateRequest) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, masterDataBulkUpdateRequest.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, masterDataBulkUpdateRequest.getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, masterDataBulkUpdateRequest.getUpdatedBy());
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }
}
