package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkReviewUploadModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class BulkReviewUploadListener {
    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private InternalProcessServiceWrapper internalProcessServiceWrapper;

    @Autowired
    private KafkaTopicProperties kafkaTopicProperties;

    @Trace(dispatcher = true)
    @KafkaListener(topics = "#{kafkaTopicProperties.getBulkReviewUploadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
    public void onDomainEventConsumed(String message) throws Exception {
        log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkReviewUploadEvent(), message);
        BulkReviewUploadModel bulkReviewUploadModel = null;
        LoggerAttributeModel loggerModel = null;
        try {
            bulkReviewUploadModel = objectMapper.readValue(message, BulkReviewUploadModel.class);
            loggerModel =
                    new LoggerAttributeModel(this, "onDomainEventConsumed", bulkReviewUploadModel.getBulkProcessCode(),
                            bulkReviewUploadModel.getCreatedBy(), bulkReviewUploadModel.getRequestId(),
                            bulkReviewUploadModel.getStoreId(), null, LoggerChannel.KAFKA.getValue(),
                            bulkReviewUploadModel.getFilePath(), String.valueOf(bulkReviewUploadModel));
            log.info(LoggerStandard.getLogInfoTemplate(loggerModel));
            setMandatoryParameters(bulkReviewUploadModel.getStoreId(), bulkReviewUploadModel.getCreatedBy());
            internalProcessServiceWrapper.uploadBulkReviewProcess(bulkReviewUploadModel.getStoreId(),
                    bulkReviewUploadModel);
        } catch (Exception e) {
            log.error(
                    LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkReviewUploadModel),
                    e);
        }
    }

    private void setMandatoryParameters(String storeId, String username) {
        MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
        MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.CLIENT_ID);
        MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
        MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
        MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
    }
}
