package com.gdn.mta.bulk.listener.kafka;

import static com.gdn.mta.bulk.service.util.ConverterUtil.populateBulkProcess;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class DownloadQRCodeListenerForProduct {
  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String QR_GENERATION_TYPE_STORE = "STORE";

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getGenerateQrCodeProduct()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getGenerateQrCodeProduct(),
        message);
    DownloadQRCodeRequest downloadQRCodeRequest =
        objectMapper.readValue(message, DownloadQRCodeRequest.class);
    LoggerAttributeModel loggerModel = null;
    BulkProcess bulkProcess = new BulkProcess();
    try {
      setMandatoryParameters(downloadQRCodeRequest.getStoreId(), downloadQRCodeRequest.getRequestId());
      loggerModel =
          new LoggerAttributeModel(this, "onDomainEventConsumed", null, null, null, null, null,
              LoggerChannel.KAFKA.getValue(), null, String.valueOf(downloadQRCodeRequest));
      log.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      bulkProcessService.saveBulkProcess(populateBulkProcess(downloadQRCodeRequest));
    } catch (Exception e) {
      bulkProcessService.setErrorCountAndTotalCountAndSave(bulkProcess, 1, 1);
      log.error("Error while generating QR for product with payload: {}, error- ", downloadQRCodeRequest, e);
    }
  }

  private void setMandatoryParameters(String storeId, String requestId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }
}
