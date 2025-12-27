package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.service.BulkProcessDataService;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import static com.gdn.mta.bulk.service.util.ConverterUtil.populateBulkProcess;

@Service
@Slf4j
public class DownloadQRCodeListenerForStore {
  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String QR_GENERATION_TYPE_STORE = "STORE";

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getGenerateQrCodeStore()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getGenerateQrCodeStore(), message);
    DownloadQRCodeRequest downloadQRCodeRequest =
        objectMapper.readValue(message, DownloadQRCodeRequest.class);
    BulkProcess bulkProcess = new BulkProcess();
    try {
      setMandatoryParameters(downloadQRCodeRequest.getStoreId(), downloadQRCodeRequest.getRequestId());
      bulkProcess = bulkProcessService.saveBulkProcess(populateBulkProcess(downloadQRCodeRequest));
      bulkProcessDataService.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcess);
    } catch (Exception e) {
      bulkProcessService.setErrorCountAndTotalCountAndSave(bulkProcess, 1, 1);
      log.error("Error while generating QR code for store with payload : {}, error- ", downloadQRCodeRequest, e);
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
