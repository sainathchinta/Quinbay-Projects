package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.QRCodeExcelQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.NotificationService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class QRCodeGenerationExcelListener {

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getGenerateQrCodeExcel()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Qr Code excel upload event consumed : {}, with message : {} ",
        kafkaTopicProperties.getGenerateQrCodeExcel(), message);
    QRCodeExcelQueue qrCodeExcelQueue = objectMapper.readValue(message, QRCodeExcelQueue.class);
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(qrCodeExcelQueue.getBulkProcessCode()),
          "Bulk Process code is empty for kafka payload: {} " + qrCodeExcelQueue);
      setMandatoryParameters(qrCodeExcelQueue.getStoreId(), qrCodeExcelQueue.getDownloadQRCodeRequest().getRequestId());
      bulkProcessService.insertQrExcelRequest(qrCodeExcelQueue);
    } catch (Exception e) {
      log.error("Error while consuming event payload : {}, error - ", message, e);
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
