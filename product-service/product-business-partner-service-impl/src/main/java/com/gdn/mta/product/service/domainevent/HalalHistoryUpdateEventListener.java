package com.gdn.mta.product.service.domainevent;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.HalalHistoryUpdateService;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import com.newrelic.api.agent.Trace;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class HalalHistoryUpdateEventListener {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private HalalHistoryUpdateService halalHistoryUpdateService;

    @Trace(dispatcher = true)
    @KafkaListener(topics = ProductDomainEventName.HALAL_HISTORY_UPDATE, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
    public void onDomainEventConsumed(String message) throws Exception {
        log.info("Message received, from topic {} , message : {} ", ProductDomainEventName.HALAL_HISTORY_UPDATE,
            message);
        try {
            HalalHistoryUpdateEventModel halalHistoryUpdateEventModel =
                objectMapper.readValue(message, HalalHistoryUpdateEventModel.class);
            setMandatoryParameters(halalHistoryUpdateEventModel.getStoreId(),
                halalHistoryUpdateEventModel.getUserName());
            halalHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
        } catch (Exception e) {
            log.error("Error while listening {} , error - ", ProductDomainEventName.HALAL_HISTORY_UPDATE, e);
        }
    }

    private void setMandatoryParameters(String storeId, String userName) {
        MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
        MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
    }
}
