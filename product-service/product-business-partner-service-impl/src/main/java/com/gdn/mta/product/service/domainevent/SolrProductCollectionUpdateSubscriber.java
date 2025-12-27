package com.gdn.mta.product.service.domainevent;


import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.newrelic.api.agent.Trace;

@Service
public class SolrProductCollectionUpdateSubscriber {

    @Autowired
    private SolrActiveProductCollectionService solrActiveProductCollectionService;

    @Autowired
    private ObjectMapper objectMapper;

    private static final Logger LOGGER = LoggerFactory.getLogger(SolrProductCollectionUpdateSubscriber.class);

    @Trace(dispatcher=true)
    @KafkaListener(topics = DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
    public void onDomainEventConsumed(String message) throws Exception {
        SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
            objectMapper.readValue(message, SolrProductCollectionUpdateEvent.class);
        try {
            if (Objects.nonNull(solrProductCollectionUpdateEvent)) {
                if (StringUtils.isNotEmpty(solrProductCollectionUpdateEvent.getDocumentId())) {
                    this.solrActiveProductCollectionService.deleteSolrProductCollectionByDocumentId(
                        solrProductCollectionUpdateEvent.getDocumentId());
                } else {
                    this.solrActiveProductCollectionService.addSolrProductCollectionDocument(
                        solrProductCollectionUpdateEvent);
                }
            }
        } catch (Exception exception) {
            LOGGER.error("Failed to update product collection DTO in solr {} ", solrProductCollectionUpdateEvent,
                    exception);
        }
    }
}
