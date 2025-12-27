package com.gdn.partners.pbp.service.listener;

import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrDeleteReviewProductCollectionKafkaSubscriberBean {

  @Autowired
  @Qualifier(value = "reviewProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        objectMapper.readValue(message, SolrReviewProductCollectionDeleteEvent.class);
    log.info(
        "[SolrDeleteReviewProductCollectionKafkaSubscriberBean] " + "Retrieved message from Topic: {}, Message: {}",
        DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionDeleteEvent);
    cloudSolrClient.deleteById(solrReviewProductCollectionDeleteEvent.getIds());
  }
}
