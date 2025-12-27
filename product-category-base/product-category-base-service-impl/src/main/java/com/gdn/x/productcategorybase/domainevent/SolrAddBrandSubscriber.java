package com.gdn.x.productcategorybase.domainevent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;


@Service
public class SolrAddBrandSubscriber {

  @Autowired
  private SolrBrandRepository solrBrandRepository;

  @Autowired
  private ObjectMapper objectMapper;

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrAddBrandSubscriber.class);

  @KafkaListener(topics = DomainEventName.SOLR_ADD_BRAND_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    LOGGER.info("[SolrAddBrandSubscriber], Retrieved message from Topic: {}, Message: {}",
        DomainEventName.SOLR_ADD_BRAND_EVENT, message);
    try {
      SolrAddBrandListDomainEventModel solrAddBrandListDomainEventModel =
          objectMapper.readValue(message, SolrAddBrandListDomainEventModel.class);
      this.solrBrandRepository.addBrandsToBrandCollectionSolr(solrAddBrandListDomainEventModel.getSolrBrandModels());
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName.SOLR_ADD_BRAND_EVENT, ex);
    }
  }

}
