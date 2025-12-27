package com.gdn.x.productcategorybase.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrDeleteBrandSubscriber {

  @Autowired
  private SolrBrandRepository solrBrandRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.SOLR_DELETE_BRAND_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume event {} with message : {}", DomainEventName.SOLR_DELETE_BRAND_EVENT, message);
    try {
       SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel =
          objectMapper.readValue(message, SolrDeleteBrandDomainEventModel.class);
      this.solrBrandRepository.deleteBrandsFromBrandCollectionSolr(solrDeleteBrandDomainEventModel.getIds());
    } catch (Exception ex) {
      log.error("error while listening '{}', error is : ", DomainEventName.SOLR_DELETE_BRAND_EVENT, ex);
    }
  }
}
