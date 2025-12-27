package com.gdn.x.productcategorybase.domainevent;

import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.repository.SolrPcbRepository;

@Service
public class SolrDeletePcbProductSubscriber {

  @Autowired
  private SolrPcbRepository solrPcbRepository;

  @Autowired
  private ObjectMapper objectMapper;

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrDeletePcbProductSubscriber.class);

  @KafkaListener(topics = DomainEventName.SOLR_DELETE_BATCH_PCB_PRODUCT_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    LOGGER.info("[SolrDeletePcbProductSubscriber], Retrieved message from Topic: {}, Message: {}",
        DomainEventName.SOLR_DELETE_BATCH_PCB_PRODUCT_EVENT, message);
    try {
      SolrDeleteBatchPcbProductDomainEventModel solrDeleteBatchPcbProductDomainEventModel =
          objectMapper.readValue(message, SolrDeleteBatchPcbProductDomainEventModel.class);
      List<String> solrDeleteProductIds =
          solrDeleteBatchPcbProductDomainEventModel.getSolrDeletePcbProductDomainEventModels().stream()
              .map(solrDeletePcbProductDomainEventModel -> solrDeletePcbProductDomainEventModel.getId())
              .collect(Collectors.toList());
      solrPcbRepository.deleteProductListFromPcbCollection(solrDeleteProductIds);
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName.SOLR_DELETE_BATCH_PCB_PRODUCT_EVENT, ex);
    }
  }

}
