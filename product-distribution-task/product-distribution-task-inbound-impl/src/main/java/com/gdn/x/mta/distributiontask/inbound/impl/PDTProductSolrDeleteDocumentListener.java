package com.gdn.x.mta.distributiontask.inbound.impl;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class PDTProductSolrDeleteDocumentListener {

  @Autowired
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PDT_PRODUCT_SOLR_DELETE_DOCUMENT_BATCH_EVENT_NAME,
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel =
        objectMapper.readValue(message, PDTProductSolrDeleteDomainEventModel.class);
    if (CollectionUtils.isNotEmpty(pdtProductSolrDeleteDomainEventModel.getProductCodes())) {
      solrVendorProductCollectionRepository
          .deleteDocumentFromSolr(pdtProductSolrDeleteDomainEventModel.getProductCodes(), false);
    }
  }
}
