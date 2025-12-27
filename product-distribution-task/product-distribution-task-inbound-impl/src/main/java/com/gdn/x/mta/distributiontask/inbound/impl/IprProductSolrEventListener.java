package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.IprProductSolrCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.AddProductToIprSolrEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@RequiredArgsConstructor
public class IprProductSolrEventListener {

  private final ObjectMapper objectMapper;

  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private final IprProductSolrCollectionRepository iprProductSolrCollectionRepository;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()}",
                 autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consumed event : {}, message : {} ",
        kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(), message);
    try {
      AddProductToIprSolrEventModel addProductToIprSolrEventModel =
          objectMapper.readValue(message, AddProductToIprSolrEventModel.class);
      if (Objects.nonNull(addProductToIprSolrEventModel.getIprProductSolr())) {
        iprProductSolrCollectionRepository.addDocumentToIPRSolr(
            addProductToIprSolrEventModel.getIprProductSolr());
      } else {
        if (addProductToIprSolrEventModel.isDeleteSolrDocument()) {
          iprProductSolrCollectionRepository.deleteIprSolrDocument(
              addProductToIprSolrEventModel.getProductSku());
        }
      }
    } catch (Exception e) {
      log.error("Error while processing event : {} , message : {} ",
          kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(), message, e);
    }
  }
}