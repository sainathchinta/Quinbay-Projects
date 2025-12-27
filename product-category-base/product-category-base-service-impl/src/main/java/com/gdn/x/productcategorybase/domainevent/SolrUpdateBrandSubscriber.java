package com.gdn.x.productcategorybase.domainevent;

import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrUpdateBrandSubscriber {

  @Autowired
  private SolrBrandRepository brandRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.SOLR_UPDATE_BRAND_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume event {} with message : {}", DomainEventName.SOLR_UPDATE_BRAND_EVENT, message);
    try {
      SolrUpdateBrandDomainEventModel solrUpdateBrandDomainEventModel =
          objectMapper.readValue(message, SolrUpdateBrandDomainEventModel.class);
      this.brandRepository.updateBrandApprovalStatusListInSolr(
          solrUpdateBrandDomainEventModel.getUpdateBrandModels().stream()
              .map(this::generateSolrBrandModelBySolrUpdateBrandDomainEventModel).collect(Collectors.toList()));
    } catch (Exception ex) {
      log.error("error while listening '{}', error is : ", DomainEventName.SOLR_UPDATE_BRAND_EVENT, ex);
    }
  }

  private SolrBrandModel generateSolrBrandModelBySolrUpdateBrandDomainEventModel(
      SolrUpdateBrandModel solrUpdateBrandModel) {
    return SolrBrandModel.builder().id(solrUpdateBrandModel.getId())
        .brandApproved(solrUpdateBrandModel.isBrandApproved())
        .businessPartnerCode(solrUpdateBrandModel.getBusinessPartnerCode())
        .brandValue(solrUpdateBrandModel.getBrandName())
        .brandNameUpdated(solrUpdateBrandModel.getBrandNameUpdated())
        .brandCode(solrUpdateBrandModel.getBrandCode()).build();
  }
}
