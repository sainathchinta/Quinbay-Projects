package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PDTProductSolrAddDocumentListener{

  @Autowired
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PDT_PRODUCT_SOLR_ADD_DOCUMENT_BATCH_EVENT_NAME,
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTProductSolrBatchAddDomainEventModel pdtProductSolrBatchAddDomainEventModel = objectMapper.readValue(message, PDTProductSolrBatchAddDomainEventModel.class);
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    for (PDTProductSolrAddDomainEventModel pdtProductSolrAddDomainEventModel :
        pdtProductSolrBatchAddDomainEventModel.getPdtProductSolrAddDomainEventModelList()) {
      VendorProductSolr vendorProductSolr = new VendorProductSolr();
      BeanUtils.copyProperties(pdtProductSolrAddDomainEventModel, vendorProductSolr, "state");
      vendorProductSolr.setState(
        StringUtils.isNotEmpty(pdtProductSolrAddDomainEventModel.getState()) ?
          WorkflowState.valueOf(pdtProductSolrAddDomainEventModel.getState()) :
          null);
      vendorProductSolrList.add(vendorProductSolr);
    }
    this.solrVendorProductCollectionRepository
        .addDocumentToSolrWithCategoryHierarchy(vendorProductSolrList);
  }
}
