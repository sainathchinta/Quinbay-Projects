package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductCombinedUpdateToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Objects;

@Service
@Slf4j
public class PDTProductCombinedUpdateToSolrEventListener {

  @Autowired
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getPdtProductCombinedUpdateToSolrEvent"
      + "()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consumed event : {}, message : {} ",
        kafkaTopicPropertiesConsumer.getPdtProductCombinedUpdateToSolrEvent(), message);
    try {
      PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
          objectMapper.readValue(message, PDTProductCombinedUpdateToSolrEventModel.class);
      if (Objects.nonNull(pdtProductCombinedUpdateToSolrEventModel.getPdtProductSolrAddDomainEventModel())) {
        addDocumentToSolr(pdtProductCombinedUpdateToSolrEventModel.getPdtProductSolrAddDomainEventModel());
      } else if (Objects.nonNull(
          pdtProductCombinedUpdateToSolrEventModel.getPdtProductUpdateProductToSolrEventModel())) {
        updateDocumentToSolr(pdtProductCombinedUpdateToSolrEventModel.getPdtProductUpdateProductToSolrEventModel());
      } else if (StringUtils.isNotBlank(pdtProductCombinedUpdateToSolrEventModel.getProductCode())) {
        deleteDocumentToSolr(pdtProductCombinedUpdateToSolrEventModel.getProductCode());
      }
    } catch (Exception e) {
      log.error("Error while processing event : {} , message : {} ",
          kafkaTopicPropertiesConsumer.getPdtProductCombinedUpdateToSolrEvent(), message, e);
    }
  }

  private void addDocumentToSolr(PDTProductSolrAddDomainEventModel pdtProductSolrAddDomainEventModel) {
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    BeanUtils.copyProperties(pdtProductSolrAddDomainEventModel, vendorProductSolr, "state",
      "sellerType", "sellerBadge");
    if (Objects.nonNull(pdtProductSolrAddDomainEventModel.getSellerType())) {
      vendorProductSolr.setSellerType(pdtProductSolrAddDomainEventModel.getSellerType().toString());
    }
    if (Objects.nonNull(pdtProductSolrAddDomainEventModel.getSellerBadge())){
      vendorProductSolr.setSellerBadge(pdtProductSolrAddDomainEventModel.getSellerBadge());
    }
    vendorProductSolr.setState(StringUtils.isNotEmpty(pdtProductSolrAddDomainEventModel.getState()) ?
        WorkflowState.valueOf(pdtProductSolrAddDomainEventModel.getState()) :
        null);
    this.solrVendorProductCollectionRepository.addDocumentToSolrWithCategoryHierarchy(Arrays.asList(vendorProductSolr));
  }

  private void updateDocumentToSolr(PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel)
      throws Exception {
    solrVendorCollectionService.updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel);
  }

  private void deleteDocumentToSolr(String productCode) throws Exception {
    solrVendorProductCollectionRepository.deleteDocumentFromSolr(Arrays.asList(productCode), false);
  }
}
