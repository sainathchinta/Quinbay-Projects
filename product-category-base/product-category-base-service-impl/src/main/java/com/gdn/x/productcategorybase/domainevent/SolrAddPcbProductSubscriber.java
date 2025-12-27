package com.gdn.x.productcategorybase.domainevent;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrPcbProductModel;
import com.gdn.x.productcategorybase.repository.SolrPcbRepository;

@Service
public class SolrAddPcbProductSubscriber {

  @Autowired
  private SolrPcbRepository solrPcbRepository;

  @Autowired
  private ObjectMapper objectMapper;

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrAddPcbProductSubscriber.class);

  @KafkaListener(topics = DomainEventName.SOLR_ADD_BATCH_PCB_PRODUCT_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    LOGGER.info("Received domain Event {} , message {} ", DomainEventName.SOLR_ADD_BATCH_PCB_PRODUCT_EVENT, message);
    try {
      SolrAddBatchPcbProductDomainEventModel solrAddBatchPcbProductDomainEventModel =
          objectMapper.readValue(message, SolrAddBatchPcbProductDomainEventModel.class);
      List<SolrPcbProductModel> solrProductModelList = new ArrayList<>();
      for (SolrAddPcbProductDomainEventModel solrAddPcbProductDomainEventModel : solrAddBatchPcbProductDomainEventModel
          .getProductDomainEventModelList()) {
        SolrPcbProductModel solrPcbProductModel =
            SolrPcbProductModel.builder().id(solrAddPcbProductDomainEventModel.getId())
                .name(solrAddPcbProductDomainEventModel.getName())
                .categoryId(solrAddPcbProductDomainEventModel.getCategoryId())
                .productCode(solrAddPcbProductDomainEventModel.getProductCode())
                .parentCategoryId(solrAddPcbProductDomainEventModel.getParentCategoryId())
                .upcCodes(solrAddPcbProductDomainEventModel.getUpcCodes())
                .skuCodes(solrAddPcbProductDomainEventModel.getSkuCodes())
                .generatedItemNames(solrAddPcbProductDomainEventModel.getGeneratedItemNames())
                .dangerousGoodsLevels(solrAddPcbProductDomainEventModel.getDangerousGoodsLevels())
                .imageLocationPaths(solrAddPcbProductDomainEventModel.getImageLocationPaths()).build();
        solrProductModelList.add(solrPcbProductModel);
      }
      solrPcbRepository.addProductListToPcbCollection(solrProductModelList);
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName.SOLR_ADD_BATCH_PCB_PRODUCT_EVENT, ex);
    }
  }
}
