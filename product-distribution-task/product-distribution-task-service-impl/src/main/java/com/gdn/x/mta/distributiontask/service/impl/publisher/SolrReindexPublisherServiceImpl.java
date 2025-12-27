package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductCombinedUpdateToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class SolrReindexPublisherServiceImpl implements SolrReindexPublisherService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public PDTProductSolrBatchAddDomainEventModel publishPDTProductSolrBatchAddDomainEventModelForReindex(
      PDTProductSolrBatchAddDomainEventModel pdtProductSolrBatchAddDomainEventModel) {
    if (CollectionUtils.isNotEmpty(pdtProductSolrBatchAddDomainEventModel.getPdtProductSolrAddDomainEventModelList())) {
      for (PDTProductSolrAddDomainEventModel pdtProductSolrAddDomainEventModel : pdtProductSolrBatchAddDomainEventModel.getPdtProductSolrAddDomainEventModelList()) {
        kafkaProducer.send(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(),
            pdtProductSolrAddDomainEventModel.getProductCode(), PDTProductCombinedUpdateToSolrEventModel.builder()
                .pdtProductSolrAddDomainEventModel(pdtProductSolrAddDomainEventModel).build());
        log.info("Publishing event : {}, message : {} ", kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(),
            pdtProductSolrAddDomainEventModel);
      }
    }
    return pdtProductSolrBatchAddDomainEventModel;
  }

  @Override
  public PDTProductUpdateProductToSolrEventModel publishPDTProductApprovalToSolr(
      PDTProductUpdateProductToSolrEventModel productAddProductToSolrEventModel) {
    kafkaProducer.send(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(),
        productAddProductToSolrEventModel.getProductCode(), PDTProductCombinedUpdateToSolrEventModel.builder()
            .pdtProductUpdateProductToSolrEventModel(productAddProductToSolrEventModel).build());
    log.info("event = {} published event model = {}", kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(),
        productAddProductToSolrEventModel);
    return productAddProductToSolrEventModel;
  }

  @Override
  public PDTProductSolrDeleteDomainEventModel publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
      PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel) {
    if (CollectionUtils.isNotEmpty(pdtProductSolrDeleteDomainEventModel.getProductCodes())) {
      for (String productCode : pdtProductSolrDeleteDomainEventModel.getProductCodes()) {
        kafkaProducer.send(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(), productCode,
            PDTProductCombinedUpdateToSolrEventModel.builder().productCode(productCode).build());
        log.info("Publishing the event : {} , pdtProductSolrDeleteDomainEventModel : {} ",
            kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(), productCode);
      }
    }
    return pdtProductSolrDeleteDomainEventModel;
  }
}
