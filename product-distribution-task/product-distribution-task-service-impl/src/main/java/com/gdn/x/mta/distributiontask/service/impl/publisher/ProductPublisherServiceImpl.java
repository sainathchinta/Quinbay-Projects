package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductRejectedEventModel;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
public class ProductPublisherServiceImpl implements ProductPublisherService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public ProductForcedRollbackSLAExceedDomainEventModel publishEvent(
      ProductDistributionTask productDistributionTaskExceedSLA) {
    ProductForcedRollbackSLAExceedDomainEventModel productForcedRollbackSLAExceedDomainEventModel =
        new ProductForcedRollbackSLAExceedDomainEventModel(productDistributionTaskExceedSLA.getTaskCode(),
            productDistributionTaskExceedSLA.getId(), productDistributionTaskExceedSLA.getProduct().getProductCode(),
            productDistributionTaskExceedSLA.getProductId());
    kafkaProducer.send(DomainEventName.PRODUCT_FORCE_CLOSE_TASK_EVENT_AND_ROLLBACK_PRODUCT_NAME,
        productDistributionTaskExceedSLA.getProduct().getProductCode(), productForcedRollbackSLAExceedDomainEventModel);
    return productForcedRollbackSLAExceedDomainEventModel;
  }

  @Override
  public ProductRejectedEventModel rejectedProductPublisher(String productCode) {
    ProductRejectedEventModel productRejectedEventModel =
        new ProductRejectedEventModel(productCode, new Date());
    kafkaProducer.send(DomainEventName.PRODUCT_REJECTED_TASK_EVENT_NAME, productCode,
        productRejectedEventModel);
    return productRejectedEventModel;
  }

}
