package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductApprovalStatusPublisherService;

/**
 * Created by Akshay Bhatt on 17/04/2018
 */
@Service
public class ProductApprovalStatusPublisherServiceImpl implements ProductApprovalStatusPublisherService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public ProductApprovalDetailStatusEvent updateProductApprovalDetailStatus(String productCode,
      ProductApprovalDetailStatus status) {
    ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent =
        new ProductApprovalDetailStatusEvent(productCode, status, System.currentTimeMillis());
    kafkaProducer
        .send(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER, Constants.PRODUCT_CODE, productApprovalDetailStatusEvent);
    return productApprovalDetailStatusEvent;
  }
}
