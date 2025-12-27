package com.gdn.mta.product.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class TrackerServiceImpl implements TrackerService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public void trackProductCreationFailure(
      String requestId, String flowType, ProductCreationRequest productRequest, String errorMessage) {
    try {
      ProductCreationFailureDomainEventModel productCreationFailureDomainEventMode =
          ConverterUtil.toProductCreationFailureDomainEventModel(productRequest);
      productCreationFailureDomainEventMode.setRequestId(requestId);
      productCreationFailureDomainEventMode.setErrorMessage(errorMessage);
      productCreationFailureDomainEventMode.setFlowType(flowType);
      kafkaProducer.send(DomainEventName.PRODUCT_CREATION_FAILURE,
          productCreationFailureDomainEventMode);
    } catch (Exception e) {
      log.error("Exception while publishing product creation failure event : {}", e.getMessage(), e);
    }
  }

}
