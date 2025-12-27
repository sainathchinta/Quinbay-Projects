package com.gdn.mta.bulk.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.AnalyticPublisherDTO;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;

@Service
public class TrackerServiceImpl implements TrackerService {

  private static final Logger LOGGER = LoggerFactory.getLogger(TrackerServiceImpl.class);

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;
  
  @Override
  public void sendTracker(String event, String attrtype, String attrname, String attrvalue,
      String customfield) {
    AnalyticPublisherDTO msg = new AnalyticPublisherDTO();
    msg.setEvent(event);
    msg.setAttrtype(attrtype);
    msg.setAttrname(attrname);
    msg.setAttrvalue(attrvalue);
    msg.setCustomfield(customfield);
    msg.setTimestamp(System.currentTimeMillis() / 1000l);

    LOGGER.info("publishing topic for tracking event : {} and attribute type : {}", event, attrtype);
    kafkaProducer.send(kafkaTopicProperties.getAnalyticBwaEventPublish(), msg);
  }

  @Override
  public void trackProductCreationFailure(String requestId, ProductCreationRequest productRequest, String errorMessage) {
    try {
      ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel =
          RequestHelper.toProductCreationFailureDomainEventModel(productRequest);
      productCreationFailureDomainEventModel.setRequestId(requestId);
      productCreationFailureDomainEventModel.setErrorMessage(errorMessage);
      kafkaProducer.send(com.gdn.x.productcategorybase.domain.event.config.DomainEventName.PRODUCT_CREATION_FAILURE,
          productCreationFailureDomainEventModel);
    } catch (Exception e) {
      LOGGER.error("Exception while publishing product creation failure event : {}", e.getMessage(), e);
    }
  }

}
