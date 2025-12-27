package com.gdn.mta.product.service.domainevent;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ItemStatusDomainEvent;

public class ItemStatusPublisherServiceBeanTest {

  @InjectMocks
  private ItemStatusPublisherServiceBean itemStatusPublisherService;

  @Mock
  private KafkaPublisher kafkaProducer;

  private ItemStatusDomainEvent itemStatusDomainEvent;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    itemStatusDomainEvent = new ItemStatusDomainEvent();
  }

  @Test
  public void publishItemStatusDomainEvent() throws Exception {
    this.itemStatusPublisherService.publishItemStatusDomainEvent(itemStatusDomainEvent);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ITEM_STATUS_EVENT), Mockito.any());
  }

  @AfterEach
  public void finalizeTest() throws Exception {}
}
