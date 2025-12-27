package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailDomainEvent;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProductEmailEventPublisherImplTest {

  private static final String EVENT = "EVENT";
  private static final String BP_CODE = "BP_CODE";
  @InjectMocks
  private ProductEmailEventPublisherImpl productEmailEventPublisher;
  @Mock
  private KafkaPublisher kafkaPublisher;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  private ProductEmailDomainEvent productEmailDomainEvent;

  @BeforeEach
  public void setUp() {
    productEmailDomainEvent = new ProductEmailDomainEvent();
    productEmailDomainEvent.setBusinessPartnerCode(BP_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaPublisher);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  void publishProductMailDomainEventForIprEvidenceRequestedProductTest() {
    Mockito.when(kafkaTopicProperties.getProductEmailEvent()).thenReturn(EVENT);
    productEmailEventPublisher.publishProductMailDomainEventForIprEvidenceRequestedProduct(
      productEmailDomainEvent);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductEmailEvent();
    Mockito.verify(kafkaPublisher).send(EVENT, BP_CODE, productEmailDomainEvent);
  }
}
