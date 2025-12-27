package com.gdn.mta.product.service.domainevent;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.PBPAttributeMigrationEventModel;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;

public class PBPAttributeMigrationEventListenerTest {

  @InjectMocks
  private PBPAttributeMigrationEventListener pbpAttributeMigrationEventListener;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductService productService;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;
  private PBPAttributeMigrationEventModel eventModel;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    mapper = new ObjectMapper();
    eventModel = new PBPAttributeMigrationEventModel();
    eventModel.setProductCode("PROD123");
    eventModel.setAttributeCode("ATTR_CODE");
    eventModel.setAttributeId("ATTR_ID");
    eventModel.setAttributeValue("VALUE");
    eventModel.setSkuValue(true);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaProducer, kafkaTopicProperties, productService);
  }


  @Test
  public void shouldProcessValidEventSuccessfully() throws Exception {
    String message = mapper.writeValueAsString(this.eventModel);
    Mockito.when(objectMapper.readValue(message, PBPAttributeMigrationEventModel.class)).thenReturn(eventModel);
    pbpAttributeMigrationEventListener.onDomainEventConsumed(message);
    Mockito.verify(productService)
        .populateProductBusinessPartnerAttribute("PROD123", "ATTR_CODE", "ATTR_ID", "VALUE", true, null);
    Mockito.verify(productService).updateStatusInPCBForBackFillAttributes(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(kafkaTopicProperties).getPbpAttributeMigrationEvent();
  }

  @Test
  public void exceptionWhenAttributeCodeIsBlank() throws Exception {
    try {
      eventModel.setAttributeCode(StringUtils.EMPTY);
      String message = "{}";
      Mockito.when(objectMapper.readValue(message,PBPAttributeMigrationEventModel.class)).thenReturn(eventModel);
      pbpAttributeMigrationEventListener.onDomainEventConsumed(message);
    } finally {
      Mockito.verify(kafkaTopicProperties,Mockito.times(2)).getPbpAttributeMigrationEvent();
      Mockito.verify(productService)
          .updateStatusInPCBForBackFillAttributes(Mockito.any(), Mockito.any(), Mockito.any());
    }
  }
}
