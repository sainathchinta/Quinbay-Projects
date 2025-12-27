package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.FbbL5ItemEventModel;
import com.gdn.mta.bulk.service.FbbConsignmentService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.doThrow;

public class FbbCreateL5ListenerTest {

  private static final String MESSAGE = "MESSAGE";
  private static final String ITEM_SKU = "item-sku";
  private static final String PROCESS_CODE = "process-code";
  private static final String BUSINESS_PARTNER_CODE = "bp-code";

  private FbbL5ItemEventModel fbbL5ItemEventModel;

  @InjectMocks
  private FbbCreateL5Listener fbbCreateL5Listener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FbbConsignmentService fbbConsignmentService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    fbbL5ItemEventModel = new FbbL5ItemEventModel();
    fbbL5ItemEventModel.setItemSku(ITEM_SKU);
    fbbL5ItemEventModel.setInternalProcessCode(PROCESS_CODE);
    fbbL5ItemEventModel.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    fbbL5ItemEventModel.setBuyable(true);
    fbbL5ItemEventModel.setDiscoverable(true);
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(fbbConsignmentService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, FbbL5ItemEventModel.class))
      .thenReturn(fbbL5ItemEventModel);
    fbbCreateL5Listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, FbbL5ItemEventModel.class);
    Mockito.verify(fbbConsignmentService)
      .processL5CreationEvent(Mockito.anyString(), Mockito.any(FbbL5CreateDTO.class));
    Mockito.verify(kafkaTopicProperties).getFbbCreateL5();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, FbbL5ItemEventModel.class))
      .thenReturn(fbbL5ItemEventModel);
    doThrow(ApplicationRuntimeException.class).when(fbbConsignmentService)
      .processL5CreationEvent(Mockito.anyString(), Mockito.any(FbbL5CreateDTO.class));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> fbbCreateL5Listener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, FbbL5ItemEventModel.class);
      Mockito.verify(fbbConsignmentService)
        .processL5CreationEvent(Mockito.anyString(), Mockito.any(FbbL5CreateDTO.class));
      Mockito.verify(kafkaTopicProperties).getFbbCreateL5();
    }
  }
}
