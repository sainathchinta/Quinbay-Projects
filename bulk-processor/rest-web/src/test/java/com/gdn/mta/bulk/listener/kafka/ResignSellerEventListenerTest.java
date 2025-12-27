package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.mta.domain.event.modal.ResignSellerDomainEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class ResignSellerEventListenerTest {

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String STORE_ID = "storeId";
  private ObjectMapper mapper = new ObjectMapper();

  private ResignSellerDomainEvent resignSellerDomainEvent;

  @Mock
  private DormantSellerService dormantSellerService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ResignSellerEventListener resignSellerEventListener;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    resignSellerDomainEvent =
      ResignSellerDomainEvent.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).storeId(STORE_ID)
        .build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(dormantSellerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(resignSellerDomainEvent), ResignSellerDomainEvent.class))
        .thenReturn(resignSellerDomainEvent);
    resignSellerEventListener.onDomainEventConsumed(mapper.writeValueAsString(resignSellerDomainEvent));
    Mockito.verify(this.dormantSellerService).processResignSellerEvent(STORE_ID,
      BUSINESS_PARTNER_CODE);
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(resignSellerDomainEvent), ResignSellerDomainEvent.class);
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(resignSellerDomainEvent), ResignSellerDomainEvent.class))
        .thenReturn(resignSellerDomainEvent);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.dormantSellerService)
      .processResignSellerEvent(STORE_ID, BUSINESS_PARTNER_CODE);
    resignSellerEventListener.onDomainEventConsumed(mapper.writeValueAsString(resignSellerDomainEvent));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(resignSellerDomainEvent), ResignSellerDomainEvent.class);
    Mockito.verify(this.dormantSellerService)
      .processResignSellerEvent(STORE_ID, BUSINESS_PARTNER_CODE);
  }
}