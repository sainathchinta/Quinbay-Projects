package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;

public class OfflineItemChangeListenerTest {

  private static final String MESSAGE = "message";

  private OfflineItemChange offlineItemChange = new OfflineItemChange();

  @Mock
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private OfflineItemChangeListener offlineItemChangeListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(itemPickupPointWrapperService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, OfflineItemChange.class))
      .thenReturn(offlineItemChange);
    this.offlineItemChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, OfflineItemChange.class);
    Mockito.verify(this.itemPickupPointWrapperService)
      .updateItemPickupPointOnOfflineItemChange(offlineItemChange);
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, OfflineItemChange.class))
      .thenThrow(RuntimeException.class);
    this.offlineItemChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, OfflineItemChange.class);
  }
}