package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verifyNoMoreInteractions;

public class ItemChangeListenerTest {

  private static final String MESSAGE = "message";

  private ItemChange itemChange = new ItemChange();

  @Mock
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ItemChangeListener itemChangeListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemChange.class)).thenReturn(itemChange);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.objectMapper);
    verifyNoMoreInteractions(this.itemPickupPointWrapperService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    itemChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemChange.class);
    Mockito.verify(this.itemPickupPointWrapperService)
      .updateItemPickupPointOnItemChange(itemChange);
  }

  @Test
  public void onDomainEventConsumed_nullPayloadTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemChange.class)).thenReturn(null);
    itemChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemChange.class);
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.itemPickupPointWrapperService)
      .updateItemPickupPointOnItemChange(itemChange);
    itemChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemChange.class);
    Mockito.verify(this.itemPickupPointWrapperService)
      .updateItemPickupPointOnItemChange(itemChange);
  }
}