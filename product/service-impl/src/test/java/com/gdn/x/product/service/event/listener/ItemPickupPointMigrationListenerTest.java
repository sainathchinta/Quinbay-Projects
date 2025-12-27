package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

public class ItemPickupPointMigrationListenerTest {

  private static final String MESSAGE = "message";
  private static final String ITEM_SKU = "itemSku";

  private ItemPickupPointMigrationEvent itemPickupPointMigrationEvent =
    ItemPickupPointMigrationEvent.builder().itemSkuList(Collections.singletonList(ITEM_SKU))
      .build();

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @InjectMocks
  private ItemPickupPointMigrationListener itemPickupPointMigrationListener;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(itemPickupPointWrapperService);
  }

  @Test
  public void onDomainEventConsumed_emptyListTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemPickupPointMigrationEvent.class))
      .thenReturn(new ItemPickupPointMigrationEvent());
    this.itemPickupPointMigrationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemPickupPointMigrationEvent.class);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemPickupPointMigrationEvent.class))
      .thenReturn(itemPickupPointMigrationEvent);
    this.itemPickupPointMigrationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemPickupPointMigrationEvent.class);
    Mockito.verify(this.itemPickupPointWrapperService)
      .processItemMigrationEvent(itemPickupPointMigrationEvent);
  }
}