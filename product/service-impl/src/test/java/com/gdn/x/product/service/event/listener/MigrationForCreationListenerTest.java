package com.gdn.x.product.service.event.listener;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.MigrationForItemCreationEvent;
import com.gdn.x.product.enums.ItemMigrationStatus;
import com.gdn.x.product.service.api.ItemPickupPointMigrationService;

public class MigrationForCreationListenerTest {

  private static final String MESSAGE = "message";
  private static final String ITEM_SKU = "itemSku";

  private List<String> itemSkuList = Collections.singletonList(ITEM_SKU);

  @Mock
  private ItemPickupPointMigrationService itemPickupPointMigrationService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private MigrationForCreationListener migrationForCreationListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(itemPickupPointMigrationService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, MigrationForItemCreationEvent.class)).thenReturn(
      MigrationForItemCreationEvent.builder().itemSkuList(itemSkuList).build());
    migrationForCreationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, MigrationForItemCreationEvent.class);
    Mockito.verify(this.itemPickupPointMigrationService).insertItemSkuByState(itemSkuList,
      ItemMigrationStatus.PENDING.name());
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, MigrationForItemCreationEvent.class)).thenThrow(
      RuntimeException.class);
    migrationForCreationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, MigrationForItemCreationEvent.class);
  }
}