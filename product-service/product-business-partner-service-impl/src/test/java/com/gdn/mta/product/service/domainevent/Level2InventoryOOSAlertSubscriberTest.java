package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.service.ProductStockAlertService;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.service.eventstore.EventStoreKafkaMessageProcessor;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.x.inventory.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;

public class Level2InventoryOOSAlertSubscriberTest {

  @InjectMocks
  private Level2InventoryOOSAlertSubscriber level2InventoryOOSAlertSubscriber;

  @Mock
  private ProductStockAlertService productStockAlertService;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private EventStoreKafkaMessageProcessor<Level2InventoryOosEvent> eventStoreKafkaMessageProcessor;

  private Level2InventoryOosEvent messageEvent;
  private PbpStockAlert pbpStockAlert;
  private ProductLevel3Aggregator productLevel3Aggregator;
  private Map<String, String> itemNames;
  private ObjectMapper mapper;

  private static final String GDN_SKU = "GDN_SKU";
  private static final String BP_CODE = "BP_CODE";
  private static final String PRD_NAME = "PROD_NAME";
  private static final String ID = "1";
  private static final String STORE_ID = "10001";
  private static final String UNIQUE_ID = "UNIQUE_ID";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.messageEvent =
      new Level2InventoryOosEvent(STORE_ID, GDN_SKU, BP_CODE, PICKUP_POINT_CODE, GDN_SKU,
        Boolean.FALSE);

    this.pbpStockAlert = new PbpStockAlert();
    this.pbpStockAlert.setId(ID);

    this.productLevel3Aggregator = new ProductLevel3Aggregator();
    this.productLevel3Aggregator.setId(ID);

    this.itemNames = new HashMap<>();
    itemNames.put(this.messageEvent.getLevel2Id(), PRD_NAME);
    mapper = new ObjectMapper();

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productStockAlertService);
    Mockito.verifyNoMoreInteractions(this.productLevel3Repository);
    Mockito.verifyNoMoreInteractions(this.eventStoreKafkaMessageProcessor);
    Mockito.verifyNoMoreInteractions(this.productLevel3AggregatorService, objectMapper);
  }

  @Test
  public void testOnDomainEventConsumed_PP_NullTest() throws Exception {
    Mockito
        .when(this.productLevel3Repository
            .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id())))
        .thenReturn(itemNames);
    messageEvent.setPickupPointCode(null);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryOosEvent.class)).thenReturn(this.messageEvent);
    this.level2InventoryOOSAlertSubscriber.onDomainEventConsumed(message);

    Mockito.verify(this.productLevel3Repository)
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id()));
    Mockito.verify(this.productStockAlertService).updateOOS(this.messageEvent, PRD_NAME, 0);
    Mockito.verify(this.productLevel3AggregatorService).updateOOS(this.messageEvent, 0);
    Mockito.verify(this.eventStoreKafkaMessageProcessor).process(this.messageEvent,
        DomainEventName.STOCK_OOS_EVENT_NAME);
    verify(objectMapper).readValue(message, Level2InventoryOosEvent.class);
  }
  @Test
  public void testOnDomainEventConsumed_PP_nonNullTest() throws Exception {
    Mockito
      .when(this.productLevel3Repository
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id())))
      .thenReturn(itemNames);
    messageEvent.setPickupPointCode(PICKUP_POINT_CODE);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryOosEvent.class)).thenReturn(this.messageEvent);
    this.level2InventoryOOSAlertSubscriber.onDomainEventConsumed(message);
    Mockito.verify(this.productLevel3Repository)
      .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id()));
    Mockito.verify(this.productStockAlertService).updateOOS(this.messageEvent, PRD_NAME, 0);
    Mockito.verify(this.eventStoreKafkaMessageProcessor).process(this.messageEvent,
      DomainEventName.STOCK_OOS_EVENT_NAME);
    verify(objectMapper).readValue(message, Level2InventoryOosEvent.class);
  }
  @Test
  public void testOnDomainEventConsumed_exception() throws Exception {
    Mockito
        .when(this.productLevel3Repository
            .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id())))
        .thenThrow(new RuntimeException());

    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryOosEvent.class)).thenReturn(this.messageEvent);
    this.level2InventoryOOSAlertSubscriber.onDomainEventConsumed(message);

    Mockito.verify(this.productLevel3Repository)
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id()));
    verify(objectMapper).readValue(message, Level2InventoryOosEvent.class);
  }

  @Test
  public void testOnDomainEventConsumed_emptyGDNSKU() throws Exception {
    this.messageEvent.setLevel2Id(null);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryOosEvent.class)).thenReturn(this.messageEvent);
    this.level2InventoryOOSAlertSubscriber.onDomainEventConsumed(message);
    Mockito.verify(this.productLevel3Repository, Mockito.times(0))
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id()));
    verify(objectMapper).readValue(message, Level2InventoryOosEvent.class);
  }
  
  @Test
  public void testOnDomainEventConsumed_productNotFound() throws Exception {
    Mockito
        .when(this.productLevel3Repository
            .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id())))
        .thenReturn(null);

    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryOosEvent.class)).thenReturn(this.messageEvent);
    this.level2InventoryOOSAlertSubscriber.onDomainEventConsumed(message);

    Mockito.verify(this.productLevel3Repository)
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getLevel2Id()));
    verify(objectMapper).readValue(message, Level2InventoryOosEvent.class);
  }

  @Test
  public void testOnDomainEventConsumed_offlineItem() throws Exception {
    this.messageEvent.setCncActivated(Boolean.TRUE);
    this.messageEvent.setUniqueId(UNIQUE_ID);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryOosEvent.class)).thenReturn(this.messageEvent);
    this.level2InventoryOOSAlertSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, Level2InventoryOosEvent.class);
  }
}
