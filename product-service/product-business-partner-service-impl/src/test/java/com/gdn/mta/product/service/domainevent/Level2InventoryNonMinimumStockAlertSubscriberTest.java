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
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;

public class Level2InventoryNonMinimumStockAlertSubscriberTest {

  @InjectMocks
  private Level2InventoryNonMinimumStockAlertSubscriber level2InventoryNonMinimumStockAlertSubscriber;

  @Mock
  private ProductStockAlertService productStockAlertService;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Mock
  private ObjectMapper objectMapper;

  private Level2InventoryMinimumStockAlertEvent messageEvent;
  private PbpStockAlert pbpStockAlert;
  private ProductLevel3Aggregator productLevel3Aggregator;
  private Map<String, String> itemNames;
  private ObjectMapper mapper;

  private static final String GDN_SKU = "GDN_SKU";
  private static final String BP_CODE = "BP_CODE";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final int MIN_STOCK = 1;
  private static final int AVL_STICK = 1;
  private static final String PRD_NAME = "PROD_NAME";
  private static final String ID = "1";
  private static final String STORE_ID = "10001";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.messageEvent =
        new Level2InventoryMinimumStockAlertEvent(STORE_ID, GDN_SKU, AVL_STICK, MIN_STOCK,
          BP_CODE, PICKUP_POINT_CODE);

    this.pbpStockAlert = new PbpStockAlert();
    this.pbpStockAlert.setId(ID);

    this.productLevel3Aggregator = new ProductLevel3Aggregator();
    this.productLevel3Aggregator.setId(ID);

    this.itemNames = new HashMap<>();
    itemNames.put(this.messageEvent.getGdnSku(), PRD_NAME);
    mapper = new ObjectMapper();

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productStockAlertService);
    Mockito.verifyNoMoreInteractions(this.productLevel3Repository);
    Mockito.verifyNoMoreInteractions(this.productLevel3AggregatorService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void testOnDomainEventConsumed_PPNull() throws Exception {
    Mockito
        .when(this.productLevel3Repository
            .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku())))
        .thenReturn(itemNames);
    messageEvent.setPickupPointCode(null);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryMinimumStockAlertEvent.class))
        .thenReturn(this.messageEvent);
    this.level2InventoryNonMinimumStockAlertSubscriber.onDomainEventConsumed(message);

    Mockito.verify(this.productLevel3Repository)
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku()));
    Mockito.verify(this.productStockAlertService).updateNonMinimumStock(this.messageEvent, PRD_NAME,
        0);
    Mockito.verify(this.productLevel3AggregatorService).updateMinimumStock(this.messageEvent, false, 0);
    verify(objectMapper).readValue(message, Level2InventoryMinimumStockAlertEvent.class);

  }
  @Test
  public void testOnDomainEventConsumed_withPP() throws Exception {
    Mockito
      .when(this.productLevel3Repository
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku())))
      .thenReturn(itemNames);
    messageEvent.setPickupPointCode(PICKUP_POINT_CODE);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryMinimumStockAlertEvent.class))
      .thenReturn(this.messageEvent);
    this.level2InventoryNonMinimumStockAlertSubscriber.onDomainEventConsumed(message);

    Mockito.verify(this.productLevel3Repository)
      .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku()));
    Mockito.verify(this.productStockAlertService).updateNonMinimumStock(this.messageEvent, PRD_NAME,
      0);

    verify(objectMapper).readValue(message, Level2InventoryMinimumStockAlertEvent.class);

  }
  @Test
  public void testOnDomainEventConsumed_exception() throws Exception {
    Mockito
        .when(this.productLevel3Repository
            .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku())))
        .thenThrow(new RuntimeException());

    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryMinimumStockAlertEvent.class))
        .thenReturn(this.messageEvent);
    this.level2InventoryNonMinimumStockAlertSubscriber.onDomainEventConsumed(message);

    Mockito.verify(this.productLevel3Repository)
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku()));
    verify(objectMapper).readValue(message, Level2InventoryMinimumStockAlertEvent.class);
  }

  @Test
  public void testOnDomainEventConsumed_emptyGDNSKU() throws Exception {
    this.messageEvent.setGdnSku(null);
    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryMinimumStockAlertEvent.class))
        .thenReturn(this.messageEvent);
    this.level2InventoryNonMinimumStockAlertSubscriber.onDomainEventConsumed(message);
    Mockito.verify(this.productLevel3Repository, Mockito.times(0))
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku()));
    verify(objectMapper).readValue(message, Level2InventoryMinimumStockAlertEvent.class);
  }

  @Test
  public void testOnDomainEventConsumed_productNotFound() throws Exception {
    Mockito.when(this.productLevel3Repository
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku()))).thenReturn(null);

    String message = mapper.writeValueAsString(this.messageEvent);
    Mockito.when(objectMapper.readValue(message, Level2InventoryMinimumStockAlertEvent.class))
        .thenReturn(this.messageEvent);
    this.level2InventoryNonMinimumStockAlertSubscriber.onDomainEventConsumed(message);
    Mockito.verify(this.productLevel3Repository)
        .getItemNameByItemSku(Arrays.asList(this.messageEvent.getGdnSku()));
    verify(objectMapper).readValue(message, Level2InventoryMinimumStockAlertEvent.class);
  }
}
