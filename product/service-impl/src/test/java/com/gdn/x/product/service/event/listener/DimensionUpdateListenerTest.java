package com.gdn.x.product.service.event.listener;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.warehouse.itemmaster.streamingmodel.WarehouseMasterSKUAllEvent;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

public class DimensionUpdateListenerTest {

  @InjectMocks
  private DimensionUpdateListener dimensionUpdateListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String ITEM_SKU = "itemSku";
  private static final String UPC_CODE = "upcCode";
  private static final String MESSAGE = "message";
  private static final String EVENT = "wms.master.data.item.event";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.when(kafkaTopicProperties.getWmsMasterDataItemEvent()).thenReturn(EVENT);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verify(kafkaTopicProperties).getWmsMasterDataItemEvent();
    Mockito.verifyNoMoreInteractions(itemService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void testOnDomainEventConsumed() throws IOException {
    WarehouseMasterSKUAllEvent warehouseMasterSKUEvent = new WarehouseMasterSKUAllEvent();
    warehouseMasterSKUEvent.setItemCode(ITEM_SKU);
    warehouseMasterSKUEvent.setLength(10.0);
    warehouseMasterSKUEvent.setWeight(10.0);
    warehouseMasterSKUEvent.setWidth(10.0);
    warehouseMasterSKUEvent.setHeight(10.0);
    warehouseMasterSKUEvent.setUpcCodes(List.of(UPC_CODE));
    Mockito.doNothing().when(itemService).updateItemDimensionsAndUpcCode(ITEM_SKU, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.when(objectMapper.readValue(MESSAGE, WarehouseMasterSKUAllEvent.class)).thenReturn(warehouseMasterSKUEvent);
    dimensionUpdateListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, WarehouseMasterSKUAllEvent.class);
    Mockito.verify(itemService).updateItemDimensionsAndUpcCode(ITEM_SKU, 10.0, 10.0, 10.0, 10.0, List.of(UPC_CODE));
  }

  @Test
  public void testOnDomainEventConsumedException() throws IOException {
    WarehouseMasterSKUAllEvent warehouseMasterSKUEvent = new WarehouseMasterSKUAllEvent();
    warehouseMasterSKUEvent.setItemCode(ITEM_SKU);
    warehouseMasterSKUEvent.setLength(10.0);
    warehouseMasterSKUEvent.setWeight(10.0);
    warehouseMasterSKUEvent.setWidth(10.0);
    warehouseMasterSKUEvent.setHeight(10.0);
    warehouseMasterSKUEvent.setUpcCodes(List.of(UPC_CODE));
    Mockito.doThrow(RuntimeException.class).when(itemService)
        .updateItemDimensionsAndUpcCode(ITEM_SKU, 10.0, 10.0, 10.0, 10.0, List.of(UPC_CODE));
    Mockito.when(objectMapper.readValue(MESSAGE, WarehouseMasterSKUAllEvent.class)).thenReturn(warehouseMasterSKUEvent);
    dimensionUpdateListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, WarehouseMasterSKUAllEvent.class);
    Mockito.verify(itemService).updateItemDimensionsAndUpcCode(ITEM_SKU, 10.0, 10.0, 10.0, 10.0, List.of(UPC_CODE));
  }
}