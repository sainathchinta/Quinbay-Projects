package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;

/**
 * Created by parvej on 15/04/20.
 */
public class SaveDGLevelAndDimensionsListenerTest {
  private static final String MESSAGE = "message";

  @InjectMocks
  private SaveDGLevelAndDimensionsListener listener;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  private WarehouseMasterSKUEvent warehouseMasterSKUEvent;
  private String message;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    ObjectMapper mapper = new ObjectMapper();

    warehouseMasterSKUEvent = new WarehouseMasterSKUEvent();
    message = mapper.writeValueAsString(warehouseMasterSKUEvent);

    Mockito.when(objectMapper.readValue(message, WarehouseMasterSKUEvent.class)).thenReturn(warehouseMasterSKUEvent);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    WarehouseMasterSKUEvent warehouseMasterSKUEvent = new WarehouseMasterSKUEvent();
    when(objectMapper.readValue(MESSAGE, WarehouseMasterSKUEvent.class)).thenReturn(warehouseMasterSKUEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productServiceWrapper).updateProductDimensions(warehouseMasterSKUEvent);
    verify(objectMapper).readValue(MESSAGE, WarehouseMasterSKUEvent.class);
  }

  @Test
  public void onDomainEventConsumedTest_whenExceptionOccurs() throws Exception {
    WarehouseMasterSKUEvent warehouseMasterSKUEvent = new WarehouseMasterSKUEvent();
    when(objectMapper.readValue(MESSAGE, WarehouseMasterSKUEvent.class)).thenReturn(warehouseMasterSKUEvent);
    Mockito.doThrow(new NullPointerException()).when(productServiceWrapper)
        .updateProductDimensions(warehouseMasterSKUEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productServiceWrapper).updateProductDimensions(warehouseMasterSKUEvent);
    verify(objectMapper).readValue(MESSAGE, WarehouseMasterSKUEvent.class);
  }
}