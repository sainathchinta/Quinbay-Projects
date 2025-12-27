package com.gdn.x.product.service.event.listener;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.inventory.domain.event.model.StockStatusLevel3Event;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.service.api.ProductL3SolrService;

public class ProductStockStatusChangeListenerMPPTest {

  private static final String MESSAGE = "message";
  private static final String MERCHANT_CODE = "merchant_code";

  @InjectMocks
  private ProductStockStatusChangeMPPListener listener;

  @Mock
  private ProductL3SolrService productL3SolrService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productL3SolrService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    StockStatusLevel3Event stockStatusLevel3Event = new StockStatusLevel3Event();
    stockStatusLevel3Event.setStatus(Constants.AVAILABLE);
    stockStatusLevel3Event.setWebProductSku(SolrFieldNames.PRODUCT_SKU);
    Mockito.when(this.objectMapper.readValue(MESSAGE, StockStatusLevel3Event.class))
      .thenReturn(stockStatusLevel3Event);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productL3SolrService).updateStockStatusInL3Solr(SolrFieldNames.PRODUCT_SKU,
      Constants.AVAILABLE, null);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, StockStatusLevel3Event.class);
  }

  @Test
  public void onDomainEventConsumedNullProductSkuTest() throws Exception {
    StockStatusLevel3Event stockStatusLevel3Event = new StockStatusLevel3Event();
    stockStatusLevel3Event.setStatus(Constants.AVAILABLE);
    stockStatusLevel3Event.setWebProductSku(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, StockStatusLevel3Event.class))
      .thenReturn(stockStatusLevel3Event);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, StockStatusLevel3Event.class);
  }


  @Test
  public void onDomainEventConsumedNullStatusTest() throws Exception {
    StockStatusLevel3Event stockStatusLevel3Event = new StockStatusLevel3Event();
    stockStatusLevel3Event.setWebProductSku(SolrFieldNames.PRODUCT_SKU);
    stockStatusLevel3Event.setStockChangeTimestamp(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, StockStatusLevel3Event.class))
      .thenReturn(stockStatusLevel3Event);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, StockStatusLevel3Event.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    StockStatusLevel3Event stockStatusLevel3Event = new StockStatusLevel3Event();
    stockStatusLevel3Event.setStatus(Constants.AVAILABLE);
    stockStatusLevel3Event.setWebProductSku(SolrFieldNames.PRODUCT_SKU);
    Mockito.doThrow(RuntimeException.class).when(this.objectMapper).readValue(MESSAGE, StockStatusLevel3Event.class);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, StockStatusLevel3Event.class);
  }
}