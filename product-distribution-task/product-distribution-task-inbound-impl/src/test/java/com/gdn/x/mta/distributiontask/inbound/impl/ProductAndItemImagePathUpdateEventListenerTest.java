package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;

public class ProductAndItemImagePathUpdateEventListenerTest {

  private ObjectMapper mapper;
  private ImagePathUpdateDomainEventModel imagePathUpdateDomainEventModel;
  private Product product;

  @InjectMocks
  private ProductAndItemImagePathUpdateEventListener productAndItemImagePathUpdateEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productService);
  }

  @Test
   void onDomainEventConsumedTest() throws Exception {
    imagePathUpdateDomainEventModel = new ImagePathUpdateDomainEventModel();
    String message = mapper.writeValueAsString(imagePathUpdateDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ImagePathUpdateDomainEventModel.class))
        .thenReturn(imagePathUpdateDomainEventModel);
    productAndItemImagePathUpdateEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ImagePathUpdateDomainEventModel.class);
    Mockito.verify(productService).productAndItemImagePathUpdate(imagePathUpdateDomainEventModel);
  }

  @Test
   void onDomainEventConsumedExceptionTest() throws Exception {
    imagePathUpdateDomainEventModel = new ImagePathUpdateDomainEventModel();
    String message = mapper.writeValueAsString(imagePathUpdateDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ImagePathUpdateDomainEventModel.class))
        .thenThrow(new NullPointerException());
    try {
      productAndItemImagePathUpdateEventListener.onDomainEventConsumed(message);
    } finally {
      Mockito.verify(objectMapper).readValue(message, ImagePathUpdateDomainEventModel.class);
    }
  }
}
