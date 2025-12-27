package com.gdn.x.productcategorybase.domainevent;

import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;

public class ProductDeletionListenerTest {
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";

  @InjectMocks
  private ProductDeletionListener productDeletionListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductDeletionWrapperService productDeletionWrapperService;

  private String json;
  private ObjectMapper mapper = new ObjectMapper();
  private ProductCodeDomainEventModel productCodeDomainEventModel;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);

    productCodeDomainEventModel = new ProductCodeDomainEventModel(STORE_ID, PRODUCT_CODE);
    json = mapper.writeValueAsString(productCodeDomainEventModel);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productDeletionWrapperService, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws IOException {
    Mockito.when(objectMapper.readValue(json,ProductCodeDomainEventModel.class)).thenReturn(productCodeDomainEventModel);
    Mockito.doNothing().when(productDeletionWrapperService).archiveAndDeleteProductData(STORE_ID, PRODUCT_CODE);
    productDeletionListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json,ProductCodeDomainEventModel.class);
    Mockito.verify(productDeletionWrapperService).archiveAndDeleteProductData(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void onDomainEventConsumedErrorTest() throws IOException {
    Mockito.when(objectMapper.readValue(json,ProductCodeDomainEventModel.class)).thenThrow(RuntimeException.class);
    productDeletionListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json,ProductCodeDomainEventModel.class);
  }
}
