package com.gdn.partners.pbp.service.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;
import com.gdn.mta.product.service.BackFillFbbFlagService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class BackFillFbbListenerTest {

  private static final String IDENTIFIER = "identifier";
  private static final String STORE_ID = "store-id";
  private static final String PP_CODE = "pp-code";
  private static final String MESSAGE = "message";

  @InjectMocks
  private BackFillFbbFlagListener backFillFbbFlagListener;

  @Mock
  private BackFillFbbFlagService backFillFbbFlagService;

  @Mock
  private ObjectMapper objectMapper;

  private ProductFbbMigrationEventModel productFbbMigration;

  @BeforeEach
  public void setup(){
    MockitoAnnotations.initMocks(this);
    productFbbMigration =
      new ProductFbbMigrationEventModel();
    productFbbMigration.setIdentifier(IDENTIFIER);
    productFbbMigration.setPickupPointCode(PP_CODE);
    productFbbMigration.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown(){
   Mockito.verifyNoMoreInteractions(backFillFbbFlagService);
   Mockito.verifyNoMoreInteractions(objectMapper);

  }

  @Test
  public void onDomainEventConsumedTest() throws JsonProcessingException {
    Mockito.doNothing().when(this.backFillFbbFlagService).backFillFbbFlag(productFbbMigration);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductFbbMigrationEventModel.class))
      .thenReturn(productFbbMigration);
    backFillFbbFlagListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.backFillFbbFlagService).backFillFbbFlag(productFbbMigration);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductFbbMigrationEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws JsonProcessingException {
    productFbbMigration.setIdentifier(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductFbbMigrationEventModel.class))
      .thenReturn(productFbbMigration);
    backFillFbbFlagListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductFbbMigrationEventModel.class);
  }
}
