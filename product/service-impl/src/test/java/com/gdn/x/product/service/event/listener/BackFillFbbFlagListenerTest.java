package com.gdn.x.product.service.event.listener;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.BackFillFbbFlagRequest;
import com.gdn.x.product.service.api.BackFillFbbFlagService;

public class BackFillFbbFlagListenerTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "store-id";
  private static final String PP_CODE = "pp-code";
  private static final String MESSAGE = "message";
  @InjectMocks
  private BackFillFbbFlagListener backFillFbbFlagListener;
  @Mock
  private BackFillFbbFlagService backFillFbbFlagService;

  @Mock
  private ObjectMapper objectMapper;
  private BackFillFbbFlagRequest backFillFbbFlagRequest;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.openMocks(this);
    backFillFbbFlagRequest =
      BackFillFbbFlagRequest.builder().pickupPointCode(PP_CODE).identifier(PRODUCT_SKU)
        .storeId(STORE_ID).build();

  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(backFillFbbFlagService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws IOException {
    Mockito.doNothing().when(this.backFillFbbFlagService).backFillFbbFlag(backFillFbbFlagRequest);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BackFillFbbFlagRequest.class))
      .thenReturn(backFillFbbFlagRequest);
    backFillFbbFlagListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.backFillFbbFlagService).backFillFbbFlag(backFillFbbFlagRequest);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BackFillFbbFlagRequest.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    backFillFbbFlagRequest.setIdentifier(StringUtils.EMPTY);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BackFillFbbFlagRequest.class))
      .thenReturn(backFillFbbFlagRequest);
    backFillFbbFlagListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BackFillFbbFlagRequest.class);
  }
}
