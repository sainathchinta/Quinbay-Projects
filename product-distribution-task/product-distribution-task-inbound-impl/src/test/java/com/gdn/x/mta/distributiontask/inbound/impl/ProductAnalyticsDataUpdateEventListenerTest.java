package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.MockitoAnnotations.initMocks;

import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.response.AutoQcConfigChangeDto;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;

public class ProductAnalyticsDataUpdateEventListenerTest {

  private static final String SELLER_CODE = "code";
  private ObjectMapper mapper;
  private AutoQcConfigChangeDto autoQcConfigChangeDto;

  @InjectMocks
  private ProductAnalyticsDataUpdateEventListener productAnalyticsDataUpdateEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductAutoApprovalService productAutoApprovalService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productAutoApprovalService);
  }

  @Test
   void onDomainEventConsumedTest() throws IOException {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(autoQcConfigChangeDto), AutoQcConfigChangeDto.class))
        .thenReturn(mapper.readValue(mapper.writeValueAsString(autoQcConfigChangeDto), AutoQcConfigChangeDto.class));
    productAnalyticsDataUpdateEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoQcConfigChangeDto));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoQcConfigChangeDto), AutoQcConfigChangeDto.class);
    Mockito.verify(productAutoApprovalService).saveAutoQcConfigChanges(autoQcConfigChangeDto);
  }

  @Test
   void onDomainEventConsumedExcpetionTest() throws IOException {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(autoQcConfigChangeDto), AutoQcConfigChangeDto.class))
        .thenReturn(mapper.readValue(mapper.writeValueAsString(autoQcConfigChangeDto), AutoQcConfigChangeDto.class));
    Mockito.doThrow(RuntimeException.class).when(productAutoApprovalService).saveAutoQcConfigChanges(autoQcConfigChangeDto);
    productAnalyticsDataUpdateEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoQcConfigChangeDto));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoQcConfigChangeDto), AutoQcConfigChangeDto.class);
    Mockito.verify(productAutoApprovalService).saveAutoQcConfigChanges(autoQcConfigChangeDto);
  }
}
