package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gdn.mta.product.service.RecategorizationService;

/**
 * Created by hardikbohra on 07/06/18.
 */
public class SaveCategoryProductSkuMappingListenerTest {

  private ObjectMapper mapper;
  @InjectMocks
  private SaveCategoryProductSkuMappingListener listener;

  @Mock
  private RecategorizationService recategorizationService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(recategorizationService, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    CategoryProductSkuMappingRequest recategorizationRequest = new CategoryProductSkuMappingRequest();
    String message = mapper.writeValueAsString(recategorizationRequest);
    Mockito.when(objectMapper.readValue(message, CategoryProductSkuMappingRequest.class))
        .thenReturn(recategorizationRequest);
    this.listener.onDomainEventConsumed(message);
    Mockito.verify(recategorizationService).saveCategoryToProductSkuMapping(recategorizationRequest);
    verify(objectMapper).readValue(message, CategoryProductSkuMappingRequest.class);
  }

  @Test
  public void onDomainEventConsumedTest_whenExceptionOccurs() throws Exception {
    CategoryProductSkuMappingRequest recategorizationRequest = new CategoryProductSkuMappingRequest();
    Mockito.doThrow(new NullPointerException()).when(recategorizationService).saveCategoryToProductSkuMapping
        (recategorizationRequest);
    String message = mapper.writeValueAsString(recategorizationRequest);
    Mockito.when(objectMapper.readValue(message, CategoryProductSkuMappingRequest.class))
        .thenReturn(recategorizationRequest);
    this.listener.onDomainEventConsumed(message);
    Mockito.verify(recategorizationService).saveCategoryToProductSkuMapping(recategorizationRequest);
    verify(objectMapper).readValue(message, CategoryProductSkuMappingRequest.class);
  }
}
