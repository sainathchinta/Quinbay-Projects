package com.gdn.x.mta.distributiontask.inbound.impl;

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
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;

public class CommonImageBackfillingListenerTest {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private CommonImageBackfillingListener commonImageBackfillingListener;

  private ObjectMapper mapper = new ObjectMapper();
  private String json;
  private CommonImageBackfillingEventModel commonImageBackfillingEventModel;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.openMocks(this);

    commonImageBackfillingEventModel = new CommonImageBackfillingEventModel(STORE_ID, PRODUCT_CODE);
    json = mapper.writeValueAsString(commonImageBackfillingEventModel);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productWrapperService, objectMapper);
  }

  @Test
   void onDomainEventConsumedTest() throws IOException {
    Mockito.doNothing().when(productWrapperService)
        .backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE);
    Mockito.when(objectMapper.readValue(json, CommonImageBackfillingEventModel.class))
        .thenReturn(commonImageBackfillingEventModel);

    commonImageBackfillingListener.onDomainEventConsumed(json);

    Mockito.verify(productWrapperService).backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE);
    Mockito.verify(objectMapper).readValue(json, CommonImageBackfillingEventModel.class);
  }

  @Test
   void onDomainEventConsumedExceptionTest() throws IOException {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productWrapperService)
        .backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE);
    Mockito.when(objectMapper.readValue(json, CommonImageBackfillingEventModel.class))
        .thenReturn(commonImageBackfillingEventModel);

    commonImageBackfillingListener.onDomainEventConsumed(json);

    Mockito.verify(productWrapperService).backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE);
    Mockito.verify(objectMapper).readValue(json, CommonImageBackfillingEventModel.class);
  }
}
