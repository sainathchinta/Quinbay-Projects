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
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;

public class GfsToGcsImageMigrationListenerTest {
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";

  @InjectMocks
  private GfsToGcsImageMigrationListener gfsToGcsImageMigrationListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  private ObjectMapper mapper;
  private CommonImageBackfillingEventModel commonImageBackfillingEventModel;
  private String payload;

  @BeforeEach
  public void init() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);

    mapper = new ObjectMapper();
    commonImageBackfillingEventModel = new CommonImageBackfillingEventModel(STORE_ID, PRODUCT_CODE);
    payload = mapper.writeValueAsString(commonImageBackfillingEventModel);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, productServiceWrapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws IOException {
    Mockito.when(objectMapper.readValue(payload, CommonImageBackfillingEventModel.class))
        .thenReturn(commonImageBackfillingEventModel);
    Mockito.doNothing().when(productServiceWrapper).migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);
    gfsToGcsImageMigrationListener.onDomainEventConsumed(payload);
    Mockito.verify(objectMapper).readValue(payload, CommonImageBackfillingEventModel.class);
    Mockito.verify(productServiceWrapper).migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);

  }
  @Test
  public void onDomainEventConsumedErrorTest() throws IOException {
    Mockito.when(objectMapper.readValue(payload, CommonImageBackfillingEventModel.class))
        .thenReturn(commonImageBackfillingEventModel);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productServiceWrapper).migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);
    gfsToGcsImageMigrationListener.onDomainEventConsumed(payload);
    Mockito.verify(objectMapper).readValue(payload, CommonImageBackfillingEventModel.class);
    Mockito.verify(productServiceWrapper).migrateImagesFromGfsToGcsForProducts(STORE_ID, PRODUCT_CODE);
  }

}
