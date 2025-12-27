package com.gdn.x.productcategorybase.domainevent;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
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

public class CommonImageBackfillingListenerTest {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private CommonImageBackfillingListener commonImageBackfillingListener;

  private ObjectMapper mapper = new ObjectMapper();
  private String json;
  private CommonImageBackfillingEventModel commonImageBackfillingEventModel;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);

    commonImageBackfillingEventModel =
      CommonImageBackfillingEventModel.builder().storeId(STORE_ID).productCode(PRODUCT_CODE)
        .migrationType(StringUtils.EMPTY).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productServiceWrapper, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws IOException {
    Mockito.doNothing().when(productServiceWrapper)
        .backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.when(objectMapper.readValue(json, CommonImageBackfillingEventModel.class))
        .thenReturn(commonImageBackfillingEventModel);

    commonImageBackfillingListener.onDomainEventConsumed(json);

    Mockito.verify(productServiceWrapper).backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE,
      StringUtils.EMPTY);
    Mockito.verify(objectMapper).readValue(json, CommonImageBackfillingEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws IOException {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productServiceWrapper)
        .backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.when(objectMapper.readValue(json, CommonImageBackfillingEventModel.class))
        .thenReturn(commonImageBackfillingEventModel);

    commonImageBackfillingListener.onDomainEventConsumed(json);

    Mockito.verify(productServiceWrapper).backfillCommonImageFlagInProductAndItemImages(STORE_ID, PRODUCT_CODE,
      StringUtils.EMPTY);
    Mockito.verify(objectMapper).readValue(json, CommonImageBackfillingEventModel.class);
  }

}
