package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;

public class UpdateSalesCategoryEventListenerTest {

  private static final String STORE_ID = "10001";
  private static final String PARENT_CODE = "parentCode";
  private static final String INTERNAL_REQUEST_ID = "internalRequestId";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private UpdateSalesCategoryEventListener updateSalesCategoryEventListener;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private InternalProcessDataDomainEventModel internalProcessDataDomainEventModel;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    internalProcessDataDomainEventModel =
        new InternalProcessDataDomainEventModel(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
            PARENT_CODE, INTERNAL_REQUEST_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.doNothing().when(internalProcessServiceWrapper).processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
        PARENT_CODE, INTERNAL_REQUEST_ID);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(internalProcessDataDomainEventModel),
        InternalProcessDataDomainEventModel.class)).thenReturn(internalProcessDataDomainEventModel);
    updateSalesCategoryEventListener
        .onDomainEventConsumed(mapper.writeValueAsString(internalProcessDataDomainEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(internalProcessDataDomainEventModel),
        InternalProcessDataDomainEventModel.class);
    Mockito.verify(internalProcessServiceWrapper).processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
        PARENT_CODE, INTERNAL_REQUEST_ID);
    Mockito.verify(kafkaTopicProperties).getUpdateSalesCategoryDetails();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(internalProcessDataDomainEventModel),
        InternalProcessDataDomainEventModel.class)).thenReturn(internalProcessDataDomainEventModel);
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessServiceWrapper)
        .processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), PARENT_CODE,
            INTERNAL_REQUEST_ID);
    updateSalesCategoryEventListener
        .onDomainEventConsumed(mapper.writeValueAsString(internalProcessDataDomainEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(internalProcessDataDomainEventModel),
        InternalProcessDataDomainEventModel.class);
    Mockito.verify(internalProcessServiceWrapper)
        .processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), PARENT_CODE,
            INTERNAL_REQUEST_ID);
    Mockito.verify(kafkaTopicProperties, times(2)).getUpdateSalesCategoryDetails();
  }

}
