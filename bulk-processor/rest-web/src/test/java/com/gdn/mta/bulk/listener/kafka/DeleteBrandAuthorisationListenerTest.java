package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeListEvent;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;

public class DeleteBrandAuthorisationListenerTest {

  private static final String ID = "ID";
  private static final String STORE_ID = "STORE_ID";
  private static final String PROCESS_TYPE = "PROCESS_TYPE";

  private InternalProcessDataDomainEventModel internalProcessDataDomainEventModel;

  @InjectMocks
  private DeleteBrandAuthorisationListener deleteBrandAuthorisationListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private InternalProcessServiceWrapper processServiceWrapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    internalProcessDataDomainEventModel = new InternalProcessDataDomainEventModel();
    internalProcessDataDomainEventModel.setInternalProcessRequestId(ID);
    internalProcessDataDomainEventModel.setProcessType(PROCESS_TYPE);
    internalProcessDataDomainEventModel.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(processServiceWrapper);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, InternalProcessDataDomainEventModel.class))
        .thenReturn(internalProcessDataDomainEventModel);
    deleteBrandAuthorisationListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(processServiceWrapper).processDeleteBrandAuthorisationEvent(STORE_ID, PROCESS_TYPE, ID);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, InternalProcessDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties).getDeleteBrandAuthorisation();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, InternalProcessDataDomainEventModel.class))
        .thenReturn(internalProcessDataDomainEventModel);
    Mockito.doThrow(ApplicationRuntimeException.class).when(processServiceWrapper)
        .processDeleteBrandAuthorisationEvent(internalProcessDataDomainEventModel.getStoreId(),
            internalProcessDataDomainEventModel.getProcessType(), internalProcessDataDomainEventModel.getInternalProcessRequestId());
    deleteBrandAuthorisationListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(processServiceWrapper).processDeleteBrandAuthorisationEvent(STORE_ID, PROCESS_TYPE, ID);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, InternalProcessDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteBrandAuthorisation();
  }
}
