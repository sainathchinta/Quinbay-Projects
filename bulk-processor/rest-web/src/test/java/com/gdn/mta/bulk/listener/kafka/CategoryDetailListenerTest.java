package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.repository.KafkaEventLogRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;

public class CategoryDetailListenerTest {

  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String CATEGORY_CODE = "WA-00002";
  private static final String NAME = "LONG DRESS";
  private static final String DESCRIPTION = "DESCRIPTION";

  private CategoryDomainEventModel categoryDomainEventModel;

  @Mock
  private KafkaEventLogRepository kafkaEventLogRepository;

  @InjectMocks
  private CategoryDetailListener categoryDetailListener;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<KafkaEventLog> kafkaEventLogArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setId(CATEGORY_ID);
    categoryDomainEventModel.setCategoryCode(CATEGORY_CODE);
    categoryDomainEventModel.setDescription(DESCRIPTION.getBytes());
    categoryDomainEventModel.setActivated(true);
    categoryDomainEventModel.setCatalog(new CatalogDomainEventModel());
    categoryDomainEventModel.setDisplay(true);
    categoryDomainEventModel.setName(NAME);
    when(kafkaEventLogRepository.save(kafkaEventLogArgumentCaptor.capture())).thenReturn(new KafkaEventLog());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(kafkaEventLogRepository);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    when(objectMapper.readValue(Constant.CLIENT_ID, CategoryDomainEventModel.class))
        .thenReturn(categoryDomainEventModel);
    categoryDetailListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Assertions.assertEquals(new ObjectMapper().writeValueAsString(categoryDomainEventModel),
        kafkaEventLogArgumentCaptor.getValue().getEventMessage());
    Assertions.assertEquals(DomainEventName.CATEGORY_PUBLISH, kafkaEventLogArgumentCaptor.getValue().getTopicName());
    verify(kafkaEventLogRepository).save(kafkaEventLogArgumentCaptor.getValue());
    verify(objectMapper).readValue(Constant.CLIENT_ID, CategoryDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_ExceptionTest() throws Exception {
    when(objectMapper.readValue(Constant.CLIENT_ID, CategoryDomainEventModel.class))
        .thenReturn(categoryDomainEventModel);
    doThrow(ApplicationRuntimeException.class).when(kafkaEventLogRepository).save(kafkaEventLogArgumentCaptor.capture());
    categoryDetailListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(kafkaEventLogRepository).save(kafkaEventLogArgumentCaptor.getValue());
    verify(objectMapper).readValue(Constant.CLIENT_ID, CategoryDomainEventModel.class);
  }
}