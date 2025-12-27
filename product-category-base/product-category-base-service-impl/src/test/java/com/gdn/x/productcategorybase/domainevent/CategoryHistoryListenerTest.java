package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.service.CategoryHistoryService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class CategoryHistoryListenerTest {

  private static final String OLD_VALUE = "Inactive";
  private static final String NEW_VALUE = "Active";
  private static final String ACTIVITY = "Change status";
  private static final String CATEGORY_CODE = "CAT-00001";

  @InjectMocks
  private CategoryHistoryListener categoryHistoryListener;

  @Mock
  private CategoryHistoryService categoryHistoryService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<CategoryHistoryEventModel> historyArgumentCaptor;

  private CategoryHistoryEventModel categoryHistoryEventModel;
  private String message;

  @AfterEach
  public void postTest() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(categoryHistoryService);
  }

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    categoryHistoryEventModel =
      CategoryHistoryEventModel.builder().categoryCode(CATEGORY_CODE).activity(ACTIVITY)
        .newStatus(NEW_VALUE).oldStatus(OLD_VALUE).build();

    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(categoryHistoryEventModel);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, CategoryHistoryEventModel.class))
      .thenReturn(categoryHistoryEventModel);
    categoryHistoryListener.onDomainEventConsumed(message);
    verify(categoryHistoryService).saveCategoryHistory(historyArgumentCaptor.capture());
    assertEquals(CATEGORY_CODE, historyArgumentCaptor.getValue().getCategoryCode());
    verify(objectMapper).readValue(message, CategoryHistoryEventModel.class);
    verify(kafkaTopicProperties,times(1)).getCategoryUpdateHistoryEvent();
  }

  @Test
  public void onDomainEventConsumed_ExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(message, CategoryHistoryEventModel.class);
    categoryHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, CategoryHistoryEventModel.class);
    verify(kafkaTopicProperties,times(2)).getCategoryUpdateHistoryEvent();
  }

  @Test
  public void onDomainEventConsumedNullTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, CategoryHistoryEventModel.class)).thenReturn(null);
    categoryHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, CategoryHistoryEventModel.class);
    verify(kafkaTopicProperties,times(1)).getCategoryUpdateHistoryEvent();
  }
}
