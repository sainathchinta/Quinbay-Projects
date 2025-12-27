package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHierarchyCacheEvictEventModel;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class CategoryHierarchyCacheEvictListenerTest {

  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "CAT-123";
  private static final String TOPIC_NAME = "category-hierarchy-cache-evict-topic";
  private static final String VALID_MESSAGE =
      "{\"storeId\":\"10001\",\"categoryCode\":\"CAT-123\"}";
  private static final String INVALID_MESSAGE = "invalid-json-message";

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private CategoryHierarchyCacheEvictListener categoryHierarchyCacheEvictListener;

  private CategoryHierarchyCacheEvictEventModel eventModel;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    eventModel = CategoryHierarchyCacheEvictEventModel.builder().storeId(STORE_ID)
        .categoryCode(CATEGORY_CODE).build();

    when(kafkaTopicProperties.getCategoryHierarchyCaffeineCacheEvictEvent()).thenReturn(TOPIC_NAME);
  }

  @Test
  public void onDomainEventConsumed_ValidMessage_ShouldEvictCache() throws Exception {
    when(objectMapper.readValue(eq(VALID_MESSAGE),
        eq(CategoryHierarchyCacheEvictEventModel.class))).thenReturn(eventModel);
    doNothing().when(applicationCacheServiceBean)
        .evictCategoryHierarchyByByStoreIdAndCategoryCodeFromCaffeine(STORE_ID, CATEGORY_CODE);
    categoryHierarchyCacheEvictListener.onDomainEventConsumed(VALID_MESSAGE);
    verify(kafkaTopicProperties).getCategoryHierarchyCaffeineCacheEvictEvent();
    verify(objectMapper).readValue(eq(VALID_MESSAGE),
        eq(CategoryHierarchyCacheEvictEventModel.class));
    verify(
        applicationCacheServiceBean).evictCategoryHierarchyByByStoreIdAndCategoryCodeFromCaffeine(
        STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void onDomainEventConsumed_Exception_ShouldHandleGracefully() throws Exception {
    when(objectMapper.readValue(eq(INVALID_MESSAGE),
        eq(CategoryHierarchyCacheEvictEventModel.class))).thenThrow(
        new RuntimeException("Invalid JSON"));
    categoryHierarchyCacheEvictListener.onDomainEventConsumed(INVALID_MESSAGE);
    verify(kafkaTopicProperties, times(2)).getCategoryHierarchyCaffeineCacheEvictEvent();
    verify(objectMapper).readValue(eq(INVALID_MESSAGE),
        eq(CategoryHierarchyCacheEvictEventModel.class));
  }
} 