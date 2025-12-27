package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.entity.Catalog;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;

import java.util.Arrays;
import java.util.Collections;

public class CategoryChangeEventListenerTest {

  private static final String MESSAGE = "message";
  private static final String ATTRIBUTE_CODE = "attributeCode";

  @InjectMocks
  private CategoryChangeEventListener categoryChangeEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private MasterDataCacheService cacheService;

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, CategoryDomainEventModel.class))
      .thenReturn(new CategoryDomainEventModel());
    this.categoryChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, CategoryDomainEventModel.class);
    verify(this.cacheService).evictAllCategoryTrees();
  }

  @Test
  public void onDomainEventConsumedTestWithAttributeCodes() throws Exception {
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setAttributeCodes(Collections.singletonList(ATTRIBUTE_CODE));
    Mockito.when(this.objectMapper.readValue(MESSAGE, CategoryDomainEventModel.class))
        .thenReturn(categoryDomainEventModel);
    this.categoryChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, CategoryDomainEventModel.class);
    verify(this.cacheService).evictAllCategoryTrees();
    verify(this.cacheService).evictCategoryCodesByAttributeCodeCache(Constants.DEFAULT_STORE_ID,
        ATTRIBUTE_CODE);
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, CategoryDomainEventModel.class))
      .thenThrow(RuntimeException.class);
    this.categoryChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, CategoryDomainEventModel.class);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.cacheService);
    verifyNoMoreInteractions(this.objectMapper);
  }
}
