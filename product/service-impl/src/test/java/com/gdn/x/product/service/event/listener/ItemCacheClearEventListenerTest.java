package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.model.ItemCacheClearModel;
import com.gdn.x.product.service.api.CacheEvictItemService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verifyNoMoreInteractions;

public class ItemCacheClearEventListenerTest {

  private static final String storeId = "storeId";
  private static final String productSku = "productSku";
  private static final String itemSku = "itemSku";
  private static final String MESSAGE = "message";
  @InjectMocks
  private ItemCacheClearEventListener itemCacheClearEventListener;
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private CacheEvictItemService cacheEvictItemService;
  private ItemCacheClearModel itemCacheClearModel;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.openMocks(this);
    itemCacheClearModel = new ItemCacheClearModel();
    itemCacheClearModel.setStoreId(storeId);
    itemCacheClearModel.setItemSku(itemSku);
    itemCacheClearModel.setProductSku(productSku);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemCacheClearModel.class))
      .thenReturn(itemCacheClearModel);
    itemCacheClearEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemCacheClearModel.class);
    Mockito.verify(this.cacheEvictItemService).evictFindItemByItemSku(storeId, itemSku);
    Mockito.verify(this.cacheEvictItemService).evictFindItemByProductSku(storeId, productSku);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.objectMapper)
      .readValue(MESSAGE, ItemCacheClearModel.class);
    itemCacheClearEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemCacheClearModel.class);
  }
}
