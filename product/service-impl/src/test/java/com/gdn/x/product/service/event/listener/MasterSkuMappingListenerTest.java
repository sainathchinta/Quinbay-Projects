package com.gdn.x.product.service.event.listener;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;

public class MasterSkuMappingListenerTest {

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductRetryEventPublishService productRetryEventPublishService;

  @InjectMocks
  private MasterSkuMappingListener masterSkuMappingListener;

  private static final String MESSAGE = "message";
  private static final String STORE_ID = "storeId";
  private static final String ITEM_SKU = "itemSku";
  private static final String MASTER_SKU = "masterSku";

  private MasterSkuMappingEventModel masterSkuMappingEventModel;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);

    masterSkuMappingEventModel = new MasterSkuMappingEventModel();
    masterSkuMappingEventModel.setStoreId(STORE_ID);
    masterSkuMappingEventModel.setItemSku(ITEM_SKU);
    masterSkuMappingEventModel.setMasterItemSku(MASTER_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(itemService);
    verifyNoMoreInteractions(productRetryEventPublishService);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws JsonProcessingException {
    Mockito.when(this.objectMapper.readValue(MESSAGE, MasterSkuMappingEventModel.class)).thenReturn(
        masterSkuMappingEventModel);
    masterSkuMappingListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, MasterSkuMappingEventModel.class);
    Mockito.verify(itemService).updateMasterSku(masterSkuMappingEventModel);
  }

  @Test
  public void onDomainEventConsumedApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, MasterSkuMappingEventModel.class)).thenThrow(
        ApplicationRuntimeException.class);
    masterSkuMappingListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, MasterSkuMappingEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    ReflectionTestUtils.setField(masterSkuMappingListener, "allowRetryForMasterSkuMappingFailedSkus", true);
    Mockito.when(objectMapper.readValue(MESSAGE, MasterSkuMappingEventModel.class)).thenThrow(
        RuntimeException.class);
    masterSkuMappingListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, MasterSkuMappingEventModel.class);
    Mockito.verify(productRetryEventPublishService).insertToRetryPublish(any());
  }

  @Test
  public void onDomainEventConsumedExceptionAllowRetryFalseTest() throws Exception {
    ReflectionTestUtils.setField(masterSkuMappingListener, "allowRetryForMasterSkuMappingFailedSkus", false);
    Mockito.when(objectMapper.readValue(MESSAGE, MasterSkuMappingEventModel.class)).thenThrow(
        RuntimeException.class);
    masterSkuMappingListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, MasterSkuMappingEventModel.class);
  }

}
