package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingSkuChanged;
import com.gdn.x.product.service.api.ItemService;

public class PromoBundlingSkuDeactivatedEventListenerTest {
  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU1 = "itemSku1";
  private static final String PROMO_BUNDLING_ID = "promoBundlingId";
  private static final String MAIN_ITEM_SKU = "mainItemSku";
  private static final String PROMO_BUNDLING_TYPE = "promoBundlingType";
  private static final String MESSAGE = "message";

  private PromoBundlingSkuChanged message;

  private List<String> itemSkuList;

  private Set<String> itemSkuSet;

  @InjectMocks
  private PromoBundlingSkuDeactivatedEventListener promoBundlingSkuDeactivatedEventListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @Test
  public void onDomainEventConsumedTest() throws Exception{
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(message);
    promoBundlingSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
    verify(this.itemService).updatePromoBundlingByItemSkusInItemPickupPoint(message.getStoreId(), this.itemSkuSet, false);
  }

  @Test
  public void onDomainEventConsumed_NullTest() throws Exception{
  	message.setStoreId(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(message);
    promoBundlingSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
  }

  @Test
  public void onDomainEventConsumed_Null2Test() throws Exception{
  	message.setSkuList(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(message);
    promoBundlingSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
  }

  @Test
  public void onDomainEventConsumed_ThrowExceptionTest() throws Exception{
  	doThrow(new ApplicationRuntimeException()).when(itemService)
  	.updatePromoBundlingByItemSkus(STORE_ID, itemSkuSet, false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(message);
    promoBundlingSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
    verify(itemService).updatePromoBundlingByItemSkusInItemPickupPoint(STORE_ID, itemSkuSet, false);
  }

  @BeforeEach
  public void setUp(){
    openMocks(this);

    this.itemSkuList = new ArrayList<>();
    this.itemSkuList.add(ITEM_SKU);
    this.itemSkuList.add(ITEM_SKU1);

    this.itemSkuSet = new HashSet<>();
    this.itemSkuSet.addAll(this.itemSkuList);

    this.message = new PromoBundlingSkuChanged();
    this.message.setStoreId(STORE_ID);
    this.message.setSkuList(itemSkuList);
  }

  @AfterEach
  public void tearDown() throws Exception{
    verifyNoMoreInteractions(this.itemService);
  }
}
