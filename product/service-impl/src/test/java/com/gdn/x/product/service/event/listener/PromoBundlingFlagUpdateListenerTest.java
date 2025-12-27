package com.gdn.x.product.service.event.listener;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingSkuCreationExpiredEventModel;
import com.gdn.x.product.service.api.ItemService;

public class PromoBundlingFlagUpdateListenerTest {

  private final String ITEM_SKU = "item-sku";
  private final boolean PROMO_BUNDLING_ACTIVATED = true;
  private final String STORE_ID = "store-id";
  private final String MERCHANT_CODE = "merchant-code";
  private static final String MESSAGE = "payload";

  private PromoBundlingSkuCreationExpiredEventModel promoBundlingChangeEvent = new PromoBundlingSkuCreationExpiredEventModel();

  @InjectMocks
  private PromoBundlingFlagUpdateListener listener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    promoBundlingChangeEvent.setStoreId(STORE_ID);
    promoBundlingChangeEvent.setSkuList(Arrays.asList(ITEM_SKU));
    promoBundlingChangeEvent.setMerchantCode(MERCHANT_CODE);
    promoBundlingChangeEvent.setPromoBundlingActive(PROMO_BUNDLING_ACTIVATED);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumed_SuccessWithoutComplementaryTest() throws Exception {
    promoBundlingChangeEvent.setSkuList(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class))
        .thenReturn(promoBundlingChangeEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException())
        .when(itemService)
        .updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.any(), ArgumentMatchers.eq(true));
    promoBundlingChangeEvent.setItemInfoList(Arrays.asList(new ItemInfo()));
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class))
        .thenReturn(promoBundlingChangeEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class);
    Mockito.verify(itemService)
        .updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.any(), ArgumentMatchers.eq(true));
  }

  @Test
  public void onDomainEventConsumedWithMPPTrueEmptyItems_SuccessTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class))
        .thenReturn(promoBundlingChangeEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class);
  }

  @Test
  public void onDomainEventConsumedWithMPPTrue_SuccessTest() throws Exception {
    promoBundlingChangeEvent.setItemInfoList(Arrays.asList(new ItemInfo()));
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class))
        .thenReturn(promoBundlingChangeEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuCreationExpiredEventModel.class);
    Mockito.verify(itemService)
        .updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.anyList(),
                ArgumentMatchers.eq(true));
  }

}
