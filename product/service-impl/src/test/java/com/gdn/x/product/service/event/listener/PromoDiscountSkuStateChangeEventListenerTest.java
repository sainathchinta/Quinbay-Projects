package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.PromoSkuStateChangeEvent;
import com.gdn.x.product.service.api.ItemService;

/**
 * Created by govind on 06/05/2019 AD.
 */
public class PromoDiscountSkuStateChangeEventListenerTest {

  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "itemSku";
  private static final String PP_CODE = "PP_CODE";
  private static final String MESSAGE = "message";

  @InjectMocks
  private PromoDiscountSkuStateChangeEventListener promoDiscountSkuStateChangeEventListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  private PromoSkuStateChangeEvent promoSkuStateChangeEvent;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    promoSkuStateChangeEvent =
        PromoSkuStateChangeEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).isPromoActive(true)
            .build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoSkuStateChangeEvent.class))
      .thenReturn(promoSkuStateChangeEvent);
    this.promoDiscountSkuStateChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoSkuStateChangeEvent.class);
  }

  @Test
  public void onDomainEventConsumed_WhenStoreIdNullTest() throws Exception {
    promoSkuStateChangeEvent.setStoreId(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoSkuStateChangeEvent.class))
      .thenReturn(promoSkuStateChangeEvent);
    this.promoDiscountSkuStateChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoSkuStateChangeEvent.class);
  }

  @Test
  public void onDomainEventConsumed_WhenItemSkuNullTest() throws Exception {
    promoSkuStateChangeEvent.setItemSku(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoSkuStateChangeEvent.class))
      .thenReturn(promoSkuStateChangeEvent);
    this.promoDiscountSkuStateChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoSkuStateChangeEvent.class);
  }

  @Test
  public void onDomainEventConsumed_WhenPPCodeNullTest() throws Exception {
    promoSkuStateChangeEvent.setPickupPointCode(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoSkuStateChangeEvent.class))
        .thenReturn(promoSkuStateChangeEvent);
    this.promoDiscountSkuStateChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoSkuStateChangeEvent.class);
  }

  @Test
  public void onDomainEventConsumedMPPTrueTest() throws Exception {
    promoSkuStateChangeEvent.setPickupPointCode(PP_CODE);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoSkuStateChangeEvent.class))
        .thenReturn(promoSkuStateChangeEvent);
    this.promoDiscountSkuStateChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoSkuStateChangeEvent.class);
    verify(this.itemService).updateMerchantPromoDiscountFlagByItemSkuAndPPCode(STORE_ID, ITEM_SKU, true, PP_CODE);
  }
}
