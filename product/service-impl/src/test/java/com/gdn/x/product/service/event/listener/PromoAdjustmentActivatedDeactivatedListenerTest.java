package com.gdn.x.product.service.event.listener;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.streaming.model.PromoAdjustmentActivatedDeactivatedEvent;
import com.gdn.partners.product.pricing.streaming.model.PromoAdjustmentDetailModel;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.service.api.ItemService;

public class PromoAdjustmentActivatedDeactivatedListenerTest {

  private static final String ITEM_SKU = "100001";
  private static final String STORE_ID = "100001";
  private static final String MESSAGE = "message";
  public static final String PP_CODE = "ppCode";

  @InjectMocks
  private PromoAdjustmentActivatedDeactivatedListener listener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  private PromoAdjustmentActivatedDeactivatedEvent promoAdjustmentActivatedDeactivatedEvent;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    promoAdjustmentActivatedDeactivatedEvent = new PromoAdjustmentActivatedDeactivatedEvent();
    promoAdjustmentActivatedDeactivatedEvent.setActivated(true);
    promoAdjustmentActivatedDeactivatedEvent.setActivatedDetail(new PromoAdjustmentDetailModel());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumedItemSkuNull() throws Exception {
    try {
      promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
      Mockito.when(
          this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
      listener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    } catch (ApplicationRuntimeException e){
    } finally {
    }
  }

  @Test
  public void onDomainEventConsumedPPCodeNull() throws Exception {
    try {
      promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
      promoAdjustmentActivatedDeactivatedEvent.setItemSku(STORE_ID);
      promoAdjustmentActivatedDeactivatedEvent.setPickupPointCode(null);
      Mockito.when(
              this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
          .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
      listener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper)
          .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    } catch (ApplicationRuntimeException e){
    } finally {
    }
  }

  @Test
  public void onDomainEventConsumedStoreIdNull() throws Exception {
    try {
      promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
      Mockito.when(
          this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
      listener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    } catch (ApplicationRuntimeException e) {
    } finally {
    }
  }

  @Test
  public void onDomainEventConsumedActivationDetailNull() throws Exception {
    try {
      promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
      promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
      promoAdjustmentActivatedDeactivatedEvent.setActivated(true);
      Mockito.when(
          this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
      listener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    } catch (ApplicationRuntimeException e) {
    } finally {
    }
  }

  @Test
  public void onDomainEventConsumedStartDateNull() throws Exception {
    try {
      promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
      promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
      promoAdjustmentActivatedDeactivatedEvent.setActivated(true);
      PromoAdjustmentDetailModel promoAdjustmentEventModel = new PromoAdjustmentDetailModel();
      promoAdjustmentEventModel.setEndDate(new Date());
      promoAdjustmentEventModel.setStartDate(null);
      promoAdjustmentActivatedDeactivatedEvent.setActivatedDetail(promoAdjustmentEventModel);
      Mockito.when(
          this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
      listener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    } catch (ApplicationRuntimeException e) {
    } finally {
    }
  }

  @Test
  public void onDomainEventConsumedEndDateNull() throws Exception {
    try {
      promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
      promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
      promoAdjustmentActivatedDeactivatedEvent.setActivated(true);
      PromoAdjustmentDetailModel promoAdjustmentEventModel = new PromoAdjustmentDetailModel();
      promoAdjustmentEventModel.setStartDate(new Date());
      promoAdjustmentEventModel.setEndDate(null);
      promoAdjustmentActivatedDeactivatedEvent.setActivatedDetail(promoAdjustmentEventModel);
      Mockito.when(
          this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
      listener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    } catch (ApplicationRuntimeException e) {
    } finally {
    }
  }

  @Test
  public void onDomainEventConsumedActivatedFalse() throws Exception {
    promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
    promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
    promoAdjustmentActivatedDeactivatedEvent.setActivated(false);
    PromoAdjustmentDetailModel promoAdjustmentEventModel = new PromoAdjustmentDetailModel();
    promoAdjustmentEventModel.setEndDate(new Date());
    promoAdjustmentEventModel.setStartDate(null);
    promoAdjustmentActivatedDeactivatedEvent.setActivatedDetail(promoAdjustmentEventModel);
    Mockito.when(
        this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
      .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper)
      .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
  }

  @Test
  public void onDomainEventConsumedActivatedFalseMPP() throws Exception {
    promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
    promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
    promoAdjustmentActivatedDeactivatedEvent.setActivated(false);
    promoAdjustmentActivatedDeactivatedEvent.setPickupPointCode(PP_CODE);
    PromoAdjustmentDetailModel promoAdjustmentEventModel = new PromoAdjustmentDetailModel();
    promoAdjustmentEventModel.setEndDate(new Date());
    promoAdjustmentEventModel.setStartDate(new Date());
    promoAdjustmentActivatedDeactivatedEvent.setActivatedDetail(promoAdjustmentEventModel);
    Mockito.when(
            this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    Mockito.verify(itemService)
        .updateItemMerchantDiscountPriceByItemSkuAndPPCode(eq(STORE_ID), eq(ITEM_SKU), Mockito.any(),
            eq(PP_CODE), eq(false));
  }

  @Test
  public void onDomainEventConsumedActivatedTrue() throws Exception {
    promoAdjustmentActivatedDeactivatedEvent.setItemSku(ITEM_SKU);
    promoAdjustmentActivatedDeactivatedEvent.setStoreId(STORE_ID);
    promoAdjustmentActivatedDeactivatedEvent.setActivated(true);
    promoAdjustmentActivatedDeactivatedEvent.setPickupPointCode(PP_CODE);
    PromoAdjustmentDetailModel promoAdjustmentEventModel = new PromoAdjustmentDetailModel();
    promoAdjustmentEventModel.setEndDate(new Date());
    promoAdjustmentEventModel.setStartDate(new Date());
    promoAdjustmentActivatedDeactivatedEvent.setActivatedDetail(promoAdjustmentEventModel);
    Mockito.when(
            this.objectMapper.readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class))
        .thenReturn(promoAdjustmentActivatedDeactivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, PromoAdjustmentActivatedDeactivatedEvent.class);
    Mockito.verify(itemService)
        .updateItemMerchantDiscountPriceByItemSkuAndPPCode(eq(STORE_ID), eq(ITEM_SKU), Mockito.any(DiscountPrice.class),
            eq(PP_CODE), eq(true));
  }
}