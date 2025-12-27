package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.PricingPwpPromoEvent;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

public class PricingPwpPromoSkuEventTest {

  @InjectMocks
  private PricingPwpPromoSkuEventListener pricingPwpPromoSkuEventListener;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private PricingPwpPromoEvent pricingPwpPromoEvent;
  private static final String MESSAGE =
      "{\"storeId\":\"10001\",\"itemSku\":\"ITEM-SKU-001\",\"pickupPointCode\":\"PP001\"}";
  private static final String TOPIC_NAME = "com.gdn.partners.product.pricing.pwp.promo.sku.event";
  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "ITEM-SKU-001";
  private static final String PICKUP_POINT_CODE = "PP001";
  private static final String MERCHANT_CODE = "MERCHANT001";
  private static final String ITEM_PICKUP_POINT_ID = "IPP001";

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    pricingPwpPromoEvent =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .merchantCode(MERCHANT_CODE).itemPickupPointId(ITEM_PICKUP_POINT_ID).isPartOfPendingPWPAsMain(true)
            .isPartOfPendingPWPAsAdditional(false).isPartOfActivePWPAsMain(true).isPartOfActivePWPAsAdditional(false)
            .build();
    when(kafkaTopicProperties.getPwpPromoSkuEvent()).thenReturn(TOPIC_NAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(itemPickupPointService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedSuccessTest() throws Exception {
    when(objectMapper.readValue(MESSAGE, PricingPwpPromoEvent.class)).thenReturn(pricingPwpPromoEvent);

    pricingPwpPromoSkuEventListener.onDomainEventConsumed(MESSAGE);

    verify(objectMapper).readValue(MESSAGE, PricingPwpPromoEvent.class);
    verify(itemPickupPointService).updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);
    verify(kafkaTopicProperties).getPwpPromoSkuEvent();
  }

  @Test
  public void onDomainEventConsumedWithAllFlagsTrueTest() throws Exception {
    PricingPwpPromoEvent eventWithAllFlags =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .isPartOfPendingPWPAsMain(true).isPartOfPendingPWPAsAdditional(true).isPartOfActivePWPAsMain(true)
            .isPartOfActivePWPAsAdditional(true).build();
    when(objectMapper.readValue(MESSAGE, PricingPwpPromoEvent.class)).thenReturn(eventWithAllFlags);

    pricingPwpPromoSkuEventListener.onDomainEventConsumed(MESSAGE);

    verify(objectMapper).readValue(MESSAGE, PricingPwpPromoEvent.class);
    verify(itemPickupPointService).updatePwpFlagsByItemSkuAndPickupPointCode(eventWithAllFlags);
    verify(kafkaTopicProperties).getPwpPromoSkuEvent();
  }

  @Test
  public void onDomainEventConsumedWithAllFlagsFalseTest() throws Exception {
    PricingPwpPromoEvent eventWithAllFlagsFalse =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .isPartOfPendingPWPAsMain(false).isPartOfPendingPWPAsAdditional(false).isPartOfActivePWPAsMain(false)
            .isPartOfActivePWPAsAdditional(false).build();
    when(objectMapper.readValue(MESSAGE, PricingPwpPromoEvent.class)).thenReturn(eventWithAllFlagsFalse);

    pricingPwpPromoSkuEventListener.onDomainEventConsumed(MESSAGE);

    verify(objectMapper).readValue(MESSAGE, PricingPwpPromoEvent.class);
    verify(itemPickupPointService).updatePwpFlagsByItemSkuAndPickupPointCode(eventWithAllFlagsFalse);
    verify(kafkaTopicProperties).getPwpPromoSkuEvent();
  }

  @Test
  public void onDomainEventConsumedRuntimeExceptionTest() throws Exception {
    when(objectMapper.readValue(MESSAGE, PricingPwpPromoEvent.class)).thenReturn(pricingPwpPromoEvent);
    Mockito.doThrow(new RuntimeException("Service error")).when(itemPickupPointService)
        .updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);

    pricingPwpPromoSkuEventListener.onDomainEventConsumed(MESSAGE);

    verify(objectMapper).readValue(MESSAGE, PricingPwpPromoEvent.class);
    verify(itemPickupPointService).updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);
    verify(kafkaTopicProperties, times(2)).getPwpPromoSkuEvent();
  }

}