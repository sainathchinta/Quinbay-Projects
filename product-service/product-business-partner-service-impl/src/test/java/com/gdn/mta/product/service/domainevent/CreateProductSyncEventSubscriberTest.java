package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verify;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.CreateProductSyncEvent;
import com.gdn.mta.product.service.CreateProductSyncService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Arrays;

/**
 * @author anand
 * @since Sep 2019
 */
public class CreateProductSyncEventSubscriberTest {

  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";

  private static final String STORE_ID = "STORE_ID";

  private static final String USER_NAME = "USER_NAME";

  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";

  private static final String GDN_ITEM_SKU = "GDN_ITEM_SKU";

  private ObjectMapper mapper;

  @Mock
  private CreateProductSyncService createProductSyncService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private CreateProductSyncEventSubscriber subscriber;

  private CreateProductSyncEvent event;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    this.event = CreateProductSyncEvent.builder()
      .businessPartnerCode(BUSINESS_PARTNER_CODE)
      .pickupPointCode(PICKUP_POINT_CODE)
      .sourceItemSkus(Arrays.asList(GDN_ITEM_SKU))
      .storeId(STORE_ID)
      .username(USER_NAME)
      .build();
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void cleanup(){
    Mockito.verifyNoMoreInteractions(createProductSyncService, objectMapper);
  }

  @Test
  public void onMessage() throws Exception {
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, CreateProductSyncEvent.class))
        .thenReturn(this.event);
    this.subscriber.onDomainEventConsumed(message);

    Mockito.verify(createProductSyncService)
      .createSyncProduct(STORE_ID, USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, Arrays.asList(GDN_ITEM_SKU));
    verify(objectMapper).readValue(message, CreateProductSyncEvent.class);
  }

  @Test
  public void onMessage_whenErrorWhileCopying() throws Exception {
    Mockito
      .doThrow(ApplicationRuntimeException.class)
      .when(createProductSyncService)
      .createSyncProduct(STORE_ID, USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, Arrays.asList(GDN_ITEM_SKU));

    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, CreateProductSyncEvent.class))
        .thenReturn(this.event);
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(createProductSyncService)
      .createSyncProduct(STORE_ID, USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, Arrays.asList(GDN_ITEM_SKU));
    verify(objectMapper).readValue(message, CreateProductSyncEvent.class);
  }

}
