package com.gdn.x.product.service.event.listener;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.BlimartSubscriptionChangeRequest;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;

public class BlimartSubscriptionChangeListenerTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String STORE_ID = "storeId";

  @InjectMocks
  private BlimartSubscriptionChangeListener blimartSubscriptionChangeListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectMapper objectMapper;

  private BlimartSubscriptionChangeRequest blimartSubscriptionChangeRequest;
  private static final String MESSAGE = "message";

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    blimartSubscriptionChangeRequest =
        BlimartSubscriptionChangeRequest.builder().itemSku(ITEM_SKU).subscribable(Boolean.TRUE)
            .pickupPointCode(PICKUP_POINT_CODE).build();
    blimartSubscriptionChangeRequest.setStoreId(STORE_ID);
    ReflectionTestUtils.setField(blimartSubscriptionChangeListener, "subscriptionAtL5Flow", false);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
    Mockito.verifyNoMoreInteractions(itemPickupPointService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.doNothing().when(this.itemService)
      .updateSubscriptionFlagByItemSku(STORE_ID, ITEM_SKU, true, null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlimartSubscriptionChangeRequest.class))
      .thenReturn(blimartSubscriptionChangeRequest);
    blimartSubscriptionChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemService).updateSubscriptionFlagByItemSku(STORE_ID, ITEM_SKU, true,
      null);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlimartSubscriptionChangeRequest.class);
  }

  @Test
  public void onDomainEventConsumedL5FlowTest() throws Exception {
    ReflectionTestUtils.setField(blimartSubscriptionChangeListener, "subscriptionAtL5Flow", true);
    Mockito.doNothing().when(this.itemService)
        .updateSubscriptionFlagByItemSku(STORE_ID, ITEM_SKU, true, null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlimartSubscriptionChangeRequest.class))
        .thenReturn(blimartSubscriptionChangeRequest);
    blimartSubscriptionChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemPickupPointService).updateSubscriptionFlag(blimartSubscriptionChangeRequest);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlimartSubscriptionChangeRequest.class);
  }

  @Test
  public void onDomainEventConsumed_expectException() throws Exception {
    blimartSubscriptionChangeRequest.setItemSku(StringUtils.EMPTY);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlimartSubscriptionChangeRequest.class))
      .thenReturn(blimartSubscriptionChangeRequest);
    blimartSubscriptionChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlimartSubscriptionChangeRequest.class);
  }
}
