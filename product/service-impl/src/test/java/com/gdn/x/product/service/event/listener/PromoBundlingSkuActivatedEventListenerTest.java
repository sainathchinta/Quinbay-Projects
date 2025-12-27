package com.gdn.x.product.service.event.listener;

import java.util.ArrayList;
import java.util.List;

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
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingSkuChanged;
import com.gdn.x.product.service.api.ItemService;

/**
 * Created by w.william on 12/11/2017.
 */
public class PromoBundlingSkuActivatedEventListenerTest {

  private static final String STORE_ID = "store-id";

  private static final String PROMO_BUNDLING_ID = "promo-bundling-id";

  private static final String PROMO_BUNDLING_TYPE = "Combo";

  private static final String MAIN_ITEM_SKU = "main-item-sku";
  private static final String MESSAGE = "message";

  @InjectMocks
  private PromoBundlingSkuActivatedEventListener promoBundlingSkuActivatedEventListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  private PromoBundlingSkuChanged promoBundlingSkuChanged;

  @Test
  public void onDomainEventConsumed_SuccessTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(promoBundlingSkuChanged);
    this.promoBundlingSkuActivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
    Mockito.verify(itemService)
        .updatePromoBundlingByItemSkusInItemPickupPoint(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.anySet(), ArgumentMatchers.eq(true));
  }

  @Test
  public void onDomainEventConsumed_SuccessWithoutComplementaryTest() throws Exception {
    promoBundlingSkuChanged.setSkuList(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(promoBundlingSkuChanged);
    this.promoBundlingSkuActivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
  }

  @Test
  public void onDomainEventConsumed_ThrowExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException())
        .when(itemService)
        .updatePromoBundlingByItemSkus(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.anySet(), ArgumentMatchers.eq(true));

    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingSkuChanged.class))
      .thenReturn(promoBundlingSkuChanged);
    this.promoBundlingSkuActivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingSkuChanged.class);
    Mockito.verify(itemService)
        .updatePromoBundlingByItemSkusInItemPickupPoint(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.anySet(), ArgumentMatchers.eq(true));
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);

    List<String> complementarySkus = new ArrayList<>();
    complementarySkus.add("sku");
    complementarySkus.add("sku1");
    complementarySkus.add("sku2");

    promoBundlingSkuChanged = new PromoBundlingSkuChanged();
    promoBundlingSkuChanged.setStoreId(STORE_ID);
    promoBundlingSkuChanged.setSkuList(complementarySkus);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
  }
}
