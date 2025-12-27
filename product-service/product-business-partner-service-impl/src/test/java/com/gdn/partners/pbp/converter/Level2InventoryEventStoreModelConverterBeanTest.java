package com.gdn.partners.pbp.converter;

import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;
import com.gdn.partners.pbp.model.InventoryEventStatus;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;

public class Level2InventoryEventStoreModelConverterBeanTest {

  private static final String EVENT_NAME = "eventName";
  private static final String STORE_ID = "storeId";
  private static final String LEVEL2_ID = "level2Id";
  private static final String LEVEL2_MERCHANT_CODE = "level2MerchantCode";
  private static final Integer AVAILABLES_TOCK = 1;
  private static final Integer MINIMUM_STOCK = 1;
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @InjectMocks
  private Level2InventoryEventStoreModelConverterBean level2InventoryEventStoreModelConverterBean;

  private Date curDate;

  private Level2InventoryNonOosEvent level2InventoryNonOosEvent;
  private Level2InventoryOosEvent level2InventoryOosEvent;
  private Level2InventoryMinimumStockAlertEvent level2InventoryMinimumStockAlertEvent;
  private Level2InventoryEventStore level2InventoryEventStore;

  @BeforeEach
  public void _setup() {
    MockitoAnnotations.initMocks(this);

    curDate = new Date();

    level2InventoryNonOosEvent =
        new Level2InventoryNonOosEvent(STORE_ID, LEVEL2_ID, LEVEL2_MERCHANT_CODE, PICKUP_POINT_CODE);
    level2InventoryNonOosEvent.setTimestamp(curDate.getTime());

    level2InventoryOosEvent =
        new Level2InventoryOosEvent(STORE_ID, LEVEL2_ID, LEVEL2_MERCHANT_CODE, PICKUP_POINT_CODE);
    level2InventoryOosEvent.setTimestamp(curDate.getTime());

    level2InventoryMinimumStockAlertEvent = new Level2InventoryMinimumStockAlertEvent(STORE_ID,
        LEVEL2_ID, AVAILABLES_TOCK, MINIMUM_STOCK, LEVEL2_MERCHANT_CODE, PICKUP_POINT_CODE);
    level2InventoryMinimumStockAlertEvent.setTimestamp(curDate.getTime());

    level2InventoryEventStore = new Level2InventoryEventStore();
    level2InventoryEventStore.setStoreId(STORE_ID);
    level2InventoryEventStore.setEventTimestamp(curDate);
    level2InventoryEventStore.setLevel2Id(LEVEL2_ID);
    level2InventoryEventStore.setMerchantCode(LEVEL2_MERCHANT_CODE);
  }

  @AfterEach
  public void _teardown() {}

  @Test
  public void testConvertToLevel2InventoryEventStore_nonOOS() {
    Level2InventoryEventStore result = level2InventoryEventStoreModelConverterBean
        .convertToLevel2InventoryEventStore(level2InventoryNonOosEvent, EVENT_NAME);
    Assertions.assertNotNull(result);
  }

  @Test
  public void testConvertToLevel2InventoryEventStore_OOS() {
    Level2InventoryEventStore result = level2InventoryEventStoreModelConverterBean
        .convertToLevel2InventoryEventStore(level2InventoryOosEvent, EVENT_NAME);
    Assertions.assertNotNull(result);
  }

  @Test
  public void testConvertToLevel2InventoryEventStore_minStock_isBelowMinimumStock_false() {
    Level2InventoryEventStore result =
        level2InventoryEventStoreModelConverterBean.convertToLevel2InventoryEventStore(
            level2InventoryMinimumStockAlertEvent, EVENT_NAME, false);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(InventoryEventStatus.NON_BELOW_MINIMUM_STOCK, result.getInventoryStatus());
  }

  @Test
  public void testConvertToLevel2InventoryEventStore_minStock_isBelowMinimumStock_true() {
    Level2InventoryEventStore result =
        level2InventoryEventStoreModelConverterBean.convertToLevel2InventoryEventStore(
            level2InventoryMinimumStockAlertEvent, EVENT_NAME, true);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(InventoryEventStatus.BELOW_MINIMUM_STOCK, result.getInventoryStatus());
  }

  @Test
  public void testConvertToLevel2InventoryOosEvent() {
    Level2InventoryOosEvent result = level2InventoryEventStoreModelConverterBean
        .convertToLevel2InventoryOosEvent(level2InventoryEventStore);
    Assertions.assertNotNull(result);
  }

  @Test
  public void testConvertToLevel2InventoryNonOosEvent() {
    Level2InventoryNonOosEvent result = level2InventoryEventStoreModelConverterBean
        .convertToLevel2InventoryNonOosEvent(level2InventoryEventStore);
    Assertions.assertNotNull(result);
  }

  @Test
  public void testConvertToLevel2InventoryMinimumStockAlertEvent() {
    Level2InventoryMinimumStockAlertEvent result = level2InventoryEventStoreModelConverterBean
        .convertToLevel2InventoryMinimumStockAlertEvent(level2InventoryEventStore);
    Assertions.assertNotNull(result);
  }

}
