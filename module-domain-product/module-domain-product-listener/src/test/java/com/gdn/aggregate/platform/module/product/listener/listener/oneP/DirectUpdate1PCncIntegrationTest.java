package com.gdn.aggregate.platform.module.product.listener.listener.oneP;

import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(properties = { "pickup.point.one-party-activated=true" })
public class DirectUpdate1PCncIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnPickupPointEvent_1P_9_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 2085, "ABC-12345-12345-00002-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 2085, "ABC-12345-12345-00002-PP-0000002", 2085);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 2085, "ABC-12345-12345-00002-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 2085, "ABC-12345-12345-00002-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_9_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2085, "ABC-12345-12345-00001-PP-0000001", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 2075, "ABC-12345-12345-00002-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 2075, "ABC-12345-12345-00002-PP-0000002", 2075);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 2075, "ABC-12345-12345-00002-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 2075, "ABC-12345-12345-00002-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_10_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2075, "ABC-12345-12345-00001-PP-0000001", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 2060, "ABC-12345-12345-00002-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 2060, "ABC-12345-12345-00002-PP-0000002", 2060);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 2060, "ABC-12345-12345-00002-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 2060, "ABC-12345-12345-00002-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_11_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 2060, "ABC-12345-12345-00001-PP-0000001", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 2050, "ABC-12345-12345-00002-PP-0000002", 2050,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 2050, "ABC-12345-12345-00002-PP-0000002", 2050);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 2050, "ABC-12345-12345-00002-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 2050, "ABC-12345-12345-00002-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_12_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 2050, "ABC-12345-12345-00001-PP-0000001", 2050);
  }

}
