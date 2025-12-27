package com.gdn.aggregate.platform.module.product.listener.listener.oneP;

import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@SpringBootTest(properties = { "pickup.point.one-party-activated=true" })
public class DirectUpdate1POnlineCncIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnPickupPointEvent_1P_1_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 4185, "ABC-12345-12345-00002-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 4185, "ABC-12345-12345-00002-PP-0000002", 4185);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 4185, "ABC-12345-12345-00002-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 4185, "ABC-12345-12345-00002-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_1_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4185, "ABC-12345-12345-00001-PP-0000001", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 4175, "ABC-12345-12345-00002-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 4175, "ABC-12345-12345-00002-PP-0000002", 4175);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 4175, "ABC-12345-12345-00002-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 4175, "ABC-12345-12345-00002-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_2_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4175, "ABC-12345-12345-00001-PP-0000001", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 4160, "ABC-12345-12345-00002-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000002", 4160, "ABC-12345-12345-00002-PP-0000002", 4160);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 4160, "ABC-12345-12345-00002-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00002-PP-0000001", 4160, "ABC-12345-12345-00002-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_3_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4160, "ABC-12345-12345-00001-PP-0000001", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 4150, "ABC-12345-12345-00002-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000002", 4150, "ABC-12345-12345-00002-PP-0000002", 4150);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 4150, "ABC-12345-12345-00002-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00002-PP-0000001", 4150, "ABC-12345-12345-00002-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_4_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000001", 4150, "ABC-12345-12345-00001-PP-0000001", 4150);
  }

}
