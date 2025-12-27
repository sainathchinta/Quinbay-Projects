package com.gdn.aggregate.platform.module.product.listener.listener.oneP;

import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(properties = { "pickup.point.one-party-activated=true" })
public class DirectUpdate1POnlineIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnPickupPointEvent_1P_5_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3135, "ABC-12345-12345-00002-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3135, "ABC-12345-12345-00002-PP-0000002", 3135);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3135, "ABC-12345-12345-00002-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3135, "ABC-12345-12345-00002-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_5_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3135, "ABC-12345-12345-00001-PP-0000001", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3125, "ABC-12345-12345-00002-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3125, "ABC-12345-12345-00002-PP-0000002", 3125);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3125, "ABC-12345-12345-00002-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3125, "ABC-12345-12345-00002-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_6_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3125, "ABC-12345-12345-00001-PP-0000001", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3110, "ABC-12345-12345-00002-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3110, "ABC-12345-12345-00002-PP-0000002", 3110);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3110, "ABC-12345-12345-00002-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3110, "ABC-12345-12345-00002-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_7_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3110, "ABC-12345-12345-00001-PP-0000001", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3100, "ABC-12345-12345-00002-PP-0000002", 3100,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 3100, "ABC-12345-12345-00002-PP-0000002", 3100);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3100, "ABC-12345-12345-00002-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 3100, "ABC-12345-12345-00002-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_8_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 3100, "ABC-12345-12345-00001-PP-0000001", 3100);
  }

}
