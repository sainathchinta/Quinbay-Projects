package com.gdn.aggregate.platform.module.product.listener.listener.oneP;

import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaItemConstructorV2;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.util.ReflectionTestUtils;

@SpringBootTest(properties = { "pickup.point.one-party-activated=true" })
public class DirectUpdate1PNotAvailableIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Autowired
  SivaItemConstructorV2 sivaItemConstructorV2;

  @Test
  public void testOnPickupPointEvent_1P_13_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000002", 1435, "ABC-12345-12345-00002-PP-0000002", 1435,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000002", 1435, "ABC-12345-12345-00002-PP-0000002", 1435);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000001", 1435, "ABC-12345-12345-00002-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000001", 1435, "ABC-12345-12345-00002-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_13_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1435, "ABC-12345-12345-00001-PP-0000001", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000002", 1410, "ABC-12345-12345-00002-PP-0000002", 1410,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000002", 1410, "ABC-12345-12345-00002-PP-0000002", 1410);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000001", 1410, "ABC-12345-12345-00002-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000001", 1410, "ABC-12345-12345-00002-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1410, "ABC-12345-12345-00001-PP-0000002", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1410, "ABC-12345-12345-00001-PP-0000002", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
  }

  @Test
  public void testOnPickupPointEvent_1P_14_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1410, "ABC-12345-12345-00001-PP-0000001", 1410);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1410, "ABC-12345-12345-00001-PP-0000002", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1410, "ABC-12345-12345-00001-PP-0000002", 1410);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_15() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000002", 1325, "ABC-12345-12345-00002-PP-0000002", 1325,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000002", 1325, "ABC-12345-12345-00002-PP-0000002", 1325);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000001", 1325, "ABC-12345-12345-00002-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00002-PP-0000001", 1325, "ABC-12345-12345-00002-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1325, "ABC-12345-12345-00001-PP-0000002", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1325, "ABC-12345-12345-00001-PP-0000002", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
  }

  @Test
  public void testOnPickupPointEvent_1P_15_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000001", 1325, "ABC-12345-12345-00001-PP-0000001", 1325);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_1() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, true, PurchasedType.ONLINE_CNC, 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4185, "ABC-12345-12345-00001-PP-0000002", 4185);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_2() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, true, false, PurchasedType.ONLINE_CNC, 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4175, "ABC-12345-12345-00001-PP-0000002", 4175);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_3() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, true, PurchasedType.ONLINE_CNC, 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 4160, "ABC-12345-12345-00001-PP-0000002", 4160);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_4() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, true, false, PurchasedType.ONLINE_CNC, 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 4150, "ABC-12345-12345-00001-PP-0000002", 4150);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_5() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, true, PurchasedType.ONLINE, 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3135, "ABC-12345-12345-00001-PP-0000002", 3135);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_6() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, true, false, false, PurchasedType.ONLINE, 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3125, "ABC-12345-12345-00001-PP-0000002", 3125);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_7() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, true, PurchasedType.ONLINE, 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3110, "ABC-12345-12345-00001-PP-0000002", 3110);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_8() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        true, false, false, false, PurchasedType.ONLINE, 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 3100, "ABC-12345-12345-00001-PP-0000002", 3100);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_9() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, true, PurchasedType.CNC, 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2085, "ABC-12345-12345-00001-PP-0000002", 2085);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_10() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, true, false, PurchasedType.CNC, 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2075, "ABC-12345-12345-00001-PP-0000002", 2075);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_11() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, true, PurchasedType.CNC, 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060,
        true, true, true,true, "ABC-12345-12345-00001-PP-0000002", 2060, "ABC-12345-12345-00001-PP-0000002", 2060);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_12() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, true, false, PurchasedType.CNC, 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050,
        true, true, false,true, "ABC-12345-12345-00001-PP-0000002", 2050, "ABC-12345-12345-00001-PP-0000002", 2050);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_13() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, true, PurchasedType.NOT_AVAILABLE, 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1435, "ABC-12345-12345-00001-PP-0000002", 1435);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_14() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, true, PurchasedType.NOT_AVAILABLE, 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1410, "ABC-12345-12345-00001-PP-0000002", 1410,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1410, "ABC-12345-12345-00001-PP-0000002", 1410);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_15() throws Exception {
    ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields", false);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, true, false, false, PurchasedType.NOT_AVAILABLE, 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1325, "ABC-12345-12345-00001-PP-0000002", 1325,
        false, false, false,true, "ABC-12345-12345-00001-PP-0000002", 1325, "ABC-12345-12345-00001-PP-0000002", 1325);
  }

  @Test
  public void testOnPickupPointEvent_1P_16_16() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, SLEEP_TIME_1P, INDICES);

    verify1P("ABC-12345-12345-00002", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 1000, "ABC-12345-12345-00002-PP-0000002", 1000,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000002", 1000, "ABC-12345-12345-00002-PP-0000002", 1000);
    verify1P("ABC-12345-12345-00002", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 1000, "ABC-12345-12345-00002-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00002-PP-0000001", 1000, "ABC-12345-12345-00002-PP-0000001", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000002",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 1000, "ABC-12345-12345-00001-PP-0000002", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000002", 1000, "ABC-12345-12345-00001-PP-0000002", 1000);
    verify1P("ABC-12345-12345-00001", "PP-0000001",
        false, false, false, false, PurchasedType.NOT_AVAILABLE, 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000,
        false, false, false,false, "ABC-12345-12345-00001-PP-0000001", 1000, "ABC-12345-12345-00001-PP-0000001", 1000);
  }

}
