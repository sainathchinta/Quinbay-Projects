package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleSchedule;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateDeleteExpiredSivaFlashsaleScheduleListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleSchedule.setTimestamp(0L);
  }

  @Test
  public void testOnDeleteExpiredSivaFlashsaleScheduleEventDirectUpdate_failed() throws Exception {
    sivaFlashsaleSchedule.setExpiryTime(futureTimestamp);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,sivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));

    sivaFlashsaleScheduleDeleteAllExpired.setExpiryTime(currentTimestamp);
    publishAndRefreshMultiple(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_SCHEDULE, sivaFlashsaleScheduleDeleteAllExpired.getId(), sivaFlashsaleScheduleDeleteAllExpired, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,sivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));
  }

  @Test
  public void testOnDeleteExpiredSivaFlashsaleScheduleEventDirectUpdate_success() throws Exception {
    sivaFlashsaleSchedule.setExpiryTime(pastTimestamp);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,sivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));

    sivaFlashsaleScheduleDeleteAllExpired.setExpiryTime(currentTimestamp);
    publishAndRefreshMultiple(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_SCHEDULE, sivaFlashsaleScheduleDeleteAllExpired.getId(), sivaFlashsaleScheduleDeleteAllExpired, NORMAL_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,sivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));
  }

}
