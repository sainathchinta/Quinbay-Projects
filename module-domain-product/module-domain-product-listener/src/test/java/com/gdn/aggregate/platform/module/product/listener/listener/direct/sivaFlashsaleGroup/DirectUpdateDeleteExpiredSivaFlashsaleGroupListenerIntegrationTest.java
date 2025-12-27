package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleGroup;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateDeleteExpiredSivaFlashsaleGroupListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleGroup.setTimestamp(0L);
  }

  @Test
  public void testOnDeleteExpiredSivaFlashsaleGroupEventDirectUpdate_failed() throws Exception {
    sivaFlashsaleGroup.setExpiryTime(futureTimestamp);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));

    sivaFlashsaleGroupDeleteAllExpired.setExpiryTime(currentTimestamp);
    publishAndRefreshMultiple(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_GROUP, sivaFlashsaleGroupDeleteAllExpired.getId(), sivaFlashsaleGroupDeleteAllExpired, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnDeleteExpiredSivaFlashsaleGroupEventDirectUpdate_success() throws Exception {
    sivaFlashsaleGroup.setExpiryTime(pastTimestamp);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));

    sivaFlashsaleGroupDeleteAllExpired.setExpiryTime(currentTimestamp);
    publishAndRefreshMultiple(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_GROUP, sivaFlashsaleGroupDeleteAllExpired.getId(), sivaFlashsaleGroupDeleteAllExpired, NORMAL_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

}
