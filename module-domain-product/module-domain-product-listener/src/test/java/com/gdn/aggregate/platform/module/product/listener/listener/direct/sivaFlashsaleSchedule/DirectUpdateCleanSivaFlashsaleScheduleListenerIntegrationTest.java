package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleSchedule;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateCleanSivaFlashsaleScheduleListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleSchedule.setTimestamp(0L);
  }

  @Test
  public void testOnCleanSivaFlashsaleScheduleEventDirectUpdate_failed_notFound() throws Exception {
    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_SCHEDULE, idTimestampSivaFlashsaleSchedule.getId(), idTimestampSivaFlashsaleSchedule, LESS_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,idTimestampSivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleScheduleEventDirectUpdate_success_hasSivaProduct() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_SCHEDULE, idTimestampSivaFlashsaleSchedule.getId(), idTimestampSivaFlashsaleSchedule, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,idTimestampSivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleScheduleEventDirectUpdate_success_doesntHaveSivaProduct() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_SCHEDULE, idTimestampSivaFlashsaleSchedule.getId(), idTimestampSivaFlashsaleSchedule, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,idTimestampSivaFlashsaleSchedule.toId(),SivaFlashsaleSchedule.class));
  }

}
