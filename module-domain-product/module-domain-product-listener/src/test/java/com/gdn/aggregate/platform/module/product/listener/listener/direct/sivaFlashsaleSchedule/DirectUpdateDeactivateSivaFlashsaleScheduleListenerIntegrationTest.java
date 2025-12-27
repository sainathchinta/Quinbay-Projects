package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleSchedule;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateDeactivateSivaFlashsaleScheduleListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleSchedule.setTimestamp(0L);
  }

  @Test
  public void testOnDeactivateSivaFlashsaleScheduleEventDirectUpdate_failed_notFound() throws Exception {
    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_SCHEDULE, flashsaleProduct.getId(), flashsaleProduct, LESS_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().getId(),SivaFlashsaleSchedule.class));
  }

  @Test
  public void testOnDeactivateSivaFlashsaleScheduleEventDirectUpdate_failed_doesntHaveGroupIds() throws Exception {
    flashsaleProduct.setGroupIds(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_SCHEDULE, flashsaleProduct.getId(), flashsaleProduct, LESS_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,SivaFlashsaleSchedule.class)));
  }

  @Test
  public void testOnDeactivateSivaFlashsaleScheduleEventDirectUpdate_success() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_SCHEDULE, flashsaleProduct.getId(), flashsaleProduct, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().getId(),SivaFlashsaleSchedule.class));
  }

}
