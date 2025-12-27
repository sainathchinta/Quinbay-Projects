package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleSchedule;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public class DirectUpdateSivaFlashsaleScheduleListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleSchedule.setTimestamp(0L);
    sivaFlashsaleSchedule.setLogo(null);
    sivaFlashsaleSchedule.setBackground(null);
  }

  @Test
  public void testOnViewSivaFlashsaleScheduleEvent_failed_notFound() throws Exception {
    String existingId = sivaFlashsaleSchedule.toId();
    String notFoundId = "any";
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    flashsaleSchedule.setId(notFoundId);
    publishAndRefreshMultiple(Topics.FLASHSALE_SCHEDULE, notFoundId, flashsaleSchedule, LESS_TIME, INDICES);

    SivaFlashsaleSchedule sivaFlashsaleScheduleExistingResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,notFoundId,SivaFlashsaleSchedule.class);
    assertNull(sivaFlashsaleScheduleExistingResult);

    SivaFlashsaleSchedule sivaFlashsaleScheduleNotFoundResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,existingId,SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleNotFoundResult);
    assertNull(sivaFlashsaleScheduleNotFoundResult.getLogo());
    assertNull(sivaFlashsaleScheduleNotFoundResult.getBackground());
  }

  @Test
  public void testOnViewSivaFlashsaleScheduleEvent_success() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.FLASHSALE_SCHEDULE, flashsaleSchedule.getId(), flashsaleSchedule, NORMAL_TIME, INDICES);

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleSchedule.toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground().getImageUrl());
    assertEquals(sivaFlashsaleScheduleMongoResult.getBackground().getImageUrl(), flashsaleSchedule.getBackground().getImageUrl());
  }

}
