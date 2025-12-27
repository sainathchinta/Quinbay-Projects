package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleGroup;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateSivaFlashsaleGroupListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleGroup.setTimestamp(0L);
  }

  @Test
  public void testOnViewSivaFlashsaleGroupEvent_success_notFound() throws Exception {
    publishAndRefreshMultiple(Topics.FLASHSALE_GROUP, sivaFlashsaleGroup.getId(), sivaFlashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnViewSivaFlashsaleGroupEvent_success_hasSchedules() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.FLASHSALE_GROUP, flashsaleGroup.getGroupId(), flashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnViewSivaFlashsaleGroupEvent_success_doesntHaveSchedules() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_SCHEDULE)
        .domain(sivaFlashsaleSchedule)
        .clazz(SivaFlashsaleSchedule.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.FLASHSALE_GROUP, flashsaleGroup.getGroupId(), flashsaleGroup, LESS_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

}
