package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleGroup;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateDeactivateSivaFlashsaleGroupListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleGroup.setTimestamp(0L);
  }

  @Test
  public void testOnDeactivateSivaFlashsaleGroupEventDirectUpdate_failed_notFound() throws Exception {
    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_GROUP, flashsaleProduct.getId(), flashsaleProduct, LESS_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnDeactivateSivaFlashsaleGroupEventDirectUpdate_failed_doesntHaveSchedule() throws Exception {
    flashsaleProduct.setSchedule(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_GROUP, flashsaleProduct.getId(), flashsaleProduct, LESS_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnDeactivateSivaFlashsaleGroupEventDirectUpdate_failed_doesntHaveGroupIds() throws Exception {
    flashsaleProduct.setGroupIds(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_GROUP, flashsaleProduct.getId(), flashsaleProduct, LESS_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_FLASHSALE_GROUP,SivaFlashsaleGroup.class)));
  }

  @Test
  public void testOnDeactivateSivaFlashsaleGroupEventDirectUpdate_success() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_GROUP, flashsaleProduct.getId(), flashsaleProduct, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnDeactivateSivaFlashsaleGroupEventDirectUpdate_success_doesntHaveSchedule() throws Exception {
    sivaFlashsaleGroup.setSchedules(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.DEACTIVATE_FLASHSALE_GROUP, flashsaleProduct.getId(), flashsaleProduct, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
  }

}
