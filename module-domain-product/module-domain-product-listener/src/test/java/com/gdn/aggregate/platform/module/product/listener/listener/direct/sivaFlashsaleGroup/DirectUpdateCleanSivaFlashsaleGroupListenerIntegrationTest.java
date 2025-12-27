package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleGroup;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateCleanSivaFlashsaleGroupListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaFlashsaleGroup.setTimestamp(0L);
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_failed_notFound() throws Exception {
    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, LESS_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_failed_doesntHaveSchedules() throws Exception {
    sivaFlashsaleGroup.setSchedules(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
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

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, LESS_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_success_hasSivaProductEmptyGroup() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
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

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_success_doesntHaveSivaProduct() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_success_scheduleHasProductsDifferentGroup() throws  Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaProduct.getFlashsale().setGroupIds(java.util.Collections.singletonList("flashsaleGroupIdTwo"));
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_success_scheduleHasProductsSameGroup() throws  Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaProduct.getFlashsale().setGroupIds(java.util.Collections.singletonList("flashsaleGroupId"));
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

  @Test
  public void testOnCleanSivaFlashsaleGroupEventDirectUpdate_success_scheduleHasProductsSameGroupInactiveFlashsale() throws  Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaProduct.getFlashsale().setGroupIds(java.util.Collections.singletonList("flashsaleGroupId"));
    sivaProduct.getFlashsale().setActive(false);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.CLEAN_FLASHSALE_GROUP, idTimestampSivaFlashsaleGroup.getId(), idTimestampSivaFlashsaleGroup, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,idTimestampSivaFlashsaleGroup.toId(),SivaFlashsaleGroup.class));
  }

}
