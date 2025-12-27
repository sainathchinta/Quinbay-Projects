package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaCampaignProduct;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateDeleteExpiredSivaCampaignProductListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaCampaignProduct.setTimestamp(0L);
  }

  @Test
  public void testOnDeleteExpiredSivaCampaignProductEventDirectUpdate_failed() throws Exception {
    sivaCampaignProduct.setExpiryTime(futureTimestamp);
    save(SaveRequest.builder()
        .index(Collections.SIVA_CAMPAIGN_PRODUCT)
        .domain(sivaCampaignProduct)
        .clazz(SivaCampaignProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    assertTrue(existsInMongo(Collections.SIVA_CAMPAIGN_PRODUCT,sivaCampaignProduct.toId(),SivaCampaignProduct.class));

    sivaCampaignProductDeleteAllExpired.setExpiryTime(currentTimestamp);
    publishAndRefreshMultiple(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_CAMPAIGN_PRODUCT, sivaCampaignProductDeleteAllExpired.getId(), sivaCampaignProductDeleteAllExpired, NORMAL_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_CAMPAIGN_PRODUCT,sivaCampaignProduct.toId(),SivaCampaignProduct.class));
  }

  @Test
  public void testOnDeleteExpiredSivaCampaignProductEventDirectUpdate_success() throws Exception {
    sivaCampaignProduct.setExpiryTime(pastTimestamp);
    save(SaveRequest.builder()
        .index(Collections.SIVA_CAMPAIGN_PRODUCT)
        .domain(sivaCampaignProduct)
        .clazz(SivaCampaignProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    assertTrue(existsInMongo(Collections.SIVA_CAMPAIGN_PRODUCT,sivaCampaignProduct.toId(),SivaCampaignProduct.class));

    sivaCampaignProductDeleteAllExpired.setExpiryTime(currentTimestamp);
    publishAndRefreshMultiple(Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_CAMPAIGN_PRODUCT, sivaCampaignProductDeleteAllExpired.getId(), sivaCampaignProductDeleteAllExpired, NORMAL_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_CAMPAIGN_PRODUCT,sivaCampaignProduct.toId(),SivaCampaignProduct.class));
  }

}
