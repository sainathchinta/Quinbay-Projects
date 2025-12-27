package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByCampaignProductEndedListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnCampaignProductEndedEventSivaProduct_failed_campaignProductNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEnded, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)));
  }

  @Test
  public void testOnCampaignProductEndedEventSivaProduct_success_endFlashsale_endSub() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublishedSub, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertTrue(campaignProductResult.isActive());
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEnded, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      if (campaignProductResult.getSku().getSessionId()==123) {
        assertFalse(campaignProductResult.isActive());
      } else {
        assertTrue(campaignProductResult.isActive());
      }
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEndedSub, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertFalse(campaignProductResult.isActive());
    }
  }

  @Test
  public void testOnCampaignProductEndedEventSivaProduct_success_endSub_endFlashsale() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublishedSub, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertTrue(campaignProductResult.isActive());
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEndedSub, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      if (campaignProductResult.getSku().getSessionId()==789) {
        assertFalse(campaignProductResult.isActive());
      } else {
        assertTrue(campaignProductResult.isActive());
      }
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEnded, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertFalse(campaignProductResult.isActive());
    }
  }

  @Test
  public void testOnCampaignProductEndedEventSivaProduct_success_endFlashsale_endSpecial() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublishedSpecial, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertTrue(campaignProductResult.isActive());
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEnded, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      if (campaignProductResult.getPriority()==CampaignPriority.FLASH_SALE) {
        assertFalse(campaignProductResult.isActive());
      } else {
        assertTrue(campaignProductResult.isActive());
      }
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEndedSpecial, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertFalse(campaignProductResult.isActive());
    }
  }

  @Test
  public void testOnCampaignProductEndedEventSivaProduct_success_endSpecial() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublishedSpecial, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertTrue(campaignProductResult.isActive());
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEndedSpecial, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      if (campaignProductResult.getPriority()==CampaignPriority.SPECIAL) {
        assertFalse(campaignProductResult.isActive());
      } else {
        assertTrue(campaignProductResult.isActive());
      }
    }

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProduct.toId(), campaignProductEnded, EXTRA_TIME, INDICES);

    for (CampaignProduct campaignProductResult : getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)) {
      assertNotNull(campaignProductResult);
      assertFalse(campaignProductResult.isActive());
    }
  }

}
