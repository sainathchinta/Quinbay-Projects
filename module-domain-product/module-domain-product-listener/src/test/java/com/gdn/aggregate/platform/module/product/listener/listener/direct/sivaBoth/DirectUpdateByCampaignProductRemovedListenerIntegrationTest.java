package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

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

public class DirectUpdateByCampaignProductRemovedListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnCampaignProductRemovedEventSivaProduct_failed_campaignProductNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_REMOVED, campaignProduct.toId(), campaignProductRemoved, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)));
  }

  @Test
  public void testOnCampaignProductRemovedEventSivaProduct_success() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductResult);
    assertTrue(campaignProductResult.isActive());

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_REMOVED, campaignProduct.toId(), campaignProductRemoved, EXTRA_TIME, INDICES);

    campaignProductResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductResult);
    assertFalse(campaignProductResult.isActive());
  }

}