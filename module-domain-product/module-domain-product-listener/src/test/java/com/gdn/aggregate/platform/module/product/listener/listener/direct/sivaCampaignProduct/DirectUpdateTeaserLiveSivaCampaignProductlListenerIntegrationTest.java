package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaCampaignProduct;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DirectUpdateTeaserLiveSivaCampaignProductlListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    sivaProduct.setPrice(null);
    sivaProduct.setPriceTeaser(null);
    sivaProduct.setImage(null);
    sivaProduct.setFlashsaleItemSkus(null);
    sivaProduct.setFlashsale(null);
    sivaProduct.setAdjustments(null);
    sivaProduct.setInventory(null);
    sivaProduct.setFlashsaleInventory(null);
    sivaProduct.setCampaignCodes(null);

    sivaItem.setItemPrice(null);
    sivaItem.setItemPriceTeaser(null);
    sivaItem.setPrice(null);
    sivaItem.setPriceTeaser(null);
    sivaItem.setProductImage(null);
    sivaItem.setItemImage(null);
    sivaItem.setFlashsaleItemSku(null);
    sivaItem.setFlashsale(null);
    sivaItem.setAdjustments(null);
    sivaItem.setInventory(null);
    sivaItem.setFlashsaleInventory(null);
    sivaItem.setCampaignCodes(null);
  }

  @Test
  public void testOnCampaignTeaserLiveEventSivaCampaignProduct_failed_processedDataNotExists() throws Exception {
    campaignProduct.setTimestamp(0L);
    campaignProduct.setCampaignCode("any");
    campaignProduct.setId(campaignProduct.toId());
    campaignProduct.setTagLabel("old");
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);
    assertEquals(0L,campaignProductMongoResult.getTimestamp());
    assertEquals("any",campaignProductMongoResult.getCampaignCode());
    assertEquals("old",campaignProductMongoResult.getTagLabel());

    campaignTeaserLive.setTimestamp(currentTimestamp);
    campaignTeaserLive.setTagLabel("new");
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE, campaignTeaserLive.toId(), campaignTeaserLive, NORMAL_TIME, INDICES);

    campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);
    assertEquals(0L,campaignProductMongoResult.getTimestamp());
    assertEquals("old",campaignProductMongoResult.getTagLabel());
  }

  @Test
  public void testOnCampaignTeaserLiveEventSivaCampaignProduct_success() throws Exception {
    campaignProduct.setTimestamp(0L);
    campaignProduct.setCampaignCode(campaignTeaserLive.getCampaignCode());
    campaignProduct.setId(campaignProduct.toId());
    campaignProduct.setTagLabel("old");
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);
    assertEquals(0L,campaignProductMongoResult.getTimestamp());
    assertEquals(campaignTeaserLive.getCampaignCode(),campaignProductMongoResult.getCampaignCode());
    assertEquals("old",campaignProductMongoResult.getTagLabel());

    campaignTeaserLive.setTimestamp(currentTimestamp);
    campaignTeaserLive.setTagLabel("new");
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE, campaignTeaserLive.toId(), campaignTeaserLive, NORMAL_TIME, INDICES);

    campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);
    assertEquals(currentTimestamp,campaignProductMongoResult.getTimestamp());
    assertEquals("new",campaignProductMongoResult.getTagLabel());
  }

}
