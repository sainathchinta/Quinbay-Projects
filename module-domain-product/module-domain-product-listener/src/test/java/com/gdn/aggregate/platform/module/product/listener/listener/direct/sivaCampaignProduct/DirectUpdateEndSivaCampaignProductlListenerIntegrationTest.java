package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaCampaignProduct;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateEndSivaCampaignProductlListenerIntegrationTest extends DummyHelper {

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
  public void testOnCampaignProductLiveEventSivaCampaignProduct_success() throws Exception {
    sivaCampaignProduct.setTimestamp(0L);
    sivaCampaignProduct.setActive(true);
    save(SaveRequest.builder()
        .index(Collections.SIVA_CAMPAIGN_PRODUCT)
        .domain(sivaCampaignProduct)
        .clazz(SivaCampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    SivaCampaignProduct sivaCampaignProductMongoResult = getFromMongo(Collections.SIVA_CAMPAIGN_PRODUCT,sivaCampaignProduct.toId(),SivaCampaignProduct.class);
    assertNotNull(sivaCampaignProductMongoResult);
    assertTrue(sivaCampaignProductMongoResult.isActive());

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_ENDED, campaignProductEnded.toId(), campaignProductEnded, MAX_TIME, INDICES);

    sivaCampaignProductMongoResult = getFromMongo(Collections.SIVA_CAMPAIGN_PRODUCT,sivaCampaignProduct.toId(),SivaCampaignProduct.class);
    assertNotNull(sivaCampaignProductMongoResult);
    assertFalse(sivaCampaignProductMongoResult.isActive());
  }

}
