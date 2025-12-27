package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignOwner;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByAdjustmentProductSavedListenerIntegrationTest extends DummyHelper {

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
  public void testOnAdjustmentProductSavedEvent_failed_adjustmentProductQuotaNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductSaved.toId(),AdjustmentProductQuota.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductSaved.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductSaved.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductSavedEvent_failed_chainDataNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductSaved.toId(),AdjustmentProductQuota.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductSaved.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductSaved.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductSavedEvent_success_processedDataNotExists() throws Exception {
    adjustmentProductSaved.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    adjustmentProductSaved.setPriority(1);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);

    AdjustmentProductQuota adjustmentProductQuotaResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaResult);
    assertEquals(CampaignPriority.FLASH_SALE,adjustmentProductQuotaResult.getPriority());

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductQuota.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductQuota.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getAdjustments()));
    assertEquals(20,sivaItemMongoResult.getInventory().getQuota());
    assertEquals(14,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaItemMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignCodes()));
  }

  @Test
  public void testOnAdjustmentProductSavedEvent_success_blibli() throws Exception {
    adjustmentProductSaved.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductSaved.toId(),AdjustmentProductQuota.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductQuota.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductQuota.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getAdjustments()));
    assertEquals(20,sivaItemMongoResult.getInventory().getQuota());
    assertEquals(14,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaItemMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignCodes()));
  }

  @Test
  public void testOnAdjustmentProductSavedEvent_success_merchant() throws Exception {
    adjustmentProductSaved.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductSaved.toId(),AdjustmentProductQuota.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductQuota.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertEquals(50,sivaProductMongoResult.getCampaignInfos().values().stream().filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode())).findFirst().orElse(null).getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductQuota.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getAdjustments()));
    assertEquals(20,sivaItemMongoResult.getInventory().getQuota());
    assertEquals(14,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertEquals(50,sivaItemMongoResult.getCampaignInfos().values().stream().filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode())).findFirst().orElse(null).getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaItemMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignCodes()));
  }

  @Test
  public void testOnAdjustmentProductSavedEventForBlackListSellers() throws Exception {
    adjustmentProductSaved.setItemSku("ALS-60070");
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved,
        EXTRA_TIME, INDICES);
    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT, SivaProduct.class).stream().filter(
            val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductSaved.getItemSku().equals(itemSku)))
        .findFirst().orElse(null);
    assertNull(sivaProductMongoResult);
    SivaItem sivaItemMongoResult =
        getFromMongo(Collections.SIVA_ITEM, adjustmentProductSaved.getItemSku(), SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

}
