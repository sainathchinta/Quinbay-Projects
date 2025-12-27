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
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByAdjustmentProductQuotaListenerIntegrationTest extends DummyHelper {

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
  public void testOnAdjustmentProductQuotaEvent_failed_idNotValid() throws Exception {
    adjustmentProductQuota.setAdjustmentProductId(null);
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,AdjustmentProductQuota.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnAdjustmentProductQuotaEvent_failed_timestampNotValid() throws Exception {
    adjustmentProductQuota.setTimestamp(currentTimestamp);
    adjustmentProductQuota.setAdjustmentName(adjustmentProduct.getAdjustmentName());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    AdjustmentProductQuota adjustmentProductQuotaMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaMongoResult);
    assertNotEquals(0L,adjustmentProductQuotaMongoResult.getTimestamp());
    assertNotEquals("any",adjustmentProductQuotaMongoResult.getAdjustmentName());

    adjustmentProductQuota.setTimestamp(0L);
    adjustmentProductQuota.setAdjustmentName("any");
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    adjustmentProductQuotaMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaMongoResult);
    assertNotEquals(0L,adjustmentProductQuotaMongoResult.getTimestamp());
    assertNotEquals("any",adjustmentProductQuotaMongoResult.getAdjustmentName());
  }

  @Test
  public void testOnAdjustmentProductQuotaEvent_failed_chainDataNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductQuota.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductQuota.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductQuotaEvent_success_processedDataNotExists() throws Exception {
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
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

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
  public void testOnAdjustmentProductQuotaEvent_racingCondition() throws Exception {
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
    adjustmentProduct.setStartDate(currentTimestamp - 1000000);
    adjustmentProduct.setEndDate(currentTimestamp + 1000000);
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setTimestamp(0L);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.setPromotionStartTime(currentTimestamp - 1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp + 1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(currentTimestamp - 1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp + 1000000);
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setTimestamp(1000L);
    publish(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, 0L);
    adjustmentProductQuota.setTimestamp(1000L);
    publish(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, 0L);
    sleep(EXTRA_TIME);

    AdjustmentProduct adjustmentProductResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductResult);
    assertTrue(adjustmentProductResult.isActivated());
    assertEquals(1000L,adjustmentProductResult.getTimestamp());
    AdjustmentProductQuota adjustmentProductQuotaResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaResult);
    assertNotEquals(1000L,adjustmentProductQuotaResult.getTimestamp());

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
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignInfoCodes()));

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
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfoCodes()));
  }

  @Test
  public void testOnAdjustmentProductQuotaEvent_seperateConsumerGroup() throws Exception {
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
    campaignProduct.setPriority(1);
    campaignProduct.setPromotionStartTime(currentTimestamp - 1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp + 1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(currentTimestamp - 1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp + 1000000);
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    adjustmentProductSaved.setPriority(1);
    adjustmentProductSaved.setId(adjustmentProduct.toId());
    adjustmentProductSaved.setStartDate(currentTimestamp - 1000000);
    adjustmentProductSaved.setEndDate(currentTimestamp + 1000000);
    adjustmentProductSaved.setActivated(true);
    adjustmentProductSaved.setQuota(10);
    adjustmentProductSaved.setTimestamp(0L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);
    adjustmentProductQuota.setPriority(1);
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(3);
    adjustmentProductQuota.setUsedQuotaPerTransaction(5);
    adjustmentProductQuota.setTimestamp(0L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId());
    adjustmentProduct.setStartDate(currentTimestamp - 1000000);
    adjustmentProduct.setEndDate(currentTimestamp + 1000000);
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setTimestamp(0L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProductQuota adjustmentProductQuotaResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaResult);
    assertEquals(1,adjustmentProductQuotaResult.getPriority());
    assertEquals(10,adjustmentProductQuotaResult.getQuota());
    assertEquals(3,adjustmentProductQuotaResult.getUsedQuota());
    assertEquals(5,adjustmentProductQuotaResult.getUsedQuotaPerTransaction());
    AdjustmentProduct adjustmentProductResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductResult);
    assertEquals(1,adjustmentProductResult.getPriority());

    SivaProduct sivaProductResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductResult.getAdjustments()));
    assertEquals(10,sivaProductResult.getAdjustments().iterator().next().getQuota());
    assertEquals(3,sivaProductResult.getAdjustments().iterator().next().getUsed());
    assertEquals(7,sivaProductResult.getAdjustments().iterator().next().getRemaining());

    SivaItem sivaItemResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemResult.getAdjustments()));
    assertEquals(10,sivaItemResult.getAdjustments().iterator().next().getQuota());
    assertEquals(3,sivaItemResult.getAdjustments().iterator().next().getUsed());
    assertEquals(7,sivaItemResult.getAdjustments().iterator().next().getRemaining());
  }

  @Test
  public void testOnAdjustmentProductQuotaEvent_verifyChooseMostRemaining() throws Exception {
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
    campaignProduct.setPriority(1);
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

    adjustmentProductQuota.setAdjustmentProductId(CampaignOwner.BLIBLI);
    adjustmentProductQuota.setBudgetOwner(CampaignOwner.BLIBLI);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(10);
    adjustmentProductQuota.setUsedQuotaPerTransaction(3);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);
    adjustmentProductQuota.setAdjustmentProductId(CampaignOwner.MERCHANT);
    adjustmentProductQuota.setBudgetOwner(CampaignOwner.MERCHANT);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(7);
    adjustmentProductQuota.setUsedQuotaPerTransaction(3);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProductQuota.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(1,sivaProductMongoResult.getAdjustments().size());
    assertEquals(CampaignOwner.MERCHANT,sivaProductMongoResult.getAdjustments().iterator().next().getOwner());
    assertEquals(7,sivaProductMongoResult.getAdjustments().iterator().next().getUsed());
    assertEquals(3,sivaProductMongoResult.getAdjustments().iterator().next().getRemaining());
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(30,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertEquals(30,sivaProductMongoResult.getCampaignInfos().values().stream().filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode())).findFirst().orElse(null).getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProductQuota.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getAdjustments()));
    assertEquals(1,sivaItemMongoResult.getAdjustments().size());
    assertEquals(CampaignOwner.MERCHANT,sivaItemMongoResult.getAdjustments().iterator().next().getOwner());
    assertEquals(7,sivaItemMongoResult.getAdjustments().iterator().next().getUsed());
    assertEquals(3,sivaItemMongoResult.getAdjustments().iterator().next().getRemaining());
    assertEquals(20,sivaItemMongoResult.getInventory().getQuota());
    assertEquals(14,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(30,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertEquals(30,sivaItemMongoResult.getCampaignInfos().values().stream().filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode())).findFirst().orElse(null).getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaItemMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignCodes()));
  }

  @Test
  public void testOnAdjustmentProductQuotaEvent_success_blibli() throws Exception {
    adjustmentProductQuota.setBudgetOwner(CampaignOwner.BLIBLI);
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
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

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
  public void testOnAdjustmentProductQuotaEvent_success_merchant() throws Exception {
    adjustmentProductQuota.setBudgetOwner(CampaignOwner.MERCHANT);
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

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
  public void testOnAdjustmentProductQuotaEvent_success_withCampaignCode() throws Exception {
    adjustmentProductQuota.setBudgetOwner(CampaignOwner.BLIBLI);
    adjustmentProductQuota.setCampaignCode(null);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProductQuota.setCampaignCode(adjustmentProduct.getCampaignCode());
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
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    AdjustmentProductQuota raw = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(raw.getCampaignCode());

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
  public void testOnAdjustmentProductQuotaEvent_success_noCampaignCode() throws Exception {
    adjustmentProductQuota.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProductQuota.setCampaignCode(null);
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
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    AdjustmentProductQuota raw = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductQuota.toId(),AdjustmentProductQuota.class);
    assertNotNull(raw.getCampaignCode());

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

}
