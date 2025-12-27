package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteAllRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.properties.EventConsumptionProperties;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.EventListener;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByCampaignProductPublishedListenerIntegrationTest extends DummyHelper {

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
  public void testOnCampaignProductPublishedEvent_failed_idNotValid() throws Exception {
    campaignProductPublished.setCampaignCode(null);
    campaignProductPublished.setId(campaignProductPublished.toId());
    campaignProductPublished.setTimestamp(0L);

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnCampaignProductPublishedEvent_failed_timestampNotValid() throws Exception {
    campaignProduct.setTimestamp(currentTimestamp);
    campaignProduct.setExclusive(true);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);
    assertNotEquals(0L,campaignProductMongoResult.getTimestamp());
    assertTrue(campaignProductMongoResult.isExclusive());

    campaignProductPublished.setTimestamp(0L);
    campaignProductPublished.setExclusive(false);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);
    assertNotEquals(0L,campaignProductMongoResult.getTimestamp());
    assertTrue(campaignProductMongoResult.isExclusive());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_failed_skuEmpty() throws Exception {
    campaignProductPublished.setSkuList(null);

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.CAMPAIGN_PRODUCT,CampaignProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnCampaignProductPublishedEvent_failed_flashsaleProductNotFound_itemNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnCampaignProductPublishedEvent_failed_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_failed_expired() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
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
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_deactivate() throws Exception {
    campaignProduct.setCampaignType(CampaignType.FLASH_SALE);
    campaignProduct.setPriority(1);
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);

    //Iteration 1 : Empty Quota

    campaignProduct.setActive(true);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    assertTrue(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());
    campaignProductPublished.setEmptyQuota(true);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setMarkForDelete(false));
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertFalse(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());

    //Iteration 2 : Mark for Delete

    campaignProduct.setActive(true);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    assertTrue(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());
    campaignProductPublished.setEmptyQuota(false);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setMarkForDelete(true));
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertFalse(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());

    //Iteration 3 : Empty Quota & Mark for Delete

    campaignProduct.setActive(true);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    assertTrue(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());
    campaignProductPublished.setEmptyQuota(true);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setMarkForDelete(true));
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertFalse(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());

    //Iteration 4 : Active

    campaignProduct.setActive(false);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    assertFalse(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());
    campaignProductPublished.setEmptyQuota(false);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setMarkForDelete(false));
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class).isActive());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_active() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
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
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
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
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.setPriority(1);
    campaignProductPublished.setPromotionStartTime(currentTimestamp-1000000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductResult);
    assertNotNull(campaignProductResult.getSku().getPickupPointCode());
    assertNotNull(campaignProductResult.getExpiryTime());
    assertTrue(campaignProductResult.getExpiryTime()>campaignProductResult.getPromotionEndTime());

    FlashsaleProduct flashsaleProductResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(campaignProductPublished.getSkuList().iterator().next().getProductSku(),campaignProductPublished.getSkuList().iterator().next().getItemSku(),campaignProduct.getCampaignCode(),campaignProductPublished.getSkuList().iterator().next().isTimeBased()),FlashsaleProduct.class);
    assertNotNull(flashsaleProductResult);
    assertNotEquals(flashsaleProductResult.getTimestamp(), 0);
    assertEquals(flashsaleProductResult.getCampaignCode(), campaignProductPublished.getCampaignCode());
    assertNotNull(flashsaleProductResult.getSchedule());
    assertNotNull(flashsaleProductResult.getFlashsaleQuotas());
    assertNotNull(flashsaleProductResult.getGroupIds());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_notActiveYetNotEnded() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp+500000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(currentTimestamp+500000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
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
    campaignProductPublished.setPromotionStartTime(currentTimestamp+500000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductResult);
    assertNotNull(campaignProductResult.getSku().getPickupPointCode());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_active_multiple() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
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
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProductQuota.setItemSku(campaignProduct.getSku().getItemSku());
    adjustmentProductQuota.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProductQuota.setAdjustmentProductId(campaignProduct.getCampaignCode());
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProductQuota.setItemSku(campaignProductNewest.getSku().getItemSku());
    adjustmentProductQuota.setCampaignCode(campaignProductNewest.getCampaignCode());
    adjustmentProductQuota.setAdjustmentProductId(campaignProductNewest.getCampaignCode());
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
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
    campaignProductNewest.setPromotionStartTime(currentTimestamp-900000);
    campaignProductNewest.setPromotionEndTime(currentTimestamp+900000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProductNewest)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.setPriority(1);
    campaignProductPublished.setPromotionStartTime(currentTimestamp-1000000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));
    
    FlashsaleProduct flashsaleProductResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(campaignProductPublished.getSkuList().iterator().next().getProductSku(),campaignProductPublished.getSkuList().iterator().next().getItemSku(),campaignProduct.getCampaignCode(),campaignProductPublished.getSkuList().iterator().next().isTimeBased()),FlashsaleProduct.class);
    assertNotNull(flashsaleProductResult);
    assertNotEquals(flashsaleProductResult.getTimestamp(), 0);
    assertEquals(flashsaleProductResult.getCampaignCode(), campaignProductPublished.getCampaignCode());
    assertNotNull(flashsaleProductResult.getSchedule());
    assertNotNull(flashsaleProductResult.getFlashsaleQuotas());
    assertNotNull(flashsaleProductResult.getGroupIds());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_notActiveYetNotEnded_multiple() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp+500000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProductQuota.setItemSku(campaignProduct.getSku().getItemSku());
    adjustmentProductQuota.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProductQuota.setAdjustmentProductId(campaignProduct.getCampaignCode());
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProductQuota.setItemSku(campaignProductNewest.getSku().getItemSku());
    adjustmentProductQuota.setCampaignCode(campaignProductNewest.getCampaignCode());
    adjustmentProductQuota.setAdjustmentProductId(campaignProductNewest.getCampaignCode());
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
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
    campaignProductNewest.setPromotionStartTime(currentTimestamp+450000);
    campaignProductNewest.setPromotionEndTime(currentTimestamp+900000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProductNewest)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProductPublished.setPromotionStartTime(currentTimestamp+500000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));
    
    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertEquals(sivaProductMongoResult.getPriceTeaser().getCampaignPrice(),campaignProductNewest.getSku().getCampaignPrice());
    assertEquals(sivaProductMongoResult.getPriceTeaser().getCampaignTag(),campaignProductNewest.getTagLabel());
    assertEquals(sivaProductMongoResult.getPriceTeaser().getCampaignCode(),campaignProductNewest.getCampaignCode());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertEquals(sivaItemMongoResult.getItemPriceTeaser().getCampaignPrice(),campaignProductNewest.getSku().getCampaignPrice());
    assertEquals(sivaItemMongoResult.getItemPriceTeaser().getCampaignTag(),campaignProductNewest.getTagLabel());
    assertEquals(sivaItemMongoResult.getItemPriceTeaser().getCampaignCode(),campaignProductNewest.getCampaignCode());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertEquals(sivaItemMongoResult.getPriceTeaser().getCampaignPrice(),campaignProductNewest.getSku().getCampaignPrice());
    assertEquals(sivaItemMongoResult.getPriceTeaser().getCampaignTag(),campaignProductNewest.getTagLabel());
    assertEquals(sivaItemMongoResult.getPriceTeaser().getCampaignCode(),campaignProductNewest.getCampaignCode());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_doesnHaveCheapestPriceDays() throws Exception {
    campaignProductPublished.setPromotionStartTime(currentTimestamp+500000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProductPublished.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProductPublished.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
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
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
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
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_hasExistingFlashsaleSchedule() throws Exception {
    save(SaveRequest.builder()
      .index(Collections.CHEAPEST_PRICE_DAY)
      .domain(cheapestPriceDay)
      .clazz(CheapestPriceDay.class)
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
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT)
      .domain(adjustmentProduct)
      .clazz(AdjustmentProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
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
      .index(Collections.SIVA_FLASHSALE_SCHEDULE)
      .domain(sivaFlashsaleSchedule)
      .clazz(SivaFlashsaleSchedule.class)
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
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(true)
      .build());
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.setPriority(1);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductResult);
    assertNotNull(campaignProductResult.getSku().getPickupPointCode());
    assertNotNull(campaignProductResult.getExpiryTime());
    assertTrue(campaignProductResult.getExpiryTime()>campaignProductResult.getPromotionEndTime());

    FlashsaleProduct flashsaleProductResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(campaignProductPublished.getSkuList().iterator().next().getProductSku(),campaignProductPublished.getSkuList().iterator().next().getItemSku(),campaignProduct.getCampaignCode(),campaignProductPublished.getSkuList().iterator().next().isTimeBased()),FlashsaleProduct.class);
    assertNotNull(flashsaleProductResult);
    assertNotEquals(flashsaleProductResult.getTimestamp(), 0);
    assertEquals(flashsaleProductResult.getCampaignCode(), campaignProductPublished.getCampaignCode());
    assertNotNull(flashsaleProductResult.getSchedule());
    assertNotNull(flashsaleProductResult.getFlashsaleQuotas());
    assertNotNull(flashsaleProductResult.getGroupIds());

    SivaFlashsaleSchedule sivaFlashsaleScheduleResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,ModuleProductUtil.toSivaFlashsaleScheduleId(campaignProductPublished.getPromotionStartTime(),campaignProductPublished.getPromotionEndTime()), SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleResult);
    assertNotNull(sivaFlashsaleScheduleResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleResult.getSubFlashsaleUrl());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_hasMultipleSkuList() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
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
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
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
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
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
    campaignProductPublishedMultiple.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublishedMultiple.setPriority(1);
    campaignProductPublishedMultiple.setPromotionStartTime(currentTimestamp+500000);
    campaignProductPublishedMultiple.setPromotionEndTime(currentTimestamp+1000000);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublishedMultiple.toId(), campaignProductPublishedMultiple, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class));

    FlashsaleProduct flashsaleProductResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(campaignProductPublished.getSkuList().iterator().next().getProductSku(),campaignProductPublished.getSkuList().iterator().next().getItemSku(),campaignProduct.getCampaignCode(),campaignProductPublished.getSkuList().iterator().next().isTimeBased()),FlashsaleProduct.class);
    assertNotNull(flashsaleProductResult);
    assertNotEquals(flashsaleProductResult.getTimestamp(), 0);
    assertEquals(flashsaleProductResult.getCampaignCode(), campaignProductPublished.getCampaignCode());
    assertNotNull(flashsaleProductResult.getSchedule());
    assertNotNull(flashsaleProductResult.getFlashsaleQuotas());
    assertNotNull(flashsaleProductResult.getGroupIds());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotEquals(sivaProductMongoResult.getTimestamp(),0);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_updateFlashsaleProductRelated_mainExclusive() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),123,currentTimestamp-1000000,currentTimestamp+1000000,true,true,false,2000,CampaignPriority.FLASH_SALE);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .mongo(true)
        .build());
    campaignProductPublished.setPromotionStartTime(currentTimestamp-1000000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setTimeBased(false));

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(item.getProductSku(),item.getItemSku(),campaignProduct.getCampaignCode(),false),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertTrue(flashsaleProductMongoResult.isExclusive());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(sivaProductMongoResult.getFlashsale().isTimeBased());
    assertTrue(sivaProductMongoResult.getFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.getFlashsale().isTimeBased());
    assertTrue(sivaItemMongoResult.getFlashsale().isExclusive());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_updateFlashsaleProductRelated_mainNotExclusive() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),123,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,2000,CampaignPriority.FLASH_SALE);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .mongo(true)
        .build());
    campaignProductPublished.setPromotionStartTime(currentTimestamp-1000000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setTimeBased(false));

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(item.getProductSku(),item.getItemSku(),campaignProduct.getCampaignCode(),false),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertFalse(flashsaleProductMongoResult.isExclusive());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(sivaProductMongoResult.getFlashsale().isTimeBased());
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.getFlashsale().isTimeBased());
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_updateFlashsaleProductRelated_subExclusive() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),123,currentTimestamp-1000000,currentTimestamp+1000000,true,true,true,2000,CampaignPriority.FLASH_SALE);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .mongo(true)
        .build());
    campaignProductPublished.setPromotionStartTime(currentTimestamp-1000000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setTimeBased(true));

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(item.getProductSku(),item.getItemSku(),campaignProduct.getCampaignCode(),true),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertTrue(flashsaleProductMongoResult.isExclusive());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(sivaProductMongoResult.getSubFlashsale().isTimeBased());
    assertTrue(sivaProductMongoResult.getSubFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.getSubFlashsale().isTimeBased());
    assertTrue(sivaItemMongoResult.getSubFlashsale().isExclusive());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_success_updateFlashsaleProductRelated_subNotExclusive() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),123,currentTimestamp-1000000,currentTimestamp+1000000,true,false,true,2000,CampaignPriority.FLASH_SALE);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .mongo(true)
        .build());
    campaignProductPublished.setPromotionStartTime(currentTimestamp-1000000);
    campaignProductPublished.setPromotionEndTime(currentTimestamp+1000000);
    campaignProductPublished.setCampaignType(CampaignType.FLASH_SALE);
    campaignProductPublished.getSkuList().stream().filter(Objects::nonNull).forEach(val -> val.setTimeBased(true));

    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(), campaignProductPublished, EXTRA_TIME, INDICES);

    CampaignProduct campaignProductMongoResult = getFromMongo(Collections.CAMPAIGN_PRODUCT,campaignProduct.toId(),CampaignProduct.class);
    assertNotNull(campaignProductMongoResult);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,ModuleProductUtil.toFlashsaleProductId(item.getProductSku(),item.getItemSku(),campaignProduct.getCampaignCode(),true),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertFalse(flashsaleProductMongoResult.isExclusive());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,campaignProductPublished.getSkuList().iterator().next().getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(sivaProductMongoResult.getSubFlashsale().isTimeBased());
    assertFalse(sivaProductMongoResult.getSubFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,campaignProductPublished.getSkuList().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.getSubFlashsale().isTimeBased());
    assertFalse(sivaItemMongoResult.getSubFlashsale().isExclusive());
  }

  @Test
  public void testOnCampaignProductPublishedEvent_blacklistedSeller() {
    campaignProductPublished.getSkuList().getFirst().setProductSku("ALS-60070-123");
    campaignProductPublished.getSkuList().getFirst().setItemSku("ALS-60070-123-00001");
    while (campaignProductPublished.getSkuList().size() > 1) {
      campaignProductPublished.getSkuList().remove(1);
    }
    publishAndRefreshMultiple(Topics.CAMPAIGN_PRODUCT_PUBLISHED, campaignProductPublished.toId(),
        campaignProductPublished, EXTRA_TIME, INDICES);
    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.CAMPAIGN_PRODUCT, CampaignProduct.class)));
  }

}
