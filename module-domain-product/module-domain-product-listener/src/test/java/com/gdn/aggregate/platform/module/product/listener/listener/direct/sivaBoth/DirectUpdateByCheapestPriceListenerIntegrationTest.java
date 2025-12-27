package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
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
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByCheapestPriceListenerIntegrationTest extends DummyHelper {

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
  public void testOnCheapestPriceEvent_failed_idNotValid() throws Exception {
    cheapestPrice.setCampaignCode(null);
    cheapestPrice.setTimestamp(0L);

    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnCheapestPriceEvent_failed_timestampNotValid() throws Exception {
    cheapestPriceDay.setTimestamp(currentTimestamp);
    cheapestPriceDay.setDays(7);
    save(SaveRequest.builder()
        .index(Collections.CHEAPEST_PRICE_DAY)
        .domain(cheapestPriceDay)
        .clazz(CheapestPriceDay.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    CheapestPriceDay cheapestPriceDayMongoResult = getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class);
    assertNotNull(cheapestPriceDayMongoResult);
    assertNotEquals(0L,cheapestPriceDayMongoResult.getTimestamp());
    assertNotEquals(0L,cheapestPriceDayMongoResult.getDays().longValue());

    cheapestPrice.setTimestamp(0L);
    cheapestPrice.getSkuCheapestPriceDetails()
        .stream()
        .filter(Objects::nonNull)
        .forEach(val -> val.setCheapestPriceDays(0));
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    cheapestPriceDayMongoResult = getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class);
    assertNotNull(cheapestPriceDayMongoResult);
    assertNotEquals(0L,cheapestPriceDayMongoResult.getTimestamp());
    assertNotEquals(0L,cheapestPriceDayMongoResult.getDays().longValue());
  }

  @Test
  public void testOnCheapestPriceEvent_failed_skuEmpty() throws Exception {
    cheapestPrice.setSkuCheapestPriceDetails(null);

    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.CHEAPEST_PRICE_DAY,CheapestPriceDay.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnCheapestPriceEvent_failed_flashsaleProductNotFound_itemNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnCheapestPriceEvent_failed_processedDataNotExists() throws Exception {
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
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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
  public void testOnCheapestPriceEvent_failed_expired() throws Exception {
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
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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
  public void testOnCheapestPriceEvent_success_active() throws Exception {
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
    campaignProduct.setPromotionStartTime(flashsaleProduct.getSchedule().getStart());
    campaignProduct.setPromotionEndTime(flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setExpiryTime(flashsaleProduct.getSchedule().getEnd()+TimeUnit.valueOf(TimeUnit.DAYS.name()).toMillis(7L));
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    CheapestPriceDay cheapestPriceDayResult = getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class);
    assertNotNull(cheapestPriceDayResult);
    assertEquals(campaignProduct.getExpiryTime(),cheapestPriceDayResult.getExpiryTime());
    assertNotNull(cheapestPriceDayResult.getPickupPointCode());
    assertNotNull(cheapestPriceDayResult.getExpiryTime());
    assertTrue(cheapestPriceDayResult.getExpiryTime()>campaignProduct.getPromotionEndTime());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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
  public void testOnCheapestPriceEvent_success_notActiveYetNotEnded() throws Exception {
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
    campaignProduct.setPromotionStartTime(currentTimestamp+500000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));
    assertEquals(campaignProduct.getExpiryTime(),getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class).getExpiryTime());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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
  public void testOnCheapestPriceEvent_success_active_multiple() throws Exception {
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
    campaignProduct.setPromotionStartTime(flashsaleProduct.getSchedule().getStart());
    campaignProduct.setPromotionEndTime(flashsaleProduct.getSchedule().getEnd());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));
    assertEquals(campaignProduct.getExpiryTime(),getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class).getExpiryTime());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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
  public void testOnCheapestPriceEvent_success_notActiveYetNotEnded_multiple() throws Exception {
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
    campaignProduct.setPromotionStartTime(currentTimestamp+500000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));
    assertEquals(campaignProduct.getExpiryTime(),getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class).getExpiryTime());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertEquals(sivaProductMongoResult.getPriceTeaser().getCampaignPrice(),campaignProductNewest.getSku().getCampaignPrice());
    assertEquals(sivaProductMongoResult.getPriceTeaser().getCampaignTag(),campaignProductNewest.getTagLabel());
    assertEquals(sivaProductMongoResult.getPriceTeaser().getCampaignCode(),campaignProductNewest.getCampaignCode());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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
  public void testOnCheapestPriceEvent_success_remove() throws Exception {
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
    sivaProduct.setPrice(invalidSivaProduct.getPrice());
    sivaProduct.setPriceTeaser(invalidSivaProduct.getPriceTeaser());
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
    campaignProduct.setPromotionStartTime(currentTimestamp+500000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    campaignProduct.setSku(sku);
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    cheapestPrice.getSkuCheapestPriceDetails().stream()
        .filter(Objects::nonNull)
        .forEach(val -> val.setCheapestPriceDays(null));

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.CHEAPEST_PRICE, cheapestPriceDay.toId(), cheapestPrice, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class));
    assertEquals(campaignProduct.getExpiryTime(),getFromMongo(Collections.CHEAPEST_PRICE_DAY,cheapestPriceDay.toId(),CheapestPriceDay.class).getExpiryTime());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,ModuleProductUtil.fromItemSkuToProductSku(cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku()),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPrice().getCheapestPriceDays());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestPrice.getSkuCheapestPriceDetails().iterator().next().getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
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

}
