package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductType;
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
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByFlashsaleProductListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    sivaProduct.setPrice(null);
    sivaProduct.setPriceTeaser(null);
    sivaProduct.setImage(null);
    sivaProduct.setFlashsaleItemSkus(null);
    sivaProduct.setFlashsale(null);
    sivaProduct.setSubFlashsale(null);
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
    sivaItem.setSubFlashsale(null);
    sivaItem.setAdjustments(null);
    sivaItem.setInventory(null);
    sivaItem.setFlashsaleInventory(null);
    sivaItem.setCampaignCodes(null);
  }

  @Test
  public void testOnFlashsaleProductEvent_failed_timestampNotValid() throws Exception {
    flashsaleProduct.setTimestamp(currentTimestamp);
    flashsaleProduct.setActive(true);
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotEquals(0L,flashsaleProductMongoResult.getTimestamp());
    assertTrue(flashsaleProductMongoResult.isActive());

    flashsaleProduct.setTimestamp(0L);
    flashsaleProduct.setActive(false);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotEquals(0L,flashsaleProductMongoResult.getTimestamp());
    assertTrue(flashsaleProductMongoResult.isActive());
  }

  @Test
  public void testOnFlashsaleProductEvent_failed_processedDataNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);

    assertFalse(existsInMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class));

    assertFalse(existsInMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class));
  }

  @Test
  public void testOnFlashsaleProductEvent_failed_hasNoSchedule() throws Exception {
    flashsaleProduct.setSchedule(null);

    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,SivaFlashsaleSchedule.class)));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);
    assertTrue(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getSchedules()));

    assertFalse(existsInMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class));

    assertFalse(existsInMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class));
  }

  @Test
  public void testOnFlashsaleProductEvent_success_doesntHaveGroupIds() throws Exception {
    flashsaleProduct.setGroupIds(null);
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

    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_FLASHSALE_GROUP,SivaFlashsaleGroup.class)));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNull(sivaItemMongoResult.getProductImage());
    assertNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_processedDataOnly() throws Exception {
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

    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNull(sivaItemMongoResult.getProductImage());
    assertNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_retail() throws Exception {
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
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
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.REGULAR,flashsaleProductMongoResult.getProductType());

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());

    flashsaleSchedule.setId(flashsaleProduct.getSchedule().toId());
    publishAndRefreshMultiple(Topics.FLASHSALE_SCHEDULE, flashsaleSchedule.getId(), flashsaleSchedule, NORMAL_TIME, INDICES);

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    flashsaleProduct.getSchedule().setLogo(null);
    flashsaleProduct.getSchedule().setSubFlashsaleUrl(null);
    flashsaleProduct.getSchedule().setBackground(null);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_digital_main() throws Exception {
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    flashsaleProduct.getSchedule().setTimeBased(false);
    flashsaleProduct.setProductType(ProductType.DIGITAL);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.DIGITAL,flashsaleProductMongoResult.getProductType());

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertNull(sivaProductMongoResult.getSubFlashsale());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnFlashsaleProductEvent_success_digital_sub() throws Exception {
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    flashsaleProduct.getSchedule().setTimeBased(true);
    flashsaleProduct.setProductType(ProductType.DIGITAL);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.DIGITAL,flashsaleProductMongoResult.getProductType());

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getFlashsale());
    assertNotNull(sivaProductMongoResult.getSubFlashsale());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnFlashsaleProductEvent_success_digital_multiple() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    //Sub 1
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    flashsaleProduct.getSchedule().setTimeBased(true);
    flashsaleProduct.setProductType(ProductType.DIGITAL);
    flashsaleProduct.setCampaignCode("CPG-SUB-1");
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.DIGITAL,flashsaleProductMongoResult.getProductType());
    assertEquals("CPG-SUB-1",flashsaleProductMongoResult.getCampaignCode());

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getFlashsale());
    assertNotNull(sivaProductMongoResult.getSubFlashsale());
    assertEquals("CPG-SUB-1",sivaProductMongoResult.getSubFlashsale().getCampaignCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);

    //Sub 2
    flashsaleProduct.getSchedule().setStart(currentTimestamp-2000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    flashsaleProduct.getSchedule().setTimeBased(true);
    flashsaleProduct.setProductType(ProductType.DIGITAL);
    flashsaleProduct.setCampaignCode("CPG-SUB-2");
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.DIGITAL,flashsaleProductMongoResult.getProductType());
    assertEquals("CPG-SUB-2",flashsaleProductMongoResult.getCampaignCode());

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getFlashsale());
    assertNotNull(sivaProductMongoResult.getSubFlashsale());
    assertEquals("CPG-SUB-1",sivaProductMongoResult.getSubFlashsale().getCampaignCode());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);

    //Main 1
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    flashsaleProduct.getSchedule().setTimeBased(false);
    flashsaleProduct.setProductType(ProductType.DIGITAL);
    flashsaleProduct.setCampaignCode("CPG-MAIN-1");
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.DIGITAL,flashsaleProductMongoResult.getProductType());
    assertEquals("CPG-MAIN-1",flashsaleProductMongoResult.getCampaignCode());

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertEquals("CPG-MAIN-1",sivaProductMongoResult.getFlashsale().getCampaignCode());
    assertNull(sivaProductMongoResult.getSubFlashsale());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);

    //Main 2
    flashsaleProduct.getSchedule().setStart(currentTimestamp-2000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    flashsaleProduct.getSchedule().setTimeBased(false);
    flashsaleProduct.setProductType(ProductType.DIGITAL);
    flashsaleProduct.setCampaignCode("CPG-MAIN-2");
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));
    assertEquals(ProductType.DIGITAL,flashsaleProductMongoResult.getProductType());
    assertEquals("CPG-MAIN-2",flashsaleProductMongoResult.getCampaignCode());

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertEquals("CPG-MAIN-1",sivaProductMongoResult.getFlashsale().getCampaignCode());
    assertNull(sivaProductMongoResult.getSubFlashsale());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnFlashsaleProductEvent_success_quotaPriority() throws Exception {
    createCampaign("ABC-12345-12345-00001","PP-A",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-123",101,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(5);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002","PP-A",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-789",202,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(10);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002","PP-B",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-789",202,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(7);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002","PP-C",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-789",202,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(8);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002","PP-D",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-789",202,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(10);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00003","PP-E",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-000",303,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.SPECIAL);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(9);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00004","PP-F",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,null,currentTimestamp-2000000,currentTimestamp+1000000,true,false,false,1000,0);
    adjustmentProductQuota.setQuota(10);
    adjustmentProductQuota.setUsedQuota(9);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertEquals("ABC-12345-12345-00002",sivaProductMongoResult.getFlashsaleItemSkus().iterator().next());
    assertEquals("ABC-12345-12345-00002",sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals("PP-C",sivaProductMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(20,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals("PP-A",sivaItemMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(50,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00002",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals("PP-C",sivaItemMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(20,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00003",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNull(sivaItemMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertNull(sivaItemMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(0,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00004",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNull(sivaItemMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertNull(sivaItemMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(0,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_fbbActivated() throws Exception {
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
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
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_multipleNonActiveSchedules() throws Exception {
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
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
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, otherFlashsaleProduct.toId(), otherFlashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,otherFlashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,otherFlashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,otherFlashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.getGroupId(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);
    assertEquals(ModuleProductUtil.toSchedulesSize(sivaFlashsaleGroupMongoResult),0);
    assertFalse(ModuleProductUtil.isAllSchedulesActive(sivaFlashsaleGroupMongoResult));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_multipleActiveSchedules() throws Exception {
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    sivaFlashsaleGroup.getSchedules().stream()
        .filter(Objects::nonNull)
        .forEach(schedule -> {
          schedule.setStart(currentTimestamp-1000000);
          schedule.setEnd(currentTimestamp+1000000);
          schedule.setId(schedule.getStart()+"-"+schedule.getEnd());
        });
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
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_FLASHSALE_GROUP)
        .domain(sivaFlashsaleGroup)
        .clazz(SivaFlashsaleGroup.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, otherFlashsaleProduct.toId(), otherFlashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,otherFlashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,otherFlashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,otherFlashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,sivaFlashsaleGroup.getGroupId(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);
    assertNotEquals(ModuleProductUtil.toSchedulesSize(sivaFlashsaleGroupMongoResult),0);
    assertTrue(ModuleProductUtil.isAllSchedulesActive(sivaFlashsaleGroupMongoResult));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_flashsaleMoreUpdated() throws Exception {
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
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
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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
    sivaFlashsaleGroup.getSchedules().add(otherSivaFlashsaleSchedule);
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
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    flashsaleProduct.setTimestamp(currentTimestamp);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_adjustmentNotActive() throws Exception {
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
    campaignProduct.setActive(false);
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
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
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_adjustmentExpired() throws Exception {
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
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
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
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnFlashsaleProductEvent_success_denpasar() throws Exception {
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
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
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
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
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
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
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class));

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class).getTimestamp(),0L);

    assertTrue(existsInMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class));
    assertNotEquals(getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class).getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
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
  public void testOnFlashsaleProductEvent_success_default() throws Exception {
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
    adjustmentProduct.setStartDate(currentTimestamp - 1000000);
    adjustmentProduct.setEndDate(currentTimestamp + 1000000);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setId(adjustmentProduct.toId());
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
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
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
    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR, idTimestampProductLevel.toId(), idTimestampProductLevel, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getFlashsale());
    assertNull(sivaProductMongoResult.getSubFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getFlashsale());
    assertNull(sivaItemMongoResult.getSubFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getAdjustments()));
    assertEquals(20,sivaItemMongoResult.getInventory().getQuota());
    assertEquals(14,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaItemMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignCodes()));
  }

  @Test
  public void testOnSubFlashsaleProductEvent_success() throws Exception {
    flashsaleProduct.getSchedule().setLogo("logo");
    flashsaleProduct.getSchedule().setSubFlashsaleUrl("subFlashsaleUrl");
    flashsaleProduct.getSchedule().setTimeBased(true);
    flashsaleProduct.getSchedule().setStart(currentTimestamp-1000000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    flashsaleProduct.getSchedule().setId(flashsaleProduct.getSchedule().getStart()+"-"+flashsaleProduct.getSchedule().getEnd());
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
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
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
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
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
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    FlashsaleProduct flashsaleProductMongoResult = getFromMongo(Collections.FLASHSALE_PRODUCT,flashsaleProduct.toId(),FlashsaleProduct.class);
    assertNotNull(flashsaleProductMongoResult);
    assertNotNull(flashsaleProductMongoResult.getExpiryTime());
    assertTrue(flashsaleProductMongoResult.getExpiryTime()> ModuleProductUtil.getFlashsaleProductEnd(flashsaleProductMongoResult));

    SivaFlashsaleSchedule sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    SivaFlashsaleGroup sivaFlashsaleGroupMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_GROUP,flashsaleProduct.getGroupIds().iterator().next(),SivaFlashsaleGroup.class);
    assertNotNull(sivaFlashsaleGroupMongoResult);
    assertNotEquals(sivaFlashsaleGroupMongoResult.getTimestamp(),0L);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNull(sivaProductMongoResult.getFlashsale());
    assertNotNull(sivaProductMongoResult.getSubFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getAdjustments()));
    assertEquals(20,sivaProductMongoResult.getInventory().getQuota());
    assertEquals(14,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignCodes()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNull(sivaItemMongoResult.getFlashsale());
    assertNotNull(sivaItemMongoResult.getSubFlashsale());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getAdjustments()));
    assertEquals(20,sivaItemMongoResult.getInventory().getQuota());
    assertEquals(14,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(50,sivaItemMongoResult.getFlashsaleInventory().getQuota().getPercentage());
    assertNotNull(sivaItemMongoResult.getFlashsaleInventory().getSchedule());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignCodes()));

    flashsaleSchedule.setId(flashsaleProduct.getSchedule().toId());
    publishAndRefreshMultiple(Topics.FLASHSALE_SCHEDULE, flashsaleSchedule.getId(), flashsaleSchedule, NORMAL_TIME, INDICES);

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNotNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());

    flashsaleProduct.getSchedule().setLogo(null);
    flashsaleProduct.getSchedule().setSubFlashsaleUrl(null);
    flashsaleProduct.getSchedule().setBackground(null);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    sivaFlashsaleScheduleMongoResult = getFromMongo(Collections.SIVA_FLASHSALE_SCHEDULE,flashsaleProduct.getSchedule().toId(),SivaFlashsaleSchedule.class);
    assertNotNull(sivaFlashsaleScheduleMongoResult);
    assertNotEquals(sivaFlashsaleScheduleMongoResult.getTimestamp(),0L);
    assertNull(sivaFlashsaleScheduleMongoResult.getLogo());
    assertNull(sivaFlashsaleScheduleMongoResult.getSubFlashsaleUrl());
    assertNull(sivaFlashsaleScheduleMongoResult.getBackground());
    assertNotNull(sivaFlashsaleScheduleMongoResult.getExpiryTime());
    assertTrue(sivaFlashsaleScheduleMongoResult.getExpiryTime()>sivaFlashsaleScheduleMongoResult.getEnd());
  }

}
