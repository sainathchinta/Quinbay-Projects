package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteAllRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignOwner;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
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
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.PickupPointLocation;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;

import java.util.HashSet;
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

public class DirectUpdateByAdjustmentProductListenerIntegrationTest extends DummyHelper {

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
  public void testOnAdjustmentProductEvent_success_idNotValid() throws Exception {
    adjustmentProduct.setItemSku(null);
    adjustmentProduct.setPickupPointCode(null);
    adjustmentProduct.setPromoType(null);
    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setTimestamp(0L);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnAdjustmentProductEvent_idCheckers() throws Exception {
    adjustmentProduct.setPromoType("any");
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(1000L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    AdjustmentProduct adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.FLASH_SALE,adjustmentProductMongoResult.getPromoType());
    assertEquals(1,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setCampaignCode("CPG1");
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(500L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.FLASH_SALE,adjustmentProductMongoResult.getPromoType());
    assertEquals(1,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setCampaignCode("CPG2");
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(300L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.FLASH_SALE,adjustmentProductMongoResult.getPromoType());
    assertEquals(1,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    adjustmentProduct.setPromoType(CampaignType.SPECIAL);
    adjustmentProduct.setPriority(2);
    adjustmentProduct.setCampaignCode("CPG3");
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(250L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.SPECIAL,adjustmentProductMongoResult.getPromoType());
    assertEquals(2,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    adjustmentProduct.setPromoType(CampaignType.NON_CAMPAIGN);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(70L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.NON_CAMPAIGN,adjustmentProductMongoResult.getPromoType());
    assertEquals(1,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    adjustmentProduct.setPromoType(CampaignType.REGULAR);
    adjustmentProduct.setPriority(7);
    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(30L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.NON_CAMPAIGN,adjustmentProductMongoResult.getPromoType());
    assertEquals(7,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    adjustmentProduct.setPromoType(CampaignType.SPECIAL);
    adjustmentProduct.setPriority(2);
    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(25L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductMongoResult = getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().findFirst().orElse(null);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals(CampaignType.NON_CAMPAIGN,adjustmentProductMongoResult.getPromoType());
    assertEquals(2,adjustmentProductMongoResult.getPriority());
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
  }

  @Test
  public void testOnAdjustmentProductEvent_whenHavePpCode_shouldSaveAdjustmentProductWithPpCode() {
    adjustmentProduct.setPromoType(CampaignType.SPECIAL);
    adjustmentProduct.setPriority(2);
    adjustmentProduct.setCampaignCode("CPG3");
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(250L);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    String id = String.format("%s-%s-%s-%s", adjustmentProduct.getItemSku(),
      adjustmentProduct.getPickupPointCode(),
      adjustmentProduct.getPromoType(), adjustmentProduct.getCampaignCode());
    AdjustmentProduct savedAdjustmentProduct = getFromMongo(Collections.ADJUSTMENT_PRODUCT,id,AdjustmentProduct.class);

    assertNotNull(savedAdjustmentProduct);
    assertEquals(adjustmentProduct.getPickupPointCode(), savedAdjustmentProduct.getPickupPointCode());
  }

  @Test
  public void testOnAdjustmentProductEvent_whenDoesNotHavePpCode_shouldSaveAdjustmentProductWithoutPpCode() {
    adjustmentProduct.setPromoType(CampaignType.SPECIAL);
    adjustmentProduct.setPriority(2);
    adjustmentProduct.setCampaignCode("CPG3");
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setValue(250L);
    adjustmentProduct.setPickupPointCode(null);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    String id = String.format("%s-%s-%s", adjustmentProduct.getItemSku(),
      adjustmentProduct.getPromoType(), adjustmentProduct.getCampaignCode());
    AdjustmentProduct savedAdjustmentProduct = getFromMongo(Collections.ADJUSTMENT_PRODUCT,id,AdjustmentProduct.class);

    assertNotNull(savedAdjustmentProduct);
    assertNull(savedAdjustmentProduct.getPickupPointCode());
  }

  @Test
  public void testOnAdjustmentProductEvent_failed_timestampNotValid() throws Exception {
    adjustmentProduct.setTimestamp(currentTimestamp);
    adjustmentProduct.setActivated(true);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    AdjustmentProduct adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertNotEquals(0L,adjustmentProductMongoResult.getTimestamp());
    assertTrue(adjustmentProductMongoResult.isActivated());

    adjustmentProduct.setTimestamp(0L);
    adjustmentProduct.setActivated(false);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertNotEquals(0L,adjustmentProductMongoResult.getTimestamp());
    assertTrue(adjustmentProductMongoResult.isActivated());
  }

  @Test
  public void testOnAllAdjustmentProductEvent_failed_timestampNotValid() throws Exception {
    adjustmentProduct.setTimestamp(currentTimestamp);
    adjustmentProduct.setActivated(true);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    AdjustmentProduct adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertNotEquals(0L,adjustmentProductMongoResult.getTimestamp());
    assertTrue(adjustmentProductMongoResult.isActivated());

    adjustmentProduct.setTimestamp(0L);
    adjustmentProduct.setActivated(false);
    publishAndRefreshMultiple(Topics.ALL_ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertNotEquals(0L,adjustmentProductMongoResult.getTimestamp());
    assertFalse(adjustmentProductMongoResult.isActivated());
  }

  @Test
  public void testOnAdjustmentProductEvent_success_active_hasValue() throws Exception {
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    adjustmentProduct.setValue(10000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct rawResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertTrue(Objects.nonNull(rawResult));
    assertTrue(rawResult.isActivated());
    assertFalse(CollectionUtils.isEmpty(rawResult.getBudgetOwners()));
    assertTrue(rawResult.getValue()==10000);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_success_active_noValue() throws Exception {
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    adjustmentProduct.setValue(10000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setValue(0);
    adjustmentProduct.setBudgetOwners(new HashSet<>());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct rawResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertTrue(Objects.nonNull(rawResult));
    assertTrue(rawResult.isActivated());
    assertFalse(CollectionUtils.isEmpty(rawResult.getBudgetOwners()));
    assertTrue(rawResult.getValue()==10000);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_success_notActive_hasValue() throws Exception {
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    adjustmentProduct.setValue(10000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setActivated(false);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct rawResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertTrue(Objects.nonNull(rawResult));
    assertFalse(rawResult.isActivated());
    assertFalse(CollectionUtils.isEmpty(rawResult.getBudgetOwners()));
    assertTrue(rawResult.getValue()==10000);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_success_notActive_noValue() throws Exception {
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    adjustmentProduct.setValue(10000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setValue(0);
    adjustmentProduct.setBudgetOwners(new HashSet<>());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct rawResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertTrue(Objects.nonNull(rawResult));
    assertFalse(rawResult.isActivated());
    assertFalse(CollectionUtils.isEmpty(rawResult.getBudgetOwners()));
    assertTrue(rawResult.getValue()==10000);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_success_endDateSame() throws Exception {
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setEndDate(adjustmentProduct.getStartDate());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct rawResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertTrue(Objects.nonNull(rawResult));
    assertFalse(ModuleProductUtil.isItSameTime(rawResult.getStartDate(),rawResult.getEndDate()));
    assertTrue(ModuleProductUtil.isItSameTime(rawResult.getStartDate()+2000000,rawResult.getEndDate()));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_success_endDateNotSame() throws Exception {
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct))
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setEndDate(currentTimestamp+2000000);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct rawResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertTrue(Objects.nonNull(rawResult));
    assertFalse(ModuleProductUtil.isItSameTime(rawResult.getStartDate(),rawResult.getEndDate()));
    assertTrue(ModuleProductUtil.isItSameTime(rawResult.getStartDate()+3000000,rawResult.getEndDate()));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_failed_itemNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnAdjustmentProductEvent_failed_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    adjustmentProduct.setPriority(1);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct adjustmentProductResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductResult);
    assertEquals(CampaignPriority.FLASH_SALE,adjustmentProductResult.getPriority());

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
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
  public void testOnAdjustmentProductEvent_topPerformerStockOOS_cheapestAdjustmentLowerThanCheapestStock_shouldUseCheapestAdjustment() throws Exception {
    List<Product.ProductAttribute> definingAttributes = product.getDefiningAttributes().stream()
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
    Product.ProductAttribute definingAttributeItem2 = new Product.ProductAttribute();
    BeanUtils.copyProperties(definingAttributes, definingAttributeItem2);
    definingAttributeItem2.setItemSku(String.format("%s-%05d",product.getProductSku(),2));
    definingAttributes.add(definingAttributeItem2);
    product.setDefiningAttributes(definingAttributes);
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
    Item item2 = new Item();
    BeanUtils.copyProperties(item,item2);
    item2.setItemSku(definingAttributeItem2.getItemSku());
    item2.setItemCode(String.format("%s-%d", item.getItemCode(), 2));
    item2.setId(item2.toId());
    save(SaveRequest.builder()
      .index(Collections.ITEM)
      .domain(item2)
      .clazz(Item.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    PickupPoint pickupPointItem2 = new PickupPoint();
    BeanUtils.copyProperties(pickupPoint,pickupPointItem2);
    pickupPointItem2.setItemSku(item2.getItemSku());
    pickupPointItem2.setPrice(MainUtil.toSet(
      PickupPoint.Price.builder()
        .currency("currency")
        .offerPrice(5000D)
        .listPrice(10000D)
        .channel("web")
        .lastUpdatedBy("lastUpdatedBy")
        .lastUpdatedDate(1539947170868L)
        .build()
    ));
    pickupPointItem2.setId(pickupPointItem2.toId());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPointItem2.toId(), pickupPointItem2, EXTRA_TIME, INDICES);
    CustomInventoryInfo inventoryInfoItem1 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem1);
    inventoryInfoItem1.setItemSku(item.getItemSku());
    inventoryInfoItem1.setId(item.getItemSku());
    inventoryInfoItem1.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
      .location("location1")
      .availableStock(0)
      .originalStock(0)
      .status("OUT_OF_STOCK")
      .stockInformationDetails(null)
      .build()));
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem1)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    CustomInventoryInfo inventoryInfoItem2 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem2);
    inventoryInfoItem2.setItemSku(item2.getItemSku());
    inventoryInfoItem2.setId(item2.getItemSku());
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem2)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(0);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibli)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(5);
    merchant.setUsedQuota(3);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchant)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibliItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibliItem2);
    blibliItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.BLIBLI, 2));
    blibliItem2.setItemSku(item2.getItemSku());
    blibliItem2.setId(blibliItem2.toId());
    blibliItem2.setQuota(5);
    blibliItem2.setUsedQuota(1);
    blibliItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibliItem2.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibliItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchantItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchantItem2);
    merchantItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.MERCHANT, 2));
    merchantItem2.setItemSku(item2.getItemSku());
    merchantItem2.setId(merchantItem2.toId());
    merchantItem2.setQuota(5);
    merchantItem2.setUsedQuota(4);
    merchantItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchantItem2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchantItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    sivaProduct.setOfflinePrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(0)
      .uniqueId("uniqueId")
      .build());
    sivaProduct.setPrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(50)
      .cheapestItemSku(item2.getItemSku())
      .cheapestPickupPointCode(pickupPointItem2.getPickupPointCode())
      .cheapestPriceDays(20)
      .build());
    sivaProduct.setUrl(sivaProduct.getUrl().replaceAll(item.getItemSku(), item2.getItemSku()));
    sivaProduct.setFlashsaleItemSkus(List.of(item.getItemSku(), item2.getItemSku()));
    save(SaveRequest.builder()
      .index(Collections.SIVA_PRODUCT)
      .domain(sivaProduct)
      .clazz(SivaProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    SivaItem sivaItem2 = new SivaItem();
    sivaItem2.setItemSku(item2.getItemSku());
    sivaItem2.setFlashsaleItemSku(item2.getItemSku());
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getPrice().setCheapestItemSku(item2.getItemSku());
      });
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getItemPrice().setCheapestItemSku(item2.getItemSku());
      });
    sivaItem2.setItemUrl(sivaItem.getItemUrl().replaceAll(sivaItem.getItemSku(), item2.getItemSku()));
    sivaItem2.setId(sivaItem2.toId());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem2)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
      .index(Collections.CAMPAIGN_PRODUCT)
      .domain(campaignProduct)
      .clazz(CampaignProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
      .index(Collections.FLASHSALE_PRODUCT)
      .domain(flashsaleProduct)
      .clazz(FlashsaleProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPointItem2, NORMAL_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(0,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());

    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setPriority(CampaignPriority.FLASH_SALE);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(300L);

    AdjustmentProduct adjustmentProductItem2 = new AdjustmentProduct();
    BeanUtils.copyProperties(adjustmentProduct, adjustmentProductItem2);
    adjustmentProductItem2.setItemSku(item2.getItemSku());
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(2,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(100L);

    adjustmentProductItem2.setCampaignCode(null);
    adjustmentProductItem2.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    adjustmentProductItem2.setValue(100L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(4,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(item2.getItemSku(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(pickupPointItem2.getPickupPointCode(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(80, sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
  }

  @Test
  public void testOnAdjustmentProductEvent_topPerformerStockOOS_cheapestAdjustmentHigherThanCheapestStock_shouldUseCheapestStock() throws Exception {
    List<Product.ProductAttribute> definingAttributes = product.getDefiningAttributes().stream()
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
    Product.ProductAttribute definingAttributeItem2 = new Product.ProductAttribute();
    BeanUtils.copyProperties(definingAttributes, definingAttributeItem2);
    definingAttributeItem2.setItemSku(String.format("%s-%05d",product.getProductSku(),2));
    definingAttributes.add(definingAttributeItem2);
    product.setDefiningAttributes(definingAttributes);
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
    Item item2 = new Item();
    BeanUtils.copyProperties(item,item2);
    item2.setItemSku(definingAttributeItem2.getItemSku());
    item2.setItemCode(String.format("%s-%d", item.getItemCode(), 2));
    item2.setId(item2.toId());
    save(SaveRequest.builder()
      .index(Collections.ITEM)
      .domain(item2)
      .clazz(Item.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    PickupPoint pickupPointItem2 = new PickupPoint();
    BeanUtils.copyProperties(pickupPoint,pickupPointItem2);
    pickupPointItem2.setItemSku(item2.getItemSku());
    pickupPointItem2.setPrice(MainUtil.toSet(
      PickupPoint.Price.builder()
        .currency("currency")
        .offerPrice(5000D)
        .listPrice(10000D)
        .channel("web")
        .lastUpdatedBy("lastUpdatedBy")
        .lastUpdatedDate(1539947170868L)
        .build()
    ));
    pickupPointItem2.setId(pickupPointItem2.toId());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPointItem2.toId(), pickupPointItem2, EXTRA_TIME, INDICES);
    CustomInventoryInfo inventoryInfoItem1 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem1);
    inventoryInfoItem1.setItemSku(item.getItemSku());
    inventoryInfoItem1.setId(item.getItemSku());
    inventoryInfoItem1.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
      .location("location1")
      .availableStock(0)
      .originalStock(0)
      .status("OUT_OF_STOCK")
      .stockInformationDetails(null)
      .build()));
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem1)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    CustomInventoryInfo inventoryInfoItem2 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem2);
    inventoryInfoItem2.setItemSku(item2.getItemSku());
    inventoryInfoItem2.setId(item2.getItemSku());
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem2)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(0);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibli)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(5);
    merchant.setUsedQuota(3);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchant)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibliItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibliItem2);
    blibliItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.BLIBLI, 2));
    blibliItem2.setItemSku(item2.getItemSku());
    blibliItem2.setId(blibliItem2.toId());
    blibliItem2.setQuota(50);
    blibliItem2.setUsedQuota(2);
    blibliItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibliItem2.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibliItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchantItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchantItem2);
    merchantItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.MERCHANT, 2));
    merchantItem2.setItemSku(item2.getItemSku());
    merchantItem2.setId(merchantItem2.toId());
    merchantItem2.setQuota(50);
    merchantItem2.setUsedQuota(2);
    merchantItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchantItem2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchantItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    sivaProduct.setOfflinePrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(0)
      .uniqueId("uniqueId")
      .build());
    sivaProduct.setPrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(50)
      .cheapestItemSku(item2.getItemSku())
      .cheapestPickupPointCode(pickupPointItem2.getPickupPointCode())
      .cheapestPriceDays(20)
      .build());
    sivaProduct.setUrl(sivaProduct.getUrl().replaceAll(item.getItemSku(), item2.getItemSku()));
    sivaProduct.setFlashsaleItemSkus(List.of(item.getItemSku(), item2.getItemSku()));
    save(SaveRequest.builder()
      .index(Collections.SIVA_PRODUCT)
      .domain(sivaProduct)
      .clazz(SivaProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    SivaItem sivaItem2 = new SivaItem();
    sivaItem2.setItemSku(item2.getItemSku());
    sivaItem2.setFlashsaleItemSku(item2.getItemSku());
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getPrice().setCheapestItemSku(item2.getItemSku());
      });
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getItemPrice().setCheapestItemSku(item2.getItemSku());
      });
    sivaItem2.setItemUrl(sivaItem.getItemUrl().replaceAll(sivaItem.getItemSku(), item2.getItemSku()));
    sivaItem2.setId(sivaItem2.toId());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem2)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
      .index(Collections.CAMPAIGN_PRODUCT)
      .domain(campaignProduct)
      .clazz(CampaignProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
      .index(Collections.FLASHSALE_PRODUCT)
      .domain(flashsaleProduct)
      .clazz(FlashsaleProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPointItem2, NORMAL_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(0,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());

    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setPriority(CampaignPriority.FLASH_SALE);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(300L);

    AdjustmentProduct adjustmentProductItem2 = new AdjustmentProduct();
    BeanUtils.copyProperties(adjustmentProduct, adjustmentProductItem2);
    adjustmentProductItem2.setItemSku(item2.getItemSku());
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(2,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(100L);

    adjustmentProductItem2.setCampaignCode(null);
    adjustmentProductItem2.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    adjustmentProductItem2.setValue(100L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(4,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(item2.getItemSku(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(pickupPointItem2.getPickupPointCode(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals(70, sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
  }

  @Test
  public void testOnAdjustmentProductEvent_topPerformerStockOOS_cheapestStockOOS_shouldOOS() throws Exception {
    List<Product.ProductAttribute> definingAttributes = product.getDefiningAttributes().stream()
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
    Product.ProductAttribute definingAttributeItem2 = new Product.ProductAttribute();
    BeanUtils.copyProperties(definingAttributes, definingAttributeItem2);
    definingAttributeItem2.setItemSku(String.format("%s-%05d",product.getProductSku(),2));
    definingAttributes.add(definingAttributeItem2);
    product.setDefiningAttributes(definingAttributes);
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
    Item item2 = new Item();
    BeanUtils.copyProperties(item,item2);
    item2.setItemSku(definingAttributeItem2.getItemSku());
    item2.setItemCode(String.format("%s-%d", item.getItemCode(), 2));
    item2.setId(item2.toId());
    save(SaveRequest.builder()
      .index(Collections.ITEM)
      .domain(item2)
      .clazz(Item.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    PickupPoint pickupPointItem2 = new PickupPoint();
    BeanUtils.copyProperties(pickupPoint,pickupPointItem2);
    pickupPointItem2.setItemSku(item2.getItemSku());
    pickupPointItem2.setPrice(MainUtil.toSet(
      PickupPoint.Price.builder()
        .currency("currency")
        .offerPrice(5000D)
        .listPrice(10000D)
        .channel("web")
        .lastUpdatedBy("lastUpdatedBy")
        .lastUpdatedDate(1539947170868L)
        .build()
    ));
    pickupPointItem2.setId(pickupPointItem2.toId());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPointItem2.toId(), pickupPointItem2, EXTRA_TIME, INDICES);
    CustomInventoryInfo inventoryInfoItem1 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem1);
    inventoryInfoItem1.setItemSku(item.getItemSku());
    inventoryInfoItem1.setId(item.getItemSku());
    inventoryInfoItem1.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
      .location("location1")
      .availableStock(0)
      .originalStock(0)
      .status("OUT_OF_STOCK")
      .stockInformationDetails(null)
      .build()));
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem1)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    CustomInventoryInfo inventoryInfoItem2 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem2);
    inventoryInfoItem2.setItemSku(item2.getItemSku());
    inventoryInfoItem2.setId(item2.getItemSku());
    inventoryInfoItem2.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
      .location("location1")
      .availableStock(0)
      .originalStock(0)
      .status("OUT_OF_STOCK")
      .stockInformationDetails(null)
      .build()));
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem2)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(0);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibli)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(5);
    merchant.setUsedQuota(3);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchant)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibliItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibliItem2);
    blibliItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.BLIBLI, 2));
    blibliItem2.setItemSku(item2.getItemSku());
    blibliItem2.setId(blibliItem2.toId());
    blibliItem2.setQuota(50);
    blibliItem2.setUsedQuota(2);
    blibliItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibliItem2.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibliItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchantItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchantItem2);
    merchantItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.MERCHANT, 2));
    merchantItem2.setItemSku(item2.getItemSku());
    merchantItem2.setId(merchantItem2.toId());
    merchantItem2.setQuota(50);
    merchantItem2.setUsedQuota(2);
    merchantItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchantItem2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchantItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    sivaProduct.setOfflinePrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(0)
      .uniqueId("uniqueId")
      .build());
    sivaProduct.setPrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(50)
      .cheapestItemSku(item2.getItemSku())
      .cheapestPickupPointCode(pickupPointItem2.getPickupPointCode())
      .cheapestPriceDays(20)
      .build());
    sivaProduct.setUrl(sivaProduct.getUrl().replaceAll(item.getItemSku(), item2.getItemSku()));
    sivaProduct.setFlashsaleItemSkus(List.of(item.getItemSku(), item2.getItemSku()));
    save(SaveRequest.builder()
      .index(Collections.SIVA_PRODUCT)
      .domain(sivaProduct)
      .clazz(SivaProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    SivaItem sivaItem2 = new SivaItem();
    sivaItem2.setItemSku(item2.getItemSku());
    sivaItem2.setFlashsaleItemSku(item2.getItemSku());
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getPrice().setCheapestItemSku(item2.getItemSku());
      });
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getItemPrice().setCheapestItemSku(item2.getItemSku());
      });
    sivaItem2.setItemUrl(sivaItem.getItemUrl().replaceAll(sivaItem.getItemSku(), item2.getItemSku()));
    sivaItem2.setId(sivaItem2.toId());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem2)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
      .index(Collections.CAMPAIGN_PRODUCT)
      .domain(campaignProduct)
      .clazz(CampaignProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
      .index(Collections.FLASHSALE_PRODUCT)
      .domain(flashsaleProduct)
      .clazz(FlashsaleProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPointItem2, NORMAL_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(0,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());

    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setPriority(CampaignPriority.FLASH_SALE);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(300L);

    AdjustmentProduct adjustmentProductItem2 = new AdjustmentProduct();
    BeanUtils.copyProperties(adjustmentProduct, adjustmentProductItem2);
    adjustmentProductItem2.setItemSku(item2.getItemSku());
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(2,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(100L);

    adjustmentProductItem2.setCampaignCode(null);
    adjustmentProductItem2.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    adjustmentProductItem2.setValue(100L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(4,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(item2.getItemSku(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(pickupPointItem2.getPickupPointCode(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals("OUT_OF_STOCK", sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
  }

  @Test
  public void testOnAdjustmentProductEvent_topPerformerStockOOS_cheapestStockNotFound_shouldOOS() throws Exception {
    List<Product.ProductAttribute> definingAttributes = product.getDefiningAttributes().stream()
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
    Product.ProductAttribute definingAttributeItem2 = new Product.ProductAttribute();
    BeanUtils.copyProperties(definingAttributes, definingAttributeItem2);
    definingAttributeItem2.setItemSku(String.format("%s-%05d",product.getProductSku(),2));
    definingAttributes.add(definingAttributeItem2);
    product.setDefiningAttributes(definingAttributes);
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
    Item item2 = new Item();
    BeanUtils.copyProperties(item,item2);
    item2.setItemSku(definingAttributeItem2.getItemSku());
    item2.setItemCode(String.format("%s-%d", item.getItemCode(), 2));
    item2.setId(item2.toId());
    save(SaveRequest.builder()
      .index(Collections.ITEM)
      .domain(item2)
      .clazz(Item.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    PickupPoint pickupPointItem2 = new PickupPoint();
    BeanUtils.copyProperties(pickupPoint,pickupPointItem2);
    pickupPointItem2.setItemSku(item2.getItemSku());
    pickupPointItem2.setPrice(MainUtil.toSet(
      PickupPoint.Price.builder()
        .currency("currency")
        .offerPrice(5000D)
        .listPrice(10000D)
        .channel("web")
        .lastUpdatedBy("lastUpdatedBy")
        .lastUpdatedDate(1539947170868L)
        .build()
    ));
    pickupPointItem2.setId(pickupPointItem2.toId());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPointItem2.toId(), pickupPointItem2, EXTRA_TIME, INDICES);
    CustomInventoryInfo inventoryInfoItem1 = new CustomInventoryInfo();
    BeanUtils.copyProperties(inventoryInfo, inventoryInfoItem1);
    inventoryInfoItem1.setItemSku(item.getItemSku());
    inventoryInfoItem1.setId(item.getItemSku());
    inventoryInfoItem1.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
      .location("location1")
      .availableStock(0)
      .originalStock(0)
      .status("OUT_OF_STOCK")
      .stockInformationDetails(null)
      .build()));
    save(SaveRequest.builder()
      .index(Collections.INVENTORY_INFORMATION_COLLECTION)
      .domain(inventoryInfoItem1)
      .clazz(CustomInventoryInfo.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(0);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibli)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(5);
    merchant.setUsedQuota(3);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchant)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota blibliItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibliItem2);
    blibliItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.BLIBLI, 2));
    blibliItem2.setItemSku(item2.getItemSku());
    blibliItem2.setId(blibliItem2.toId());
    blibliItem2.setQuota(50);
    blibliItem2.setUsedQuota(2);
    blibliItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibliItem2.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(blibliItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    AdjustmentProductQuota merchantItem2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchantItem2);
    merchantItem2.setAdjustmentProductId(String.format("%s-%d", CampaignOwner.MERCHANT, 2));
    merchantItem2.setItemSku(item2.getItemSku());
    merchantItem2.setId(merchantItem2.toId());
    merchantItem2.setQuota(50);
    merchantItem2.setUsedQuota(2);
    merchantItem2.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchantItem2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
      .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
      .domain(merchantItem2)
      .clazz(AdjustmentProductQuota.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    sivaProduct.setOfflinePrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(0)
      .uniqueId("uniqueId")
      .build());
    sivaProduct.setPrice(SivaProduct.FinalPrice.builder()
      .list("Rp 10,000")
      .offer("Rp 5,000")
      .offerValue(5000D)
      .minOfferValue(5000D)
      .maxOfferValue(5000D)
      .discount(50)
      .cheapestItemSku(item2.getItemSku())
      .cheapestPickupPointCode(pickupPointItem2.getPickupPointCode())
      .cheapestPriceDays(20)
      .build());
    sivaProduct.setUrl(sivaProduct.getUrl().replaceAll(item.getItemSku(), item2.getItemSku()));
    sivaProduct.setFlashsaleItemSkus(List.of(item.getItemSku(), item2.getItemSku()));
    save(SaveRequest.builder()
      .index(Collections.SIVA_PRODUCT)
      .domain(sivaProduct)
      .clazz(SivaProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    SivaItem sivaItem2 = new SivaItem();
    sivaItem2.setItemSku(item2.getItemSku());
    sivaItem2.setFlashsaleItemSku(item2.getItemSku());
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getPrice().setCheapestItemSku(item2.getItemSku());
      });
    Optional.ofNullable(sivaItem2.getPrice())
      .ifPresent(price -> {
        sivaItem2.getItemPrice().setCheapestItemSku(item2.getItemSku());
      });
    sivaItem2.setItemUrl(sivaItem.getItemUrl().replaceAll(sivaItem.getItemSku(), item2.getItemSku()));
    sivaItem2.setId(sivaItem2.toId());
    save(SaveRequest.builder()
      .index(Collections.SIVA_ITEM)
      .domain(sivaItem2)
      .clazz(SivaItem.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
      .index(Collections.CAMPAIGN_PRODUCT)
      .domain(campaignProduct)
      .clazz(CampaignProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
      .index(Collections.FLASHSALE_PRODUCT)
      .domain(flashsaleProduct)
      .clazz(FlashsaleProduct.class)
      .mongo(true)
      .elasticsearch(false)
      .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPointItem2, NORMAL_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(0,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());

    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setPriority(CampaignPriority.FLASH_SALE);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(300L);

    AdjustmentProduct adjustmentProductItem2 = new AdjustmentProduct();
    BeanUtils.copyProperties(adjustmentProduct, adjustmentProductItem2);
    adjustmentProductItem2.setItemSku(item2.getItemSku());
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(2,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(100L);

    adjustmentProductItem2.setCampaignCode(null);
    adjustmentProductItem2.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProductItem2.setId(adjustmentProductItem2.toId());
    adjustmentProductItem2.setValue(100L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProductItem2, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(4,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory());
    assertNotNull(sivaProductMongoResult.getFlashsaleInventory().getQuota());
    assertEquals(item2.getItemSku(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getItemSku());
    assertEquals(pickupPointItem2.getPickupPointCode(), sivaProductMongoResult.getFlashsaleInventory().getQuota().getPickupPointCode());
    assertEquals("OUT_OF_STOCK", sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
  }

  @Test
  public void testOnAdjustmentProductEvent_success_quotaNotFound_availableInventory() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
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
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
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
  public void testOnAdjustmentProductEvent_success_quotaNotFound_oosInventory() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(emptyInventoryInfo)
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

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
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
  public void testOnAdjustmentProductEvent_success_blibli() throws Exception {
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
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(cheapestItem.getItemSku());
    inventoryInfo.setId(cheapestItem.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    quota1.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(4);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    quota2.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    quota3.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProduct.setPickupPointCode(pickupPoint.getPickupPointCode());
    AdjustmentProduct adjustmentProductResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductResult);
    assertNotNull(adjustmentProductResult.getPickupPointCode());

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.PICKUP_POINT,PickupPoint.class)));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnAdjustmentProductEvent_success_merchant() throws Exception {
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
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(cheapestItem.getItemSku());
    inventoryInfo.setId(cheapestItem.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    quota1.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(4);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    quota2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    quota3.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProduct.setPickupPointCode(pickupPoint.getPickupPointCode());
    AdjustmentProduct adjustmentProductResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductResult);
    assertNotNull(adjustmentProductResult.getPickupPointCode());

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.PICKUP_POINT,PickupPoint.class)));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnAdjustmentProductEvent_success_both() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(0);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(blibli)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(5);
    merchant.setUsedQuota(3);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(merchant)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.setPromotionStartTime(currentTimestamp-1000000);
    campaignProduct.setPromotionEndTime(currentTimestamp+1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(0,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());

    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    adjustmentProduct.setPriority(CampaignPriority.FLASH_SALE);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setCampaignCode(campaignProduct.getCampaignCode());
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(300L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(1,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    adjustmentProduct.setCampaignCode(null);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setValue(50L);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class)));
    assertEquals(2,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertTrue(getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).stream().filter(Objects::isNull).allMatch(val -> val.getExpiryTime()>val.getEndDate()));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("9650.0",sivaProductMongoResult.getPrice().getOfferValue().toString());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("9650.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("9650.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
  }

  @Test
  public void testOnAdjustmentProductEvent_success_adjustmentNotActive() throws Exception {
    sivaProduct.setAdjustments(sivaItem.getAdjustments());
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
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
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setStartDate(currentTimestamp-1000000);
    adjustmentProduct.setEndDate(currentTimestamp+1000000);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
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
  public void testOnAdjustmentProductEvent_success_adjustmentExpiredAndNoCampaignCode() throws Exception {
    sivaProduct.setAdjustments(sivaItem.getAdjustments());
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(adjustmentProductQuota)
        .clazz(AdjustmentProductQuota.class)
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
    adjustmentProduct.setCampaignCode(null);

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
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
  public void testOnAllAdjustmentProductEvent_success_multipleItemsAndFlashsaleFlag() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(otherCheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, otherCheapestPickupPoint.toId(), otherCheapestPickupPoint, EXTRA_TIME, INDICES);
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(4);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.getSku().setItemSku(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(otherCheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(otherCheapestItem.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ALL_ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals(cheapestItem.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnAllAdjustmentProductEvent_success_multipleItemsAndNoCampaignProduct() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(otherCheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, otherCheapestPickupPoint.toId(), otherCheapestPickupPoint, EXTRA_TIME, INDICES);
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(4);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
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
    adjustmentProduct.setItemSku(otherCheapestItem.getItemSku());
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
    adjustmentProduct.setItemSku(item.getItemSku());
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
    flashsaleProduct.getSchedule().setStart(currentTimestamp+500000);
    flashsaleProduct.getSchedule().setEnd(currentTimestamp+1000000);
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ALL_ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals(cheapestItem.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals(item.toId(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals(item.toId(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void testOnAdjustmentProductEvent_success_denpasar() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, EXTRA_TIME, INDICES);
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(cheapestItem.getItemSku());
    inventoryInfo.setId(cheapestItem.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    quota1.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(4);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    quota2.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota,quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    quota3.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setActivated(true);
    adjustmentProduct.setStartDate(campaignProduct.getPromotionStartTime());
    adjustmentProduct.setEndDate(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ALL_ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ADJUSTMENT_PRODUCT,ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct).toId(),AdjustmentProduct.class));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
  }

  @Test
  public void onAdjustmentProductEvent_verifyCampaignInfos_shouldNotRemoved_whenFlashSaleCampaignIsStillActiveAlthoughOutOfStock() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, EXTRA_TIME, INDICES);
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(cheapestItem.getItemSku());
    inventoryInfo.setId(cheapestItem.getItemSku());
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
    campaignProduct.setPromotionStartTime(currentTimestamp + 500000);
    campaignProduct.setPromotionEndTime(currentTimestamp + 1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    flashsaleProduct.getSchedule().setTimeBased(false);
    flashsaleProduct.setItemSku(item.getItemSku());
    flashsaleProduct.setId(flashsaleProduct.toId());
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime()-100000);
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    flashsaleProduct.getSchedule().setTimeBased(true);
    flashsaleProduct.setItemSku(item.getItemSku());
    flashsaleProduct.setId(flashsaleProduct.toId());
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    flashsaleProduct.getSchedule().setTimeBased(false);
    flashsaleProduct.setItemSku(cheapestItem.getItemSku());
    flashsaleProduct.setId(flashsaleProduct.toId());
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime()-100000);
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    flashsaleProduct.getSchedule().setTimeBased(true);
    flashsaleProduct.setItemSku(cheapestItem.getItemSku());
    flashsaleProduct.setId(flashsaleProduct.toId());
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    assertEquals(0,getAllFromMongo(Collections.ADJUSTMENT_PRODUCT,AdjustmentProduct.class).size());
    assertEquals(4,getAllFromMongo(Collections.FLASHSALE_PRODUCT,FlashsaleProduct.class).size());

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
            .filter(val -> val.getItemSkus().stream()
            .anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
            .findFirst()
            .orElseGet(SivaProduct::new);
    CampaignInfo campaignInfoOnSivaProduct = sivaProductMongoResult.getCampaignInfos().values().stream()
            .filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()))
            .findFirst()
            .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getPriceTeaser());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(campaignInfoOnSivaProduct);
    assertTrue(Objects.nonNull(sivaProductMongoResult.getFlashsale()));
    assertTrue(Objects.nonNull(sivaProductMongoResult.getSubFlashsale()));
    assertEquals(campaignProduct.getPromotionStartTime()-100000+"",sivaProductMongoResult.getCampaignInfos().get(ModuleProductUtil.toCampaignInfoKey(campaignProduct.getCampaignCode(),campaignProduct.getSku().getSessionId())).getStartTime()+"");
    assertEquals(campaignProduct.getPromotionEndTime()+"",sivaProductMongoResult.getCampaignInfos().get(ModuleProductUtil.toCampaignInfoKey(campaignProduct.getCampaignCode(),campaignProduct.getSku().getSessionId())).getEndTime()+"");

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    CampaignInfo campaignInfoOnSivaItem = sivaItemMongoResult.getCampaignInfos().values().stream()
            .filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()))
            .findFirst()
            .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(campaignInfoOnSivaItem);
    assertTrue(Objects.nonNull(sivaItemMongoResult.getFlashsale()));
    assertTrue(Objects.nonNull(sivaItemMongoResult.getSubFlashsale()));
    assertEquals(campaignProduct.getPromotionStartTime()-100000+"",sivaItemMongoResult.getCampaignInfos().get(ModuleProductUtil.toCampaignInfoKey(campaignProduct.getCampaignCode(),campaignProduct.getSku().getSessionId())).getStartTime()+"");
    assertEquals(campaignProduct.getPromotionEndTime()+"",sivaItemMongoResult.getCampaignInfos().get(ModuleProductUtil.toCampaignInfoKey(campaignProduct.getCampaignCode(),campaignProduct.getSku().getSessionId())).getEndTime()+"");
  }

  @Test
  public void onAdjustmentProductEvent_whenSuccessEncryptMetaData_shouldConstructMetaDataParameterOnUrlRelated() {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, EXTRA_TIME, INDICES);
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(cheapestItem.getItemSku());
    inventoryInfo.setId(cheapestItem.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    quota1.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(5);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    quota2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    quota3.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.setPromotionStartTime(currentTimestamp + 500000);
    campaignProduct.setPromotionEndTime(currentTimestamp + 1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setStartDate(currentTimestamp - 1000000);
    adjustmentProduct.setEndDate(currentTimestamp + 1000000);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
    adjustmentProduct.setId(adjustmentProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setId(adjustmentProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)
      .stream()
      .filter(val -> val.getItemSkus().stream()
        .anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
      .findFirst()
      .orElseGet(SivaProduct::new);
    CampaignInfo campaignInfoOnSivaProduct = sivaProductMongoResult.getCampaignInfos().values()
      .stream()
      .filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()))
      .findFirst()
      .orElseGet(CampaignInfo::new);
    assertEquals("/p/generateditemname/is--ABC-12345-12345-00002?pickupPointCode=pickupPointCode"
            + "&metaData=rvgwOoFKOZW6G3eKAKhN/WZgreiAAxUXY5PjrPUoQ6S55euXopNNxJyl/tymteAk/hFe3L5L9UuZfULdUeDx8A==",
        sivaProductMongoResult.getUrl());
    assertEquals("/p/samsung-galaxy-8/ps--ABC-12345-12345?pickupPointCode=pickupPointCode"
        + "&metaData=rvgwOoFKOZW6G3eKAKhN/WZgreiAAxUXY5PjrPUoQ6S55euXopNNxJyl/tymteAk/hFe3L5L9UuZfULdUeDx8A==",
        campaignInfoOnSivaProduct.getProductUrl());
    assertEquals("/p/generateditemname/is--ABC-12345-12345-00002?pickupPointCode=pickupPointCode"
        + "&metaData=rvgwOoFKOZW6G3eKAKhN/WZgreiAAxUXY5PjrPUoQ6S55euXopNNxJyl/tymteAk/hFe3L5L9UuZfULdUeDx8A==",
        campaignInfoOnSivaProduct.getItemUrl());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,adjustmentProduct.getItemSku(),SivaItem.class);
    CampaignInfo campaignInfoOnSivaItem = sivaItemMongoResult.getCampaignInfos().values().stream()
      .filter(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()))
      .findFirst()
      .orElseGet(CampaignInfo::new);
    assertEquals("/p/samsung-galaxy-8/ps--ABC-12345-12345?pickupPointCode=pickupPointCode"
        + "&metaData=IXX3G17qN9Z8N3dvg8OhNZloPBPAXbM7KN5clTMoHHlV21zq9Xy0TYsEqd12n/n+C2fP/NjuPG6o1GCgX9DtjA==",
        sivaItemMongoResult.getProductUrl());
    assertEquals("/p/generateditemname/is--ABC-12345-12345-00001?pickupPointCode=pickupPointCode"
        + "&metaData=IXX3G17qN9Z8N3dvg8OhNZloPBPAXbM7KN5clTMoHHlV21zq9Xy0TYsEqd12n/n+C2fP/NjuPG6o1GCgX9DtjA==",
        sivaItemMongoResult.getItemUrl());
    assertEquals("/p/samsung-galaxy-8/ps--ABC-12345-12345?pickupPointCode=pickupPointCode"
        + "&metaData=IXX3G17qN9Z8N3dvg8OhNZloPBPAXbM7KN5clTMoHHlV21zq9Xy0TYsEqd12n/n+C2fP/NjuPG6o1GCgX9DtjA==",
        campaignInfoOnSivaItem.getProductUrl());
    assertEquals("/p/generateditemname/is--ABC-12345-12345-00001?pickupPointCode=pickupPointCode"
        + "&metaData=IXX3G17qN9Z8N3dvg8OhNZloPBPAXbM7KN5clTMoHHlV21zq9Xy0TYsEqd12n/n+C2fP/NjuPG6o1GCgX9DtjA==",
        campaignInfoOnSivaItem.getItemUrl());
  }

  @Test
  public void onAdjustmentProductEvent_verifyValueUpdates() throws Exception {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
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

    //Iteration 1 : ADD BLIBLI 10K, total 10K
    AdjustmentProductQuota blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(0);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, blibli.toId(), blibli, EXTRA_TIME, INDICES);

    adjustmentProduct.setValue(10000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI));
    adjustmentProduct.setActivated(true);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    AdjustmentProduct adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals("10000",MainUtil.fromLongToString(adjustmentProductMongoResult.getValue()));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertFalse(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.MERCHANT));
    assertEquals("10000",MainUtil.fromLongToString(adjustmentProductMongoResult.getInitValue()));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertFalse(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.MERCHANT));

    //Iteration 2 : ADD MERCHANT 7K, total 17K
    AdjustmentProductQuota merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(7);
    merchant.setUsedQuota(0);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, merchant.toId(), merchant, EXTRA_TIME, INDICES);

    adjustmentProduct.setValue(17000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI,CampaignOwner.MERCHANT));
    adjustmentProduct.setActivated(true);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals("17000",MainUtil.fromLongToString(adjustmentProductMongoResult.getValue()));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.MERCHANT));
    assertEquals("17000",MainUtil.fromLongToString(adjustmentProductMongoResult.getInitValue()));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.MERCHANT));

    //Iteration 3 : UPDATE MERCHANT 5K, total 15K
    merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(7);
    merchant.setUsedQuota(2);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, merchant.toId(), merchant, EXTRA_TIME, INDICES);

    adjustmentProduct.setValue(15000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI,CampaignOwner.MERCHANT));
    adjustmentProduct.setActivated(true);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals("15000",MainUtil.fromLongToString(adjustmentProductMongoResult.getValue()));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.MERCHANT));
    assertEquals("15000",MainUtil.fromLongToString(adjustmentProductMongoResult.getInitValue()));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.MERCHANT));

    //Iteration 4 : END BLIBLI, total 5K
    blibli = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, blibli);
    blibli.setAdjustmentProductId(CampaignOwner.BLIBLI);
    blibli.setId(blibli.toId());
    blibli.setQuota(3);
    blibli.setUsedQuota(3);
    blibli.setCampaignCode(adjustmentProduct.getCampaignCode());
    blibli.setBudgetOwner(CampaignOwner.BLIBLI);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, blibli.toId(), blibli, EXTRA_TIME, INDICES);

    adjustmentProduct.setValue(5000);
    adjustmentProduct.setBudgetOwners(MainUtil.toSet(CampaignOwner.MERCHANT));
    adjustmentProduct.setActivated(true);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals("5000",MainUtil.fromLongToString(adjustmentProductMongoResult.getValue()));
    assertFalse(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.MERCHANT));
    assertEquals("15000",MainUtil.fromLongToString(adjustmentProductMongoResult.getInitValue()));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.MERCHANT));

    //Iteration 5 : END MERCHANT, total 0K
    merchant = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, merchant);
    merchant.setAdjustmentProductId(CampaignOwner.MERCHANT);
    merchant.setId(merchant.toId());
    merchant.setQuota(7);
    merchant.setUsedQuota(7);
    merchant.setCampaignCode(adjustmentProduct.getCampaignCode());
    merchant.setBudgetOwner(CampaignOwner.MERCHANT);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, merchant.toId(), merchant, EXTRA_TIME, INDICES);

    adjustmentProduct.setValue(0);
    adjustmentProduct.setBudgetOwners(new HashSet<>());
    adjustmentProduct.setActivated(false);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    adjustmentProductMongoResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT,adjustmentProduct.toId(),AdjustmentProduct.class);
    assertNotNull(adjustmentProductMongoResult);
    assertEquals("15000",MainUtil.fromLongToString(adjustmentProductMongoResult.getValue()));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getBudgetOwners().contains(CampaignOwner.MERCHANT));
    assertEquals("15000",MainUtil.fromLongToString(adjustmentProductMongoResult.getInitValue()));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.BLIBLI));
    assertTrue(adjustmentProductMongoResult.getInitBudgetOwners().contains(CampaignOwner.MERCHANT));
  }

  @Test
  public void onAdjustmentProductEvent_whenSuccessEncryptMetaDataWithNoPickupPointCode_shouldConstructMetaDataParameterOnFirstParameter() {
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, EXTRA_TIME, INDICES);
    inventoryInfo.setItemSku(item.getItemSku());
    inventoryInfo.setId(item.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.setItemSku(cheapestItem.getItemSku());
    inventoryInfo.setId(cheapestItem.getItemSku());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota1 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, quota1);
    quota1.setId("id1");
    quota1.setQuota(3);
    quota1.setUsedQuota(3);
    quota1.setCampaignCode("OTHER");
    quota1.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota1)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota2 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, quota2);
    quota2.setId("id2");
    quota2.setQuota(5);
    quota2.setUsedQuota(5);
    quota2.setCampaignCode(adjustmentProduct.getCampaignCode());
    quota2.setBudgetOwner(CampaignOwner.MERCHANT);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota2)
        .clazz(AdjustmentProductQuota.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    AdjustmentProductQuota quota3 = new AdjustmentProductQuota();
    BeanUtils.copyProperties(adjustmentProductQuota, quota3);
    quota3.setId("id3");
    quota3.setQuota(7);
    quota3.setUsedQuota(7);
    quota3.setCampaignCode("OTHER");
    quota3.setBudgetOwner(CampaignOwner.BLIBLI);
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .domain(quota3)
        .clazz(AdjustmentProductQuota.class)
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
    campaignProduct.setPromotionStartTime(currentTimestamp + 500000);
    campaignProduct.setPromotionEndTime(currentTimestamp + 1000000);
    campaignProduct.getSku().setItemSku(item.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    campaignProduct.getSku().setItemSku(cheapestItem.getItemSku());
    campaignProduct.setId(campaignProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.CAMPAIGN_PRODUCT)
        .domain(campaignProduct)
        .clazz(CampaignProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setStartDate(currentTimestamp - 1000000);
    adjustmentProduct.setEndDate(currentTimestamp + 1000000);
    adjustmentProduct.setPriority(1);
    adjustmentProduct.setPromoType(CampaignType.FLASH_SALE);
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setItemSku(cheapestItem.getItemSku());
    adjustmentProduct.setId(adjustmentProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    adjustmentProduct.setItemSku(item.getItemSku());
    adjustmentProduct.setId(adjustmentProduct.toId());
    save(SaveRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .domain(adjustmentProduct)
        .clazz(AdjustmentProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    flashsaleProduct.getSchedule().setStart(campaignProduct.getPromotionStartTime());
    flashsaleProduct.getSchedule().setEnd(campaignProduct.getPromotionEndTime());
    save(SaveRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .domain(flashsaleProduct)
        .clazz(FlashsaleProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)
      .stream()
      .filter(val -> val.getItemSkus().stream()
        .anyMatch(itemSku -> adjustmentProduct.getItemSku().equals(itemSku)))
      .findFirst()
      .orElseGet(SivaProduct::new);
    assertEquals("/p/generateditemname/is--ABC-12345-12345-00002?pickupPointCode=pickupPointCode&" +
            "metaData=rvgwOoFKOZW6G3eKAKhN/WZgreiAAxUXY5PjrPUoQ6S55euXopNNxJyl/tymteAk/hFe3L5L9UuZfULdUeDx8A==",
      sivaProductMongoResult.getUrl());
  }

  @Test
  public void onAdjustmentProductEvent_nonCampaignCheaperThanFlashsale() throws Exception {
    createCampaign("ABC-12345-12345-00001", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, null, null, currentTimestamp - 5000000, currentTimestamp + 5000000, true, false, false, 2000, null);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, null, null, currentTimestamp - 5000000, currentTimestamp + 5000000, true, false, false, 2000, null);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-12345", 123, currentTimestamp - 1000000, currentTimestamp + 1000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("CPG-12345",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaProductMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaProductMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaProductMongoResult.getPickupPointCode());
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertNull(sivaItemMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-00001",sivaItemMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-00001",sivaItemMongoResult.getPickupPointCode());
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00002",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("CPG-12345",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaItemMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaItemMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
  }

  @Test
  public void onAdjustmentProductEvent_regularCheaperThanFlashsale() throws Exception {
    createCampaign("ABC-12345-12345-00001", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-00001", null, currentTimestamp - 5000000, currentTimestamp + 5000000, true, false, false, 2000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-00001", null, currentTimestamp - 5000000, currentTimestamp + 5000000, true, false, false, 2000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00002", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-12345", 123, currentTimestamp - 1000000, currentTimestamp + 1000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("CPG-12345",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaProductMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaProductMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaProductMongoResult.getPickupPointCode());
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("CPG-00001",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.REGULAR,sivaItemMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-00001",sivaItemMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-00001",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00002",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("CPG-12345",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaItemMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaItemMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
  }

  @Test
  public void onAdjustmentProductEvent_campaignCodesValidation_xpromo() throws Exception {
    createCampaign("ABC-12345-12345-00001", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-00001", null, currentTimestamp - 5000000, currentTimestamp + 5000000, true, false, false, 2000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00001", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-00002", null, currentTimestamp + 3000000, currentTimestamp + 5000000, true, false, false, 2000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00001", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-12345", 123, currentTimestamp - 1000000, currentTimestamp + 1000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    delete(DeleteRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .id(flashsaleProduct.toId())
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00001", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-67890", 456, currentTimestamp + 1000000, currentTimestamp + 3000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    delete(DeleteRequest.builder()
        .index(Collections.FLASHSALE_PRODUCT)
        .id(flashsaleProduct.toId())
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("CPG-12345",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaProductMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaProductMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaProductMongoResult.getPickupPointCode());
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-00002"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-67890-456"));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("CPG-12345",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaItemMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaItemMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-00002"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-67890-456"));
  }

  @Test
  public void onAdjustmentProductEvent_campaignCodesValidation_denpasar() throws Exception {
    createCampaign("ABC-12345-12345-00001", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-00001", null, currentTimestamp - 5000000, currentTimestamp + 5000000, true, false, false, 2000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00001", "PP-00001",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-00002", null, currentTimestamp + 3000000, currentTimestamp + 5000000, true, false, false, 2000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00001", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-12345", 123, currentTimestamp - 1000000, currentTimestamp + 1000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    delete(DeleteRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .id(adjustmentProduct.toId())
        .mongo(true)
        .elasticsearch(false)
        .build());
    delete(DeleteRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .id(adjustmentProductQuota.toId())
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);
    createCampaign("ABC-12345-12345-00001", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-67890", 456, currentTimestamp + 1000000, currentTimestamp + 3000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    delete(DeleteRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .id(adjustmentProduct.toId())
        .mongo(true)
        .elasticsearch(false)
        .build());
    delete(DeleteRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
        .id(adjustmentProductQuota.toId())
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.FLASHSALE_PRODUCT, flashsaleProduct.toId(), flashsaleProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("CPG-12345",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaProductMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaProductMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaProductMongoResult.getPickupPointCode());
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-00002"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-67890-456"));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("CPG-12345",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals(CampaignPriority.FLASH_SALE,sivaItemMongoResult.getPrice().getAdjustment().getPriority());
    assertEquals("PP-12345",sivaItemMongoResult.getPrice().getAdjustment().getPickupPointCode());
    assertEquals("PP-12345",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-00001"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-00002"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-67890-456"));
  }

  @Test
  public void onAdjustmentProductEvent_campaignCodesValidation_upcoming() throws Exception {
    createCampaign("ABC-12345-12345-00001", "PP-12345",item.getMerchantCode(), InventoryType.TYPE_ONLINE_MERCHANT, "CPG-12345", 123, currentTimestamp + 1000000, currentTimestamp + 3000000, true, false, false, 1000, CampaignPriority.FLASH_SALE);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignInfos()));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-12345-123"));
  }

  @Test
  public void onAdjustmentProductEvent_multipleSessionId() throws Exception {
    createCampaign("ABC-12345-12345-00001",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-OLD",101,currentTimestamp-2000000,currentTimestamp-1000000,false,false,false,1000, CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00002",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",202,currentTimestamp-1000000,currentTimestamp+1000000,false,false,false,5000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00003",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",303,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00003",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,303,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00004",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",404,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,2000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00004",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,404,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00005",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",null,currentTimestamp+1000000,currentTimestamp+2000000,true,false,false,7000,CampaignPriority.REGULAR);
    createCampaign("ABC-12345-12345-00005",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,null,currentTimestamp+1000000,currentTimestamp+2000000,true,false,false,500,CampaignPriority.REGULAR);
    createCampaign("ABC-12345-12345-00006",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-UPCOMING",606,currentTimestamp+1000000,currentTimestamp+2000000,true,false,false,15000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00007",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-UPCOMING",707,currentTimestamp+1000000,currentTimestamp+2000000,true,false,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00008",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",null,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,8000,0);

    idTimestampItemLevel.setId("ABC-12345-12345-00001");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00002");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00003");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00004");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00005");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00006");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00007");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00008");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignInfos()));
    assertFalse(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-707"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00004",sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaProductMongoResult.getUrl().contains("is"));
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertEquals(1,sivaProductMongoResult.getFlashsaleItemSkus().size());
    assertTrue(sivaProductMongoResult.getFlashsaleItemSkus().contains("ABC-12345-12345-00004"));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-707"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertNull(sivaItemMongoResult.getFlashsale());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00002",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00003",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00003",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00003",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00004",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00004",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00004",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00005",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-707"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00005",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00005",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertNull(sivaItemMongoResult.getFlashsale());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00006",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-707"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00006",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00006",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00007",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-707"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00007",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00007",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00008",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-303"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-404"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-606"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-707"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00008",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00008",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertNull(sivaItemMongoResult.getFlashsale());
  }

  @Test
  public void onAdjustmentProductEvent_multipleAdjustmentsOnSameSessionId() throws Exception {
    createCampaign("ABC-12345-12345-00001",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-OLD",101,currentTimestamp-2000000,currentTimestamp-1000000,false,true,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00002",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",202,currentTimestamp-1000000,currentTimestamp+1000000,false,true,false,5000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00003",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",202,currentTimestamp-1000000,currentTimestamp+1000000,true,true,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00003",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,202,currentTimestamp-1000000,currentTimestamp+1000000,true,true,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00004",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",202,currentTimestamp-1000000,currentTimestamp+1000000,true,true,false,2000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00004",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,202,currentTimestamp-1000000,currentTimestamp+1000000,true,true,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00005",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",null,currentTimestamp+1000000,currentTimestamp+2000000,true,true,false,7000,CampaignPriority.REGULAR);
    createCampaign("ABC-12345-12345-00005",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,null,null,currentTimestamp+1000000,currentTimestamp+2000000,true,true,false,500,CampaignPriority.REGULAR);
    createCampaign("ABC-12345-12345-00006",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-UPCOMING",303,currentTimestamp+1000000,currentTimestamp+2000000,true,true,false,15000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00007",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-UPCOMING",303,currentTimestamp+1000000,currentTimestamp+2000000,true,true,false,1000,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00008",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"CPG-LIVE",null,currentTimestamp-1000000,currentTimestamp+1000000,true,true,false,8000,0);

    idTimestampItemLevel.setId("ABC-12345-12345-00001");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00002");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00003");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00004");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00005");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00006");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00007");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00008");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getCampaignInfos()));
    assertFalse(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaProductMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00004",sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaProductMongoResult.getUrl().contains("is"));
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertEquals(1,sivaProductMongoResult.getFlashsaleItemSkus().size());
    assertTrue(sivaProductMongoResult.getFlashsaleItemSkus().contains("ABC-12345-12345-00004"));
    assertTrue(sivaProductMongoResult.getFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertNull(sivaItemMongoResult.getFlashsale());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00002",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertTrue(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00003",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00003",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00003",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertTrue(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00004",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00004",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00004",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertTrue(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00005",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00005",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00005",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertNull(sivaItemMongoResult.getFlashsale());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00006",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00006",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00006",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertTrue(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00007",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00007",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00007",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertTrue(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00008",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getCampaignInfos()));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-OLD-101"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE-202"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-LIVE"));
    assertFalse(sivaItemMongoResult.getCampaignInfos().containsKey("CPG-UPCOMING-303"));
    assertTrue(sivaItemMongoResult.getCampaignInfos().containsKey(Default.CAMPAIGN_CODE));
    assertEquals("ABC-12345-12345-00008",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00008",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("ps"));
    assertTrue(sivaItemMongoResult.getItemUrl().contains("is"));
    assertNull(sivaItemMongoResult.getFlashsale());
  }

  @Test
  public void onAdjustmentProductEvent_imageValidation() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5000,CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());

    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getItemCode());
    assertNotNull(itemMongoResult.getMasterDataItem());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.OOS));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.OOS));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
  }

  @Test
  public void onAdjustmentProductEvent_discountValidation() throws Exception {
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));

    //Iteration 1 : Bellow 1%
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,91,CampaignPriority.FLASH_SALE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9909.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertNull(sivaProductMongoResult.getPrice().getDiscount());
    assertEquals(StringUtils.EMPTY,sivaProductMongoResult.getPrice().getList());
    assertNull(sivaProductMongoResult.getPrice().getListValue());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("9909.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9909.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    //Iteration 2 : Between 1% - 99% <0.5
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5030,CampaignPriority.FLASH_SALE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("4970.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertEquals(Integer.valueOf(50),sivaProductMongoResult.getPrice().getDiscount());
    assertEquals("Rp 10,000",sivaProductMongoResult.getPrice().getList());
    assertEquals(Double.valueOf(10000),sivaProductMongoResult.getPrice().getListValue());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("4970.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("4970.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    //Iteration 3 : Between 1% - 99% =0.5
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5050,CampaignPriority.FLASH_SALE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("4950.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertEquals(Integer.valueOf(51),sivaProductMongoResult.getPrice().getDiscount());
    assertEquals("Rp 10,000",sivaProductMongoResult.getPrice().getList());
    assertEquals(Double.valueOf(10000),sivaProductMongoResult.getPrice().getListValue());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("4950.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("4950.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    //Iteration 4 : Between 1% - 99% >0.5
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5070,CampaignPriority.FLASH_SALE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("4930.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertEquals(Integer.valueOf(51),sivaProductMongoResult.getPrice().getDiscount());
    assertEquals("Rp 10,000",sivaProductMongoResult.getPrice().getList());
    assertEquals(Double.valueOf(10000),sivaProductMongoResult.getPrice().getListValue());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("4930.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("4930.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    //Iteration 5 : Between >99% - 100%
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,9999,CampaignPriority.FLASH_SALE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(val.getOfferPrice()));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("1.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertEquals(Integer.valueOf(99),sivaProductMongoResult.getPrice().getDiscount());
    assertEquals("Rp 10,000",sivaProductMongoResult.getPrice().getList());
    assertEquals(Double.valueOf(10000),sivaProductMongoResult.getPrice().getListValue());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("1.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("1.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    //Real Case Iteration
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5900,CampaignPriority.FLASH_SALE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(17900));
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(15900));
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setListPrice(17900));
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(15900));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ADJUSTMENT_PRODUCT)
        .mongo(true)
        .build());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("15900.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertEquals(Integer.valueOf(44),sivaProductMongoResult.getPrice().getDiscount());
    assertEquals("Rp 17,900",sivaProductMongoResult.getPrice().getList());
    assertEquals(Double.valueOf(17900),sivaProductMongoResult.getPrice().getListValue());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("15900.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("15900.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());
  }

  @Test
  public void onAdjustmentProductEvent_multipleFlashsaleValidation() throws Exception {
    createCampaign("ABC-12345-12345-00001",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,50,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00002",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,50,CampaignPriority.FLASH_SALE);
    createCampaign("ABC-12345-12345-00003",pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,50,CampaignPriority.FLASH_SALE);

    idTimestampItemLevel.setId("ABC-12345-12345-00001");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00002");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);
    idTimestampItemLevel.setId("ABC-12345-12345-00003");
    publishAndRefreshMultiple(Topics.ITEM_DENPASAR,idTimestampItemLevel.toId(),idTimestampItemLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,"ABC-12345-12345",SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsaleItemSkus());
    assertEquals("ABC-12345-12345-00001",sivaProductMongoResult.getFlashsaleItemSkus().iterator().next());
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9950.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("9950.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9950.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    adjustmentProduct.setValue(0);
    adjustmentProduct.setBudgetOwners(new HashSet<>());
    adjustmentProduct.setActivated(false);
    adjustmentProduct.setItemSku("ABC-12345-12345-00001");
    adjustmentProduct = ModuleProductUtil.generateAdjMandatoryData(adjustmentProduct);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);
    adjustmentProductQuota.setQuota(0);
    adjustmentProductQuota.setUsedQuota(0);
    adjustmentProductQuota.setUsedQuotaPerTransaction(0);
    adjustmentProductQuota.setItemSku("ABC-12345-12345-00001");
    adjustmentProductQuota.setAdjustmentProductId(adjustmentProduct.toId());
    adjustmentProductQuota.setId(adjustmentProductQuota.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,"ABC-12345-12345",SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFlashsaleItemSkus());
    assertEquals("ABC-12345-12345-00002",sivaProductMongoResult.getFlashsaleItemSkus().iterator().next());
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9950.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaProductMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaProductMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00001",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00001",sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.OOS));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00002",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00002",sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("9950.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9950.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00003",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertEquals("ABC-12345-12345-00003",sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("9950.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("9950.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertTrue(sivaItemMongoResult.getFlashsaleInventory().getQuota().getStatus().equals(StockStatus.AVAILABLE));
    assertTrue(sivaItemMongoResult.getInventory().getStatus().equals(StockStatus.AVAILABLE));
    assertFalse(sivaItemMongoResult.getFlashsale().isExclusive());
  }

  @Test
  public void onAdjustmentProductEvent_nearestPickupPointsValidation() throws Exception {
    //Iteration 1 : pp01 null FS 3K
    createCampaign(item.getItemSku(),"pp01",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,null,null,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,1000,CampaignPriority.REGULAR);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("9000.0",price.getFinalOfferPrice()+"");
          assertEquals("1000.0",price.getAdjustment()+"");
          assertNull(price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("9000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("1000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertNull(sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("9000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("1000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertNull(sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("9000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("1000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertNull(sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));

    //Iteration 2 : ppM cpgA FS 3K MERCHANT
    createCampaign(item.getItemSku(),"ppM",item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,"cpgA",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,3000,CampaignPriority.FLASH_SALE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("7000.0",price.getFinalOfferPrice()+"");
          assertEquals("3000.0",price.getAdjustment()+"");
          assertEquals("cpgA",price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("ppM",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("ppM")));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("7000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("3000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("ppM",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("ppM")));

    //Iteration 2 : pp01 cpgA FS 3K
    createCampaign(item.getItemSku(),"pp01",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,"cpgA",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,3000,CampaignPriority.FLASH_SALE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("7000.0",price.getFinalOfferPrice()+"");
          assertEquals("3000.0",price.getAdjustment()+"");
          assertEquals("cpgA",price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("7000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("3000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));

    //Iteration 3 : pp02 cpgA FS 3K
    createCampaign(item.getItemSku(),"pp02",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,"cpgA",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,3000,CampaignPriority.FLASH_SALE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("7000.0",price.getFinalOfferPrice()+"");
          assertEquals("3000.0",price.getAdjustment()+"");
          assertEquals("cpgA",price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(2,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp02")));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("7000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("3000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(2,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp02")));

    //Iteration 4 : pp03 cpgA FS 3K
    createCampaign(item.getItemSku(),"pp03",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,"cpgA",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,3000,CampaignPriority.FLASH_SALE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("7000.0",price.getFinalOfferPrice()+"");
          assertEquals("3000.0",price.getAdjustment()+"");
          assertEquals("cpgA",price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaProductMongoResult.getPickupPointCode());
    assertNull(sivaProductMongoResult.getNearestPickupPoints());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("7000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("3000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaItemMongoResult.getPickupPointCode());
    assertNull(sivaItemMongoResult.getNearestPickupPoints());

    //Iteration 5 : pp02 cpgA FS 3K DELETED
    createCampaign(item.getItemSku(),"pp02",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,"cpgA",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,3000,CampaignPriority.FLASH_SALE);
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("7000.0",price.getFinalOfferPrice()+"");
          assertEquals("3000.0",price.getAdjustment()+"");
          assertEquals("cpgA",price.getCampaignCode());
        });
    assertTrue(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(2,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp03")));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("7000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("3000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp01",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(2,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp01")));
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp03")));

    //Iteration 6 : pp01 cpgA FS 3K UNBUYABLE
    createCampaign(item.getItemSku(),"pp01",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,"cpgA",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,3000,CampaignPriority.FLASH_SALE);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setChannel(Channel.DEFAULT);
      val.setBuyable(false);
      val.getItemBuyableSchedules().setBuyable(false);
    });
    pickupPoint.setBuyable(false);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("7000.0",price.getFinalOfferPrice()+"");
          assertEquals("3000.0",price.getAdjustment()+"");
          assertEquals("cpgA",price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp03",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp03")));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("7000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("3000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("7000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("3000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgA",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp03",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp03")));

    //Iteration 7 : pp02 cpgB FS 4K
    createCampaign(item.getItemSku(),"pp02",item.getMerchantCode(),InventoryType.TYPE_ONLINE_WAREHOUSE,"cpgB",101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,4000,CampaignPriority.FLASH_SALE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    pickupPointMongoResult.getPrice().stream()
        .filter(Objects::nonNull)
        .forEach(price -> {
          assertEquals("6000.0",price.getFinalOfferPrice()+"");
          assertEquals("4000.0",price.getAdjustment()+"");
          assertEquals("cpgB",price.getCampaignCode());
        });
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("6000.0",sivaProductMongoResult.getPrice().getOfferValue()+"");
    assertEquals("4000.0",sivaProductMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgB",sivaProductMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp02",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaProductMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp02")));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered()+"");
    assertEquals("6000.0",sivaItemMongoResult.getItemPrice().getFinalOffered()+"");
    assertEquals("4000.0",sivaItemMongoResult.getItemPrice().getAdjustment().getValue()+"");
    assertEquals("cpgB",sivaItemMongoResult.getItemPrice().getAdjustment().getCampaignCode());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue()+"");
    assertEquals("6000.0",sivaItemMongoResult.getPrice().getOfferValue()+"");
    assertEquals("4000.0",sivaItemMongoResult.getPrice().getAdjustment().getValue()+"");
    assertEquals("cpgB",sivaItemMongoResult.getPrice().getAdjustment().getCampaignCode());
    assertEquals("pp02",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
    assertTrue(sivaItemMongoResult.getNearestPickupPoints().stream().map(PickupPointLocation::getPickupPointCode).anyMatch(ppCode -> ppCode.equals("pp02")));
  }

  @Test
  public void testOnFlashSaleAdjustmentProductEventForBlackListSellers() throws Exception {
    adjustmentProduct.setItemSku("ALS-60070");
    adjustmentProduct.setPriority(1);
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT, adjustmentProduct.toId(), adjustmentProduct,
        EXTRA_TIME, INDICES);
    SivaItem sivaItemMongoResult =
        getFromMongo(Collections.SIVA_ITEM, adjustmentProduct.getItemSku(), SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

}
