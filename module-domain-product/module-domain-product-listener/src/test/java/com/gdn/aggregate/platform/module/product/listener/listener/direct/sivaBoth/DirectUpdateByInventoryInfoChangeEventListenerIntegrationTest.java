package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByInventoryInfoChangeEventListenerIntegrationTest extends DummyHelper {

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
    sivaProduct.setPickupPointCode(null);

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
    sivaItem.setPickupPointCode(null);
  }

  @Test
  public void testOnInventoryInfoChangeEvent_failed_mainDataNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, LESS_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnInventoryInfoChangeEvent_failed_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
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
  public void testOnInventoryInfoChangeEvent_failed_inventoryDataNotValid() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    inventoryPickupPointInfo.setMarkForDelete(true);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
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
    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
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
  public void   testOnInventoryInfoChangeEvent_success() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    inventoryInfoChangeEvent.setTimestamp(1733936400000L);
    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME*5, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertEquals(1733936400000L,sivaProductMongoResult.getTimestamp());
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals(1733936400000L,sivaItemMongoResult.getTimestamp());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
  }

  @Test
  public void testOnInventoryInfoChangeEvent_success_online() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setBuyable(true);
      val.setDiscoverable(true);
    });
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setCncActive(false);
    pickupPoint.setInStock(false);
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

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.ONLINE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());

    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.ONLINE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getInventory());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getInventory());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
  }

  @Test
  public void testOnInventoryInfoChangeEvent_success_online_L5Deleted() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setBuyable(true);
      val.setDiscoverable(true);
    });
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setCncActive(false);
    pickupPoint.setInStock(false);
    pickupPoint.setMarkForDelete(true);
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

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.ONLINE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertTrue(pickupPointMongoResult.isMarkForDelete());

    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.ONLINE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertTrue(pickupPointMongoResult.isMarkForDelete());

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getInventory());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getInventory());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
  }

  @Test
  public void testOnInventoryInfoChangeEvent_success_offline() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setBuyable(false);
      val.setDiscoverable(false);
    });
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setCncActive(true);
    pickupPoint.setInStock(false);
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

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.CNC,pickupPointMongoResult.getPurchasedType());
    assertTrue(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));

    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.CNC,pickupPointMongoResult.getPurchasedType());
    assertTrue(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getInventory());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getInventory());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
  }

  @Test
  public void testOnInventoryInfoChangeEvent_success_offline_L5Deleted() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setBuyable(false);
      val.setDiscoverable(false);
    });
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setCncActive(true);
    pickupPoint.setInStock(false);
    pickupPoint.setMarkForDelete(true);
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

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.CNC,pickupPointMongoResult.getPurchasedType());
    assertTrue(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertTrue(pickupPointMongoResult.isMarkForDelete());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));

    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.CNC,pickupPointMongoResult.getPurchasedType());
    assertTrue(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertTrue(pickupPointMongoResult.isMarkForDelete());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getInventory());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getInventory());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
  }

  @Test
  public void testOnInventoryInfoChangeEvent_success_onlineToNA() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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

    //Iteration 1 : Online

    inventoryInfo.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
        .location("location1")
        .availableStock(0)
        .originalStock(0)
        .status("OUT_OF_STOCK")
        .stockInformationDetails(null)
        .build()));
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setBuyable(true);
      val.setDiscoverable(true);
    });
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setCncActive(false);
    pickupPoint.setInStock(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.ONLINE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertFalse(pickupPointMongoResult.isInStock());

    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.ONLINE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertFalse(pickupPointMongoResult.isInStock());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getInventory());
    assertEquals(0L,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(0L,sivaProductMongoResult.getInventory().getQuota());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getInventory());
    assertEquals(0L,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(0L,sivaItemMongoResult.getInventory().getQuota());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());

    //Iteration 1 : NA

    inventoryInfo.setStockInformations(MainUtil.toList(CustomInventoryInfo.StockInformation.builder()
        .location("location1")
        .availableStock(7)
        .originalStock(10)
        .status("IN_STOCK")
        .stockInformationDetails(MainUtil.toList(CustomInventoryInfo.StockInformationDetail.builder()
            .pickupPointCode(pickupPoint.getPickupPointCode())
            .availableStock(7)
            .originalStock(10)
            .status("IN_STOCK")
            .build()))
        .build()));
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_INFORMATION_COLLECTION)
        .domain(inventoryInfo)
        .clazz(CustomInventoryInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPurchasedType(PurchasedType.NOT_AVAILABLE);
    pickupPoint.getItemViewConfigs().stream().filter(Objects::nonNull).forEach(val -> {
      val.setBuyable(false);
      val.setDiscoverable(false);
    });
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setCncActive(false);
    pickupPoint.setInStock(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.NOT_AVAILABLE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));

    publishAndRefreshMultiple(Topics.INVENTORY_INFO_CHANGE, inventoryInfoChangeEvent.toId(), inventoryInfoChangeEvent, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals(PurchasedType.NOT_AVAILABLE,pickupPointMongoResult.getPurchasedType());
    assertFalse(pickupPointMongoResult.isCncActive());
    assertTrue(pickupPointMongoResult.isInStock());

    sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> inventoryInfoChangeEvent.getItemSku().equals(itemSku)))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNull(sivaProductMongoResult.getPriceTeaser());
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getFlashsaleItemSkus()));
    assertNotNull(sivaProductMongoResult.getInventory());
    assertEquals(7L,sivaProductMongoResult.getInventory().getRemaining());
    assertEquals(10L,sivaProductMongoResult.getInventory().getQuota());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,inventoryInfoChangeEvent.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNull(sivaItemMongoResult.getItemPriceTeaser());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNull(sivaItemMongoResult.getPriceTeaser());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNull(sivaItemMongoResult.getFlashsaleItemSku());
    assertNotNull(sivaItemMongoResult.getInventory());
    assertEquals(7L,sivaItemMongoResult.getInventory().getRemaining());
    assertEquals(10L,sivaItemMongoResult.getInventory().getQuota());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
  }

}
