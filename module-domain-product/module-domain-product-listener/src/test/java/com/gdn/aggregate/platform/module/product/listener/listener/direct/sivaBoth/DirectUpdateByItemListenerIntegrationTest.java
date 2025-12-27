package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Objects;

import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaItemConstructorV2;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteAllRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.constants.PurchasedType;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
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
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

public class DirectUpdateByItemListenerIntegrationTest extends DummyHelper {

  @Autowired
  private SivaItemConstructorV2 sivaItemConstructorV2;

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    sivaProduct.setName(null);
    sivaProduct.setBuyable(null);
    sivaProduct.setDiscoverable(null);

    sivaItem.setItemName(null);
    sivaItem.setProductName(null);
    sivaItem.setBuyable(null);
    sivaItem.setDiscoverable(null);
  }

  @Test
  public void testOnItemEvent_failed_idNotValid() throws Exception {
    item.setItemSku(null);
    item.setId(item.toId());

    publishAndRefreshMultiple(Topics.ITEM, item.getItemSku(), item, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.ITEM,Item.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnItemEvent_failed_timestampNotValid() throws Exception {
    item.setTimestamp(currentTimestamp);
    item.setMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotEquals(0L,itemMongoResult.getTimestamp());
    assertFalse(itemMongoResult.isMarkForDelete());

    item.setTimestamp(0L);
    item.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.ITEM, item.getItemSku(), item, EXTRA_TIME, INDICES);

    itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotEquals(0L,itemMongoResult.getTimestamp());
  }

  @Test
  public void testOnAllItemEvent_failed_timestampNotValid() throws Exception {
    item.setTimestamp(currentTimestamp);
    item.setMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotEquals(0L,itemMongoResult.getTimestamp());
    assertFalse(itemMongoResult.isMarkForDelete());

    item.setTimestamp(0L);
    item.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.ALL_ITEM, item.getItemSku(), item, EXTRA_TIME, INDICES);

    itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotEquals(0L,itemMongoResult.getTimestamp());
    assertTrue(itemMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnItemEvent_failed_itemNotFound_productNotFound_processedDataNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnItemEvent_failed_itemNotFound_productNotFound() throws Exception {
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
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnItemEvent_failed_productNotFound_processedDataNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnItemEvent_success_hasNewestMasterDataItem() throws Exception {
    masterDataItem.setItemLength(1D);
    masterDataItem.setItemWidth(2D);
    masterDataItem.setItemHeight(3D);
    masterDataItem.setItemWeight(4D);
    masterDataItem.setTimestamp(0L);
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    masterDataProduct.setLength(null);
    masterDataProduct.setWidth(null);
    masterDataProduct.setHeight(null);
    masterDataProduct.setWeight(null);
    masterDataProduct.setTimestamp(0L);
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_PRODUCT)
        .domain(masterDataProduct)
        .clazz(MasterDataProduct.class)
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

    item.setMasterDataItem(masterDataItem);
    item.getMasterDataItem().setTimestamp(item.getTimestamp());
    product.setMasterDataProduct(masterDataProduct);
    product.getMasterDataProduct().setTimestamp(product.getTimestamp());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.getItemSku(), item, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));
    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1L, sivaProductMongoResult.getMeasurement().getLength().longValue());
    assertEquals(2L, sivaProductMongoResult.getMeasurement().getWidth().longValue());
    assertEquals(3L, sivaProductMongoResult.getMeasurement().getHeight().longValue());
    assertEquals(4L, sivaProductMongoResult.getMeasurement().getWeight().longValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(1L, sivaItemMongoResult.getMeasurement().getLength().longValue());
    assertEquals(2L, sivaItemMongoResult.getMeasurement().getWidth().longValue());
    assertEquals(3L, sivaItemMongoResult.getMeasurement().getHeight().longValue());
    assertEquals(4L, sivaItemMongoResult.getMeasurement().getWeight().longValue());
  }

  @Test
  public void testOnAllItemEventWithSivaDataMissingAndSwitchToBackFillOn_success() throws Exception {
    Object originalFlagValue = ReflectionTestUtils.getField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields");
    try {
      ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields",
        false);
      pickupPoint.setCncActive(true);
      pickupPoint.setItemViewConfigs(MainUtil.toSet(
        PickupPoint.ItemViewConfig.builder().channel(Channel.DEFAULT).buyable(false)
          .discoverable(false).build()));
      pickupPoint.setBuyable(false);
      pickupPoint.setDiscoverable(false);
      pickupPoint.setPurchasedType(PurchasedType.CNC);
      publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint,
        NORMAL_TIME, INDICES);
      save(SaveRequest.builder().index(Collections.ITEM).domain(item).clazz(Item.class).mongo(true)
        .elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.PRODUCT).domain(product).clazz(Product.class)
        .mongo(true).elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.MASTER_DATA).domain(masterData)
        .clazz(MasterData.class).mongo(true).elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.MASTER_DATA_PRODUCT).domain(masterDataProduct)
        .clazz(MasterDataProduct.class).mongo(true).elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.MASTER_DATA_ITEM).domain(masterDataItem)
        .clazz(MasterDataItem.class).mongo(true).elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo).clazz(CustomInventoryPickupPointInfo.class).mongo(true)
        .elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.SIVA_PRODUCT).domain(sivaProduct)
        .clazz(SivaProduct.class).mongo(true).elasticsearch(true).build());
      save(SaveRequest.builder().index(Collections.SIVA_ITEM).domain(sivaItem).clazz(SivaItem.class)
        .mongo(true).elasticsearch(true).build());

      publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

      Item itemMongoResult = getFromMongo(Collections.ITEM, item.getItemSku(), Item.class);
      assertNotNull(itemMongoResult);

      SivaProduct sivaProductMongoResult =
        getFromMongo(Collections.SIVA_PRODUCT, item.getProductSku(), SivaProduct.class);
      assertNotNull(sivaProductMongoResult);
      assertNull(sivaProductMongoResult.getName());
      assertNotNull(sivaProductMongoResult.getBuyable());
      assertFalse(sivaProductMongoResult.getBuyable().isValue());
      assertNotNull(sivaProductMongoResult.getDiscoverable());
      assertFalse(sivaProductMongoResult.getDiscoverable().isValue());
      assertNotNull(sivaProductMongoResult.getMeasurement());
      assertEquals(1, sivaProductMongoResult.getItemSkus().size());
      assertNotNull(sivaProductMongoResult.getPrice());
      assertNotNull(sivaProductMongoResult.getOfflinePrice());
      assertEquals(item.getItemSku(),
        ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult, true));
      assertEquals(item.getItemSku(),
        ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult, false));
      assertEquals(
        item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),
        sivaProductMongoResult.getImage());
      assertFalse(sivaProductMongoResult.isArchived());
      assertFalse(sivaProductMongoResult.isMarkForDelete());
      assertNotNull(sivaProductMongoResult.getPickupPointCode());
      assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

      SivaItem sivaItemMongoResult =
        getFromMongo(Collections.SIVA_ITEM, item.getItemSku(), SivaItem.class);
      assertNotNull(sivaItemMongoResult);
      assertFalse(sivaItemMongoResult.isOnL2());
      assertNotNull(sivaItemMongoResult.getItemName());
      assertNull(sivaItemMongoResult.getProductName());
      assertNotNull(sivaItemMongoResult.getBuyable());
      assertFalse(sivaItemMongoResult.getBuyable().isValue());
      assertNotNull(sivaItemMongoResult.getDiscoverable());
      assertFalse(sivaItemMongoResult.getDiscoverable().isValue());
      assertNotNull(sivaItemMongoResult.getItemCode());
      assertNotNull(sivaItemMongoResult.getMeasurement());
      assertNotNull(sivaItemMongoResult.getItemPrice());
      assertNotNull(sivaItemMongoResult.getOfflineItemPrice());
      assertNotNull(sivaItemMongoResult.getPrice());
      assertNotNull(sivaItemMongoResult.getOfflinePrice());
      assertEquals(item.getItemSku(),
        ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult, true));
      assertEquals(item.getItemSku(),
        ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult, false));
      assertEquals(
        item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),
        sivaItemMongoResult.getItemImage());
      assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next()
        .getLocationPath(), sivaItemMongoResult.getProductImage());
      assertFalse(sivaItemMongoResult.isArchived());
      assertFalse(sivaItemMongoResult.isMarkForDelete());
      assertNotNull(sivaItemMongoResult.getPickupPointCode());
      assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    }
    finally {
      ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields", originalFlagValue);
    }
  }


  @Test
  public void testOnItemEvent_success_unsync_hasMasterDataItem() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    item.setSync(false);
    publishAndRefreshMultiple(Topics.ITEM, item.getItemSku(), item, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
  }

  @Test
  public void testOnItemEvent_success_unsync_hasNoMasterDataItem() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
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
    item.setSync(false);
    item.setMasterDataItem(null);
    publishAndRefreshMultiple(Topics.ITEM, item.getItemSku(), item, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
  }

  @Test
  public void testOnItemEvent_success_processedDataNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
  }

  @Test
  public void testOnItemEvent_success_productNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
  }

  @Test
  public void testOnItemEvent_success() throws Exception {
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE_CNC);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult,true));
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult,false));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isArchived());
    assertFalse(sivaProductMongoResult.isMarkForDelete());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult,true));
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult,false));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertFalse(sivaItemMongoResult.isArchived());
    assertFalse(sivaItemMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnItemEvent_success_pickupPointCodeNull_pickupPointNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
  }

  @Test
  public void testOnItemEvent_success_pickupPointCodeNull_pickupPointExists() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
  }

  @Test
  public void testOnItemEvent_success_multipleItems() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.getProductSku(), item, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, cheapestItem.getProductSku(), cheapestItem, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));
    assertTrue(existsInMongo(Collections.ITEM,cheapestItem.getItemSku(),Item.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,cheapestItem.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(2,sivaProductMongoResult.getItemSkus().size());
    assertEquals(cheapestItem.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestItem.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(cheapestItem.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(cheapestItem.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(cheapestItem.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
  }

  @Test
  public void testOnItemEvent_archived() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    item.setArchived(true);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, NORMAL_TIME, INDICES);
    cheapestItem.setArchived(false);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(cheapestItem)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, otherCheapestPickupPoint.toId(), otherCheapestPickupPoint, NORMAL_TIME, INDICES);
    otherCheapestItem.setArchived(true);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(otherCheapestItem)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);
    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);
    PickupPoint pickupPointResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointResult);

    publishAndRefreshMultiple(Topics.ITEM, cheapestItem.toId(), cheapestItem, EXTRA_TIME, INDICES);
    Item cheapestItemMongoResult = getFromMongo(Collections.ITEM,cheapestItem.getItemSku(),Item.class);
    assertNotNull(cheapestItemMongoResult);
    PickupPoint cheapestPickupPointResult = getFromMongo(Collections.PICKUP_POINT,cheapestPickupPoint.toId(),PickupPoint.class);
    assertNotNull(cheapestPickupPointResult);

    publishAndRefreshMultiple(Topics.ITEM, otherCheapestItem.toId(), otherCheapestItem, EXTRA_TIME, INDICES);
    Item otherCheapestItemMongoResult = getFromMongo(Collections.ITEM,otherCheapestItem.getItemSku(),Item.class);
    assertNotNull(otherCheapestItemMongoResult);
    PickupPoint otherCheapestPickupPointResult = getFromMongo(Collections.PICKUP_POINT,otherCheapestPickupPoint.toId(),PickupPoint.class);
    assertNotNull(otherCheapestPickupPointResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getItemSkus()));
    assertEquals(sivaProductMongoResult.getPrice().getCheapestItemSku(),cheapestItem.getItemSku());
    assertEquals(sivaProductMongoResult.getImage(),"locationPath");
    assertFalse(sivaProductMongoResult.isArchived());
    assertFalse(sivaProductMongoResult.isMarkForDelete());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(sivaItemMongoResult.getItemPrice().getCheapestItemSku(),item.getItemSku());
    assertEquals(sivaItemMongoResult.getPrice().getCheapestItemSku(),item.getItemSku());
    assertEquals(sivaItemMongoResult.getItemImage(),"locationPath");
    assertEquals(sivaItemMongoResult.getProductImage(),"new-locationPath");
    assertTrue(sivaItemMongoResult.isArchived());
    assertFalse(sivaItemMongoResult.isMarkForDelete());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());

    SivaItem cheapestSivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,cheapestItem.getItemSku(),SivaItem.class);
    assertNotNull(cheapestSivaItemMongoResult);
    assertNotNull(cheapestSivaItemMongoResult.getItemName());
    assertNotNull(cheapestSivaItemMongoResult.getProductName());
    assertNotNull(cheapestSivaItemMongoResult.getBuyable());
    assertTrue(cheapestSivaItemMongoResult.getBuyable().isValue());
    assertNotNull(cheapestSivaItemMongoResult.getDiscoverable());
    assertTrue(cheapestSivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(cheapestSivaItemMongoResult.getItemPrice().getCheapestItemSku(),cheapestItem.getItemSku());
    assertEquals(cheapestSivaItemMongoResult.getItemImage(),"locationPath");
    assertEquals(cheapestSivaItemMongoResult.getProductImage(),"locationPath");
    assertFalse(cheapestSivaItemMongoResult.isArchived());
    assertFalse(cheapestSivaItemMongoResult.isMarkForDelete());
    assertNotNull(cheapestSivaItemMongoResult.getPickupPointCode());
    assertNotNull(cheapestSivaItemMongoResult.getExternalPickupPointCode());

    SivaItem otherCheapestSivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,otherCheapestItem.getItemSku(),SivaItem.class);
    assertNotNull(otherCheapestSivaItemMongoResult);
    assertNotNull(otherCheapestSivaItemMongoResult.getItemName());
    assertNotNull(otherCheapestSivaItemMongoResult.getProductName());
    assertNotNull(otherCheapestSivaItemMongoResult.getBuyable());
    assertTrue(otherCheapestSivaItemMongoResult.getBuyable().isValue());
    assertNotNull(otherCheapestSivaItemMongoResult.getDiscoverable());
    assertTrue(otherCheapestSivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(otherCheapestSivaItemMongoResult.getItemPrice().getCheapestItemSku(),otherCheapestItem.getItemSku());
    assertEquals(otherCheapestSivaItemMongoResult.getItemImage(),"locationPath");
    assertEquals(otherCheapestSivaItemMongoResult.getProductImage(),"locationPath");
    assertTrue(otherCheapestSivaItemMongoResult.isArchived());
    assertFalse(otherCheapestSivaItemMongoResult.isMarkForDelete());
    assertNotNull(otherCheapestSivaItemMongoResult.getPickupPointCode());
    assertNotNull(otherCheapestSivaItemMongoResult.getExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_productMarkForDeleteTrue() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);

    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    product.setMarkForDelete(true);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
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
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    PickupPoint pickupPointResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isArchived());
    assertTrue(sivaProductMongoResult.isMarkForDelete());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertFalse(sivaItemMongoResult.isArchived());
    assertTrue(sivaItemMongoResult.isMarkForDelete());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_itemMarkForDeleteTrue() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    item.setMarkForDelete(true);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    PickupPoint pickupPointResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isArchived());
    assertTrue(sivaProductMongoResult.isMarkForDelete());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertFalse(sivaItemMongoResult.isArchived());
    assertTrue(sivaItemMongoResult.isMarkForDelete());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_bothMarkForDeleteTrue() throws Exception {
    ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields", false);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    item.setMarkForDelete(true);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    product.setMarkForDelete(true);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    PickupPoint pickupPointResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isArchived());
    assertTrue(sivaProductMongoResult.isMarkForDelete());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertFalse(sivaItemMongoResult.isArchived());
    assertTrue(sivaItemMongoResult.isMarkForDelete());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
  }

  @Test
  public void testOnAllItemEvent_success() throws Exception {
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(false)
        .discoverable(false)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertFalse(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getMeasurement());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getOfflinePrice());
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult,true));
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult,false));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isArchived());
    assertFalse(sivaProductMongoResult.isMarkForDelete());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isOnL2());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertFalse(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertFalse(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getMeasurement());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getOfflineItemPrice());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getOfflinePrice());
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult,true));
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult,false));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertFalse(sivaItemMongoResult.isArchived());
    assertFalse(sivaItemMongoResult.isMarkForDelete());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
  }

  @Test
  public void testOnAllItemEvent_whenMasterDataItemNameIsNotDefined_shouldGenerateItemNameUrlWithProductName() {
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.NOT_AVAILABLE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
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

    item.setMasterDataItem(null);
    publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertTrue(sivaProductMongoResult.getUrl().contains("/p/samsung-galaxy-8/"));
  }

  @Test
  public void testOnAllItemEvent_whenItemSkuIsEmpty_shouldUseDefaultItemSkuOnUrl() {
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.NOT_AVAILABLE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
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

    item.setGeneratedItemName(null);
    item.setMasterDataItem(null);
    publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertTrue(sivaProductMongoResult.getUrl().contains("/p/generateditemname/is--ABC-12345-12345-00001"));
  }

  @Test
  public void testOnAllItemEvent_whenPriceIsNotDefinedOnItem_shouldConstructForceRepublishMetadataOnUrlParam() {
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.NOT_AVAILABLE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
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

    item.setGeneratedItemName(null);
    item.setMasterDataItem(null);
    publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertEquals("/p/generateditemname/is--ABC-12345-12345-00001?pickupPointCode=pickupPointCode&" +
            "metaData=IXX3G17qN9Z8N3dvg8OhNZloPBPAXbM7KN5clTMoHHlV21zq9Xy0TYsEqd12n/n+C2fP/NjuPG6o1GCgX9DtjA==",
        sivaProductMongoResult.getUrl());
  }

  @Test
  public void testOnAllItemEvent_whenMasterDataProductIsNotDefined_shouldConstructProductSkuAsProductName() {
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.NOT_AVAILABLE);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    product.setMasterDataProduct(null);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
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

    item.setGeneratedItemName(null);
    item.setMasterDataItem(null);
    publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertTrue(sivaProductMongoResult.getUrl().contains(String.format("/p/%s/is--", product.getProductSku().toLowerCase())));
  }

  @Test
  public void testOnAllItemEvent_success_multiplePickupPoint() throws Exception {
    pickupPoint.setPickupPointCode("pickupPointCodeCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(false)
        .discoverable(false)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeOnline");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(false);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(9000D));
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    cheapestPickupPoint.setPickupPointCode("pickupPointCodeCnc");
    cheapestPickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    cheapestPickupPoint.setId(cheapestPickupPoint.toId());
    cheapestPickupPoint.setCncActive(true);
    cheapestPickupPoint.setItemViewConfigs(ModuleProductUtil.toDefaultItemViewConfigs(false,false));
    cheapestPickupPoint.setBuyable(false);
    cheapestPickupPoint.setDiscoverable(false);
    cheapestPickupPoint.setPurchasedType(PurchasedType.CNC);
    cheapestPickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, NORMAL_TIME, INDICES);
    cheapestPickupPoint.setPickupPointCode("pickupPointCodeOnline");
    cheapestPickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    cheapestPickupPoint.setId(cheapestPickupPoint.toId());
    cheapestPickupPoint.setCncActive(false);
    cheapestPickupPoint.setItemViewConfigs(ModuleProductUtil.toDefaultItemViewConfigs(true,true));
    cheapestPickupPoint.setBuyable(true);
    cheapestPickupPoint.setDiscoverable(true);
    cheapestPickupPoint.setPurchasedType(PurchasedType.ONLINE);
    cheapestPickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(9000D));
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, cheapestPickupPoint.toId(), cheapestPickupPoint, NORMAL_TIME, INDICES);
    otherCheapestPickupPoint.setPickupPointCode("pickupPointCodeCnc");
    otherCheapestPickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    otherCheapestPickupPoint.setId(otherCheapestPickupPoint.toId());
    otherCheapestPickupPoint.setCncActive(true);
    otherCheapestPickupPoint.setItemViewConfigs(ModuleProductUtil.toDefaultItemViewConfigs(false,false));
    otherCheapestPickupPoint.setBuyable(false);
    otherCheapestPickupPoint.setDiscoverable(false);
    otherCheapestPickupPoint.setPurchasedType(PurchasedType.CNC);
    otherCheapestPickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, otherCheapestPickupPoint.toId(), otherCheapestPickupPoint, NORMAL_TIME, INDICES);
    otherCheapestPickupPoint.setPickupPointCode("pickupPointCodeOnline");
    otherCheapestPickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    otherCheapestPickupPoint.setId(otherCheapestPickupPoint.toId());
    otherCheapestPickupPoint.setCncActive(false);
    otherCheapestPickupPoint.setItemViewConfigs(ModuleProductUtil.toDefaultItemViewConfigs(true,true));
    otherCheapestPickupPoint.setBuyable(true);
    otherCheapestPickupPoint.setDiscoverable(true);
    otherCheapestPickupPoint.setPurchasedType(PurchasedType.ONLINE);
    otherCheapestPickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(9000D));
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, otherCheapestPickupPoint.toId(), otherCheapestPickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ALL_ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertEquals(1,sivaProductMongoResult.getItemSkus().size());
    assertNotNull(sivaProductMongoResult.getPrice());
    assertNotNull(sivaProductMongoResult.getOfflinePrice());
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult,true));
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaProduct(sivaProductMongoResult,false));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isArchived());
    assertFalse(sivaProductMongoResult.isMarkForDelete());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnline",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnline",sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertNotNull(sivaItemMongoResult.getOfflineItemPrice());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertNotNull(sivaItemMongoResult.getOfflinePrice());
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult,true));
    assertEquals(item.getItemSku(), ModuleProductUtil.getCheapestItemSkuFromSivaItem(sivaItemMongoResult,false));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertFalse(sivaItemMongoResult.isArchived());
    assertFalse(sivaItemMongoResult.isMarkForDelete());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnline",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnline",sivaItemMongoResult.getExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_success_cncValidation_samePrice() throws Exception {
    pickupPoint.setPickupPointCode("pickupPointCodeOnline");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(false);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(10000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(false)
        .discoverable(false)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(10000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeOnlineCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnlineCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE_CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(10000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull("10000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull("10000.0",sivaProductMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaProductMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaProductMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaProductMongoResult.getOfflineExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull("10000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull("10000.0",sivaItemMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaItemMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaItemMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaItemMongoResult.getOfflineExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_success_cncValidation_onlineCheapest() throws Exception {
    pickupPoint.setPickupPointCode("pickupPointCodeOnline");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(false);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(7000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(false)
        .discoverable(false)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(9000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeOnlineCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnlineCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE_CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull("9000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull("9000.0",sivaProductMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaProductMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaProductMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaProductMongoResult.getOfflineExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull("9000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull("9000.0",sivaItemMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaItemMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaItemMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaItemMongoResult.getOfflineExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_success_cncValidation_cncCheapest() throws Exception {
    pickupPoint.setPickupPointCode("pickupPointCodeOnline");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(false);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(9000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(false)
        .discoverable(false)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    pickupPoint.setPickupPointCode("pickupPointCodeOnlineCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnlineCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE_CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull("9000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull("8000.0",sivaProductMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaProductMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaProductMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaProductMongoResult.getOfflineExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull("9000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull("8000.0",sivaItemMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnlineCnc",sivaItemMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaItemMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnlineCnc",sivaItemMongoResult.getOfflineExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_success_cncValidation_onlineOnly() throws Exception {
    pickupPoint.setPickupPointCode("pickupPointCodeOnline");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeOnline");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(false);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(true)
        .discoverable(true)
        .build()));
    pickupPoint.setBuyable(true);
    pickupPoint.setDiscoverable(true);
    pickupPoint.setPurchasedType(PurchasedType.ONLINE);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull("8000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnline",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnline",sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull("9000.0",sivaProductMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnline",sivaProductMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaProductMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnline",sivaProductMongoResult.getOfflineExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull("8000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeOnline",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnline",sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull("9000.0",sivaItemMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeOnline",sivaItemMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaItemMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeOnline",sivaItemMongoResult.getOfflineExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_success_cncValidation_cncOnly() throws Exception {
    pickupPoint.setPickupPointCode("pickupPointCodeCnc");
    pickupPoint.setExternalPickupPointCode("externalPickupPointCodeCnc");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(true);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(PickupPoint.ItemViewConfig.builder()
        .channel(Channel.DEFAULT)
        .buyable(false)
        .discoverable(false)
        .build()));
    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(PurchasedType.CNC);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(8000D));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
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

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.getItemSku(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertFalse(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull("9000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeCnc",sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeCnc",sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull("8000.0",sivaProductMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaProductMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeCnc",sivaProductMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaProductMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeCnc",sivaProductMongoResult.getOfflineExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertFalse(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertFalse(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull("9000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCodeCnc",sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCodeCnc",sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull("8000.0",sivaItemMongoResult.getOfflinePrice().getOfferValue().toString());
    assertNotNull(sivaItemMongoResult.getOfflinePickupPointCode());
    assertEquals("pickupPointCodeCnc",sivaItemMongoResult.getOfflinePickupPointCode());
    assertNotNull(sivaItemMongoResult.getOfflineExternalPickupPointCode());
    assertEquals("externalPickupPointCodeCnc",sivaItemMongoResult.getOfflineExternalPickupPointCode());
  }

  @Test
  public void testOnItemEvent_imageValidation() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5000, CampaignPriority.REGULAR);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.ITEM)
        .mongo(true)
        .build());

    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

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
  public void testOnItemEvent_itemCodeValidation() throws Exception {
    publishAndRefreshMultiple(Topics.ITEM,item.toId(),item,EXTRA_TIME,INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getItemCode());

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(CollectionUtils.isEmpty(pickupPointMongoResult.getPrice()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getPrice());
  }

  @Test
  public void testOnItemEvent_newItem() throws Exception {
    item.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.ITEM,item.toId(),item,EXTRA_TIME,INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getItemCode());

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(CollectionUtils.isEmpty(pickupPointMongoResult.getPrice()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getPrice());
  }

  @Test
  public void testOnItemEvent_oldItemAndSourceNull() throws Exception {
    item.setNewData(false);
    publishAndRefreshMultiple(Topics.ITEM,item.toId(),item,EXTRA_TIME,INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getItemCode());

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(CollectionUtils.isEmpty(pickupPointMongoResult.getPrice()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getPrice());
  }

  @Test
  public void testOnItemEvent_oldItemSourceProductPublish() throws Exception {
    item.setNewData(false);
    item.setSource("PRODUCT_PUBLISH");
    publishAndRefreshMultiple(Topics.ITEM,item.toId(),item,0L,INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,0L,INDICES);
    sleep(EXTRA_TIME);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNull(itemMongoResult);
  }
}
