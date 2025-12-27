package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.DeleteAllRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductTag;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
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

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByPickupPointListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    pickupPoint.setBuyable(false);
    pickupPoint.setDiscoverable(false);
    pickupPoint.setPurchasedType(null);

    sivaProduct.setPickupPointCode(null);
    sivaProduct.setExternalPickupPointCode(null);
    sivaProduct.setUrl(null);
    sivaProduct.setImage(null);
    sivaProduct.setHasCncActivated(false);
    sivaProduct.setOnlineOnly(false);

    sivaItem.setPickupPointCode(null);
    sivaItem.setExternalPickupPointCode(null);
    sivaItem.setProductUrl(null);
    sivaItem.setItemUrl(null);
    sivaItem.setItemImage(null);
    sivaItem.setProductImage(null);
    sivaItem.setHasCncActivated(false);
    sivaItem.setOnlineOnly(false);
  }

  @Test
  public void testOnPickupPointEvent_failed_idNotValid() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode(null);
    pickupPoint.setItemSku(null);
    pickupPoint.setId(pickupPoint.toId());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.PICKUP_POINT,PickupPoint.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnPickupPointEvent_failed_timestampNotValid() throws Exception {
    pickupPoint.setTimestamp(currentTimestamp);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotEquals(0L,pickupPointMongoResult.getTimestamp());
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    pickupPoint.setTimestamp(0L);
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotEquals(0L,pickupPointMongoResult.getTimestamp());
    assertFalse(pickupPointMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnAllPickupPointEvent_failed_timestampNotValid() throws Exception {
    pickupPoint.setTimestamp(currentTimestamp);
    pickupPoint.setMarkForDelete(false);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotEquals(0L,pickupPointMongoResult.getTimestamp());
    assertFalse(pickupPointMongoResult.isMarkForDelete());

    pickupPoint.setTimestamp(0L);
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotEquals(0L,pickupPointMongoResult.getTimestamp());
    assertTrue(pickupPointMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnPickupPointEvent_failed_processedDataNotExists() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isHasCncActivated());
    assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaProductMongoResult.isOnlineOnly());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertFalse(sivaItemMongoResult.isHasCncActivated());
    assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaItemMongoResult.isOnlineOnly());
  }

  @Test
  public void testOnPickupPointEvent_failed_itemNotFound() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isHasCncActivated());
    assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaProductMongoResult.isOnlineOnly());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertFalse(sivaItemMongoResult.isHasCncActivated());
    assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaItemMongoResult.isOnlineOnly());
  }

  @Test
  public void testOnPickupPointEvent_failed_deleted() throws Exception {
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

    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertTrue(sivaProductMongoResult.getUrl().contains("?pickupPointCode"));
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isHasCncActivated());
    assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaProductMongoResult.isOnlineOnly());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertTrue(sivaItemMongoResult.getItemUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertFalse(sivaItemMongoResult.isHasCncActivated());
    assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaItemMongoResult.isOnlineOnly());
  }

  @Test
  public void testOnPickupPointEvent_failed_oos() throws Exception {
    inventoryPickupPointInfo.setAvailableStock(0);
    inventoryPickupPointInfo.setOriginalStock(0);
    inventoryPickupPointInfo.setSafetyStock(0);
    inventoryPickupPointInfo.getWarehouseInfos().stream()
        .filter(Objects::nonNull)
        .forEach(val -> {
          val.setAvailableStock(0);
          val.setOriginalStock(0);
        });
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryInfo.getStockInformations().stream()
        .filter(Objects::nonNull)
        .forEach(val -> {
          val.setAvailableStock(0);
          val.setOriginalStock(0);
          val.setStatus(StockStatus.OOS);
          val.getStockInformationDetails().stream()
              .filter(Objects::nonNull)
              .forEach(subVal -> {
                subVal.setAvailableStock(0);
                subVal.setOriginalStock(0);
                subVal.setStatus(StockStatus.OOS);
              });
        });
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertTrue(sivaProductMongoResult.getUrl().contains("?pickupPointCode"));
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isHasCncActivated());
    assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaProductMongoResult.isOnlineOnly());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertTrue(sivaItemMongoResult.getItemUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertFalse(sivaItemMongoResult.isHasCncActivated());
    assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaItemMongoResult.isOnlineOnly());
  }

  @Test
  public void testOnPickupPointEvent_success() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertTrue(sivaProductMongoResult.getUrl().contains("?pickupPointCode"));
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isHasCncActivated());
    assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaProductMongoResult.isOnlineOnly());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertTrue(sivaItemMongoResult.getItemUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertFalse(sivaItemMongoResult.isHasCncActivated());
    assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaItemMongoResult.isOnlineOnly());
  }

  @Test
  public void testOnPickupPointEvent_success_update() throws Exception {
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertTrue(sivaProductMongoResult.getUrl().contains("?pickupPointCode"));
    assertNotNull(sivaProductMongoResult.getImage());
    assertFalse(sivaProductMongoResult.isHasCncActivated());
    assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaProductMongoResult.isOnlineOnly());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertTrue(sivaItemMongoResult.getItemUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertFalse(sivaItemMongoResult.isHasCncActivated());
    assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertTrue(sivaItemMongoResult.isOnlineOnly());
  }

  @Test
  public void testOnPickupPointEvent_multiple_allOnline() throws Exception {
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode2");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode3");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode4");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
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

    testOnPickupPointEvent( "",10000d,false,true,true,1,"", "",item.getItemSku());
    assertBuyableAndDiscoverable(true, true, true, true);
    testOnPickupPointEvent("2", 7000d,false,true,true,2,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(true, true, true, true);
    testOnPickupPointEvent("3", 8000d,false,true,true,3,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(true, true, true, true);
    testOnPickupPointEvent("4",10000d,false,true,true,4,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(true, true, true, true);
  }

  @Test
  public void testOnPickupPointEvent_multiple_allOffline() throws Exception {
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode2");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode3");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode4");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
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

    testOnPickupPointEvent( "",10000d,true,false,false,1,"", "",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("2", 7000d,true,false,false,2,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("3", 8000d,true,false,false,3,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("4",10000d,true,false,false,4,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
  }

  @Test
  public void testOnPickupPointEvent_multiple_allNA() throws Exception {
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode2");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode3");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode4");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
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

    testOnPickupPointEvent( "",10000d,false,false,false,1,"", "",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("2", 7000d,false,false,false,2,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("3", 8000d,false,false,false,3,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("4",10000d,false,false,false,4,"2","2",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
  }

  @Test
  public void testOnPickupPointEvent_multiple_onlineAndNA() throws Exception {
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode2");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode3");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode4");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
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

    testOnPickupPointEvent( "",10000d,false,false,false,1,"", "",item.getItemSku());
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("2", 7000d,false,false,false,2,"2","2",item.getItemSku());
    testOnPickupPointEvent("3", 8000d,false,true,true,3,"3","3",item.getItemSku());
    assertBuyableAndDiscoverable(true, true, true, true);
    testOnPickupPointEvent("4", 6000d,false,false,false,4,"3","3",item.getItemSku());
    assertBuyableAndDiscoverable(true, true, true, true);
  }

  @Test
  public void testOnPickupPointEvent_multipleItems_onlineAndNA() throws Exception {
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode2");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode3");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCode4");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
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
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.setItemSku("ABC-12345-12345-00001");
    item.setId(item.toId());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.setItemSku("ABC-12345-12345-00002");
    item.setId(item.toId());
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

    testOnPickupPointEvent( "",10000d,false,false,false,1,"", "","ABC-12345-12345-00001");
    assertBuyableAndDiscoverable(false, false, false, false);
    testOnPickupPointEvent("2", 7000d,false,true,true,2,"2","2","ABC-12345-12345-00001");
    assertBuyableAndDiscoverable(true, true, true, true);
    testOnPickupPointEvent("3", 8000d,false,false,false,3,"2","3","ABC-12345-12345-00002");
    assertBuyableAndDiscoverable(true, true, false, false);
    testOnPickupPointEvent("4", 6000d,false,true,true,4,"4","4","ABC-12345-12345-00002");
    assertBuyableAndDiscoverable(true, true, true, true);
  }

  private void testOnPickupPointEvent(String pp, double price, boolean cncActive, boolean buyable, boolean discoverable, int expectedSize, String sivaProductPP, String sivaItemPP, String itemSku) throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setItemSku(itemSku);
    pickupPoint.setPickupPointCode("pickupPointCode" + pp);
    pickupPoint.setExternalPickupPointCode("externalPickupPointCode" + pp);
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> val.setOfferPrice(price));
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setCncActive(cncActive);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(
      PickupPoint.ItemViewConfig.builder().buyable(false).discoverable(false).build(),
      PickupPoint.ItemViewConfig.builder().channel(Channel.DEFAULT).buyable(buyable).discoverable(discoverable).build()
    ));
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    assertEquals(expectedSize,getAllFromMongo(Collections.PICKUP_POINT,PickupPoint.class).size());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);

    PickupPoint pickupPointResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointResult.getPickupPointCode());
    assertEquals("pickupPointCode" + pp, pickupPointResult.getPickupPointCode());
    assertNotNull(pickupPointResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCode" + pp, pickupPointResult.getExternalPickupPointCode());
    assertEquals(1, pickupPointResult.getItemViewConfigs().size());
    assertEquals(1, pickupPointResult.getItemViewConfigs().stream()
      .filter(itemViewConfig -> Channel.DEFAULT.equals(itemViewConfig.getChannel())).count());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertEquals("pickupPointCode" + sivaProductPP,sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCode" + sivaProductPP,sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertTrue(sivaProductMongoResult.getUrl().contains("?pickupPointCode=pickupPointCode" + sivaProductPP));
    assertNotNull(sivaProductMongoResult.getImage());
    if (!cncActive) {
      assertFalse(sivaProductMongoResult.isHasCncActivated());
      assertFalse(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
      assertTrue(sivaProductMongoResult.isOnlineOnly());
    } else {
      assertTrue(sivaProductMongoResult.isHasCncActivated());
      assertTrue(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
      assertFalse(sivaProductMongoResult.isOnlineOnly());
    }

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertEquals("pickupPointCode" + sivaItemPP,sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertEquals("externalPickupPointCode" + sivaItemPP,sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("?pickupPointCode=pickupPointCode" + sivaItemPP));
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertTrue(sivaItemMongoResult.getItemUrl().contains("?pickupPointCode=pickupPointCode" + sivaItemPP));
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    if (!cncActive) {
      assertFalse(sivaItemMongoResult.isHasCncActivated());
      assertFalse(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
      assertTrue(sivaItemMongoResult.isOnlineOnly());
    } else {
      assertTrue(sivaItemMongoResult.isHasCncActivated());
      assertTrue(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
      assertFalse(sivaItemMongoResult.isOnlineOnly());
    }
  }

  private void assertBuyableAndDiscoverable(boolean productBuyable, boolean productDiscavorable,
    boolean itemBuyable, boolean itemDiscavorable) {
    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertEquals(productBuyable, sivaProductMongoResult.getBuyable().isValue());
    assertEquals(productDiscavorable, sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertEquals(itemBuyable, sivaItemMongoResult.getBuyable().isValue());
    assertEquals(itemDiscavorable, sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnAllPickupPointEvent_success() throws Exception {
    pickupPoint.setId(null);
    pickupPoint.setCncActive(true);
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

    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());
    assertEquals(0, pickupPointMongoResult.getItemViewConfigs().stream()
            .filter(pp -> "B2B".equals(pp.getChannel()))
            .count());
    assertNotNull(pickupPointMongoResult.getItemViewConfigs());

    Item itemResult = getFromMongo(Collections.ITEM,pickupPoint.getItemSku(),Item.class);
    assertNotNull(itemResult);
    assertTrue(itemResult.isSync());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,pickupPoint.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaProductMongoResult.getUrl());
    assertTrue(sivaProductMongoResult.getUrl().contains("?pickupPointCode"));
    assertNotNull(sivaProductMongoResult.getImage());
    assertTrue(sivaProductMongoResult.isHasCncActivated());
    assertTrue(sivaProductMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertFalse(sivaProductMongoResult.isOnlineOnly());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,pickupPoint.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
    assertNotNull(sivaItemMongoResult.getProductUrl());
    assertTrue(sivaItemMongoResult.getProductUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getItemUrl());
    assertTrue(sivaItemMongoResult.getItemUrl().contains("?pickupPointCode"));
    assertNotNull(sivaItemMongoResult.getProductImage());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertTrue(sivaItemMongoResult.isHasCncActivated());
    assertTrue(sivaItemMongoResult.getTags().contains(ProductTag.CNC_AVAILABLE));
    assertFalse(sivaItemMongoResult.isOnlineOnly());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnPickupPointEvent_imageValidation() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),item.getMerchantCode(),InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5000, CampaignPriority.REGULAR);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.PICKUP_POINT)
        .mongo(true)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertNotNull(pickupPointMongoResult.getPurchasedType());

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
  public void testOnPickupPointEvent_restrictionValidation() throws Exception {
    createCampaign(item.getItemSku(),pickupPoint.getPickupPointCode(),"ALFA-MART",InventoryType.TYPE_ONLINE_MERCHANT,campaignProduct.getCampaignCode(),101,currentTimestamp-1000000,currentTimestamp+1000000,true,false,false,5000, CampaignPriority.REGULAR);
    deleteAll(DeleteAllRequest.builder()
        .index(Collections.PICKUP_POINT)
        .mongo(true)
        .build());

    //Iteration 1 : Initiation
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals("10000.0",pickupPointMongoResult.getPrice().stream().filter(Objects::nonNull).findFirst().orElse(null).getOfferPrice()+"");

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("5000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("5000.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("5000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());

    //Iteration 2 : Trigger By Pickup Point
    pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(val -> {
      val.setOfferPrice(20_000D);
      val.setListPrice(40_000D);
      val.setAdjustment(5_000);
      val.setFinalOfferPrice(val.getOfferPrice()-val.getAdjustment());
    });
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals("20000.0",pickupPointMongoResult.getPrice().stream().filter(Objects::nonNull).findFirst().orElse(null).getOfferPrice()+"");

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("10000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("5000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("10000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("5000.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("10000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("5000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());

    //Iteration 3 : Trigger By Item
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertEquals("20000.0",pickupPointMongoResult.getPrice().stream().filter(Objects::nonNull).findFirst().orElse(null).getOfferPrice()+"");

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPrice());
    assertEquals("20000.0",sivaProductMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("15000.0",sivaProductMongoResult.getPrice().getOfferValue().toString());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemPrice());
    assertEquals("20000.0",sivaItemMongoResult.getItemPrice().getOffered().toString());
    assertEquals("15000.0",sivaItemMongoResult.getItemPrice().getFinalOffered().toString());
    assertNotNull(sivaItemMongoResult.getPrice());
    assertEquals("20000.0",sivaItemMongoResult.getPrice().getBaseOfferValue().toString());
    assertEquals("15000.0",sivaItemMongoResult.getPrice().getOfferValue().toString());
  }

  @Test
  public void testOnPickupPointEvent_warehousePriority() throws Exception {
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

    //Iteration 1 : ONLINE_MERCHANT SAME PRICE
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCodeMerchant");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pickupPointCodeMerchant");
    businessPartnerPickupPoint.setCode("pickupPointCodeMerchant");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pickupPointCodeMerchant");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,10000D,10000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isWarehouse());
    assertTrue(pickupPointMongoResult.isFbbActivated());

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("pickupPointCodeMerchant",sivaProductMongoResult.getPickupPointCode());
    assertTrue(sivaProductMongoResult.isFbbActivated());
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertNull(sivaProductMongoResult.getNonFbb());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("pickupPointCodeMerchant",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.isFbbActivated());
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertNull(sivaItemMongoResult.getNonFbb());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 2 : ONLINE_WAREHOUSE SAME PRICE
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCodeWarehouse");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(MainUtil.toList(new CustomInventoryPickupPointInfo.WarehouseInfo("warehouseCode","warehouseName","warehouseLocation",5,5)));
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pickupPointCodeWarehouse");
    businessPartnerPickupPoint.setCode("pickupPointCodeWarehouse");
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pickupPointCodeWarehouse");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,10000D,10000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isWarehouse());
    assertFalse(pickupPointMongoResult.isFbbActivated());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("pickupPointCodeWarehouse",sivaProductMongoResult.getPickupPointCode());
    assertFalse(sivaProductMongoResult.isFbbActivated());
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pickupPointCodeWarehouse",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("pickupPointCodeWarehouse",sivaItemMongoResult.getPickupPointCode());
    assertFalse(sivaItemMongoResult.isFbbActivated());
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pickupPointCodeWarehouse",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 3 : ONLINE_MERCHANT CHEAPER
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCodeMerchant");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pickupPointCodeMerchant");
    businessPartnerPickupPoint.setCode("pickupPointCodeMerchant");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pickupPointCodeMerchant");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,9000D,9000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isWarehouse());
    assertTrue(pickupPointMongoResult.isFbbActivated());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("pickupPointCodeMerchant",sivaProductMongoResult.getPickupPointCode());
    assertTrue(sivaProductMongoResult.isFbbActivated());
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pickupPointCodeWarehouse",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("pickupPointCodeMerchant",sivaItemMongoResult.getPickupPointCode());
    assertTrue(sivaItemMongoResult.isFbbActivated());
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pickupPointCodeWarehouse",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 4 : ONLINE_WAREHOUSE CHEAPER
    inventoryPickupPointInfo.setPickupPointCode("pickupPointCodeWarehouse");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pickupPointCodeWarehouse");
    businessPartnerPickupPoint.setCode("pickupPointCodeWarehouse");
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pickupPointCodeWarehouse");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,8000D,8000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isWarehouse());
    assertFalse(pickupPointMongoResult.isFbbActivated());

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals("pickupPointCodeWarehouse",sivaProductMongoResult.getPickupPointCode());
    assertFalse(sivaProductMongoResult.isFbbActivated());
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pickupPointCodeWarehouse",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals("pickupPointCodeWarehouse",sivaItemMongoResult.getPickupPointCode());
    assertFalse(sivaItemMongoResult.isFbbActivated());
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pickupPointCodeMerchant",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pickupPointCodeWarehouse",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
  }

  @Test
  public void testOnPickupPointEvent_fbbPriority() throws Exception {
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

    //Iteration 1 : pp01 non fbb
    inventoryPickupPointInfo.setPickupPointCode("pp01");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp01");
    businessPartnerPickupPoint.setCode("pp01");
    businessPartnerPickupPoint.setFbbActivated(null);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp01");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,10000D,10000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertNull(sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(0,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(0,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertNull(sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(0,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(0,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 2 : pp02 non fbb lower
    inventoryPickupPointInfo.setPickupPointCode("pp02");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp02");
    businessPartnerPickupPoint.setCode("pp02");
    businessPartnerPickupPoint.setFbbActivated(null);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp02");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,9000D,9000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertNull(sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(0,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(0,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp02",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("9000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertNull(sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(0,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(0,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp02",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("9000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 3 : pp02 non fbb higher
    inventoryPickupPointInfo.setPickupPointCode("pp02");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp02");
    businessPartnerPickupPoint.setCode("pp02");
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp02");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,11000D,11000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertNull(sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("11000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(0,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(0,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertNull(sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("11000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(0,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(0,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 4 : pp03 fbb
    inventoryPickupPointInfo.setPickupPointCode("pp03");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp03");
    businessPartnerPickupPoint.setCode("pp03");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp03");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,7000D,7000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp03",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("7000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp03",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("7000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 5 : pp04 fbb lower
    inventoryPickupPointInfo.setPickupPointCode("pp04");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp04");
    businessPartnerPickupPoint.setCode("pp04");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp04");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,5000D,5000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp03",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("7000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp03",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("7000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 6 : pp04 fbb higher
    inventoryPickupPointInfo.setPickupPointCode("pp04");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp04");
    businessPartnerPickupPoint.setCode("pp04");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp04");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,8000D,8000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp04",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp04",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 7 : pp01 non fbb deleted
    inventoryPickupPointInfo.setPickupPointCode("pp01");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp01");
    businessPartnerPickupPoint.setCode("pp01");
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp01");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,10000D,10000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp04",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp02",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("11000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp04",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp02",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("11000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 8 : pp02 non fbb deleted
    inventoryPickupPointInfo.setPickupPointCode("pp02");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp02");
    businessPartnerPickupPoint.setCode("pp02");
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp02");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,11000D,11000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp04",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp04",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 9 : pp04 fbb deleted
    inventoryPickupPointInfo.setPickupPointCode("pp04");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp04");
    businessPartnerPickupPoint.setCode("pp04");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp04");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,8000D,8000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp03",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("7000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp03",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("7000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 10 : pp03 fbb deleted
    inventoryPickupPointInfo.setPickupPointCode("pp03");
    inventoryPickupPointInfo.setId(inventoryPickupPointInfo.toId());
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setId("pp03");
    businessPartnerPickupPoint.setCode("pp03");
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setPickupPointCode("pp03");
    pickupPoint.setId(pickupPoint.toId());
    pickupPoint.setPrice(MainUtil.toSet(new PickupPoint.Price("Rupiah",20000D,7000D,7000D,0D,null,false,null,null,"channel","lastUpdatedBy",currentTimestamp)));
    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,EXTRA_TIME,INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getFbb());
    assertEquals("pp04",sivaProductMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaProductMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNonFbb());
    assertEquals("pp01",sivaProductMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaProductMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaProductMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaProductMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getFbb());
    assertEquals("pp04",sivaItemMongoResult.getFbb().getPickupPointCode());
    assertEquals("8000.0",sivaItemMongoResult.getFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNonFbb());
    assertEquals("pp01",sivaItemMongoResult.getNonFbb().getPickupPointCode());
    assertEquals("10000.0",sivaItemMongoResult.getNonFbb().getPrice()+"");
    assertEquals(20,sivaItemMongoResult.getNonFbb().getOriginalStock());
    assertEquals(14,sivaItemMongoResult.getNonFbb().getAvailableStock());
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
  }

  @Test
  public void testOnPickupPointEvent_fbbTags() throws Exception {
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
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    //Iteration 1 : Item no tags - fbbActivated false
    item.setTag(null);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(sivaProductMongoResult.isFbbActivated());
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isFbbActivated());
    assertTrue(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 2 : Item fbb tags - fbbActivated false
    item.setTag(Item.Tag.builder().fbb(true).build());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertFalse(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(sivaProductMongoResult.isFbbActivated());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));
    assertTrue(sivaProductMongoResult.getTags().contains(ProductTag.BLIBLI_SHIPPING));
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(sivaItemMongoResult.isFbbActivated());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));
    assertTrue(sivaItemMongoResult.getTags().contains(ProductTag.BLIBLI_SHIPPING));
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 3 : Item no tags - fbbActivated true
    item.setTag(null);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(sivaProductMongoResult.isFbbActivated());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));
    assertTrue(sivaProductMongoResult.getTags().contains(ProductTag.BLIBLI_SHIPPING));
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.isFbbActivated());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));
    assertTrue(sivaItemMongoResult.getTags().contains(ProductTag.BLIBLI_SHIPPING));
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());

    //Iteration 4 : Item fbb tags - fbbActivated true
    item.setTag(Item.Tag.builder().fbb(true).build());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(sivaProductMongoResult.isFbbActivated());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));
    assertTrue(sivaProductMongoResult.getTags().contains(ProductTag.BLIBLI_SHIPPING));
    assertNotNull(sivaProductMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaProductMongoResult.getNearestPickupPoints().size());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.isFbbActivated());
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));
    assertTrue(sivaItemMongoResult.getTags().contains(ProductTag.BLIBLI_SHIPPING));
    assertNotNull(sivaItemMongoResult.getNearestPickupPoints());
    assertEquals(1,sivaItemMongoResult.getNearestPickupPoints().size());
  }

}
