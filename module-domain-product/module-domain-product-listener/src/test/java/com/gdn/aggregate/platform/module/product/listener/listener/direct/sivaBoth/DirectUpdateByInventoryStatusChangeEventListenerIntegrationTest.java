package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DirectUpdateByInventoryStatusChangeEventListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnInventoryStatusChangeEvent() throws Exception {
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

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);

    inventoryPickupPointInfo.setOriginalStock(10);
    inventoryPickupPointInfo.setAvailableStock(10);
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.INVENTORY_NON_OOS, inventoryStatusChangeEvent.toId(), inventoryStatusChangeEvent, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getInventory().getStatus());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals(StockStatus.AVAILABLE,sivaItemMongoResult.getInventory().getStatus());

    inventoryPickupPointInfo.setOriginalStock(0);
    inventoryPickupPointInfo.setAvailableStock(0);
    inventoryPickupPointInfo.setWarehouseInfos(null);
    save(SaveRequest.builder()
        .index(Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
        .domain(inventoryPickupPointInfo)
        .clazz(CustomInventoryPickupPointInfo.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.INVENTORY_OOS, inventoryStatusChangeEvent.toId(), inventoryStatusChangeEvent, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(StockStatus.OOS,sivaProductMongoResult.getInventory().getStatus());

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertEquals(StockStatus.OOS,sivaItemMongoResult.getInventory().getStatus());
  }

}
