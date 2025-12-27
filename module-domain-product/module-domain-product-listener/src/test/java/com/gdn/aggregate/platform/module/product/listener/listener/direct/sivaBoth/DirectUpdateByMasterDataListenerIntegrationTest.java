package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByMasterDataListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    sivaProduct.setProductCode(null);
    sivaProduct.setName(null);
    sivaProduct.setBuyable(null);
    sivaProduct.setDiscoverable(null);

    sivaItem.setProductCode(null);
    sivaItem.setItemCode(null);
    sivaItem.setItemName(null);
    sivaItem.setProductName(null);
    sivaItem.setBuyable(null);
    sivaItem.setDiscoverable(null);
  }

  @Test
  public void testOnMasterDataEvent_failed_idNotValid() throws Exception {
    masterData.setProductCode(null);
    masterData.setId(masterData.toId());

    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.MASTER_DATA,MasterData.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnMasterDataEvent_failed_timestampNotValid() throws Exception {
    masterData.setTimestamp(currentTimestamp);
    masterData.setProductMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    MasterData masterDataMongoResult = getFromMongo(Collections.MASTER_DATA,masterData.toId(),MasterData.class);
    assertNotNull(masterDataMongoResult);
    assertNotEquals(0L,masterDataMongoResult.getTimestamp());
    assertFalse(masterDataMongoResult.isProductMarkForDelete());

    masterData.setTimestamp(0L);
    masterData.setProductMarkForDelete(true);
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    masterDataMongoResult = getFromMongo(Collections.MASTER_DATA,masterData.toId(),MasterData.class);
    assertNotNull(masterDataMongoResult);
    assertNotEquals(0L,masterDataMongoResult.getTimestamp());
    assertFalse(masterDataMongoResult.isProductMarkForDelete());
  }

  @Test
  public void testOnAllMasterDataEvent_failed_timestampNotValid() throws Exception {
    masterData.setTimestamp(currentTimestamp);
    masterData.setProductMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA)
        .domain(masterData)
        .clazz(MasterData.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    MasterData masterDataMongoResult = getFromMongo(Collections.MASTER_DATA,masterData.toId(),MasterData.class);
    assertNotNull(masterDataMongoResult);
    assertNotEquals(0L,masterDataMongoResult.getTimestamp());
    assertFalse(masterDataMongoResult.isProductMarkForDelete());

    masterData.setTimestamp(0L);
    masterData.setProductMarkForDelete(true);
    publishAndRefreshMultiple(Topics.ALL_MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    masterDataMongoResult = getFromMongo(Collections.MASTER_DATA,masterData.toId(),MasterData.class);
    assertNotNull(masterDataMongoResult);
    assertNotEquals(0L,masterDataMongoResult.getTimestamp());
    // since event is not processed if L1 is MFD true, if will remain MFD true
    assertFalse(masterDataMongoResult.isProductMarkForDelete());
  }

  @Test
  public void testOnMasterDataEvent_failed_productNotFound_itemNotFound() throws Exception {
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
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.MASTER_DATA,masterData.toId(),MasterData.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getProductCode());
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertFalse(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getProductCode());
    assertNull(sivaItemMongoResult.getItemCode());
    assertNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNull(sivaItemMongoResult.getBuyable());
    assertNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnMasterDataEvent_failed_productNotFound() throws Exception {
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
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME * 5, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getProductCode());
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getProductCode());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnMasterDataEvent_success_itemNotFound() throws Exception {
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
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getProductCode());
    assertEquals(masterData.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getProductCode());
    assertNull(sivaItemMongoResult.getItemCode());
    assertNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNull(sivaItemMongoResult.getBuyable());
    assertNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnMasterDataEvent_failed_processedDataNotExists() throws Exception {
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
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.MASTER_DATA,masterData.toId(),MasterData.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getProductCode());
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getProductCode());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnMasterDataEvent_failed_masterDataSplitted() throws Exception {
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
    masterData.setImages(null);
    masterData.setProductCategories(null);
    masterData.setProductAttributes(null);
    masterData.setProductItems(null);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getProductCode());
    assertEquals(masterData.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getProductCode());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
  }

  @Test
  public void testOnMasterDataEvent_success() throws Exception {
    sivaProduct.setImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaItem.setItemImage(null);
    sivaItem.setProductImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    product.getMasterDataProduct().setMasterDataProductImages(null);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.getMasterDataItem().setMasterDataItemImages(null);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME * 5, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getProductCode());
    assertEquals(masterData.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getImage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getProductCode());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertEquals(masterDataItem.getGeneratedItemName(),sivaItemMongoResult.getItemName());
    assertEquals(masterData.getName(),sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getProductImage());

    long totalMasterDataItemsWithReviewPending = getAllFromMongo(Collections.MASTER_DATA_ITEM,MasterDataItem.class).stream()
            .filter(val -> masterData.getProductCode().equals(val.getProductCode()))
            .collect(Collectors.toList()).stream()
            .filter(MasterDataItem::isReviewPending)
            .count();
    assertEquals(0, totalMasterDataItemsWithReviewPending);
  }

  @Test
  public void testOnMasterDataEventWithReiewPending_success() throws Exception {
    sivaProduct.setImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaItem.setItemImage(null);
    sivaItem.setProductImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    product.getMasterDataProduct().setMasterDataProductImages(null);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.getMasterDataItem().setMasterDataItemImages(null);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MASTER_DATA, masterData.toId(), masterDataWithReviewPending, EXTRA_TIME * 5, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getProductCode());
    assertEquals(masterData.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getImage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getProductCode());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertEquals(masterDataItem.getGeneratedItemName(),sivaItemMongoResult.getItemName());
    assertEquals(masterData.getName(),sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getProductImage());

    long totalMasterDataItemsWithReviewPending = getAllFromMongo(Collections.MASTER_DATA_ITEM,MasterDataItem.class).stream()
            .filter(val -> masterData.getProductCode().equals(val.getProductCode()))
            .collect(Collectors.toList()).stream()
            .filter(MasterDataItem::isReviewPending)
            .count();
    assertEquals(1, totalMasterDataItemsWithReviewPending);
  }

  @Test
  public void testOnAllMasterDataEvent_success() throws Exception {
    sivaProduct.setImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaItem.setItemImage(null);
    sivaItem.setProductImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    product.getMasterDataProduct().setMasterDataProductImages(null);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.getMasterDataItem().setMasterDataItemImages(null);
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ALL_MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME * 5, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getProductCode());
    assertEquals(masterData.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaProductMongoResult.getImage());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getProductCode());
    assertNotNull(sivaItemMongoResult.getItemCode());
    assertEquals(masterDataItem.getGeneratedItemName(),sivaItemMongoResult.getItemName());
    assertEquals(masterData.getName(),sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertNotNull(sivaItemMongoResult.getItemImage());
    assertNotNull(sivaItemMongoResult.getProductImage());
  }

  @Test
  public void testOnAllMasterDataEvent_whenMasterDataProductIsNotDefined_shouldConstructProductSkuAsProductName() {
    product.setMasterDataProduct(null);
    item.setGeneratedItemName(null);
    item.setMasterDataItem(null);
    sivaProduct.setImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaItem.setItemImage(null);
    sivaItem.setProductImage(null);
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
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
    masterData.setName(null);
    masterData.getProductItems().stream()
      .filter(Objects::nonNull)
      .forEach(productAndItem -> productAndItem.setGeneratedItemName(null));

    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ALL_MASTER_DATA, masterData.toId(), masterData, EXTRA_TIME * 5, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertEquals(product.getProductSku(), sivaProductMongoResult.getName());
    assertEquals(product.getProductSku().toLowerCase(), sivaProductMongoResult.getUrlName());
    assertTrue(sivaProductMongoResult.getUrl().contains(String.format("/p/%s/is--", product.getProductSku().toLowerCase())));
  }

}
