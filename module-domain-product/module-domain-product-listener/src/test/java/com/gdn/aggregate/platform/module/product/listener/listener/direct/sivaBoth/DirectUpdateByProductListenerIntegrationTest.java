package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByProductListenerIntegrationTest extends DummyHelper {

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
  public void testOnProductEvent_failed_idNotValid() throws Exception {
    product.setProductSku(null);
    product.setId(product.toId());

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.PRODUCT,Product.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnProductEvent_failed_timestampNotValid() throws Exception {
    product.setTimestamp(currentTimestamp);
    product.setMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    Product productMongoResult = getFromMongo(Collections.PRODUCT,product.toId(),Product.class);
    assertNotNull(productMongoResult);
    assertNotEquals(0L,productMongoResult.getTimestamp());
    assertFalse(productMongoResult.isMarkForDelete());

    product.setTimestamp(0L);
    product.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    productMongoResult = getFromMongo(Collections.PRODUCT,product.getProductSku(),Product.class);
    assertNotNull(productMongoResult);
    assertNotEquals(0L,productMongoResult.getTimestamp());
    assertFalse(productMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnAllProductEvent_failed_timestampNotValid() throws Exception {
    product.setTimestamp(currentTimestamp);
    product.setMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    Product productMongoResult = getFromMongo(Collections.PRODUCT,product.toId(),Product.class);
    assertNotNull(productMongoResult);
    assertNotEquals(0L,productMongoResult.getTimestamp());
    assertFalse(productMongoResult.isMarkForDelete());

    product.setTimestamp(0L);
    product.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.ALL_PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    productMongoResult = getFromMongo(Collections.PRODUCT,product.getProductSku(),Product.class);
    assertNotNull(productMongoResult);
    assertNotEquals(0L,productMongoResult.getTimestamp());
    assertTrue(productMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnProductEvent_success_hasNewestMasterDataProduct() throws Exception {
    masterDataItem.setItemLength(null);
    masterDataItem.setItemWidth(null);
    masterDataItem.setItemHeight(null);
    masterDataItem.setItemWeight(null);
    masterDataItem.setTimestamp(0L);
    save(SaveRequest.builder()
        .index(Collections.MASTER_DATA_ITEM)
        .domain(masterDataItem)
        .clazz(MasterDataItem.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    masterDataProduct.setLength(1D);
    masterDataProduct.setWidth(2D);
    masterDataProduct.setHeight(3D);
    masterDataProduct.setWeight(4D);
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME * 4,
      INDICES);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME * 4, INDICES);
    publishAndRefreshMultiple(Topics.ITEM, item.getItemSku(), item, EXTRA_TIME * 4, INDICES);

    assertTrue(existsInMongo(Collections.ITEM,item.getItemSku(),Item.class));
    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
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

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
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
  public void testOnProductEvent_success_unsync_hasMasterDataProduct() throws Exception {
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
    product.setSync(false);

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNull(sivaProductMongoResult.getBuyable());
    assertNull(sivaProductMongoResult.getDiscoverable());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNull(sivaItemMongoResult.getBuyable());
    assertNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnProductEvent_success_unsync_hasNoMasterDataProduct() throws Exception {
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
    product.setSync(false);
    product.setMasterDataProduct(null);

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(product.getProductSku(), sivaProductMongoResult.getName());
    assertNull(sivaProductMongoResult.getBuyable());
    assertNull(sivaProductMongoResult.getDiscoverable());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getItemName());
    assertEquals(product.getProductSku(), sivaItemMongoResult.getProductName());
    assertNull(sivaItemMongoResult.getBuyable());
    assertNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnProductEvent_success_productNotFound_itemNotFound_processedDataNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnProductEvent_success_productNotFound_itemNotFound() throws Exception {
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

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNull(sivaProductMongoResult.getBuyable());
    assertNull(sivaProductMongoResult.getDiscoverable());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNull(sivaItemMongoResult.getBuyable());
    assertNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnProductEvent_success_itemNotFound_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnProductEvent_success_processedDataNotExists() throws Exception {
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
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnProductEvent_success_itemNotFound() throws Exception {
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

    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNull(sivaProductMongoResult.getBuyable());
    assertNull(sivaProductMongoResult.getDiscoverable());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNull(sivaItemMongoResult.getBuyable());
    assertNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnProductEvent_success() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(pcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(otherPcbCategory)
        .clazz(CustomPcbCategory.class)
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
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);
    for (int i=1; i<=25; i++) {
      item.setArchived(false);
      item.setMarkForDelete(false);
      if (i==11) {
        item.setArchived(true);
      }
      if (i==12) {
        item.setMarkForDelete(true);
      }
      item.setItemSku(String.format("%s-%05d",product.getProductSku(),i));
      item.setId(item.toId());

      pickupPoint.setItemSku(item.getItemSku());
      pickupPoint.setId(pickupPoint.toId());
      pickupPoint.getPrice().stream().filter(Objects::nonNull).forEach(price -> {
        price.setListPrice(price.getListPrice()-100);
        price.setOfferPrice(price.getOfferPrice()-100);
      });

      publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
      publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);
    }

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertEquals("ABC-12345-12345-00025",sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(23,sivaProductMongoResult.getItemSkus().size());
    assertFalse(sivaProductMongoResult.getItemSkus().contains("ABC-12345-12345-00011"));
    assertFalse(sivaProductMongoResult.getItemSkus().contains("ABC-12345-12345-00012"));
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertEquals(2, sivaProductMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertFalse(sivaProductMongoResult.isMarkForDelete());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,"ABC-12345-12345-00025",SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertNotNull(sivaItemMongoResult.getItemSku());
    assertEquals("ABC-12345-12345-00025",sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals("ABC-12345-12345-00025",sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(2, sivaItemMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaItemMongoResult.getBreadCrumb().contains("nameLevel1"));
    assertFalse(sivaItemMongoResult.getBreadCrumb().contains("otherNameLevel1"));
    assertFalse(sivaItemMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnProductEvent_productMarkForDeleteTrue() throws Exception {
    product.setMarkForDelete(true);
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
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(pcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(otherPcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertEquals(2, sivaProductMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaProductMongoResult.isMarkForDelete());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(2, sivaItemMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaItemMongoResult.getBreadCrumb().contains("nameLevel1"));
    assertFalse(sivaItemMongoResult.getBreadCrumb().contains("otherNameLevel1"));
    assertTrue(sivaItemMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnProductEvent_itemMarkForDeleteTrue() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.setMarkForDelete(true);
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
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(pcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(otherPcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertEquals(2, sivaProductMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaProductMongoResult.isMarkForDelete());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(2, sivaItemMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaItemMongoResult.getBreadCrumb().contains("nameLevel1"));
    assertFalse(sivaItemMongoResult.getBreadCrumb().contains("otherNameLevel1"));
    assertTrue(sivaItemMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnProductEvent_bothMarkForDeleteTrue() throws Exception {
    product.setMarkForDelete(true);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    item.setMarkForDelete(true);
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
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(pcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(otherPcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertEquals(2, sivaProductMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaProductMongoResult.isMarkForDelete());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(2, sivaItemMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaItemMongoResult.getBreadCrumb().contains("nameLevel1"));
    assertFalse(sivaItemMongoResult.getBreadCrumb().contains("otherNameLevel1"));
    assertTrue(sivaItemMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnAllProductEvent_success() throws Exception {
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
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(pcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.PCB_CATEGORIES)
        .domain(otherPcbCategory)
        .clazz(CustomPcbCategory.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.ALL_PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.ALL_PRODUCT, product.toId(), product, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.PRODUCT,product.getProductSku(),Product.class));

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,product.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaProductMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaProductMongoResult.getImage());
    assertEquals(2, sivaProductMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertFalse(sivaProductMongoResult.isMarkForDelete());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> product.getProductSku().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getDiscoverable().isValue());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getItemPrice().getCheapestItemSku());
    assertEquals(item.getItemSku(),sivaItemMongoResult.getPrice().getCheapestItemSku());
    assertEquals(item.getMasterDataItem().getMasterDataItemImages().iterator().next().getLocationPath(),sivaItemMongoResult.getItemImage());
    assertEquals(product.getMasterDataProduct().getMasterDataProductImages().iterator().next().getLocationPath(),sivaItemMongoResult.getProductImage());
    assertEquals(2, sivaItemMongoResult.getSalesCategories().size());
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("categoryCode")));
    assertTrue(sivaProductMongoResult.getSalesCategories().stream().anyMatch(val -> val.getCode().contains("otherCategoryCode")));
    assertTrue(sivaItemMongoResult.getBreadCrumb().contains("nameLevel1"));
    assertFalse(sivaItemMongoResult.getBreadCrumb().contains("otherNameLevel1"));
    assertFalse(sivaItemMongoResult.isMarkForDelete());
  }

}
