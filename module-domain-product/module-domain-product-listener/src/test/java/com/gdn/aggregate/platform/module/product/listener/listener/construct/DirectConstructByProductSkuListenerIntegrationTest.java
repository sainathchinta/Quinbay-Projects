package com.gdn.aggregate.platform.module.product.listener.listener.construct;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Channel;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Default;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectConstructByProductSkuListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnDirectConstructByProductSku_failed_productNotFound_itemNotFound_processedDataNotExists() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnDirectConstructByProductSku_failed_productNotFound_processedDataNotExists() throws Exception {
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,NORMAL_TIME,INDICES);

    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.isFbbActivated());
  }

  @Test
  public void testOnDirectConstructByProductSku_failed_productNotFound() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,NORMAL_TIME,INDICES);
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

    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
  }

  @Test
  public void testOnDirectConstructByProductSku_success_itemNotFound_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnDirectConstructByProductSku_success_itemNotFound() throws Exception {
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

    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
  }

  @Test
  public void testOnDirectConstructByProductSku_success_processedDataNotExists() throws Exception {
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
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
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,NORMAL_TIME,INDICES);

    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(sivaProductMongoResult.isFbbActivated());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.isFbbActivated());
  }

  @Test
  public void testOnDirectConstructByProductSku_success_full() throws Exception {
    fullSaveDataDummy();
    businessPartnerPickupPoint.setFbbActivated(true);
    save(SaveRequest.builder()
        .index(Collections.BUSINESS_PARTNER_PICKUP_POINT)
        .domain(businessPartnerPickupPoint)
        .clazz(CustomBusinessPartnerPickupPoint.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setFbbActivated(false);
    pickupPoint.setWarehouse(false);
    pickupPoint.setInStock(false);
    pickupPoint.setItemViewConfigs(MainUtil.toSet(
        PickupPoint.ItemViewConfig.builder().buyable(true).discoverable(true).build(),
        PickupPoint.ItemViewConfig.builder().channel("B2B").buyable(true).discoverable(true).build(),
        PickupPoint.ItemViewConfig.builder().channel("DEFAULT").buyable(false).discoverable(false).build()
    ));
    publishAndRefreshMultiple(Topics.PICKUP_POINT,pickupPoint.toId(),pickupPoint,NORMAL_TIME,INDICES);
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
    sivaProduct.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.SIVA_PRODUCT)
        .domain(sivaProduct)
        .clazz(SivaProduct.class)
        .mongo(true)
        .elasticsearch(true)
        .build());
    sivaItem.setFbbActivated(false);
    save(SaveRequest.builder()
        .index(Collections.SIVA_ITEM)
        .domain(sivaItem)
        .clazz(SivaItem.class)
        .mongo(true)
        .elasticsearch(true)
        .build());

    publishAndRefreshMultiple(Topics.PRODUCT_DENPASAR,idTimestampProductLevel.toId(),idTimestampProductLevel,EXTRA_TIME,INDICES);

    PickupPoint pickupPointMongoResult = getFromMongo(Collections.PICKUP_POINT,pickupPoint.toId(),PickupPoint.class);
    assertNotNull(pickupPointMongoResult);
    assertTrue(pickupPointMongoResult.isFbbActivated());
    assertTrue(pickupPointMongoResult.isWarehouse());
    assertTrue(pickupPointMongoResult.isInStock());
    assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
    assertTrue(Optional.ofNullable(pickupPointMongoResult)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .map(PickupPoint.ItemViewConfig::getChannel)
        .filter(Objects::nonNull)
        .anyMatch(Channel.DEFAULT::equals));
    assertNotNull(pickupPointMongoResult.getPickupPointCode());
    assertNotNull(pickupPointMongoResult.getLatitude());
    assertNotNull(pickupPointMongoResult.getLongitude());
    assertNotNull(pickupPointMongoResult.getCityName());

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,sivaProduct.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(sivaProductMongoResult.isFbbActivated());
    assertTrue(CollectionUtils.isNotEmpty(sivaProductMongoResult.getNearestPickupPoints()));
    Optional.ofNullable(sivaProductMongoResult)
        .map(SivaProduct::getNearestPickupPoints)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .forEach(val -> {
          assertNotNull(val.getPickupPointCode());
          assertNotNull(val.getLatitude());
          assertNotNull(val.getLongitude());
          assertNotNull(val.getCityName());
        });

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,sivaItem.toId(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(sivaItemMongoResult.isFbbActivated());
    assertTrue(CollectionUtils.isNotEmpty(sivaItemMongoResult.getNearestPickupPoints()));
    Optional.ofNullable(sivaItemMongoResult)
        .map(SivaItem::getNearestPickupPoints)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .forEach(val -> {
          assertNotNull(val.getPickupPointCode());
          assertNotNull(val.getLatitude());
          assertNotNull(val.getLongitude());
          assertNotNull(val.getCityName());
        });
  }

}
