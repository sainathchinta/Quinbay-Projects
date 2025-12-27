package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaItemConstructorV2;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByMerchantDiscountPriceListenerIntegrationTest extends DummyHelper {

  @Autowired
  SivaItemConstructorV2 sivaItemConstructorV2;

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
  public void testOnMerchantDiscountPriceEvent_failed_idNotValid() throws Exception {
    merchantDiscountPrice.setItemSku(null);
    merchantDiscountPrice.setPickupPointCode(null);
    merchantDiscountPrice.setId(merchantDiscountPrice.toId());

    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.MERCHANT_DISCOUNT_PRICE,MerchantDiscountPrice.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnMerchantDiscountPriceEvent_failed_timestampNotValid() throws Exception {
    merchantDiscountPrice.setTimestamp(currentTimestamp);
    merchantDiscountPrice.setMarkForDelete(false);
    save(SaveRequest.builder()
        .index(Collections.MERCHANT_DISCOUNT_PRICE)
        .domain(merchantDiscountPrice)
        .clazz(MerchantDiscountPrice.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    MerchantDiscountPrice merchantDiscountPriceMongoResult = getFromMongo(Collections.MERCHANT_DISCOUNT_PRICE,merchantDiscountPrice.toId(),MerchantDiscountPrice.class);
    assertNotNull(merchantDiscountPriceMongoResult);
    assertNotEquals(0L,merchantDiscountPriceMongoResult.getTimestamp());
    assertFalse(merchantDiscountPriceMongoResult.isMarkForDelete());

    merchantDiscountPrice.setTimestamp(0L);
    merchantDiscountPrice.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    merchantDiscountPriceMongoResult = getFromMongo(Collections.MERCHANT_DISCOUNT_PRICE,merchantDiscountPrice.toId(),MerchantDiscountPrice.class);
    assertNotNull(merchantDiscountPriceMongoResult);
    assertNotEquals(0L,merchantDiscountPriceMongoResult.getTimestamp());
    assertFalse(merchantDiscountPriceMongoResult.isMarkForDelete());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_failed_itemNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_failed_productNotFound_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_failed_eventMarkForDeleteTrue() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    merchantDiscountPrice.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_failed_pickupPointMarkForDeleteTrue() throws Exception {
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

    pickupPoint.setMarkForDelete(true);
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertFalse(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertFalse(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_failed_eventHasNoPrice() throws Exception {
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
    merchantDiscountPrice.setPrice(null);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_success_eventHasNoPromo() throws Exception {
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
    merchantDiscountPrice.getPrice().stream().filter(Objects::nonNull).findFirst().orElse(null).setMerchantPromoDiscountPrice(null);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_success_processedDataNotExists() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNotNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_success_productNotFound() throws Exception {
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
    publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE, merchantDiscountPrice.toId(), merchantDiscountPrice, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,merchantDiscountPrice.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getBuyable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());
    assertNotNull(sivaProductMongoResult.getDiscoverable());
    assertTrue(sivaProductMongoResult.getBuyable().isValue());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,merchantDiscountPrice.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getItemName());
    assertNull(sivaItemMongoResult.getProductName());
    assertNotNull(sivaItemMongoResult.getBuyable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
    assertNotNull(sivaItemMongoResult.getDiscoverable());
    assertTrue(sivaItemMongoResult.getBuyable().isValue());
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEvent_success() throws Exception {
    Object originalFlagValue =
      ReflectionTestUtils.getField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields");
    try {
      ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields",
        false);
      save(SaveRequest.builder().index(Collections.ITEM).domain(item).clazz(Item.class).mongo(true)
        .elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.PRODUCT).domain(product).clazz(Product.class)
        .mongo(true).elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.SIVA_PRODUCT).domain(sivaProduct)
        .clazz(SivaProduct.class).mongo(true).elasticsearch(true).build());
      save(SaveRequest.builder().index(Collections.SIVA_ITEM).domain(sivaItem).clazz(SivaItem.class)
        .mongo(true).elasticsearch(true).build());

      publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME,
        INDICES);
      merchantDiscountPriceMultiple.getPrice().stream().filter(Objects::nonNull)
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .filter(val -> val.getAdjustmentName().equals("adjustmentNameOther")).forEach(val -> {
          val.setStartDateTime(currentTimestamp - 1000000);
          val.setEndDateTime(currentTimestamp + 1000000);
        });
      publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE,
        merchantDiscountPriceMultiple.toId(), merchantDiscountPriceMultiple, EXTRA_TIME, INDICES);

      MerchantDiscountPrice merchantDiscountPriceResult =
        getFromMongo(Collections.MERCHANT_DISCOUNT_PRICE, merchantDiscountPriceMultiple.toId(),
          MerchantDiscountPrice.class);
      assertNotNull(merchantDiscountPriceResult);
      assertNotNull(merchantDiscountPriceResult.getPickupPointCode());
      assertNotNull(merchantDiscountPriceResult.getExpiryTime());
      assertTrue(
        merchantDiscountPriceResult.getExpiryTime() > ModuleProductUtil.getMerchantDiscountPriceEnd(
          merchantDiscountPrice));

      assertFalse(
        CollectionUtils.isEmpty(getAllFromMongo(Collections.PICKUP_POINT, PickupPoint.class)));

      PickupPoint pickupPointMongoResult =
        getFromMongo(Collections.PICKUP_POINT, pickupPoint.toId(), PickupPoint.class);
      assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
      assertEquals(1, pickupPointMongoResult.getItemViewConfigs().stream()
        .filter(itemViewConfig -> itemViewConfig.getChannel().equals("DEFAULT"))
        .filter(PickupPoint.ItemViewConfig::isBuyable)
        .filter(PickupPoint.ItemViewConfig::isDiscoverable).count());

      SivaProduct sivaProductMongoResult =
        getFromMongo(Collections.SIVA_PRODUCT, merchantDiscountPriceMultiple.getProductSku(),
          SivaProduct.class);
      assertNotNull(sivaProductMongoResult);
      assertNull(sivaProductMongoResult.getName());
      assertNotNull(sivaProductMongoResult.getBuyable());
      assertTrue(sivaProductMongoResult.getBuyable().isValue());
      assertNotNull(sivaProductMongoResult.getDiscoverable());
      assertTrue(sivaProductMongoResult.getBuyable().isValue());

      SivaItem sivaItemMongoResult =
        getFromMongo(Collections.SIVA_ITEM, merchantDiscountPriceMultiple.getItemSku(),
          SivaItem.class);
      assertNotNull(sivaItemMongoResult);
      assertNotNull(sivaItemMongoResult.getItemName());
      assertNull(sivaItemMongoResult.getProductName());
      assertNotNull(sivaItemMongoResult.getBuyable());
      assertTrue(sivaItemMongoResult.getBuyable().isValue());
      assertNotNull(sivaItemMongoResult.getDiscoverable());
      assertTrue(sivaItemMongoResult.getBuyable().isValue());
    }
    finally {
      ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields", originalFlagValue);
    }
  }

  @Test
  public void testOnMerchantDiscountPriceChangeEventWithSwitchOn_success() throws Exception {
    Object originalFlagValue =
      ReflectionTestUtils.getField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields");
    try {
      ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields",
        false);
      save(SaveRequest.builder().index(Collections.ITEM).domain(item).clazz(Item.class).mongo(true)
        .elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.PRODUCT).domain(product).clazz(Product.class)
        .mongo(true).elasticsearch(false).build());
      save(SaveRequest.builder().index(Collections.SIVA_PRODUCT).domain(sivaProduct)
        .clazz(SivaProduct.class).mongo(true).elasticsearch(true).build());
      save(SaveRequest.builder().index(Collections.SIVA_ITEM).domain(sivaItem).clazz(SivaItem.class)
        .mongo(true).elasticsearch(true).build());

      publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME,
        INDICES);
      merchantDiscountPriceMultiple.getPrice().stream().filter(Objects::nonNull)
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .filter(val -> val.getAdjustmentName().equals("adjustmentNameOther")).forEach(val -> {
          val.setStartDateTime(currentTimestamp - 1000000);
          val.setEndDateTime(currentTimestamp + 1000000);
        });
      publishAndRefreshMultiple(Topics.MERCHANT_DISCOUNT_PRICE,
        merchantDiscountPriceMultiple.toId(), merchantDiscountPriceMultiple, EXTRA_TIME, INDICES);

      MerchantDiscountPrice merchantDiscountPriceResult =
        getFromMongo(Collections.MERCHANT_DISCOUNT_PRICE, merchantDiscountPriceMultiple.toId(),
          MerchantDiscountPrice.class);
      assertNotNull(merchantDiscountPriceResult);
      assertNotNull(merchantDiscountPriceResult.getPickupPointCode());
      assertNotNull(merchantDiscountPriceResult.getExpiryTime());
      assertTrue(
        merchantDiscountPriceResult.getExpiryTime() > ModuleProductUtil.getMerchantDiscountPriceEnd(
          merchantDiscountPrice));

      assertFalse(
        CollectionUtils.isEmpty(getAllFromMongo(Collections.PICKUP_POINT, PickupPoint.class)));

      PickupPoint pickupPointMongoResult =
        getFromMongo(Collections.PICKUP_POINT, pickupPoint.toId(), PickupPoint.class);
      assertEquals(1, pickupPointMongoResult.getItemViewConfigs().size());
      assertEquals(1, pickupPointMongoResult.getItemViewConfigs().stream()
        .filter(itemViewConfig -> itemViewConfig.getChannel().equals("DEFAULT"))
        .filter(PickupPoint.ItemViewConfig::isBuyable)
        .filter(PickupPoint.ItemViewConfig::isDiscoverable).count());

      SivaProduct sivaProductMongoResult =
        getFromMongo(Collections.SIVA_PRODUCT, merchantDiscountPriceMultiple.getProductSku(),
          SivaProduct.class);
      assertNotNull(sivaProductMongoResult);
      assertNull(sivaProductMongoResult.getName());
      assertNotNull(sivaProductMongoResult.getBuyable());
      assertTrue(sivaProductMongoResult.getBuyable().isValue());
      assertNotNull(sivaProductMongoResult.getDiscoverable());
      assertTrue(sivaProductMongoResult.getBuyable().isValue());

      SivaItem sivaItemMongoResult =
        getFromMongo(Collections.SIVA_ITEM, merchantDiscountPriceMultiple.getItemSku(),
          SivaItem.class);
      assertNotNull(sivaItemMongoResult);
      assertNotNull(sivaItemMongoResult.getItemName());
      assertNull(sivaItemMongoResult.getProductName());
      assertNotNull(sivaItemMongoResult.getBuyable());
      assertTrue(sivaItemMongoResult.getBuyable().isValue());
      assertNotNull(sivaItemMongoResult.getDiscoverable());
      assertTrue(sivaItemMongoResult.getBuyable().isValue());
    }
    finally {
      ReflectionTestUtils.setField(sivaItemConstructorV2, "backFillSivaDocsForMissingFields", originalFlagValue);
    }

  }

}
