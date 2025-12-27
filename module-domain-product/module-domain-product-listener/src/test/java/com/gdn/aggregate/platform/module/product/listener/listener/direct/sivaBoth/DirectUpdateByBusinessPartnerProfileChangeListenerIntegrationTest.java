package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByBusinessPartnerProfileChangeListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaProduct.setTags(null);
    sivaItem.setTags(null);
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_failed_customMerchantNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.BUSINESS_PARTNER_PROFILE_CHANGE, businessPartnerProfileChangeEvent.getId(), businessPartnerProfileChangeEvent, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .noneMatch(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId())));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_failed_productNotFound() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.BUSINESS_PARTNER_PROFILE_CHANGE, businessPartnerProfileChangeEvent.getId(), businessPartnerProfileChangeEvent, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .noneMatch(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId())));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_failed_itemNotFound_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
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

    publishAndRefreshMultiple(Topics.BUSINESS_PARTNER_PROFILE_CHANGE, businessPartnerProfileChangeEvent.getId(), businessPartnerProfileChangeEvent, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .noneMatch(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId())));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_success() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.BUSINESS_PARTNER_PROFILE_CHANGE, businessPartnerProfileChangeEvent.getId(), businessPartnerProfileChangeEvent, EXTRA_TIME, INDICES);

    assertTrue(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId()))
        .findFirst()
        .map(val -> !CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));

    assertTrue(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId()))
        .findFirst()
        .map(val -> !CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_success_itemFoundButNoTags() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
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
    item.setTag(null);
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
    publishAndRefreshMultiple(Topics.BUSINESS_PARTNER_PROFILE_CHANGE, businessPartnerProfileChangeEvent.getId(), businessPartnerProfileChangeEvent, EXTRA_TIME, INDICES);

    assertTrue(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId()))
        .findFirst()
        .map(val -> !CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));

    assertTrue(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId()))
        .findFirst()
        .map(val -> !CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_success_noTags() throws Exception {
    merchant.setType(null);
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.BUSINESS_PARTNER_PROFILE_CHANGE, businessPartnerProfileChangeEvent.getId(), businessPartnerProfileChangeEvent, EXTRA_TIME, INDICES);

    assertTrue(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId()))
        .findFirst()
        .map(val -> CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));

    assertTrue(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> val.getMerchantCode().equals(businessPartnerProfileChangeEvent.getId()))
        .findFirst()
        .map(val -> CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_success_withItem_withTag() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    item.setTag(new Item.Tag(true,false,false,false));
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getTag());
    assertTrue(itemMongoResult.getTag().isFbb());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));

    item.setTag(null);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getTag());
    assertTrue(itemMongoResult.getTag().isFbb());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));
  }

  @Test
  public void testOnBusinessPartnerProfileChangeEvent_success_withItem_noTag() throws Exception {
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

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    item.setTag(null);
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    Item itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNull(itemMongoResult.getTag());

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertTrue(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertTrue(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));

    item.setTag(new Item.Tag(true,false,false,false));
    publishAndRefreshMultiple(Topics.ITEM, item.toId(), item, EXTRA_TIME, INDICES);

    itemMongoResult = getFromMongo(Collections.ITEM,item.toId(),Item.class);
    assertNotNull(itemMongoResult);
    assertNotNull(itemMongoResult.getTag());
    assertTrue(itemMongoResult.getTag().isFbb());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,item.getProductSku(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getTags()));

    sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,item.getItemSku(),SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertFalse(CollectionUtils.isEmpty(sivaItemMongoResult.getTags()));
  }

}
