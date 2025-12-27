package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByLogisticOptionChangeListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    sivaProduct.setTags(null);
    sivaItem.setTags(null);
  }

  @Test
  public void testOnDenpasarShippingLogisticOptionChangeEvent_failed_itemNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, denpasarShippingLogisticOptionChange.getItemSku(), denpasarShippingLogisticOptionChange, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertFalse(existsInMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class));
  }

  @Test
  public void testOnDenpasarShippingLogisticOptionChangeEvent_failed_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, denpasarShippingLogisticOptionChange.getItemSku(), denpasarShippingLogisticOptionChange, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));
    
    assertFalse(existsInMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class));
  }

  @Test
  public void testOnDenpasarShippingLogisticOptionChangeEvent_success() throws Exception {
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
    publishAndRefreshMultiple(Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, denpasarShippingLogisticOptionChange.getItemSku(), denpasarShippingLogisticOptionChange, EXTRA_TIME, INDICES);

    assertTrue(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> denpasarShippingLogisticOptionChange.getItemSku().equals(itemSku)))
        .findFirst()
        .map(val -> !CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));

    assertTrue(existsInMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class));
    assertFalse(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class).getTags()));
  }

  @Test
  public void testOnDenpasarShippingLogisticOptionChangeEvent_success_itemFoundButNoTags() throws Exception {
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
    publishAndRefreshMultiple(Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, denpasarShippingLogisticOptionChange.getItemSku(), denpasarShippingLogisticOptionChange, EXTRA_TIME, INDICES);

    assertTrue(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> denpasarShippingLogisticOptionChange.getItemSku().equals(itemSku)))
        .findFirst()
        .map(val -> !CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));

    assertTrue(existsInMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class));
    assertFalse(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class).getTags()));
  }

  @Test
  public void testOnDenpasarShippingLogisticOptionChangeEvent_success_noTags() throws Exception {
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
    denpasarShippingLogisticOptionChange.setLogisticOptions(new ArrayList<>());

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, denpasarShippingLogisticOptionChange.getItemSku(), denpasarShippingLogisticOptionChange, EXTRA_TIME, INDICES);

    assertTrue(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> val.getItemSkus().stream().anyMatch(itemSku -> denpasarShippingLogisticOptionChange.getItemSku().equals(itemSku)))
        .findFirst()
        .map(val -> CollectionUtils.isEmpty(val.getTags()))
        .orElseGet(() -> false));

    assertTrue(existsInMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class));
    assertTrue(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class).getTags()));
  }

  @Test
  public void testOnDenpasarSearchLogisticOptionChangeEventSivaBoth_failed_itemNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, denpasarSearchLogisticOptionChange.getProductSku(), denpasarSearchLogisticOptionChange, LESS_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class));

    assertFalse(existsInMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class));
  }

  @Test
  public void testOnDenpasarSearchLogisticOptionChangeEventSivaBoth_failed_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, denpasarSearchLogisticOptionChange.getProductSku(), denpasarSearchLogisticOptionChange, LESS_TIME, INDICES);

    assertFalse(existsInMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class));

    assertFalse(existsInMongo(Collections.SIVA_ITEM,denpasarShippingLogisticOptionChange.getItemSku(),SivaItem.class));
  }

  @Test
  public void testOnDenpasarSearchLogisticOptionChangeEventSivaBoth_success() throws Exception {
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
    publishAndRefreshMultiple(Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, denpasarSearchLogisticOptionChange.getProductSku(), denpasarSearchLogisticOptionChange, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class));
    assertFalse(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class).getTags()));

    assertTrue(existsInMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class));
    assertFalse(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class).getTags()));
  }

  @Test
  public void testOnDenpasarSearchLogisticOptionChangeEventSivaBoth_success_itemFoundButNoTags() throws Exception {
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
    publishAndRefreshMultiple(Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, denpasarSearchLogisticOptionChange.getProductSku(), denpasarSearchLogisticOptionChange, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class));
    assertFalse(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class).getTags()));

    assertTrue(existsInMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class));
    assertFalse(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class).getTags()));
  }

  @Test
  public void testOnDenpasarSearchLogisticOptionChangeEventSivaBoth_success_noTags() throws Exception {
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
    denpasarSearchLogisticOptionChange.setTags(null);
    denpasarSearchLogisticOptionChange.setBadge(null);

    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, NORMAL_TIME, INDICES);
    publishAndRefreshMultiple(Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, denpasarSearchLogisticOptionChange.getProductSku(), denpasarSearchLogisticOptionChange, EXTRA_TIME, INDICES);

    assertTrue(existsInMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class));
    assertTrue(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_PRODUCT,denpasarSearchLogisticOptionChange.getProductSku(),SivaProduct.class).getTags()));

    assertTrue(existsInMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class));
    assertTrue(CollectionUtils.isEmpty(getFromMongo(Collections.SIVA_ITEM,denpasarSearchLogisticOptionChange.getItemSku(),SivaItem.class).getTags()));
  }

}
