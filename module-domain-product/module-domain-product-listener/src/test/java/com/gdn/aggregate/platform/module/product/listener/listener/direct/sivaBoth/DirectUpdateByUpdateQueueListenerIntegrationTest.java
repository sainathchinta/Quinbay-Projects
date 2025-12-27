package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateQueue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByUpdateQueueListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    sivaProduct.setPickupPointCode(null);
    sivaProduct.setExternalPickupPointCode(null);

    sivaItem.setPickupPointCode(null);
    sivaItem.setExternalPickupPointCode(null);
  }

  @Test
  public void testOnUpdateQueueEvent_failed_itemNotFound() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .domain(updateQueueL3)
        .clazz(UpdateQueue.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    save(SaveRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .domain(updateQueueL4)
        .clazz(UpdateQueue.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    idTimestampUpdateQueue.setTimestamp(currentTimestamp);
    publishAndRefreshMultiple(Topics.UPDATE_QUEUE_DENPASAR, idTimestampUpdateQueue.toId(), idTimestampUpdateQueue, EXTRA_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.UPDATE_QUEUE,UpdateQueue.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  @Test
  public void testOnUpdateQueueEvent_success() throws Exception {
    int size = 10;
    for (int i=1; i<=size; i++) {
      prepareTestsuccess(updateQueueL3.getId(),updateQueueL3.getId()+"-0000"+i);
    }

    idTimestampUpdateQueue.setTimestamp(currentTimestamp);
    publishAndRefreshMultiple(Topics.UPDATE_QUEUE_DENPASAR, idTimestampUpdateQueue.toId(), idTimestampUpdateQueue, EXTRA_TIME, INDICES);

    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));
    assertFalse(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class)));
  }

  public void prepareTestsuccess(String productSku, String itemSku) {
    updateQueueL3.setId(productSku);
    save(SaveRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .domain(updateQueueL3)
        .clazz(UpdateQueue.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    updateQueueL4.setId(itemSku);
    save(SaveRequest.builder()
        .index(Collections.UPDATE_QUEUE)
        .domain(updateQueueL4)
        .clazz(UpdateQueue.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    pickupPoint.setProductSku(productSku);
    pickupPoint.setItemSku(itemSku);
    pickupPoint.setId(pickupPoint.toId());
    publishAndRefreshMultiple(Topics.PICKUP_POINT, pickupPoint.toId(), pickupPoint, EXTRA_TIME, INDICES);
    item.setProductSku(productSku);
    item.setItemSku(itemSku);
    item.setId(item.toId());
    save(SaveRequest.builder()
        .index(Collections.ITEM)
        .domain(item)
        .clazz(Item.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
    product.setProductSku(productSku);
    product.setId(product.toId());
    save(SaveRequest.builder()
        .index(Collections.PRODUCT)
        .domain(product)
        .clazz(Product.class)
        .mongo(true)
        .elasticsearch(false)
        .build());
  }

  public void assertTestsuccess(String productSku, String itemSku) {
    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,productSku,SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getExternalPickupPointCode());

    SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,itemSku,SivaItem.class);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getPickupPointCode());
    assertNotNull(sivaItemMongoResult.getExternalPickupPointCode());
  }

}
