package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public class DirectUpdateByProductReviewListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
    
    sivaProduct.setReview(null);
    
    sivaItem.setReview(null);
  }

  @Test
  public void testOnProductReviewChangeEvent_failed_productReviewNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.PRODUCT_REVIEW_CHANGE, productReviewChangeEvent.toId(), productReviewChangeEvent, LESS_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,productReviewChangeEvent.toId(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> productReviewChangeEvent.getId().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnProductReviewChangeEvent_failed_processedDataNotExists() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.PRODUCT_REVIEW)
        .domain(productReview)
        .clazz(CustomProductReview.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.PRODUCT_REVIEW_CHANGE, productReviewChangeEvent.toId(), productReviewChangeEvent, LESS_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,productReviewChangeEvent.toId(),SivaProduct.class);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> productReviewChangeEvent.getId().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnProductReviewChangeEvent_success() throws Exception {
    //Iteration 1 : Normal
    save(SaveRequest.builder()
        .index(Collections.PRODUCT_REVIEW)
        .domain(productReview)
        .clazz(CustomProductReview.class)
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

    publishAndRefreshMultiple(Topics.PRODUCT_REVIEW_CHANGE, productReviewChangeEvent.toId(), productReviewChangeEvent, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,productReviewChangeEvent.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getReview());
    assertEquals("4.2",sivaProductMongoResult.getReview().getRating()+"");
    assertEquals("5",sivaProductMongoResult.getReview().getCount()+"");

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> productReviewChangeEvent.getId().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getReview());
    assertEquals("4.2",sivaItemMongoResult.getReview().getRating()+"");
    assertEquals("5",sivaItemMongoResult.getReview().getCount()+"");

    //Iteration 2 : Empty
    productReview.setDecimalRating(0D);
    productReview.setReviewCount(0);
    save(SaveRequest.builder()
        .index(Collections.PRODUCT_REVIEW)
        .domain(productReview)
        .clazz(CustomProductReview.class)
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

    publishAndRefreshMultiple(Topics.PRODUCT_REVIEW_CHANGE, productReviewChangeEvent.toId(), productReviewChangeEvent, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,productReviewChangeEvent.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNull(sivaProductMongoResult.getReview());

    sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> productReviewChangeEvent.getId().equals(val.getProductSku()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNull(sivaItemMongoResult.getReview());
  }

}
