package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public class DirectUpdateByStoreClosingMerchantChangeListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();

    sivaProduct.setMerchant(null);

    sivaItem.setMerchant(null);
  }

  @Test
  public void testOnStoreClosingMerchantChangeEvent_failed_merchantNotFound() throws Exception {
    publishAndRefreshMultiple(Topics.STORE_CLOSING_MERCHANT_CHANGE, storeClosingMerchantChangeEvent.toId(), storeClosingMerchantChangeEvent, LESS_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnStoreClosingMerchantChangeEvent_failed_productNotFound() throws Exception {
    save(SaveRequest.builder()
        .index(Collections.MERCHANT)
        .domain(merchant)
        .clazz(CustomMerchant.class)
        .mongo(true)
        .elasticsearch(false)
        .build());

    publishAndRefreshMultiple(Topics.STORE_CLOSING_MERCHANT_CHANGE, storeClosingMerchantChangeEvent.toId(), storeClosingMerchantChangeEvent, LESS_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnStoreClosingMerchantChangeEvent_failed_processedDataNotExists() throws Exception {
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

    publishAndRefreshMultiple(Topics.STORE_CLOSING_MERCHANT_CHANGE, storeClosingMerchantChangeEvent.toId(), storeClosingMerchantChangeEvent, LESS_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNull(sivaProductMongoResult);

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNull(sivaItemMongoResult);
  }

  @Test
  public void testOnStoreClosingMerchantChangeEvent_success() throws Exception {
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

    publishAndRefreshMultiple(Topics.STORE_CLOSING_MERCHANT_CHANGE, storeClosingMerchantChangeEvent.toId(), storeClosingMerchantChangeEvent, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaProductMongoResult);
    assertNotNull(sivaProductMongoResult.getMerchant());
    assertNotNull(sivaProductMongoResult.getMerchant().getType());
    assertNotNull(sivaProductMongoResult.getMerchant().getDeliveryType());

    SivaItem sivaItemMongoResult = getAllFromMongo(Collections.SIVA_ITEM,SivaItem.class).stream()
        .filter(val -> storeClosingMerchantChangeEvent.toId().equals(val.getMerchantCode()))
        .findFirst()
        .orElse(null);
    assertNotNull(sivaItemMongoResult);
    assertNotNull(sivaItemMongoResult.getMerchant());
    assertNotNull(sivaItemMongoResult.getMerchant().getType());
    assertNotNull(sivaItemMongoResult.getMerchant().getDeliveryType());
  }

}
