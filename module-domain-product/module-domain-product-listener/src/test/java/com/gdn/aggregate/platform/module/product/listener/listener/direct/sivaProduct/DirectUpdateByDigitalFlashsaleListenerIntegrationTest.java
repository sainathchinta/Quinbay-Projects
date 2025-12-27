package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaProduct;

import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductType;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import org.apache.commons.collections.MapUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByDigitalFlashsaleListenerIntegrationTest extends DummyHelper {

  @BeforeEach
  public void setUp() throws Exception {
    setDataDummyForTest();
    cleanDataDummyBeforeTest();
  }

  @Test
  public void testOnDigitalFlashsaleEvent_failed_idNotValid() throws Exception {
    digitalFlashsale.setProductSku(null);
    digitalFlashsale.setId(digitalFlashsale.toId());

    publishAndRefreshMultiple(Topics.DIGITAL_FLASHSALE, digitalFlashsale.toId(), digitalFlashsale, LESS_TIME, INDICES);

    assertTrue(CollectionUtils.isEmpty(getAllFromMongo(Collections.SIVA_PRODUCT,SivaProduct.class)));
  }

  @Test
  public void testOnDigitalFlashsaleEvent_failed_timestampNotValid() throws Exception {
    digitalFlashsale.setTimestamp(currentTimestamp);
    digitalFlashsale.setName("insert");
    publishAndRefreshMultiple(Topics.DIGITAL_FLASHSALE, digitalFlashsale.toId(), digitalFlashsale, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(digitalFlashsale.getName(),sivaProductMongoResult.getName());

    digitalFlashsale.setTimestamp(0L);
    digitalFlashsale.setName("update");
    publishAndRefreshMultiple(Topics.DIGITAL_FLASHSALE, digitalFlashsale.toId(), digitalFlashsale, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertNotEquals(digitalFlashsale.getName(),sivaProductMongoResult.getName());
  }

  @Test
  public void testOnDigitalFlashsaleEvent_success() throws Exception {
    digitalFlashsale.setName("insert");
    publishAndRefreshMultiple(Topics.DIGITAL_FLASHSALE, digitalFlashsale.toId(), digitalFlashsale, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(digitalFlashsale.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getChannelId());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getMinVersion()));

    digitalFlashsale.setName("update");
    publishAndRefreshMultiple(Topics.DIGITAL_FLASHSALE, digitalFlashsale.toId(), digitalFlashsale, EXTRA_TIME, INDICES);

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(digitalFlashsale.getName(),sivaProductMongoResult.getName());
    assertNotNull(sivaProductMongoResult.getChannelId());
    assertFalse(CollectionUtils.isEmpty(sivaProductMongoResult.getMinVersion()));
  }

  @Test
  public void testOnAllDigitalFlashsaleEvent_success() throws Exception {
    digitalFlashsale.setMinVersion(null);
    digitalFlashsale.getFlashsaleInventory().getQuota().setStatus(StockStatus.AVAILABLE);
    digitalFlashsale.getFlashsaleInventory().getQuota().setPercentage(50);
    digitalFlashsale.setId(digitalFlashsale.toId());
    publishAndRefreshMultiple(Topics.ALL_DIGITAL_FLASHSALE, digitalFlashsale.toId(), digitalFlashsale, EXTRA_TIME, INDICES);

    SivaProduct sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(ProductType.DIGITAL,sivaProductMongoResult.getPickupPointCode());
    assertNotNull(sivaProductMongoResult.getMinVersion());
    assertTrue(MapUtils.isEmpty(sivaProductMongoResult.getMinVersion()));

    adjustmentProductSaved.setQuota(100);
    adjustmentProductSaved.setItemSku(digitalFlashsale.getProductSku());
    adjustmentProductSaved.setId(adjustmentProductSaved.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_SAVED, adjustmentProductSaved.toId(), adjustmentProductSaved, EXTRA_TIME, INDICES);

    AdjustmentProductQuota adjustmentProductQuotaResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductSaved.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaResult);
    assertEquals(ProductType.DIGITAL,adjustmentProductQuotaResult.getPickupPointCode());
    assertEquals(100,adjustmentProductQuotaResult.getQuota());
    assertEquals(0,adjustmentProductQuotaResult.getUsedQuota());
    assertEquals(100,adjustmentProductQuotaResult.getQuota() - adjustmentProductQuotaResult.getUsedQuota());
    assertEquals(0,adjustmentProductQuotaResult.getUsedQuotaPerTransaction());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(ProductType.DIGITAL,sivaProductMongoResult.getPickupPointCode());
    assertEquals(StockStatus.AVAILABLE,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(100,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());

    adjustmentProductQuota.setQuota(100);
    adjustmentProductQuota.setUsedQuota(98);
    adjustmentProductQuota.setUsedQuotaPerTransaction(1);
    adjustmentProductQuota.setItemSku(digitalFlashsale.getProductSku());
    adjustmentProductQuota.setId(adjustmentProductSaved.toId());
    publishAndRefreshMultiple(Topics.ADJUSTMENT_PRODUCT_QUOTA, adjustmentProductQuota.toId(), adjustmentProductQuota, EXTRA_TIME, INDICES);

    adjustmentProductQuotaResult = getFromMongo(Collections.ADJUSTMENT_PRODUCT_QUOTA,adjustmentProductSaved.toId(),AdjustmentProductQuota.class);
    assertNotNull(adjustmentProductQuotaResult);
    assertEquals(ProductType.DIGITAL,adjustmentProductQuotaResult.getPickupPointCode());
    assertEquals(100,adjustmentProductQuotaResult.getQuota());
    assertEquals(98,adjustmentProductQuotaResult.getUsedQuota());
    assertEquals(2,adjustmentProductQuotaResult.getQuota() - adjustmentProductQuotaResult.getUsedQuota());
    assertEquals(1,adjustmentProductQuotaResult.getUsedQuotaPerTransaction());

    sivaProductMongoResult = getFromMongo(Collections.SIVA_PRODUCT,digitalFlashsale.toId(),SivaProduct.class);
    assertNotNull(sivaProductMongoResult);
    assertEquals(ProductType.DIGITAL,sivaProductMongoResult.getPickupPointCode());
    assertEquals(StockStatus.LIMITED,sivaProductMongoResult.getFlashsaleInventory().getQuota().getStatus());
    assertEquals(2,sivaProductMongoResult.getFlashsaleInventory().getQuota().getPercentage());
  }

}
