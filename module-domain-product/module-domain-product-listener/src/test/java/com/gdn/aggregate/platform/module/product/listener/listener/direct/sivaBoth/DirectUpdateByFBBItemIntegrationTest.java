package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.helper.DummyHelper;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DirectUpdateByFBBItemIntegrationTest extends DummyHelper {

    @BeforeEach
    public void setUp() throws Exception {
        setDataDummyForTest();
        cleanDataDummyBeforeTest();

        sivaItem.setItemName(null);
        sivaItem.setProductName(null);
        sivaItem.setItemCode(null);
        sivaItem.setMerchantCode(null);
    }

    @Test
    public void testOnFBBItemEvent_success_onL2_created() {
        sivaItem.setItemName("ITEM-NAME1");
        sivaItem.setProductName("PRODUCT-NAME1");
        sivaItem.setItemCode("ITEM_CODE1");
        sivaItem.setMerchantCode("MERCHANT_CODE1");
        sivaItem.setItemSku("ITEM_SKU1");
        String id = String.format("%s-%s",sivaItem.getItemCode(),sivaItem.getMerchantCode());
        sivaItem.setId(id);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNull(sivaItemMongoResult);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNotNull(sivaItemMongoResult);
        assertEquals(id,sivaItemMongoResult.toId());
        assertTrue(sivaItemMongoResult.isOnL2());
        assertEquals(0L,sivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME1", sivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME1", sivaItemMongoResult.getProductName());
        assertFalse(sivaItemMongoResult.isMarkForDelete());
    }

    @Test
    public void testOnFBBItemEvent_success_onL2_update() {
        sivaItem.setItemName("ITEM-NAME2");
        sivaItem.setProductName("PRODUCT-NAME2");
        sivaItem.setItemCode("ITEM_CODE2");
        sivaItem.setMerchantCode("MERCHANT_CODE2");
        sivaItem.setItemSku("ITEM_SKU2");
        String id = String.format("%s-%s",sivaItem.getItemCode(),sivaItem.getMerchantCode());
        sivaItem.setId(id);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNotNull(sivaItemMongoResult);

        sivaItem.setItemName("ITEM-NAME2-UPDATED");
        sivaItem.setProductName("PRODUCT-NAME2-UPDATED");
        sivaItem.setTimestamp(currentTimestamp);
        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNotNull(sivaItemMongoResult);
        assertEquals(id,sivaItemMongoResult.toId());
        assertTrue(sivaItemMongoResult.isOnL2());
        assertNotEquals(0L,sivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME2-UPDATED", sivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME2-UPDATED", sivaItemMongoResult.getProductName());
        assertFalse(sivaItemMongoResult.isMarkForDelete());
    }

    @Test
    public void testOnFBBItemEvent_success_onL4_created() {
        sivaItem.setItemName("ITEM-NAME3");
        sivaItem.setProductName("PRODUCT-NAME3");
        sivaItem.setItemCode("ITEM_CODE3");
        sivaItem.setMerchantCode("MERCHANT_CODE3");
        sivaItem.setItemSku("ITEM_SKU3");
        String id = sivaItem.getItemSku();
        sivaItem.setId(id);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNull(sivaItemMongoResult);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNotNull(sivaItemMongoResult);
        assertEquals(id,sivaItemMongoResult.toId());
        assertFalse(sivaItemMongoResult.isOnL2());
        assertEquals(0L,sivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME3", sivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME3", sivaItemMongoResult.getProductName());
        assertFalse(sivaItemMongoResult.isMarkForDelete());
    }

    @Test
    public void testOnFBBItemEvent_success_onL4_update() {
        sivaItem.setItemName("ITEM-NAME4");
        sivaItem.setProductName("PRODUCT-NAME4");
        sivaItem.setItemCode("ITEM_CODE4");
        sivaItem.setMerchantCode("MERCHANT_CODE4");
        sivaItem.setItemSku("ITEM_SKU4");
        String id = sivaItem.getItemSku();
        sivaItem.setId(id);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        SivaItem sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNotNull(sivaItemMongoResult);

        sivaItem.setItemName("ITEM-NAME4-UPDATED");
        sivaItem.setProductName("PRODUCT-NAME4-UPDATED");
        sivaItem.setTimestamp(currentTimestamp);
        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        sivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,id,SivaItem.class);
        assertNotNull(sivaItemMongoResult);
        assertEquals(id,sivaItemMongoResult.toId());
        assertFalse(sivaItemMongoResult.isOnL2());
        assertNotEquals(0L,sivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME4-UPDATED", sivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME4-UPDATED", sivaItemMongoResult.getProductName());
        assertFalse(sivaItemMongoResult.isMarkForDelete());
    }

    @Test
    public void testOnFBBItemEvent_success_l2_l4() {
        sivaItem.setItemName("ITEM-NAME5");
        sivaItem.setProductName("PRODUCT-NAME5");
        sivaItem.setItemCode("ITEM_CODE5");
        sivaItem.setMerchantCode("MERCHANT_CODE5");
        sivaItem.setItemSku("ITEM_SKU5");
        String idL2 = String.format("%s-%s",sivaItem.getItemCode(),sivaItem.getMerchantCode());
        String idL4 = sivaItem.getItemSku();

        sivaItem.setId(idL2);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        SivaItem l2SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL2,SivaItem.class);
        assertNull(l2SivaItemMongoResult);
        SivaItem l4SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL4,SivaItem.class);
        assertNull(l4SivaItemMongoResult);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        l2SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL2,SivaItem.class);
        assertNotNull(l2SivaItemMongoResult);
        assertEquals(idL2,l2SivaItemMongoResult.toId());
        assertTrue(l2SivaItemMongoResult.isOnL2());
        assertEquals(0L,l2SivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME5", l2SivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME5", l2SivaItemMongoResult.getProductName());
        assertFalse(l2SivaItemMongoResult.isMarkForDelete());
        l4SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL4,SivaItem.class);
        assertNull(l4SivaItemMongoResult);

        sivaItem.setId(idL4);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        l2SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL2,SivaItem.class);
        assertNull(l2SivaItemMongoResult);
        l4SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL4,SivaItem.class);
        assertNotNull(l4SivaItemMongoResult);
        assertEquals(idL4,l4SivaItemMongoResult.toId());
        assertFalse(l4SivaItemMongoResult.isOnL2());
        assertEquals(0L,l4SivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME5", l4SivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME5", l4SivaItemMongoResult.getProductName());
        assertFalse(l4SivaItemMongoResult.isMarkForDelete());
    }

    @Test
    public void testOnFBBItemEvent_success_l4_l2() {
        sivaItem.setItemName("ITEM-NAME6");
        sivaItem.setProductName("PRODUCT-NAME6");
        sivaItem.setItemCode("ITEM_CODE6");
        sivaItem.setMerchantCode("MERCHANT_CODE6");
        sivaItem.setItemSku("ITEM_SKU6");
        String idL2 = String.format("%s-%s",sivaItem.getItemCode(),sivaItem.getMerchantCode());
        String idL4 = sivaItem.getItemSku();

        sivaItem.setId(idL4);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        SivaItem l2SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL2,SivaItem.class);
        assertNull(l2SivaItemMongoResult);
        SivaItem l4SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL4,SivaItem.class);
        assertNull(l4SivaItemMongoResult);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        l2SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL2,SivaItem.class);
        assertNull(l2SivaItemMongoResult);
        l4SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL4,SivaItem.class);
        assertNotNull(l4SivaItemMongoResult);
        assertEquals(idL4,l4SivaItemMongoResult.toId());
        assertFalse(l4SivaItemMongoResult.isOnL2());
        assertEquals(0L,l4SivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME6", l4SivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME6", l4SivaItemMongoResult.getProductName());
        assertFalse(l4SivaItemMongoResult.isMarkForDelete());

        sivaItem.setId(idL2);
        sivaItem.setCreatedTimestamp(0L);
        sivaItem.setTimestamp(0L);

        publishAndRefreshMultiple(Topics.FBB_ITEM_SAVE, sivaItem.toId(), sivaItem, EXTRA_TIME, INDICES);

        l2SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL2,SivaItem.class);
        assertNull(l2SivaItemMongoResult);
        l4SivaItemMongoResult = getFromMongo(Collections.SIVA_ITEM,idL4,SivaItem.class);
        assertNotNull(l4SivaItemMongoResult);
        assertEquals(idL4,l4SivaItemMongoResult.toId());
        assertFalse(l4SivaItemMongoResult.isOnL2());
        assertEquals(0L,l4SivaItemMongoResult.getTimestamp());
        assertEquals("ITEM-NAME6", l4SivaItemMongoResult.getItemName());
        assertEquals("PRODUCT-NAME6", l4SivaItemMongoResult.getProductName());
        assertFalse(l4SivaItemMongoResult.isMarkForDelete());
    }

}
