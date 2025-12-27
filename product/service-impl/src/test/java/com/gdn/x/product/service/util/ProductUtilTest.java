package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAttributeDetailVo;

class ProductUtilTest {

    private static final String TEST_ITEM_SKU = "TEST-ITEM-SKU";
    private static final String TEST_PRODUCT_SKU = "TEST-PRODUCT-SKU";
    private static final String TEST_MERCHANT_CODE = "TEST-MERCHANT";
    private static final String TEST_CATEGORY = "TEST-CATEGORY";
    private static final String TEST_BRAND = "TEST-BRAND";
    private static final String TEST_STORE = "TEST-STORE";
    private static final String TEST_USER = "TEST-USER";
    private static final String TEST_CHANNEL = "TEST-CHANNEL";
    private static final String TEST_CLIENT = "TEST-CLIENT";
    private static final String COLOR_ATTRIBUTE_CODE = "COLOR";
    private static final String COLOR_ATTRIBUTE_NAME = "Color";
    private static final String COLOR_ATTRIBUTE_VALUE = "Red";
    private static final String SIZE_ATTRIBUTE_CODE = "SIZE";
    private static final String SIZE_ATTRIBUTE_NAME = "Size";
    private static final String SIZE_ATTRIBUTE_VALUE = "Large";
    private static final String DIFFERENT_STORE = "DIFFERENT-STORE";
    private static final String DIFFERENT_USER = "DIFFERENT-USER";
    private static final String TEST_CHANNEL_1 = "CHANNEL-1";
    private static final String TEST_OFFLINE_ITEM_SKU_1 = "OFFLINE-ITEM-1";
    private static final String TEST_OFFLINE_ITEM_SKU_2 = "OFFLINE-ITEM-2";
    private static final String TEST_PRICE_CHANNEL = "PRICE-CHANNEL";
    private static final Double TEST_PRICE_VALUE = 100.0;

    private AddVariantRequestVo addVariantRequestVo;
    private Product product;
    private MandatoryRequestParam mandatoryRequestParam;

    @BeforeEach
    void setUp() {
        addVariantRequestVo = new AddVariantRequestVo();
        addVariantRequestVo.setItemSku(TEST_ITEM_SKU);
        
        List<ProductAttributeDetailVo> definingAttributes = new ArrayList<>();
        ProductAttributeDetailVo attributeDetailVo = new ProductAttributeDetailVo();
        attributeDetailVo.setAttributeCode(COLOR_ATTRIBUTE_CODE);
        attributeDetailVo.setAttributeName(COLOR_ATTRIBUTE_NAME);
        attributeDetailVo.setAttributeValue(COLOR_ATTRIBUTE_VALUE);
        definingAttributes.add(attributeDetailVo);
        addVariantRequestVo.setDefiningAttributes(definingAttributes);

        product = new Product();
        product.setProductSku(TEST_PRODUCT_SKU);
        product.setMerchantCode(TEST_MERCHANT_CODE);
        product.setSynchronized(true);
        product.setProductType(ProductType.REGULAR);
        product.setOff2OnChannelActive(true);
        product.setFreeSample(false);
        product.setCategoryCode(TEST_CATEGORY);
        product.setBrand(TEST_BRAND);

        try {
            mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(
                TEST_STORE, TEST_CHANNEL, TEST_CLIENT, TEST_USER, TEST_USER, TEST_CHANNEL);
        } catch (Exception e) {
            throw new RuntimeException("Failed to create MandatoryRequestParam", e);
        }
    }

    @Test
    void testConvertToItem_WithRegularProduct() {
        product.setProductType(ProductType.REGULAR);
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertEquals(TEST_ITEM_SKU, result.getItemSku());
        assertEquals(TEST_PRODUCT_SKU, result.getProductSku());
        assertEquals(TEST_MERCHANT_CODE, result.getMerchantCode());
        assertTrue(result.isSynchronized());
        assertEquals(TEST_ITEM_SKU, result.getItemCatentryId());
        assertFalse(result.isLateFulfillment());
        assertTrue(result.isOff2OnChannelActive());
        assertFalse(result.isFreeSample());
        assertEquals(TEST_CATEGORY, result.getCategoryCode());
        assertEquals(TEST_BRAND, result.getBrand());
        assertEquals(TEST_STORE, result.getStoreId());
        assertEquals(TEST_USER, result.getCreatedBy());
        assertEquals(TEST_USER, result.getUpdatedBy());
        
        assertNotNull(result.getDefiningAttributes());
        assertEquals(1, result.getDefiningAttributes().size());
        ProductAttributeDetail detail = result.getDefiningAttributes().get(0);
        assertEquals(COLOR_ATTRIBUTE_CODE, detail.getAttributeCode());
        assertEquals(COLOR_ATTRIBUTE_NAME, detail.getAttributeName());
        assertEquals(COLOR_ATTRIBUTE_VALUE, detail.getAttributeValue());
    }

    @Test
    void testConvertToItem_WithNonRegularProduct() {
        product.setProductType(ProductType.BIG_PRODUCT);
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertTrue(result.isLateFulfillment());
    }

    @Test
    void testConvertToItem_WithNullProductType() {
        product.setProductType(null);
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertTrue(result.isLateFulfillment());
    }

    @Test
    void testConvertToItem_WithEmptyDefiningAttributes() {
        addVariantRequestVo.setDefiningAttributes(new ArrayList<>());
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertNotNull(result.getDefiningAttributes());
        assertTrue(result.getDefiningAttributes().isEmpty());
    }

    @Test
    void testConvertToItem_WithNullDefiningAttributes() {
        addVariantRequestVo.setDefiningAttributes(null);
        
        assertThrows(NullPointerException.class, () -> {
            ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        });
    }

    @Test
    void testConvertToItem_WithMultipleDefiningAttributes() {
        List<ProductAttributeDetailVo> definingAttributes = new ArrayList<>();
        
        ProductAttributeDetailVo attribute1 = new ProductAttributeDetailVo();
        attribute1.setAttributeCode(COLOR_ATTRIBUTE_CODE);
        attribute1.setAttributeName(COLOR_ATTRIBUTE_NAME);
        attribute1.setAttributeValue(COLOR_ATTRIBUTE_VALUE);
        definingAttributes.add(attribute1);

        ProductAttributeDetailVo attribute2 = new ProductAttributeDetailVo();
        attribute2.setAttributeCode(SIZE_ATTRIBUTE_CODE);
        attribute2.setAttributeName(SIZE_ATTRIBUTE_NAME);
        attribute2.setAttributeValue(SIZE_ATTRIBUTE_VALUE);
        definingAttributes.add(attribute2);

        addVariantRequestVo.setDefiningAttributes(definingAttributes);
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertEquals(2, result.getDefiningAttributes().size());
        
        ProductAttributeDetail detail1 = result.getDefiningAttributes().get(0);
        assertEquals(COLOR_ATTRIBUTE_CODE, detail1.getAttributeCode());
        assertEquals(COLOR_ATTRIBUTE_NAME, detail1.getAttributeName());
        assertEquals(COLOR_ATTRIBUTE_VALUE, detail1.getAttributeValue());
        
        ProductAttributeDetail detail2 = result.getDefiningAttributes().get(1);
        assertEquals(SIZE_ATTRIBUTE_CODE, detail2.getAttributeCode());
        assertEquals(SIZE_ATTRIBUTE_NAME, detail2.getAttributeName());
        assertEquals(SIZE_ATTRIBUTE_VALUE, detail2.getAttributeValue());
    }

    @Test
    void testConvertToItem_WithDifferentProductProperties() {
        product.setOff2OnChannelActive(false);
        product.setFreeSample(true);
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertFalse(result.isOff2OnChannelActive());
        assertTrue(result.isFreeSample());
    }

    @Test
    void testConvertToItem_WithDifferentMandatoryRequestParam() {
        mandatoryRequestParam.setStoreId(DIFFERENT_STORE);
        mandatoryRequestParam.setUsername(DIFFERENT_USER);
        
        Item result = ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
        
        assertNotNull(result);
        assertEquals(DIFFERENT_STORE, result.getStoreId());
        assertEquals(DIFFERENT_USER, result.getCreatedBy());
        assertEquals(DIFFERENT_USER, result.getUpdatedBy());
    }

    @Test
    void testConvertToItem_WithNullProduct() {
        assertThrows(NullPointerException.class, () -> {
            ProductUtil.convertToItem(addVariantRequestVo, null, mandatoryRequestParam);
        });
    }

    @Test
    void testConvertToItem_WithNullMandatoryRequestParam() {
        assertThrows(NullPointerException.class, () -> {
            ProductUtil.convertToItem(addVariantRequestVo, product, null);
        });
    }

    @Test
    void testConvertToItem_WithNullAddVariantRequestVo() {
        assertThrows(IllegalArgumentException.class, () -> {
            ProductUtil.convertToItem(null, product, mandatoryRequestParam);
        });
    }

    @Test
    void testGetBuyableStatusByChannel_WithValidConfig() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setBuyable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);

        assertTrue(result);
    }

    @Test
    void testGetBuyableStatusByChannel_WithArchivedItem() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setBuyable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, true);

        assertFalse(result);
    }

    @Test
    void testGetBuyableStatusByChannel_WithSchedule() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setBuyable(false);

        ItemBuyableSchedule schedule = new ItemBuyableSchedule();
        schedule.setBuyable(true);
        schedule.setStartDateTime(new Date(System.currentTimeMillis() - 1000));
        schedule.setEndDateTime(new Date(System.currentTimeMillis() + 1000));
        config.setItemBuyableSchedules(schedule);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);

        assertTrue(result);
    }

    @Test
    void testGetBuyableStatusByChannel_WithNoConfig() {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();

        assertThrows(Exception.class, () -> {
            ProductUtil.getBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);
        });
    }

    @Test
    void testGetDiscoverableStatusByChannel_WithValidConfig() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setDiscoverable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);

        assertTrue(result);
    }

    @Test
    void testGetDiscoverableStatusByChannel_WithArchivedItem() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setDiscoverable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, true);

        assertFalse(result);
    }

    @Test
    void testGetDiscoverableStatusByChannel_WithSchedule() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setDiscoverable(false);

        ItemDiscoverableSchedule schedule = new ItemDiscoverableSchedule();
        schedule.setDiscoverable(true);
        schedule.setStartDateTime(new Date(System.currentTimeMillis() - 1000));
        schedule.setEndDateTime(new Date(System.currentTimeMillis() + 1000));
        config.setItemDiscoverableSchedules(schedule);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);

        assertTrue(result);
    }

    @Test
    void testGetDiscoverableStatusByChannel_WithNoConfig() {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();

        assertThrows(Exception.class, () -> {
            ProductUtil.getDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);
        });
    }

    @Test
    void testGetOriginalBuyableStatusByChannel_WithValidConfig() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setBuyable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getOriginalBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);

        assertTrue(result);
    }

    @Test
    void testGetOriginalBuyableStatusByChannel_WithArchivedItem() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setBuyable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getOriginalBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, true);

        assertFalse(result);
    }

    @Test
    void testGetOriginalBuyableStatusByChannel_WithNoConfig() {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();

        assertThrows(Exception.class, () -> {
            ProductUtil.getOriginalBuyableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);
        });
    }

    @Test
    void testGetOriginalDiscoverableStatusByChannel_WithValidConfig() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setDiscoverable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getOriginalDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);

        assertTrue(result);
    }

    @Test
    void testGetOriginalDiscoverableStatusByChannel_WithArchivedItem() throws Exception {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config = new ItemViewConfig();
        config.setChannel(TEST_CHANNEL_1);
        config.setDiscoverable(true);
        itemViewConfigs.add(config);

        boolean result = ProductUtil.getOriginalDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, true);

        assertFalse(result);
    }

    @Test
    void testGetOriginalDiscoverableStatusByChannel_WithNoConfig() {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();

        assertThrows(Exception.class, () -> {
            ProductUtil.getOriginalDiscoverableStatusByChannel(itemViewConfigs, TEST_CHANNEL_1, false);
        });
    }

    @Test
    void testGetPriceByChannel_WithValidPrice() throws Exception {
        Set<Price> prices = new HashSet<>();
        Price price = new Price();
        price.setChannel(TEST_PRICE_CHANNEL);
        price.setOfferPrice(TEST_PRICE_VALUE);
        prices.add(price);

        Price result = ProductUtil.getPriceByChannel(prices, TEST_PRICE_CHANNEL);

        assertNotNull(result);
        assertEquals(TEST_PRICE_CHANNEL, result.getChannel());
        assertEquals(TEST_PRICE_VALUE, result.getOfferPrice());
    }

    @Test
    void testGetPriceByChannel_WithNoPrice() {
        Set<Price> prices = new HashSet<>();

        assertThrows(Exception.class, () -> {
            ProductUtil.getPriceByChannel(prices, TEST_PRICE_CHANNEL);
        });
    }

    @Test
    void testConvertToOfflineItemMap_WithValidItems() {
        Map<String, List<OfflineItem>> offlineItemsMap = new HashMap<>();
        List<OfflineItem> offlineItems = new ArrayList<>();

        OfflineItem item1 = new OfflineItem();
        item1.setItemSku(TEST_OFFLINE_ITEM_SKU_1);
        offlineItems.add(item1);

        OfflineItem item2 = new OfflineItem();
        item2.setItemSku(TEST_OFFLINE_ITEM_SKU_2);
        offlineItems.add(item2);

        OfflineItem item3 = new OfflineItem();
        item3.setItemSku(TEST_OFFLINE_ITEM_SKU_1);
        offlineItems.add(item3);

        ProductUtil.convertToOfflineItemMap(offlineItemsMap, offlineItems);

        assertNotNull(offlineItemsMap.get(TEST_OFFLINE_ITEM_SKU_1));
        assertNotNull(offlineItemsMap.get(TEST_OFFLINE_ITEM_SKU_2));
        assertEquals(2, offlineItemsMap.get(TEST_OFFLINE_ITEM_SKU_1).size());
        assertEquals(1, offlineItemsMap.get(TEST_OFFLINE_ITEM_SKU_2).size());
    }

    @Test
    void testConvertToOfflineItemMap_WithEmptyList() {
        Map<String, List<OfflineItem>> offlineItemsMap = new HashMap<>();
        List<OfflineItem> offlineItems = new ArrayList<>();

        ProductUtil.convertToOfflineItemMap(offlineItemsMap, offlineItems);

        assertTrue(offlineItemsMap.isEmpty());
    }

    @Test
    void testConvertToOfflineItemMap_WithNullList() {
        Map<String, List<OfflineItem>> offlineItemsMap = new HashMap<>();

        assertThrows(NullPointerException.class, () -> {
            ProductUtil.convertToOfflineItemMap(offlineItemsMap, null);
        });
    }

    @Test
    void testGenerateSpecificationDetail_WithValidProduct() {
        Product product = new Product();
        MasterDataProduct masterDataProduct = new MasterDataProduct();
        product.setMasterDataProduct(masterDataProduct);
        
        List<ProductAttribute> definingAttributes = new ArrayList<>();
        ProductAttribute attribute = new ProductAttribute();
        List<ProductAttributeDetail> details = new ArrayList<>();
        ProductAttributeDetail detail = new ProductAttributeDetail();
        detail.setAttributeCode("COLOR");
        detail.setAttributeValue("Red");
        details.add(detail);
        attribute.setProductAttributeDetails(details);
        definingAttributes.add(attribute);
        product.setDefiningAttributes(definingAttributes);

        List<MasterDataProductAttribute> masterDataAttributes = new ArrayList<>();
        MasterDataProductAttribute masterDataAttribute = new MasterDataProductAttribute();
        MasterDataAttribute attributeEntity = new MasterDataAttribute();
        attributeEntity.setAttributeName("Color");
        attributeEntity.setAttributeCode("COLOR");
        attributeEntity.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
        attributeEntity.setSkuValue(false);
        masterDataAttribute.setMasterDataAttribute(attributeEntity);
        masterDataAttributes.add(masterDataAttribute);
        masterDataProduct.setMasterDataProductAttributes(masterDataAttributes);

        String result = ProductUtil.generateSpecificationDetail(product);

        assertNotNull(result);
        assertTrue(result.contains("<ul>"));
        assertTrue(result.contains("</ul>"));
        assertTrue(result.contains("Color"));
        assertTrue(result.contains("Red"));
    }

    @Test
    void testGenerateSpecificationDetail_WithEmptyAttributes() {
        Product product = new Product();
        MasterDataProduct masterDataProduct = new MasterDataProduct();
        product.setMasterDataProduct(masterDataProduct);
        product.setDefiningAttributes(new ArrayList<>());
        masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());

        String result = ProductUtil.generateSpecificationDetail(product);

        assertNotNull(result);
        assertEquals("<ul></ul>", result);
    }

    @Test
    void testGenerateSpecificationDetail_WithNullProduct() {
        assertThrows(NullPointerException.class, () -> {
            ProductUtil.generateSpecificationDetail(null);
        });
    }

    @Test
    void testGenerateSpecificationDetail_WithNullMasterDataProduct() {
        Product product = new Product();
        product.setMasterDataProduct(null);

        assertThrows(NullPointerException.class, () -> {
            ProductUtil.generateSpecificationDetail(product);
        });
    }

    @Test
    void testConstructProductWithItemAndMasterData_WithValidInputs() {
        List<Product> productAvailables = new ArrayList<>();
        Product product = new Product();
        product.setProductSku("PROD-123");
        product.setProductCode("PROD-CODE-123");
        product.setSynchronized(true);
        productAvailables.add(product);

        Map<String, List<Item>> itemAvailableCandidate = new HashMap<>();
        List<Item> items = new ArrayList<>();
        Item item = new Item();
        item.setItemSku("ITEM-123");
        item.setItemCode("ITEM-CODE-123");
        items.add(item);
        itemAvailableCandidate.put("PROD-123", items);

        Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
        MasterDataProductAndItemsVO masterDataVO = new MasterDataProductAndItemsVO();
        MasterDataProduct masterDataProduct = new MasterDataProduct();
        masterDataProduct.setShippingWeight(1.5);
        masterDataVO.setMasterDataProduct(masterDataProduct);
        
        Map<String, MasterDataItem> masterDataItems = new HashMap<>();
        MasterDataItem masterDataItem = new MasterDataItem();
        masterDataItem.setItemDeliveryWeight(0.5);
        masterDataItems.put("ITEM-CODE-123", masterDataItem);
        masterDataVO.setMasterDataItems(masterDataItems);
        masterDataProducts.put("PROD-CODE-123", masterDataVO);

        Map<String, List<ProductAndItemsVO>> result = ProductUtil.constructProductWithItemAndMasterData(
            productAvailables, itemAvailableCandidate, masterDataProducts);

        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertTrue(result.containsKey("PROD-CODE-123"));
        assertEquals(1, result.get("PROD-CODE-123").size());
    }

    @Test
    void testConstructProductWithItemAndMasterData_WithEmptyProducts() {
        List<Product> productAvailables = new ArrayList<>();
        Map<String, List<Item>> itemAvailableCandidate = new HashMap<>();
        Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();

        Map<String, List<ProductAndItemsVO>> result = ProductUtil.constructProductWithItemAndMasterData(
            productAvailables, itemAvailableCandidate, masterDataProducts);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testConstructProductWithItemAndMasterData_WithNullInputs() {
        Map<String, List<ProductAndItemsVO>> result = ProductUtil.constructProductWithItemAndMasterData(null, new HashMap<>(), new HashMap<>());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetSettlementType_WithRegularProduct() {
        Product product = new Product();
        product.setProductType(ProductType.REGULAR);
        Item item = new Item();

        String result = ProductUtil.getSettlementType(product, item);

        assertEquals("REGULAR", result);
    }

    @Test
    void testGetSettlementType_WithBigProduct() {
        Product product = new Product();
        product.setProductType(ProductType.BIG_PRODUCT);
        Item item = new Item();

        String result = ProductUtil.getSettlementType(product, item);

        assertEquals("REGULAR", result);
    }

    @Test
    void testGetSettlementType_WithBarcodeItem() {
        Product product = new Product();
        product.setProductType(ProductType.BOPIS);
        Item item = new Item();
        item.setTicketTemplateCode("TEMPLATE-123");

        String result = ProductUtil.getSettlementType(product, item);

        assertEquals("BARCODE", result);
    }

    @Test
    void testGetSettlementType_WithSettlementCode() {
        Product product = new Product();
        product.setProductType(ProductType.BOPIS);
        Item item = new Item();
        item.setTicketTemplateCode(null);

        String result = ProductUtil.getSettlementType(product, item);

        assertEquals("SETTLEMENT_CODE", result);
    }

    @Test
    void testGetSettlementType_WithEmptyTicketTemplateCode() {
        Product product = new Product();
        product.setProductType(ProductType.BOPIS);
        Item item = new Item();
        item.setTicketTemplateCode("");

        String result = ProductUtil.getSettlementType(product, item);

        assertEquals("SETTLEMENT_CODE", result);
    }

    @Test
    void testDeleteItemAttributeFromProductAttribute_WithValidProduct() {
        Product product = new Product();
        product.setProductSku(TEST_PRODUCT_SKU);
        
        ProductAttribute attribute1 = new ProductAttribute();
        attribute1.setItemSku("ITEM-1");
        
        ProductAttribute attribute2 = new ProductAttribute();
        attribute2.setItemSku("ITEM-2");
        
        List<ProductAttribute> attributes = new ArrayList<>();
        attributes.add(attribute1);
        attributes.add(attribute2);
        product.setDefiningAttributes(attributes);

        Product result = ProductUtil.deleteItemAttributeFromProductAttribute(product, "ITEM-1");

        assertNotNull(result);
        assertEquals(TEST_PRODUCT_SKU, result.getProductSku());
        assertEquals(1, result.getDefiningAttributes().size());
        assertEquals("ITEM-2", result.getDefiningAttributes().get(0).getItemSku());
    }

    @Test
    void testDeleteItemAttributeFromProductAttribute_WithNullProduct() {
        Product result = ProductUtil.deleteItemAttributeFromProductAttribute(null, "ITEM-1");

        assertNull(result);
    }

    @Test
    void testDeleteItemAttributeFromProductAttribute_WithNonExistentItemSku() {
        Product product = new Product();
        product.setProductSku(TEST_PRODUCT_SKU);
        
        ProductAttribute attribute = new ProductAttribute();
        attribute.setItemSku("ITEM-1");
        
        List<ProductAttribute> attributes = new ArrayList<>();
        attributes.add(attribute);
        product.setDefiningAttributes(attributes);

        Product result = ProductUtil.deleteItemAttributeFromProductAttribute(product, "NON-EXISTENT");

        assertNotNull(result);
        assertEquals(TEST_PRODUCT_SKU, result.getProductSku());
        assertEquals(1, result.getDefiningAttributes().size());
        assertEquals("ITEM-1", result.getDefiningAttributes().get(0).getItemSku());
    }

    @Test
    void testOverwriteItemPriceWithOfflinePrice_WithValidInputs() {
        Set<Price> prices = new HashSet<>();
        Price price1 = new Price();
        price1.setChannel("CHANNEL-1");
        price1.setListPrice(100.0);
        price1.setOfferPrice(90.0);
        prices.add(price1);
        
        Price price2 = new Price();
        price2.setChannel("CHANNEL-2");
        price2.setListPrice(200.0);
        price2.setOfferPrice(180.0);
        prices.add(price2);

        OfflineItem offlineItem = new OfflineItem();
        offlineItem.setListPrice(150.0);
        offlineItem.setOfferPrice(120.0);
        offlineItem.setUpdatedBy("TEST-USER");
        Date updateDate = new Date();
        offlineItem.setUpdatedDate(updateDate);

        ProductUtil.overwriteItemPriceWithOfflinePrice(prices, offlineItem);

        for (Price price : prices) {
            assertEquals(150.0, price.getListPrice());
            assertEquals(120.0, price.getOfferPrice());
            assertEquals("TEST-USER", price.getLastUpdatedBy());
            assertEquals(updateDate, price.getLastUpdatedDate());
        }
    }

    @Test
    void testOverwriteItemPriceWithOfflinePrice_WithNullListPrice() {
        Set<Price> prices = new HashSet<>();
        Price price = new Price();
        price.setChannel("CHANNEL-1");
        price.setListPrice(100.0);
        price.setOfferPrice(90.0);
        prices.add(price);

        OfflineItem offlineItem = new OfflineItem();
        offlineItem.setListPrice(null);
        offlineItem.setOfferPrice(120.0);
        offlineItem.setUpdatedBy("TEST-USER");
        Date updateDate = new Date();
        offlineItem.setUpdatedDate(updateDate);

        ProductUtil.overwriteItemPriceWithOfflinePrice(prices, offlineItem);

        for (Price p : prices) {
            assertEquals(120.0, p.getListPrice());
            assertEquals(120.0, p.getOfferPrice());
            assertEquals("TEST-USER", p.getLastUpdatedBy());
            assertEquals(updateDate, p.getLastUpdatedDate());
        }
    }

    @Test
    void testOverwriteItemPriceWithOfflinePrice_WithEmptyPrices() {
        Set<Price> prices = new HashSet<>();
        OfflineItem offlineItem = new OfflineItem();
        offlineItem.setListPrice(150.0);
        offlineItem.setOfferPrice(120.0);

        ProductUtil.overwriteItemPriceWithOfflinePrice(prices, offlineItem);
        Assertions.assertNotNull(prices);
    }

    @Test
    void testOverwriteItemPriceWithOfflinePrice_WithNullOfflineItem() {
        Set<Price> prices = new HashSet<>();
        Price price = new Price();
        price.setChannel("CHANNEL-1");
        price.setListPrice(100.0);
        prices.add(price);

        ProductUtil.overwriteItemPriceWithOfflinePrice(prices, null);
        Assertions.assertNotNull(prices);
    }

    @Test
    void testOverwriteItemViewConfigsForOfflineItem_WithValidInputs() {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
        ItemViewConfig config1 = new ItemViewConfig();
        config1.setChannel("CHANNEL-1");
        config1.setBuyable(false);
        config1.setDiscoverable(false);
        itemViewConfigs.add(config1);
        
        ItemViewConfig config2 = new ItemViewConfig();
        config2.setChannel("CHANNEL-2");
        config2.setBuyable(false);
        config2.setDiscoverable(false);
        itemViewConfigs.add(config2);

        ProductUtil.overwriteItemViewConfigsForOfflineItem(itemViewConfigs);

        for (ItemViewConfig config : itemViewConfigs) {
            assertTrue(config.isBuyable());
            assertTrue(config.isDiscoverable());
            assertNull(config.getItemBuyableSchedules());
            assertNull(config.getItemDiscoverableSchedules());
        }
    }

    @Test
    void testOverwriteItemViewConfigsForOfflineItem_WithEmptyConfigs() {
        Set<ItemViewConfig> itemViewConfigs = new HashSet<>();

        ProductUtil.overwriteItemViewConfigsForOfflineItem(itemViewConfigs);
    }

    @Test
    void testOverwriteItemViewConfigsForOfflineItem_WithNullConfigs() {
        ProductUtil.overwriteItemViewConfigsForOfflineItem(null);
    }
}

