package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;

class MasterDataUtilTest {

    private static final String TEST_ITEM_SKU = "TEST-ITEM-SKU";
    private static final String COLOR_ATTRIBUTE_CODE = "COLOR";
    private static final String COLOR_ATTRIBUTE_NAME = "Color";
    private static final String COLOR_ATTRIBUTE_VALUE = "Red";
    private static final String DESCRIPTION_ATTRIBUTE_CODE = "DESCRIPTION";
    private static final String DESCRIPTION_ATTRIBUTE_NAME = "Description";
    private static final String DESCRIPTION_ATTRIBUTE_VALUE = "Test Description";
    private static final String SIZE_ATTRIBUTE_CODE = "SIZE";
    private static final String SIZE_ATTRIBUTE_NAME = "Size";
    private static final String SIZE_ATTRIBUTE_VALUE = "Large";

    private Product product;
    private MasterDataItem masterDataItem;
    private MasterDataProduct masterDataProduct;
    private List<MasterDataItemAttributeValue> itemAttributeValues;
    private MasterDataItemAttributeValue itemAttributeValue;

    @BeforeEach
    void setUp() {
        product = new Product();
        product.setDefiningAttributes(new ArrayList<>());

        masterDataItem = new MasterDataItem();
        masterDataItem.setItemDeliveryWeight(null);
        masterDataItem.setItemHeight(null);
        masterDataItem.setItemLength(null);
        masterDataItem.setItemWeight(null);
        masterDataItem.setItemWidth(null);

        masterDataProduct = new MasterDataProduct();
        masterDataProduct.setShippingWeight(10.0);
        masterDataProduct.setHeight(20.0);
        masterDataProduct.setLength(30.0);
        masterDataProduct.setWeight(15.0);
        masterDataProduct.setWidth(25.0);

        itemAttributeValue = new MasterDataItemAttributeValue();
        MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
        masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
        masterDataAttribute.setAttributeCode(COLOR_ATTRIBUTE_CODE);
        masterDataAttribute.setAttributeName(COLOR_ATTRIBUTE_NAME);
        masterDataAttribute.setVariantCreation(true);
        itemAttributeValue.setMasterDataAttribute(masterDataAttribute);
        itemAttributeValue.setAttributeValue(COLOR_ATTRIBUTE_VALUE);

        itemAttributeValues = new ArrayList<>();
        itemAttributeValues.add(itemAttributeValue);
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithValidInputs() {
        Product result = MasterDataUtil.addItemAttributeToProductAttribute(product, TEST_ITEM_SKU, itemAttributeValues);
        
        assertNotNull(result);
        assertEquals(1, result.getDefiningAttributes().size());
        
        ProductAttribute productAttribute = result.getDefiningAttributes().get(0);
        assertEquals(TEST_ITEM_SKU, productAttribute.getItemSku());
        assertEquals(1, productAttribute.getProductAttributeDetails().size());
        
        ProductAttributeDetail detail = productAttribute.getProductAttributeDetails().get(0);
        assertEquals(COLOR_ATTRIBUTE_CODE, detail.getAttributeCode());
        assertEquals(COLOR_ATTRIBUTE_NAME, detail.getAttributeName());
        assertEquals(COLOR_ATTRIBUTE_VALUE, detail.getAttributeValue());
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithNullProduct() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            MasterDataUtil.addItemAttributeToProductAttribute(null, TEST_ITEM_SKU, itemAttributeValues);
        });
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithBlankItemSku() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            MasterDataUtil.addItemAttributeToProductAttribute(product, "", itemAttributeValues);
        });
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithNullItemSku() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            MasterDataUtil.addItemAttributeToProductAttribute(product, null, itemAttributeValues);
        });
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithNullItemAttributeValues() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            MasterDataUtil.addItemAttributeToProductAttribute(product, TEST_ITEM_SKU, null);
        });
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithEmptyItemAttributeValues() {
        List<MasterDataItemAttributeValue> emptyValues = new ArrayList<>();
        
        Product result = MasterDataUtil.addItemAttributeToProductAttribute(product, TEST_ITEM_SKU, emptyValues);
        
        assertNotNull(result);
        assertEquals(1, result.getDefiningAttributes().size());
        
        ProductAttribute productAttribute = result.getDefiningAttributes().get(0);
        assertEquals(TEST_ITEM_SKU, productAttribute.getItemSku());
        assertTrue(productAttribute.getProductAttributeDetails().isEmpty());
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithNonDefiningAttribute() {
        MasterDataItemAttributeValue nonDefiningAttribute = new MasterDataItemAttributeValue();
        MasterDataAttribute nonDefiningMasterDataAttribute = new MasterDataAttribute();
        nonDefiningMasterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
        nonDefiningMasterDataAttribute.setAttributeCode(DESCRIPTION_ATTRIBUTE_CODE);
        nonDefiningMasterDataAttribute.setAttributeName(DESCRIPTION_ATTRIBUTE_NAME);
        nonDefiningMasterDataAttribute.setVariantCreation(false);
        nonDefiningAttribute.setMasterDataAttribute(nonDefiningMasterDataAttribute);
        nonDefiningAttribute.setAttributeValue(DESCRIPTION_ATTRIBUTE_VALUE);
        
        List<MasterDataItemAttributeValue> mixedValues = new ArrayList<>();
        mixedValues.add(itemAttributeValue);
        mixedValues.add(nonDefiningAttribute);
        
        Product result = MasterDataUtil.addItemAttributeToProductAttribute(product, TEST_ITEM_SKU, mixedValues);
        
        assertNotNull(result);
        assertEquals(1, result.getDefiningAttributes().size());
        
        ProductAttribute productAttribute = result.getDefiningAttributes().get(0);
        assertEquals(1, productAttribute.getProductAttributeDetails().size());
        assertEquals(COLOR_ATTRIBUTE_CODE, productAttribute.getProductAttributeDetails().get(0).getAttributeCode());
    }

    @Test
    void testAddItemAttributeToProductAttribute_WithVariantCreationAttribute() {
        MasterDataItemAttributeValue variantAttribute = new MasterDataItemAttributeValue();
        MasterDataAttribute variantMasterDataAttribute = new MasterDataAttribute();
        variantMasterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
        variantMasterDataAttribute.setAttributeCode(SIZE_ATTRIBUTE_CODE);
        variantMasterDataAttribute.setAttributeName(SIZE_ATTRIBUTE_NAME);
        variantMasterDataAttribute.setVariantCreation(true);
        variantAttribute.setMasterDataAttribute(variantMasterDataAttribute);
        variantAttribute.setAttributeValue(SIZE_ATTRIBUTE_VALUE);
        
        List<MasterDataItemAttributeValue> variantValues = new ArrayList<>();
        variantValues.add(variantAttribute);
        
        Product result = MasterDataUtil.addItemAttributeToProductAttribute(product, TEST_ITEM_SKU, variantValues);
        
        assertNotNull(result);
        assertEquals(1, result.getDefiningAttributes().size());
        
        ProductAttribute productAttribute = result.getDefiningAttributes().get(0);
        assertEquals(1, productAttribute.getProductAttributeDetails().size());
        assertEquals(SIZE_ATTRIBUTE_CODE, productAttribute.getProductAttributeDetails().get(0).getAttributeCode());
    }

    @Test
    void testConstructItemDimensionFields_WithValidInputs() {
        MasterDataItem result = MasterDataUtil.constructItemDimensionFields(masterDataItem, masterDataProduct);
        
        assertNotNull(result);
        assertEquals(10.0, result.getItemDeliveryWeight());
        assertEquals(20.0, result.getItemHeight());
        assertEquals(30.0, result.getItemLength());
        assertEquals(15.0, result.getItemWeight());
        assertEquals(25.0, result.getItemWidth());
    }

    @Test
    void testConstructItemDimensionFields_WithNullMasterDataItem() {
        MasterDataItem result = MasterDataUtil.constructItemDimensionFields(null, masterDataProduct);
        
        assertNull(result);
    }

    @Test
    void testConstructItemDimensionFields_WithNullMasterDataProduct() {
        MasterDataItem result = MasterDataUtil.constructItemDimensionFields(masterDataItem, null);
        
        assertNotNull(result);
        assertEquals(masterDataItem, result);
    }

    @Test
    void testConstructItemDimensionFields_WithBothNull() {
        MasterDataItem result = MasterDataUtil.constructItemDimensionFields(null, null);
        
        assertNull(result);
    }

    @Test
    void testConstructItemDimensionFields_WithExistingValues() {
        masterDataItem.setItemDeliveryWeight(5.0);
        masterDataItem.setItemHeight(10.0);
        masterDataItem.setItemLength(15.0);
        masterDataItem.setItemWeight(7.5);
        masterDataItem.setItemWidth(12.5);
        
        MasterDataItem result = MasterDataUtil.constructItemDimensionFields(masterDataItem, masterDataProduct);
        
        assertNotNull(result);
        assertEquals(5.0, result.getItemDeliveryWeight());
        assertEquals(10.0, result.getItemHeight());
        assertEquals(15.0, result.getItemLength());
        assertEquals(7.5, result.getItemWeight());
        assertEquals(12.5, result.getItemWidth());
    }

    @Test
    void testConstructItemDimensionFields_WithPartialExistingValues() {
        masterDataItem.setItemDeliveryWeight(5.0);
        masterDataItem.setItemHeight(null);
        masterDataItem.setItemLength(15.0);
        masterDataItem.setItemWeight(null);
        masterDataItem.setItemWidth(12.5);
        
        MasterDataItem result = MasterDataUtil.constructItemDimensionFields(masterDataItem, masterDataProduct);
        
        assertNotNull(result);
        assertEquals(5.0, result.getItemDeliveryWeight());
        assertEquals(20.0, result.getItemHeight());
        assertEquals(15.0, result.getItemLength());
        assertEquals(15.0, result.getItemWeight());
        assertEquals(12.5, result.getItemWidth());
    }
}

