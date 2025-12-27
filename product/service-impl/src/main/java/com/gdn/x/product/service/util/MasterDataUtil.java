package com.gdn.x.product.service.util;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;

/**
 * Utility class for master data-related operations.
 * Contains static methods that were previously in service classes to avoid circular dependencies.
 */
public class MasterDataUtil {

    private static final String PRODUCT_MUST_NOT_BE_NULL = "product must not be null";
    private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";
    private static final String ITEM_ATTRIBUTE_VALUES_MUST_NOT_BE_NULL = "itemAttributeValues must not be null";

    /**
     * Adds item attribute to product attribute.
     * 
     * @param product the product entity
     * @param itemSku the item SKU
     * @param itemAttributeValues the item attribute values
     * @return the updated product
     */
    public static Product addItemAttributeToProductAttribute(Product product, String itemSku,
        List<MasterDataItemAttributeValue> itemAttributeValues) {
        checkArgument(product != null, PRODUCT_MUST_NOT_BE_NULL);
        checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);
        checkArgument(itemAttributeValues != null, ITEM_ATTRIBUTE_VALUES_MUST_NOT_BE_NULL);

        ProductAttribute productAttribute = new ProductAttribute();
        productAttribute.setItemSku(itemSku);

        List<ProductAttributeDetail> productAttributeDetails = new ArrayList<>();
        if (itemAttributeValues != null) {
            for (MasterDataItemAttributeValue itemAttributeValue : itemAttributeValues) {
                if (isDefiningAttributeOrVariantCreationTrue(itemAttributeValue)) {
                ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
                productAttributeDetail.setAttributeCode(itemAttributeValue.getMasterDataAttribute().getAttributeCode());
                productAttributeDetail.setAttributeName(itemAttributeValue.getMasterDataAttribute().getAttributeName());
                productAttributeDetail.setAttributeValue(itemAttributeValue.getAttributeValue());
                productAttributeDetails.add(productAttributeDetail);
                }
            }
        }
        productAttribute.setProductAttributeDetails(productAttributeDetails);
        if (product != null) {
            product.getDefiningAttributes().add(productAttribute);
        }
        return product;
    }

    /**
     * Checks if the attribute is defining or variant creation.
     * 
     * @param itemAttributeValue the item attribute value
     * @return true if it's defining or variant creation
     */
    private static boolean isDefiningAttributeOrVariantCreationTrue(MasterDataItemAttributeValue itemAttributeValue) {
        return MasterDataAttributeType.DEFINING_ATTRIBUTE
            .equals(itemAttributeValue.getMasterDataAttribute().getAttributeType()) || itemAttributeValue
            .getMasterDataAttribute().isVariantCreation();
    }

    /**
     * Constructs item dimension fields from master data.
     * 
     * @param masterDataItem the master data item
     * @param masterDataProduct the master data product
     * @return the updated master data item
     */
    public static MasterDataItem constructItemDimensionFields(MasterDataItem masterDataItem,
        MasterDataProduct masterDataProduct) {
        if (masterDataItem == null || masterDataProduct == null) {
            return masterDataItem;
        }
        if (masterDataItem.getItemDeliveryWeight() == null) {
            masterDataItem.setItemDeliveryWeight(masterDataProduct.getShippingWeight());
        }
        if (masterDataItem.getItemHeight() == null) {
            masterDataItem.setItemHeight(masterDataProduct.getHeight());
        }
        if (masterDataItem.getItemLength() == null) {
            masterDataItem.setItemLength(masterDataProduct.getLength());
        }
        if (masterDataItem.getItemWeight() == null) {
            masterDataItem.setItemWeight(masterDataProduct.getWeight());
        }
        if (masterDataItem.getItemWidth() == null) {
            masterDataItem.setItemWidth(masterDataProduct.getWidth());
        }
        return masterDataItem;
    }
}
