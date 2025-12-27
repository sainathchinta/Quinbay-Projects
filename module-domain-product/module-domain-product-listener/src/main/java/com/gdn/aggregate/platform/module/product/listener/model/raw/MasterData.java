package com.gdn.aggregate.platform.module.product.listener.model.raw;

import java.util.List;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.MASTER_DATA)
public class MasterData extends BaseData {

    private String name;

    private String productCode;

    private Double length;

    private Double width;

    private Double weight;

    private Double height;

    private Double shippingWeight;

    private boolean productMarkForDelete;

    private String storeId;

    private byte[] description;

    private String specificationDetail;

    private String productStory;

    private String brand;

    private String brandLogoUrl;

    private String uniqueSellingPoint;

    private String uom;

    private boolean activated;

    private boolean viewable;

    private String url;

    private List<ProductItemDomainEventModel> productItems;

    private List<ProductCategoryDomainEventModel> productCategories;

    private List<ProductAttributeDomainEventModel> productAttributes;

    private List<ImageDomainEventModel> images;

    private boolean newProduct;

    private boolean reviewPending;

    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.productCode);
        return MainUtil.toList(id);
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductItemDomainEventModel {

        private String generatedItemName;

        private String upcCode;

        private String skuCode;

        private boolean activated;

        private boolean viewable;

        private List<ImageDomainEventModel> images;

        private Integer dangerousGoodsLevel;

        private Boolean newData;

        private boolean markForDelete;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductCategoryDomainEventModel {

        private CategoryDomainEventModel category;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CategoryDomainEventModel {

        private String id;

        private String name;

        private String categoryCode;

        private Integer sequence;

        private byte[] description;

        private boolean display;

        private Integer logisticAdjustment;

        private boolean warranty;

        private boolean needIdentity;

        private boolean activated;

        private boolean viewable;

        private CatalogDomainEventModel catalog;

        private String parentCategoryId;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CatalogDomainEventModel {

        private String name;

        private String catalogCode;

        private String catalogType;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductAttributeDomainEventModel {

        private AttributeDomainEventModel attribute;

        private String productAttributeName;

        private boolean ownByProductItem;

        private Integer sequence;

        private List<ProductAttributeValueDomainEventModel> productAttributeValues;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AttributeDomainEventModel {

        private String name;

        private String attributeCode;

        private String attributeType;

        private byte[] description;

        private boolean skuValue;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductAttributeValueDomainEventModel {

        private AllowedAttributeValueDomainEventModel allowedAttributeValue;

        private String descriptiveAttributeValue;

        private String descriptiveAttributeValueType;

        private PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValue;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AllowedAttributeValueDomainEventModel {

        private String allowedAttributeCode;

        private String value;

        private Integer sequence;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PredefinedAllowedAttributeValueDomainEventModel {

        private String predefinedAllowedAttributeCode;

        private String value;

        private Integer sequence;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ImageDomainEventModel {

        private boolean mainImage;

        private String locationPath;

        private Integer sequence;

    }

}
