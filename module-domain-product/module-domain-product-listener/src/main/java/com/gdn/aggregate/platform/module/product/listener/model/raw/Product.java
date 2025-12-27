package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import lombok.experimental.SuperBuilder;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.ArrayList;
import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.PRODUCT)
public class Product extends BaseData {

    private String productSku;

    private String productCode;

    private String productType;

    private String settlementType;

    private String merchantCode;

    @JsonProperty("synchronized")
    private boolean sync;

    private String productCatentryId;

    private List<ProductAttribute> definingAttributes = new ArrayList<ProductAttribute>();

    private List<ProductSpecialAttribute> productSpecialAttributes;

    private MasterCatalog masterCatalog;

    private List<SalesCatalog> salesCatalogs;

    private MasterDataProduct masterDataProduct;

    private List<SalesCategorySequence> salesCategorySequences;

    private boolean installationRequired;

    private boolean off2OnChannelActive;

    @Transient
    private boolean sharedProduct;

    @Transient
    private List<String> productChangeEventType = new ArrayList<>();

    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.productSku);
        return MainUtil.toList(id);
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductAttribute {

        private String itemSku;

        private List<ProductAttributeDetail> productAttributeDetails;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductAttributeDetail {

        private String attributeCode;

        private String attributeName;

        private String attributeValue;

        private String sequence;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductSpecialAttribute {

        private String attributeCode;

        private String attributeName;

        private String attributeValue;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MasterCatalog {

        private String catalogCode;

        private Category category;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Category {

        private String categoryCode;

        private String catgroupId;

        private int sequence;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SalesCatalog {

        private String catalogCode;

        private List<Category> listOfCategories = new ArrayList<Category>();

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SalesCategorySequence {

        private String categoryCode;

        private int sequence;

    }

}
