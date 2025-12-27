package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import lombok.experimental.SuperBuilder;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.ITEM)
public class Item extends BaseData {

    private String merchantCode;

    private String itemSku;

    private String productSku;

    private String merchantSku;

    private String itemCode;

    @JsonProperty("synchronized")
    private boolean sync;

    private String itemCatentryId;

    private MasterDataItem masterDataItem;

    private Boolean isLateFulfillment;

    private boolean off2OnChannelActive;

    private boolean archived;

    private PristineDataItemEventModel pristineDataItem;

    private String uniqueId;

    private List<String> itemChangeEventTypes;

    private Tag tag;

    private boolean subscribable;

    private boolean forceReview;

    private boolean contentChanged;

    private String generatedItemName;

    private String mainImageUrl;

    private Boolean newData;

    private String source;

    private String omniChannelSku;

    @Transient
    private List<String> itemChangeEventTypesV2 = new ArrayList<>();

    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.itemSku);
        return MainUtil.toList(id);
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PristineDataItemEventModel {

        private String pcbProductItemId;

        private String pristineId;

        private String pristineMasterId;

        private String pristineProductName;

        private String pristineModel;

        private String productCondition;

        private String pristineBrand;

        private String pristineCategory;

        private String defaultProductCode;

        private String pristineShortId;

        private Map<String,String> pristineListingAttributes;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Tag {

        private boolean fbb;

        private boolean cnc;

        private boolean tradeIn;

        private boolean thd;

    }

}
