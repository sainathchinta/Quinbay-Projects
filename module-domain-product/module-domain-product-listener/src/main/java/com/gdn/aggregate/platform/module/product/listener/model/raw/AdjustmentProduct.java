package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.ADJUSTMENT_PRODUCT)
public class AdjustmentProduct extends BaseData {

    private String adjustmentName;

    private String description;

    @JsonProperty("productSku")
    private String itemSku;

    private String campaignCode;

    private String promoType;

    private Long startDate;

    private Long endDate;

    private Long initValue;

    private long value;

    private boolean activated;

    private boolean exclusiveProduct;

    private boolean promoCampaign;

    private Integer sessionId;

    private int priority;

    private String pickupPointCode;

    private Set<String> initBudgetOwners;

    private Set<String> budgetOwners;

    @Override
    public List<String> toIds() {
        if (StringUtils.isEmpty(this.itemSku)) {
            return new ArrayList<>();
        }
        else {
            return ModuleProductUtil.toAdjustmentProductIds(this.itemSku,this.pickupPointCode,ModuleProductUtil.toAdjPromoType(this),ModuleProductUtil.toAdjCampaignCode(this));
        }
    }

}
