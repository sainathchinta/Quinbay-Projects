package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.PICKUP_POINT)
public class PickupPoint extends BaseData {

    private String merchantCode;

    private String itemSku;

    private String productSku;

    private String merchantSku;

    private Set<Price> price = new HashSet<Price>();

    private Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();

    private boolean buyable;

    private boolean discoverable;

    private boolean buyableCnc;

    private boolean discoverableCnc;

    private String purchasedType;

    private int purchasedTypeScore;

    private boolean fbbActivated;

    private String pickupPointCode;

    private String externalPickupPointCode;

    private Double latitude;

    private Double longitude;

    private String cityName;

    private boolean cncActive;

    private boolean wholesalePriceExists;

    private boolean merchantPromoDiscount;

    private boolean flashSaleActive;

    private boolean promoBundling;

    private Set<String> activePromoBundlings;

    private boolean warehouse;

    private boolean inStock;

    private boolean inQuota;

    private int quotaPercentage;

    private Boolean newData;

    private boolean distribution;

    @Transient
    private List<String> itemPickupPointChangeEventTypesV2 = new ArrayList<>();


    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.itemSku);
        String subId = MainUtil.toNotNullString(this.pickupPointCode);
        return MainUtil.toList(id,subId);
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Price {

        private String currency;

        private double listPrice;

        private double offerPrice;

        private double finalOfferPrice;

        private double adjustment;

        private String campaignCode;

        private boolean promoCampaign;

        private Integer priority;

        private DiscountPrice merchantPromoDiscountPrice;

        private String channel;

        private String lastUpdatedBy;

        private Long lastUpdatedDate;

    }


    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DiscountPrice {

        private double discountPrice;

        private Long startDateTime;

        private Long endDateTime;

        private String adjustmentName;

        private String adjustmentType;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ItemViewConfig {

        private boolean buyable;

        private boolean discoverable;

        private String channel;

        private ItemDiscoverableSchedule itemDiscoverableSchedules;

        private ItemBuyableSchedule itemBuyableSchedules;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ItemDiscoverableSchedule {

        private boolean discoverable;

        private Long startDateTime;

        private Long endDateTime;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ItemBuyableSchedule {

        private boolean buyable;

        private Long startDateTime;

        private Long endDateTime;

    }

}
