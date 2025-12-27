package com.gdn.aggregate.platform.module.product.listener.model.other;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CalculatedItem {

    private String itemSku;

    private String pickupPointCode;

    private String externalPickupPointCode;

    private PriceDetail price;

    private Stock stock;

    private Stock flashsaleStock;

    private String campaignCode;

    private Integer sessionId;

    private String adjustmentName;

    private Long start;

    private Long end;

    private Double value;

    private boolean activated;

    private boolean markForDelete;

    private boolean currentlyActive;

    private String purchasedType;

    private int purchasedTypeScore;

    private boolean cncActivated;

    private boolean fbbActivated;

    private Integer cheapestPriceDays;

    private Integer priority;

    @Builder.Default
    private Boolean flashsale = false;

    @Builder.Default
    private Boolean available = false;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PriceDetail {

        private ScheduleFlag buyable;

        private ScheduleFlag discoverable;

        private ScheduleFlag buyableCnc;

        private ScheduleFlag discoverableCnc;

        private String purchasedType;

        private int purchasedTypeScore;

        private Double listed;

        private Double offered;

        private Double finalOffered;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ScheduleFlag {

        private boolean value;

        private SivaProduct.ViewSchedule schedule;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Price {

        private Double listed;

        private Double offered;

        private Double finalOffered;

        private SivaProduct.PriceSchedule adjustment;

        private String cheapestItemSku;

        private String cheapestPickupPointCode;

        private Integer cheapestPriceDays;

    }

}
