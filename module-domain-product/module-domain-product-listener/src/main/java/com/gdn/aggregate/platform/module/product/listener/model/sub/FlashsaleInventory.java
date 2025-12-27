package com.gdn.aggregate.platform.module.product.listener.model.sub;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FlashsaleInventory {

    private FlashsaleQuota quota;

    private FlashsaleSchedule schedule;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FlashsaleQuota {

        private String itemSku;

        private String pickupPointCode;

        private String status;

        private int percentage;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FlashsaleSchedule {

        String id;

        Long start;

        Long end;

    }

}
