package com.gdn.aggregate.platform.module.product.listener.model.sub;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Stock {

    private String itemSku;

    private String pickupPointCode;

    @Deprecated
    private int quota;

    @Deprecated
    private int remaining;

    private boolean warehouse;

    private boolean exists;

    private String status;

}
