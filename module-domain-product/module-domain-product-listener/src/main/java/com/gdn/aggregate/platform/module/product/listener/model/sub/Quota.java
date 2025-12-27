package com.gdn.aggregate.platform.module.product.listener.model.sub;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Quota {

    private String itemSku;

    private String pickupPointCode;

    private String owner;

    private int used;

    private int quota;

    private int remaining;

}
