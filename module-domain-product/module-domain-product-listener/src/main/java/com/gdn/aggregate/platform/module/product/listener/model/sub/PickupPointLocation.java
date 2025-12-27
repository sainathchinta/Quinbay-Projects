package com.gdn.aggregate.platform.module.product.listener.model.sub;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PickupPointLocation {

    private String pickupPointCode;

    private Double latitude;

    private Double longitude;

    private String cityName;

}
