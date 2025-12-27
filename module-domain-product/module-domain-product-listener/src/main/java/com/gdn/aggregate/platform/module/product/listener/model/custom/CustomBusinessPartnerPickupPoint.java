package com.gdn.aggregate.platform.module.product.listener.model.custom;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.DayOfWeek;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.BUSINESS_PARTNER_PICKUP_POINT)
public class CustomBusinessPartnerPickupPoint extends BaseData {

    private String code;

    private String name;

    private String address;

    private String countryCode;

    private String provinceCode;

    private String cityCode;

    private String districtCode;

    private String subDistrictCode;

    private String zipCode;

    private String provinceName;

    private String cityName;

    private String districtName;

    private String subDistrictName;

    private String telephone;

    private String warehouseId;

    private String originId;

    private GeolocationVO geolocation;

    private String businessPartnerCode;

    private boolean cncActivated;

    private String fax;

    private String additionalInfo;

    private String locationId;

    private boolean archived;

    private String businessPartnerName;

    private String externalPickupPointCode;

    private Integer gmv;

    private Integer orderCount;

    private Integer productCount;

    private String fullPickupPointAddress;

    private String generatedName;

    private String displayName;

    private Boolean fbbActivated;

    private GeoPointVO geoPoint;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class GeolocationVO {

        private String placeId;

        private Double latitude;

        private Double longitude;

        private String streetAddress;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BusinessHourVO {

        private DayOfWeek day;

        private int openingTimeInSeconds;

        private int closingTimeInSeconds;

        private boolean open;

    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class GeoPointVO {

        private Double lat;

        private Double lon;

    }

}
