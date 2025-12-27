package com.gdn.sellerLogistics;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class SellerLogisticsProductResponse {
    private String logisticProductCode;
    private boolean selected;
    private boolean requiredLongLat;
    private String logisticProductName;
    private String highlightedInformation;
}
