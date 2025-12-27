package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL5DetailResponse extends BaseResponse {

    private String productCode;
    private String productSku;
    private String skuCode;
    private String itemSku;
    private String merchantSku;
    private String merchantCode;
    private String itemName;
    private Set<String> priceEditDisabledReason;
    private String pickupPointCode;
    private boolean freeSample;
    private boolean priceEditDisabled;
    private boolean enableEdit;
    private boolean cncActive;
    private boolean fbbActivated;
    private boolean productSyncStatus;
    private String categoryCode;
    private List<PriceResponse> prices;
    private List<ViewConfigResponse> viewConfigs;
    private boolean archived;
    private String upcCode;
    private boolean markForDelete;
    private boolean L5MarkForDelete;
    private String brand;
    private boolean suspended;
}
