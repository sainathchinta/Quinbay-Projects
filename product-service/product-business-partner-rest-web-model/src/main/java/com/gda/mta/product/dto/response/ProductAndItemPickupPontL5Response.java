package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemPickupPontL5Response extends BaseResponse implements Serializable {

    private static final long serialVersionUID = 6652905286417794373L;
    //Fields from x-product or PBP(for need revision)
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
    private List<ProductLevel3PriceResponse> prices;
    private List<ProductLevel3ViewConfigResponse> viewConfigs;
    private boolean archived;

    //Fields from PCB
    private String upcCode;

    //Fields from inventory
    private boolean webSyncStock;
    private Integer availableStockLevel2;
    private Integer reservedStockLevel2;

    //Fields from campaign
    private Double campaignMaxPrice;
    private Double campaignMinPrice;
    private List<String> priceUpdateCriteria;

}
