package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProductLevel3ViewConfigStockRequest extends ProductLevel3ViewConfigRequest {
    private Integer availableStock;
    private boolean cncActive;

    public ProductLevel3ViewConfigStockRequest(String channelId, Boolean display, Boolean buyable,
            Integer availableStock) {
        super(channelId, display, buyable);
        this.availableStock = availableStock;
    }
}
