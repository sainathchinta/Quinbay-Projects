package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProductEANResponse extends BaseResponse {
    private String productSku;
    private String productName;
    private String itemSku;
    private String itemName;
    private String upcCode;
}
