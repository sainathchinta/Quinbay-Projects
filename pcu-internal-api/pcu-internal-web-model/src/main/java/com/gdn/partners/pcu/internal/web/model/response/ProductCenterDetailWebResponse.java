package com.gdn.partners.pcu.internal.web.model.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductCenterDetailWebResponse {

    private String productName;
    private String productSku;
    private String productCode;
    private String status;
    private String imagePath;
    private Date lastUpdated;
    private String sellerCode;
    private String sellerName;
    private CategoryTreeWebResponse masterCategory;
    private List<CatalogTreeWebResponse> salesCategory = new ArrayList<>();
}