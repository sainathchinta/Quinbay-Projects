package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandDomainEventModel extends GdnBaseDomainEventModel {
    private String brandCode;
    private String brandName;
    private byte[] brandDescription;
    private String brandLogoPath;
    private String storeId;
    private String businessPartnerCode;
    private String brandRequestCode;
    private boolean validBrand;
    private boolean protectedBrand;
}
