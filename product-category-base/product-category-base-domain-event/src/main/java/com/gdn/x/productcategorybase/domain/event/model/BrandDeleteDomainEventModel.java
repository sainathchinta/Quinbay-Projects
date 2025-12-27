package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandDeleteDomainEventModel extends GdnBaseDomainEventModel {
    private String brandCode;
    private String storeId;

    public String getBrandCode() {
        return brandCode;
    }

    public String getStoreId() {
        return storeId;
    }

    public void setBrandCode(String brandCode) {
        this.brandCode = brandCode;
    }

    public void setStoreId(String storeId) {
        this.storeId = storeId;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("BrandDeleteDomainEventModel{");
        sb.append("brandCode='")
                .append(brandCode)
                .append('\'');
        sb.append(", storeId='")
                .append(storeId)
                .append('\'');
        sb.append('}');
        return sb.toString();
    }
}
