package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterEventModel extends GdnBaseDomainEventModel {
    /**
     * container for ProductMaster event. Possible values are "created", "activated"
     */
    private String eventName;
    private String productMasterCode;
    private String source;

    public ProductMasterEventModel() {
        this.source = "pcb";
    }

    public ProductMasterEventModel(String productCode, String eventName) {
        this();
        this.productMasterCode = productCode;
        this.eventName = eventName;
    }

    public String getEventName() {
        return eventName;
    }

    public void setEventName(String eventName) {
        this.eventName = eventName;
    }

    public String getProductMasterCode() {
        return productMasterCode;
    }

    public void setProductMasterCode(String productMasterCode) {
        this.productMasterCode = productMasterCode;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }
}
