package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;
import java.util.List;

/**
 * Created by riteshkumar on 23/02/17.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateImageRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    private String itemSku;

    private String productSku;

    private String productCode;

    private List<ProductLevel3ImageRequest> masterDataItemImages;

    public UpdateImageRequest() {};

    public UpdateImageRequest(String itemSku, String productSku, String productCode,
        List<ProductLevel3ImageRequest> masterDataItemImages) {
        this.itemSku = itemSku;
        this.productSku = productSku;
        this.productCode = productCode;
        this.masterDataItemImages = masterDataItemImages;
    }

    public String getItemSku() {
        return itemSku;
    }

    public void setItemSku(String itemSku) {
        this.itemSku = itemSku;
    }

    public String getProductSku() {
        return productSku;
    }

    public void setProductSku(String productSku) {
        this.productSku = productSku;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public List<ProductLevel3ImageRequest> getMasterDataItemImages() {
        return masterDataItemImages;
    }

    public void setMasterDataItemImages(List<ProductLevel3ImageRequest> masterDataItemImages) {
        this.masterDataItemImages = masterDataItemImages;
    }

    @Override
    public String toString() {
        return "UpdateImageRequest{" + "itemSku='" + itemSku + '\'' + ", productSku='" + productSku
            + '\'' + ", productCode='" + productCode + '\'' + ", masterDataItemImages="
            + masterDataItemImages + '}';
    }
}
