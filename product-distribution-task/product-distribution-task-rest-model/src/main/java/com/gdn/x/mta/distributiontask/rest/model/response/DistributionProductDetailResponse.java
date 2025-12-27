package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Poornima on 9/20/16.
 */
public class DistributionProductDetailResponse extends DistributionProductResponse {

    private static final long serialVersionUID = 2599086034367413735L;
    private List<DistributionProductImageResponse> productImages = new ArrayList<>();
    private List<DistributionProductItemResponse> productItems = new ArrayList<>();
    private List<DistributionProductAttributeResponse> productAttributes = new ArrayList<>();

    public DistributionProductDetailResponse() {
        //no implementation
    }

    public List<DistributionProductImageResponse> getProductImages() {
        return productImages;
    }

    public void setProductImages(
        List<DistributionProductImageResponse> productImages) {
        this.productImages = productImages;
    }

    public List<DistributionProductItemResponse> getProductItems() {
        return productItems;
    }

    public void setProductItems(
        List<DistributionProductItemResponse> productItems) {
        this.productItems = productItems;
    }

    public List<DistributionProductAttributeResponse> getProductAttributes() {
        return productAttributes;
    }

    public void setProductAttributes(
        List<DistributionProductAttributeResponse> productAttributes) {
        this.productAttributes = productAttributes;
    }
}
