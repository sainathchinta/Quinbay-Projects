package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCodesResponse extends BaseResponse {

    private static final long serialVersionUID = -1674436208772709113L;
    private List<String> productCodes;

    public List<String> getProductCodes() {
        return productCodes;
    }

    public void setProductCodes(List<String> productCodes) {
        this.productCodes = productCodes;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("ProductCodesResponse{");
        sb.append("productCodes=").append(productCodes);
        sb.append('}');
        return sb.toString();
    }
}
