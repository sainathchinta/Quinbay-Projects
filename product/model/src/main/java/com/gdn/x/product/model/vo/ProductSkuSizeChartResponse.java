package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSkuSizeChartResponse extends BaseResponse implements Serializable {
    private static final long serialVersionUID = -7016921149604335534L;

    private String productSku;
    private String sizeChartCode;
}