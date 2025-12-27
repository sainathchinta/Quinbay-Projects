package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class WholesaleConfigDTO {

    private Integer quantity;
    private List<MinWholesaleDiscountDTO> minWholesaleDiscount = new ArrayList<>();
}
