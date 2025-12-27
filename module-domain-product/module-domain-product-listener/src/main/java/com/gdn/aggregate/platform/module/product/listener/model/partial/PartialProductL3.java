package com.gdn.aggregate.platform.module.product.listener.model.partial;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Category;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class PartialProductL3 extends BaseData {

    private String productCode;

    private String productType;

    private String merchantCode;

    @JsonProperty("synchronized")
    private boolean sync;

    private Review review;

    private CustomMerchant merchant;

    private String name;

    private String urlName;

    private String productSku;

    private String brand;

    private String brandLogoUrl;

    private List<Category> masterCategories;

    private List<Category> salesCategories;

}
