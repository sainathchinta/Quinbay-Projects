package com.gdn.aggregate.platform.module.product.listener.model.partial;

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
public class PartialProductL4 extends BaseData {

    private String productCode;

    private String productType;

    private String merchantCode;

    private boolean off2OnChannelActiveProduct;

    private Review review;

    private CustomMerchant merchant;

    private String productName;

    private String productUrlName;

    private String productSku;

    private String brand;

    private String brandLogoUrl;

    private List<Category> masterCategories;

    private List<Category> salesCategories;

    /*will be removed in SP17*/
    private String description;
    private String breadCrumb;
    private Category levelCategory;
    private Category topLevelCategory;
    /*End of will be removed in SP17*/

}
