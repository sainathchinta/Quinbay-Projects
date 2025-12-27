package com.gdn.aggregate.platform.module.product.listener.constants;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;

import java.util.List;

public interface PartialClasses {

    String PRODUCT_REVIEW_CHANGE_EVENT = "ProductReviewChangeEvent";
    String STORE_CLOSING_CHANGE_EVENT = "PartialStoreClosingMerchantChangeEvent";
    String PRODUCT = "Product";
    String TAGS = "Tags";
    String CLEAN_SIVA_FLASHSALE_SCHEDULE = "CleanSivaFlashsaleSchedule";
    String CLEAN_SIVA_FLASHSALE_GROUP = "CleanSivaFlashsaleGroup";
    String DEACTIVATE_SIVA_FLASHSALE_SCHEDULE = "DeactivateSivaFlashsaleSchedule";
    String DEACTIVATE_SIVA_FLASHSALE_GROUP = "DeactivateSivaFlashsaleGroup";
    String END_SIVA_CAMPAIGN_PRODUCT = "EndSivaCampaignProduct";
    List<String> ALLOWED_CLASSES = MainUtil.toList(
        PRODUCT_REVIEW_CHANGE_EVENT,
        STORE_CLOSING_CHANGE_EVENT,
        PRODUCT,
        TAGS,
        CLEAN_SIVA_FLASHSALE_SCHEDULE,
        CLEAN_SIVA_FLASHSALE_GROUP,
        DEACTIVATE_SIVA_FLASHSALE_SCHEDULE,
        DEACTIVATE_SIVA_FLASHSALE_GROUP,
        END_SIVA_CAMPAIGN_PRODUCT
    );

}
