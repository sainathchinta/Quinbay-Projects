package com.gdn.aggregate.platform.module.product.listener.model.processed;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Category;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Fbb;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Flashsale;
import com.gdn.aggregate.platform.module.product.listener.model.sub.FlashsaleInventory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Measurement;
import com.gdn.aggregate.platform.module.product.listener.model.sub.PickupPointLocation;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;
import java.util.Map;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.SIVA_PRODUCT)
public class SivaProduct extends BaseData {

    private String name;

    private String urlName;

    private String url;

    private String productSku;

    private List<String> itemSkus;

    private String productCode;

    private String productType;

    private String merchantCode;

    private Measurement measurement;

    @JsonProperty("synchronized")
    private boolean sync;

    private String brand;

    private String brandLogoUrl;

    private List<Category> masterCategories;

    private List<Category> salesCategories;

    private String image;

    private FinalPrice offlinePrice;

    private FinalPrice price;

    private PriceTeaser priceTeaser;

    private Map<String, CampaignInfo> campaignInfos;

    private ViewSchedule buyable;

    private ViewSchedule discoverable;

    private SivaProduct.ViewSchedule buyableCnc;

    private SivaProduct.ViewSchedule discoverableCnc;

    private Flashsale flashsale;

    private Flashsale subFlashsale;

    private List<String> flashsaleItemSkus;

    private FlashsaleInventory flashsaleInventory;

    private List<Quota> adjustments;

    private Stock inventory;

    private List<String> campaignCodes;

    private List<String> campaignInfoCodes;

    private Review review;

    private List<String> tags;

    private CustomMerchant merchant;

    private String pickupPointCode;

    private String externalPickupPointCode;

    private String purchasedType;

    private int purchasedTypeScore;

    private String offlinePickupPointCode;

    private String offlineExternalPickupPointCode;

    private String offlinePurchasedType;

    private int offlinePurchasedTypeScore;

    private List<PickupPointLocation> nearestPickupPoints;

    private boolean fbbActivated;

    private Fbb fbb;

    private Fbb nonFbb;

    private boolean hasCncActivated;

    private boolean onlineOnly;

    private boolean hasBuyableCnc;

    private boolean hasDiscoverableCnc;

    private boolean archived;

    private List<String> channelId;

    private Map<String,Long> minVersion;

    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.productSku);
        return MainUtil.toList(id);
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ViewSchedule {

        private boolean value;

        private Long start;

        private Long end;

    }

    @Data
    @SuperBuilder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PriceSchedule {

        private String itemSku;

        private String pickupPointCode;

        private String externalPickupPointCode;

        private String adjustmentName;

        private String campaignCode;

        private Integer priority;

        private Long start;

        private Long end;

        private Double value;

        private boolean activated;

    }

    @Data
    @SuperBuilder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CampaignPriceSchedule {

        private String campaignCode;

        private String campaignName;

        private Long start;

        private Long end;

        private Double discount;

        private int quota;

        private String campaignPrice;

        private String campaignTag;

        private double finalPrice;

    }

    @Data
    @SuperBuilder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FinalPrice {

        private String list;

        private String offer;

        private PriceSchedule adjustment;

        private Double listValue;

        private Double baseOfferValue;

        private Double offerValue;

        private Double minOfferValue;

        private Double maxOfferValue;

        private Integer discount;

        private String uniqueId;

        private String purchasedType;

        private int purchasedTypeScore;

        private String cheapestItemSku;

        private String cheapestPickupPointCode;

        private String cheapestExternalPickupPointCode;

        private Integer cheapestPriceDays;

    }

    @Data
    @SuperBuilder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PriceTeaser {

        private Long start;

        private Long end;

        private String campaignPrice;

        private String campaignTag;

        private String campaignCode;

    }

}
