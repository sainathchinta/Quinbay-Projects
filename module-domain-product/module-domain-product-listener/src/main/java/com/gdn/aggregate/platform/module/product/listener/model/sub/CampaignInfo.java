package com.gdn.aggregate.platform.module.product.listener.model.sub;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Flashsale;
import com.gdn.aggregate.platform.module.product.listener.model.sub.FlashsaleInventory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.PickupPointLocation;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CampaignInfo {

    private String campaignCode;

    private String campaignName;

    private String tagLabel;

    private Integer sessionId;

    private SivaProduct.ViewSchedule buyable;

    private SivaProduct.ViewSchedule discoverable;

    private SivaProduct.ViewSchedule buyableCnc;

    private SivaProduct.ViewSchedule discoverableCnc;

    private SivaProduct.FinalPrice price;

    private String purchasedType;

    private int purchasedTypeScore;

    private SivaProduct.FinalPrice offlinePrice;

    private String offlinePurchasedType;

    private int offlinePurchasedTypeScore;

    private SivaProduct.PriceTeaser priceTeaser;

    private SivaProduct.PriceTeaser currentPriceTeaser;

    private Flashsale flashsale;

    private Flashsale subFlashsale;

    private List<Quota> adjustments;

    private Stock inventory;

    private FlashsaleInventory flashsaleInventory;

    private List<String> flashsaleItemSkus;

    private List<String> itemSkus;

    private String productSku;

    private String cheapestItemSku;

    private String offlineCheapestItemSku;

    private String pickupPointCode;

    private String externalPickupPointCode;

    private String offlinePickupPointCode;

    private String offlineExternalPickupPointCode;

    private List<PickupPointLocation> nearestPickupPoints;

    private boolean fbbActivated;

    private String image;

    private String productUrl;

    private String itemUrl;

    private Long startTime;

    private Long endTime;

    private boolean activated;

}
