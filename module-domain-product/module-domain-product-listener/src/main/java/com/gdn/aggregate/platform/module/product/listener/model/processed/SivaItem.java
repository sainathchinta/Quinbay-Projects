package com.gdn.aggregate.platform.module.product.listener.model.processed;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.other.CalculatedItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
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
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
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
@Document(collection = Collections.SIVA_ITEM)
public class SivaItem extends BaseData {

    private boolean onL2;

    private String itemName;

    private String productName;

    private String itemUrlName;

    private String productUrlName;

    private String itemUrl;

    private String productUrl;

    private String itemSku;

    private String productSku;

    private String itemCode;

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

    private String itemImage;

    private String productImage;

    /*will be removed in SP17*/
    private List<MasterDataItem.MasterDataItemAttributeValue> masterDataItemAttributeValues;
    private String description;
    private String breadCrumb;
    private Category levelCategory;
    private Category topLevelCategory;
    private SivaProduct.FinalPrice offlineItemPrice;
    private CalculatedItem.Price itemPrice;
    private SivaProduct.PriceTeaser itemPriceTeaser;
    /*End of will be removed in SP17*/

    private SivaProduct.FinalPrice offlinePrice;

    private SivaProduct.FinalPrice price;

    private SivaProduct.PriceTeaser priceTeaser;

    private Map<String, CampaignInfo> campaignInfos;

    private SivaProduct.ViewSchedule buyable;

    private SivaProduct.ViewSchedule discoverable;

    private SivaProduct.ViewSchedule buyableCnc;

    private SivaProduct.ViewSchedule discoverableCnc;

    private Flashsale flashsale;

    private Flashsale subFlashsale;

    private String flashsaleItemSku;

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

    private boolean off2OnChannelActiveItem;

    private boolean off2OnChannelActiveProduct;

    private boolean subscribable;

    private boolean hasCncActivated;

    private boolean onlineOnly;

    private boolean hasBuyableCnc;

    private boolean hasDiscoverableCnc;

    private boolean archived;

    private List<String> channelId;

    private Map<String,Long> minVersion;

    private String omniChannelSku;

    @Override
    public List<String> toIds() {
        return ModuleProductUtil.toSivaItemIds(this);
    }

}
