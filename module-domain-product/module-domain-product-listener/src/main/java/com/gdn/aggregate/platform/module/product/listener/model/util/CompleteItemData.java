package com.gdn.aggregate.platform.module.product.listener.model.util;

import java.util.ArrayList;
import java.util.List;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CompleteItemData {
  private List<Product> allProducts = new ArrayList<>();
  private List<Item> allItems = new ArrayList<>();
  private List<PickupPoint> allPickupPoints = new ArrayList<>();
  private List<AdjustmentProductQuota> allAdjustmentProductQuotas = new ArrayList<>();
  private List<AdjustmentProduct> allAdjustmentProducts = new ArrayList<>();
  private List<CustomInventoryPickupPointInfo> allCustomInventoryPickupPointInfos = new ArrayList<>();
  private List<CustomInventoryInfo> allCustomInventoryInfos = new ArrayList<>();
  private List<FlashsaleProduct> allFlashSaleProducts = new ArrayList<>();
  private List<MerchantDiscountPrice> allMerchantDiscountPrice = new ArrayList<>();
  private List<CampaignProduct> allCampaignProducts = new ArrayList<>();
  private List<CheapestPriceDay> allCheapestPriceDays = new ArrayList<>();
  private List<MasterDataItem> allMasterDataItem = new ArrayList<>();
  private List<MasterDataProduct> allMasterDataProduct = new ArrayList<>();
  private List<CustomPcbCategory> allCustomPcbCategory = new ArrayList<>();
  private List<CustomMerchant> allCustomMerchants = new ArrayList<>();
  private List<CustomBusinessPartnerPickupPoint> allCustomBusinessPartnerPickupPoint = new ArrayList<>();
  private List<CustomProductReview> allCustomProductReviews = new ArrayList<>();
  private List<SivaItem> allSivaItems = new ArrayList<>();
  private List<SivaProduct> allSivaProduct = new ArrayList<>();
  private List<PickupPointInventory> allPickupPointInventory = new ArrayList<>();
  private StockUpdateSearchEvent stockUpdateSearchEvent;
  private Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent;
  private boolean fetchStockFromInventoryModule;
}
