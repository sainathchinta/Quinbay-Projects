package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.SalesCatalog;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class  ItemSummaryResponseVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String storeId;
  private String itemSku;
  private String itemCode;
  private String merchantSku;
  private String generatedItemName;
  private MasterCatalog masterCatalog;
  private String categoryName;
  private List<SalesCatalog> salesCatalogs;
  private Set<Price> price;
  private double originalSellingPrice;
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
  private Set<ItemViewConfig> itemViewConfigB2b = new HashSet<>();
  private ProductType productType;
  private String pickupPointCode;
  private List<MasterDataItemImage> masterDataItemImages;
  private Boolean isLateFulfillment;
  private String merchantCode;
  private String ticketTemplateCode;
  private String productSku;
  private boolean off2OnChannelActive;
  private Date createdDate;
  private String brand;
  private String productCode;
  private Date updatedDate;
  private boolean isArchived;
  private boolean markForDelete;
  private boolean promoBundling;
  private boolean cncActivated;
  private boolean fbbActivated;
  private List<OfflineItemPriceVO> offlinePrices;
  private String productName;
  private B2bFields b2bFields;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private Set<String> priceEditDisabledReasons;
  private List<String> promoTypes;
  private boolean forceReview;
  private Boolean wholesalePriceActivated;
  private double productScore;
  private Long version;
  private Set<String> activePromoBundlings;
  private String pristineId;
  private String upcCode;
  private boolean flashSaleActive;
  private boolean wholesalePriceExists;
  private boolean suspended;
  private boolean buyable;
  private boolean discoverable;
  private boolean isPreOrder;
  private boolean freeSample;
  private Set<String> sellerActivePromoBundlings;
  private boolean cncActive;
  private Set<BundleRecipeVo> bundleRecipe;
  private boolean distribution;
  private Date preOrderDate;

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

}
