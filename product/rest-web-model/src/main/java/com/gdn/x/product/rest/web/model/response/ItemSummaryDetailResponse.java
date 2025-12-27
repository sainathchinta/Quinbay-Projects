package com.gdn.x.product.rest.web.model.response;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.OfflineItemPriceDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSummaryDetailResponse extends BaseResponse {
  private String itemSku;
  private String itemCode;
  private String merchantSku;
  private String generatedItemName;
  private MasterCatalogDTO masterCatalog;
  private String categoryName;
  private List<SalesCatalogDTO> salesCatalogs;
  private Set<PriceDTO> price;
  private Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<ItemViewConfigDTO>();
  private Set<ItemViewConfigDTO> itemViewConfigB2b = new HashSet<>();
  private ProductType productType;
  private String pickupPointCode;
  private List<MasterDataItemImageDTO> masterDataItemImages;
  private Boolean isLateFulfillment;
  private B2bFieldsDTO b2bFields;
  private String merchantCode;
  private String ticketTemplateCode;
  private String productSku;
  private boolean off2OnChannelActive;
  private String productCode;
  private String brand;
  private boolean isArchived;
  private boolean markForDelete;
  private boolean promoBundling;
  private boolean cncActivated;
  private boolean fbbActivated;
  private List<OfflineItemPriceDTO> offlinePrices;
  private String productName;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
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
  private boolean freeSample;
  private double originalPrice;
  private Set<BundleRecipeVo> bundleRecipe;
  private boolean distribution;
}
