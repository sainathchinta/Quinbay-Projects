package com.gdn.x.product.rest.web.model.response;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.VideoUrl;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSummaryListResponse extends BaseResponse {

  private static final long serialVersionUID = 1746895691063478L;
  private String itemSku;
  private String pickupPointCode;
  private String itemCode;
  private String merchantSku;
  private String productName;
  private String itemName;
  private Set<PriceDTO> price;
  private double originalSellingPrice;
  private Set<ItemViewConfigDTO> itemViewConfigs;
  private List<ProductAttributeDetailDTO> definingAttributes;
  private String mainImageUrl;
  private String merchantCode;
  private String productSku;
  private List<String> salesCategoryCode;
  private String masterCategoryCode;
  private boolean cncActive;
  private boolean fbbActivated;
  private List<ItemCatalogVO> itemCatalogs;
  private String pickupPointName;
  private boolean freeSample;
  private boolean merchantPromoDiscount;
  private boolean off2OnChannelActive;
  private Boolean wholesalePriceActivated;
  private ProductType productType;
  private boolean forceReview;
  private boolean lateFulfillment;
  private boolean archived;
  private boolean merchantPromoDiscountActivated;
  private boolean promoBundling;
  private boolean markForDelete;
  private String brandName;
  private String productCode;
  private boolean b2cActivated;
  private boolean b2bActivated;
  private B2bFieldsDTO b2bFields;
  private boolean halalProduct;
  private boolean suspended;
  private boolean subscribable;
  private boolean subscribableAtL5Level;
  private String sizeChartCode;
  private Boolean dimensionsMissing;
  private Set<String> missingFields = new HashSet<>();
  private String url;
  private VideoUrl video;
  private boolean distribution;
}
