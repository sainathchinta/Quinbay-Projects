package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.AiGeneratedFieldsResponse;
import com.gdn.x.product.rest.web.model.dto.DistributionInfoDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCategorySequenceDTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3Response extends BaseResponse {
  private static final long serialVersionUID = 1L;

  private String productSku;
  private String productCode;
  private ProductType productType;
  private String settlementType;
  private String merchantCode;
  private boolean isSynchronized;
  private boolean isSuspended;
  private boolean isArchived;
  private boolean markForDelete;
  private String productCatentryId;
  private MasterCatalogDTO masterCatalog;
  private List<SalesCatalogDTO> salesCatalogs;
  private MasterDataProductDTO masterDataProduct;
  private List<ProductAttributeDTO> definingAttributes = new ArrayList<ProductAttributeDTO>();
  private List<ProductAttributeDetailDTO> descriptiveAttributes = new ArrayList<ProductAttributeDetailDTO>();
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private List<ItemCatalogDTO> itemCatalogs = new ArrayList<ItemCatalogDTO>();
  private List<SalesCategorySequenceDTO> salesCategorySequences;
  private boolean installationRequired;
  private boolean off2OnChannelActive;
  private boolean forceReview;
  private ProductScoreResponse productScore;
  private PreOrderDTO preOrderDTO;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String defaultItemSku;
  private int itemCount;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActive;
  private boolean disableUnSync;
  private boolean wholesalePriceExists;
  private Boolean wholesalePriceActivated;
  private boolean productEditable;
  private Long version;
  private boolean takenDown;
  private Boolean isLateFulfillment;
  private List<String> pickupPointCodes;
  private List<String> itemSkus;
  private List<String> promoLabels;
  private boolean freeSample;
  private Map<String, String> itemSkuItemCodeMap;
  private boolean activeFreeSamplePromo;
  private boolean cncActivated;
  private boolean online;
  private boolean activeL5Mapped = true;
  private Date productCenterUpdatedDate;
  private String etdNotes;
  private boolean fbbActivated;
  private List<String> fbbPickupPointCodes;
  private boolean otherFbbPickupPointCodesMapped = false;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private boolean bundleProduct;
  private String sizeChartCode;
  private Boolean dimensionsMissing;
  private String videoUrl;
  private String coverImagePath;
  private String url;
  private Integer dangerousLevel;
  private DistributionInfoDTO distributionInfoDTO;
  private String distributionMappingStatus = Constants.NON_DISTRIBUTION;
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;
}
