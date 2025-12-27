package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.VideoAddEditRequest;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3UpdateRequest extends BaseRequest implements MasterDataUpdateRequest {
  private static final long serialVersionUID = -8611382808482810163L;

  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String categoryName;
  private String categoryHierarchy;
  private String brand;
  private String brandCode;
  private String description;
  private String specificationDetail;
  private String uniqueSellingPoint;
  private String url;
  private String categoryId;
  private Long version;
  private boolean productEditable;
  private boolean off2OnChannelActive;
  private String merchantCode;
  private List<ProductLevel3AttributeRequest> attributes;
  private boolean freeSample;
  private boolean activeFreeSamplePromo;
  private boolean needCorrection;
  private Boolean online;
  private Boolean cnc;
  private Boolean fbbActiveAtL3Level;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private List<ProductLevel3Logistics> productLevel3LogisticsRequest;
  private Boolean lateFulfillment;
  private PreOrderRequest preOrder;
  private String defaultPickupPointCode;
  private boolean markDefaultAddress;
  private boolean differentLocation;
  private Boolean b2bActivated;
  private Boolean b2cActivated;
  private List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
  private List<DeletedProductItems> deletedProductItems = new ArrayList<>();
  private String accessChannel;
  private List<ProductLevel3SummaryDetailsImageRequest> commonImages = new ArrayList<>();
  private List<ItemPickupPointRequest> addPickupPoints = new ArrayList<>();
  private List<PickupPointDeleteRequest> deletePickupPoints = new ArrayList<>();
  private Integer dangerousGoodsLevel;
  private Boolean installationRequired;
  private String productStory;
  private boolean bundleProduct;
  private String sizeChartCode;
  private boolean sizeChartChanged;
  private Boolean videoUpdated;
  private VideoAddEditRequest videoAddEditRequest;
  private List<ProductBundleRecipeRequest> productBundleRecipe = new ArrayList<>();
  private boolean pureInstoreProduct;
  private boolean sellerOmg;
  private Map<String, String> distributionInfoRequest;
  private List<ProductItemDistributionInfoRequest> distributionAndUOMRequest;

  @Override
  public Set<L3InfoUpdateChangeType> getMasterDataEditChangeTypes() {
    return Collections.emptySet();
  }
}
