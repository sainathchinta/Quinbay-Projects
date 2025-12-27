package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductEditInfoV2WebRequest {

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
  private Boolean online;
  private Boolean cncActivated;
  private Boolean fbbActivated;
  private Boolean fbb;
  private String categoryId;
  private Long version;
  private boolean productEditable;
  private boolean off2OnChannelActive;
  private String merchantCode;
  private List<ProductLevel3AttributeWebRequest> attributes = new ArrayList<>();
  private Integer resubmitCount;
  private boolean freeSample;
  private boolean activeFreeSamplePromo;
  private boolean needCorrection;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private List<ProductLevel3LogisticsWebRequest> logistics;
  private PreOrderWebRequest preOrder;
  private String defaultPickupPointCode;
  private boolean markDefaultAddress;
  private boolean differentLocation;
  private String productStory;
  private Boolean b2bActivated;
  private Boolean b2cActivated;
  private boolean bundleProduct;
  private List<ProductVariantPriceStockAndImagesWebRequest> productItems = new ArrayList<>();
  private List<DeletedProductItemsWebRequest> deletedProductItems = new ArrayList<>();
  private List<ProductLevel3CommonImageWebRequest> commonImages = new ArrayList<>();
  private List<ItemPickupPointWebRequest> addPickupPoints;
  private List<ItemPickupPointDeleteWebRequest> deletePickupPoints;
  private List<ProductBundleRecipeWebRequest> productBundleRecipe = new ArrayList<>();
  private String sizeChartCode;
  private boolean sizeChartChanged;
  private VideoAddEditRequest videoAddEditRequest;
  private Boolean videoUpdated;
  private Map<String, String> distributionInfoRequest;
  private List<ProductItemUomInfoWebRequest> distributionAndUOMRequest;
}
