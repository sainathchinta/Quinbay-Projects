package com.gdn.partners.pcu.external.web.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3DetailWebResponse {

  private static final long serialVersionUID = -3170967969331156399L;
  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private Boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String categoryName;
  private String categoryNameEnglish;
  private String categoryHierarchy;
  private String categoryHierarchyEnglish;
  private String brand;
  private String brandCode;
  private String brandApprovalStatus;
  private String description;
  private String specificationDetail;
  private String uniqueSellingPoint;
  private String url;
  private Boolean installationRequired;
  private String categoryId;
  private Long version;
  private boolean wholesalePriceConfigEnabled;
  private ProductScoreResponse productScore;
  private boolean productEditable;
  private String accessChannel;
  private boolean forceReview;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String defaultItemSku;
  private int itemCount;
  private boolean merchantPromoDiscount;
  private boolean disableUnSync;
  private Boolean wholesalePriceActivated;
  private boolean off2OnChannelActive;
  private String settlementType;
  private String merchantCode;
  private boolean markForDelete;
  private boolean takenDown;
  private Boolean isLateFulfillment;
  private List<String> pickupPointCodes = new ArrayList<>();
  private List<String> promoLabels;
  private List<ProductItemLevel3LogisticsWebResponse> productLevel3Logistics = new ArrayList<>();
  private List<ProductLevel3AttributeWebResponse> attributes = new ArrayList<>();
  private List<ProductL3CommonImageWebResponse> commonImages = new ArrayList<>();
  private PreOrderResponse preOrder;
  private int resubmitCount;
  private boolean freeSample;
  private boolean activeFreeSamplePromo;
  private boolean isSuspended;
  private boolean isRejected;
  private boolean isArchived;
  private Boolean cogsViewable;
  private boolean cncActivated;
  private boolean online;
  private String productDetailPageLink;
  private boolean activeL5Mapped;
  private boolean fbbActivated;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private boolean bundleProduct;
  private String sizeChartCode;
  private String sizeChartName;
  private String sizeChartBusinessPartnerCode;
  private List<String> fbbPickupPointCodes = new ArrayList<>();
  private Boolean dimensionsMissing;
  private String videoUrl;
  private String coverImagePath;
  private DistributionInfoWebResponse distributionInfoResponse;
  private boolean aiGeneratedCategory;
  private boolean aiGeneratedBrand;
}
