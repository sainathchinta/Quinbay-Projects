package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductL3CommonImageResponse;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.response.DistributionInfo;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ProductL3DetailsResponse extends ProductLevel3DetailResponse implements Serializable {
  private static final long serialVersionUID = -2012351970676576529L;

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
  private List<ProductLevel3AttributeResponse> attributes = new ArrayList<>();
  private List<ProductL3CommonImageResponse> commonImages = new ArrayList<>();
  private String url;
  private Boolean installationRequired;
  private String categoryId;
  private String accessChannel;
  private boolean forceReview;
  private boolean wholesalePriceConfigEnabled;
  private ProductScoreResponse productScore;
  private boolean isSuspended;
  private boolean isRejected;
  private boolean isArchived;
  private List<ProductItemLevel3LogisticResponse> productLevel3Logistics = new ArrayList<>();
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
  private boolean productEditable;
  private boolean off2OnChannelActive;
  private Long version;
  private String settlementType;
  private String merchantCode;
  private boolean markForDelete;
  private boolean takenDown;
  private Boolean isLateFulfillment;
  private List<String> pickupPointCodes;
  private PreOrderResponse preOrder;
  private ProductL3Response productL3Response;
  private int resubmitCount;
  private boolean freeSample;
  private boolean activeFreeSamplePromo;
  private boolean cncActivated;
  private boolean online;
  private boolean activeL5Mapped;
  private boolean fbbActivated;
  private boolean b2cActivated;
  private boolean b2bActivated;
  private List<String> fbbPickupPointCodes = new ArrayList<>();
  private boolean bundleProduct;
  private String sizeChartCode;
  private Boolean dimensionsMissing;
  private String videoUrl;
  private String coverImagePath;
  private DistributionInfo distributionInfoResponse;
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;
}