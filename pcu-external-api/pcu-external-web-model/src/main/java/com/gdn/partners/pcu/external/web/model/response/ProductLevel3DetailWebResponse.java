package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3DetailWebResponse {
  private static final long serialVersionUID = 5396987035880931557L;
  private String id;
  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private Boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String categoryName;
  private String categoryHierarchy;
  private String brand;
  private String description;
  private String specificationDetail;
  private String uniqueSellingPoint;
  private String productStory;
  private String url;
  private Boolean installationRequired;
  private String categoryId;
  private Long version;
  private boolean wholesalePriceConfigEnabled;
  private ProductScoreResponse productScore;
  private boolean isSuspended;
  private boolean productEditable;
  private String accessChannel;
  private boolean forceReview;
  private boolean isRejected;
  private boolean isArchived;
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
  private boolean off2OnChannelActive;
  private String settlementType;
  private String merchantCode;
  private boolean markForDelete;
  private boolean takenDown;
  private Boolean isLateFulfillment;
  private Boolean cogsViewable;
  private List<String> pickupPointCodes;
  private List<String> promoLabels;
  private List<ProductItemLevel3LogisticsWebResponse> productLevel3Logistics = new ArrayList();
  private List<ProductLevel3AttributeWebResponse> attributes = new ArrayList<>();
  private List<ProductLevel3ImageWebResponse> images = new ArrayList<>();
  private PreOrderResponse preOrder;
  private int resubmitCount;
  private boolean freeSample;
  private boolean activeFreeSamplePromo;
}
