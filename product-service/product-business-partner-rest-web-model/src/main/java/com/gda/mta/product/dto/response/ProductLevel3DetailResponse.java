package com.gda.mta.product.dto.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3DetailResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -3170967969331156399L;
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
  private List<ProductLevel3AttributeResponse> attributes = new ArrayList<>();
  private List<ProductLevel3ImageResponse> images = new ArrayList<>();
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
  private boolean merchantPromoDiscountActive;
  private boolean disableUnSync;
  private boolean wholesalePriceExists;
  private Boolean wholesalePriceActivated;
  private boolean enableEdit;
  private boolean productEditable;
  private boolean off2OnChannelActive;
  private Long version;
  private String settlementType;
  private String merchantCode;
  private boolean markForDelete;
  private boolean takenDown;
  private Boolean isLateFulfillment;
  private List<String> pickupPointCodes;
  private List<String> itemSkus;
  private PreOrderResponse preOrder;
  private ProfileResponse profileResponse;
  private ProductL3Response productL3Response;
  private int resubmitCount;
  private boolean freeSample;
  private boolean activeFreeSamplePromo;
  private List<String> fbbPickupPointCodes;
  private String sizeChartCode;
  private boolean sizeChartChanged;
}
