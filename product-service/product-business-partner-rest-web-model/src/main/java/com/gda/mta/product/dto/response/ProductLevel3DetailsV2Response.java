package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductL3CommonImageResponse;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3DetailsV2Response extends ProductL3DetailsResponse
  implements Serializable {

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
  private String brandCode;
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
  private boolean postLive;
  private String state;
  private String revisionNotes;
  private String rejectReason;
  private String productCreatedDate;
  private String createdBy;
  private String productUpdatedDate;
  private String updatedBy;
  private String needCorrectionNotes;
  private Integer totalStock;
  private double maxNormalPrice;
  private double maxSellingPrice;
  private double minNormalPrice;
  private double minSellingPrice;
  private int totalWebStock;
  private int totalWarehouseStock;
  private int totalActiveStock;
  private boolean fbbActivated;
  private boolean isSynchronized;
  private DistributionInfo distributionInfoResponse;
}
