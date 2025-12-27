package com.gdn.x.product.model.solr;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSolr implements Serializable {

  private static final long serialVersionUID = -720020507991795754L;

  private String productSku; // Unique key
  private String productCode;
  private String productName;
  private String merchantCode;
  private String masterCatalog; // catalogCode#categoryCode
  private String brand;
  private String storeId;
  private boolean isSynchronized;
  private boolean markForDelete;
  private boolean isSuspended;
  private Date createdDate;
  private Date updatedDate;
  private List<String> salesCatalog;
  private String productMainImage;
  private Double productScoreTotal;
  private Date productCenterUpdatedDate;
  private List<String> pickupPointCodes;
  private Boolean isArchived;
  private Boolean off2OnChannelActive;
  private List<String> promoItemSkus;
  private Boolean inStock;
  private Boolean isPreOrderActive;
  private List<String> wholesaleItemSkus;
  private Integer variantCount;
  private Double maximumSellingPrice;
  private Double minimumSellingPrice;
  private Double maximumListPrice;
  private Double minimumListPrice;
  private boolean freeSample;
  private boolean cncActive;
  private boolean tradingProduct;
  private int l5Count;
  private boolean fbbActivated;
  private boolean b2bActivated;
  private Boolean b2cActivated;
  private int curationStatus;
  private boolean bundleProduct;
  private String sizeChartCode;
  private boolean productCategoryEligibleForSizeChart;
  private int distributionStatus;
}
