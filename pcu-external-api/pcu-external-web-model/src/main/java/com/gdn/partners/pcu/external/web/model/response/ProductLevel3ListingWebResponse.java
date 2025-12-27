package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ListingWebResponse {

  private String productSku;
  private String productCode;
  private String merchantCode;
  private String categoryCode;
  private String categoryName;
  private String catalogCode;
  private String productName;
  private String brand;
  private String productDetailPageLink;
  private String productMainImage;
  private boolean isActiveImage = true;
  private int variantCount;
  private boolean off2OnActiveFlag;
  private boolean isArchived;
  private boolean markForDelete;
  private boolean suspended;
  private long minSellingPrice;
  private long maxSellingPrice;
  private long minNormalPrice;
  private long maxNormalPrice;
  private int availableStockLevel1;
  private int reservedStockLevel1;
  private int  availableStockLevel2;
  private int reservedStockLevel2;
  private List<String> promoLabels;
  private ProductScoreResponse productScore;
  private ItemL3ListingWebResponse itemSummary;
  private Date createdDate;
  private Date updatedDate;
  private String suspensionReason;
  private boolean showL3Stock = true;
  private boolean freeSample;
  private Boolean dimensionsMissing;
  private Integer productType;
}
