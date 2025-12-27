package com.gdn.mta.bulk.models;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.BaseRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = false)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductMasterDataEditRequest extends BaseRequest {
  private static final long serialVersionUID = 5745683537652821084L;
  private String productCode;
  private String productSku;
  private String businessPartnerCode;
  private String description;
  private String url;
  private VideoAddEditRequest videoAddEditRequest;
  private String productName;
  private Integer productType;
  private String sizeChartCode;
  private Boolean b2cActivated;
  private boolean instore;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String categoryCode;
  private String categoryName;
  private List<ProductLevel3SummaryDetailsImageRequest> productLevel3SummaryDetailsImageRequests = new ArrayList<>();
  private Set<L3InfoUpdateChangeType> masterDataEditChangeTypes = EnumSet.noneOf(L3InfoUpdateChangeType.class);
  private boolean pureInstoreProduct;
  private boolean officialStoreSeller;
  private boolean sellerBopisFlag;
  private boolean sellerBigProductFlag;
  private boolean trustedSeller;
  private Integer dangerousGoodsLevel;
  private boolean videoDelete;
  private String brand;
  private String uniqueSellingPoint;
}
