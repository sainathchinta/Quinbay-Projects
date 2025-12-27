package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.mta.product.commons.constant.DimensionHolder;
import com.gdn.mta.product.entity.VideoAddEditRequest;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

@EqualsAndHashCode(callSuper = false)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductMasterDataEditRequest extends BaseRequest
  implements MasterDataUpdateRequest, DimensionHolder {
  @Serial
  private static final long serialVersionUID = 8594257704238017752L;
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
  public boolean sellerBopisFlag;
  public boolean sellerBigProductFlag;
  private boolean trustedSeller;
  private Integer dangerousGoodsLevel;
  private String accessChannel;
  private boolean videoDelete;
  private String brand;
  private String uniqueSellingPoint;
  private boolean publishImageQcForContentChange;

  @Override
  public boolean isOff2OnChannelActive() {
    return this.instore;
  }
}
