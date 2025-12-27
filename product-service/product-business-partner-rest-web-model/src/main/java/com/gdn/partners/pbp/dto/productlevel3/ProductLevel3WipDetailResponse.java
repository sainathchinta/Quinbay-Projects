package com.gdn.partners.pbp.dto.productlevel3;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3WipDetailResponse extends BaseResponse {

  private static final long serialVersionUID = -7356207766425368280L;

  private String businessPartnerCode;
  private String businessPartnerName;
  private String productLevel1Id;
  private String productSku;
  private String productCode;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String brandName;
  private boolean active = false;
  private List<ProductLevel3Logistics> productLevel3Logistics = new ArrayList<>();
  private List<ProductLevel3ItemWipResponse> items = new ArrayList<>();
  private List<ProductLevel3AttributeWipResponse> attributes = new ArrayList<>();
  private String state;
  private String notes;

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3WipDetailResponse [businessPartnerCode=");
    builder.append(businessPartnerCode);
    builder.append(", businessPartnerName=");
    builder.append(businessPartnerName);
    builder.append(", productLevel1Id=");
    builder.append(productLevel1Id);
    builder.append(", productSku=");
    builder.append(productSku);
    builder.append(", productCode=");
    builder.append(productCode);
    builder.append(", productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", categoryName=");
    builder.append(categoryName);
    builder.append(", brandName=");
    builder.append(brandName);
    builder.append(", active=");
    builder.append(active);
    builder.append(", items=");
    builder.append(items);
    builder.append(", attributes=");
    builder.append(attributes);
    builder.append(", state=");
    builder.append(state);
    builder.append(", notes=");
    builder.append(notes);
    builder.append("]");
    return builder.toString();
  }
}
