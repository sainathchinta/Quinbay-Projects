package com.gdn.partners.pcu.external.web.model.response;

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
public class ProductLevel3WipDetailWebResponse {

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
  private List<ProductItemLevel3LogisticsWebResponse> productLevel3Logistics;
  private List<ProductLevel3ItemWipWebResponse> items;
  private List<ProductLevel3AttributeWipWebResponse> attributes;
}
