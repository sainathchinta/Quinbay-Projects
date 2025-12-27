package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class HalalDashboardProductsWebResponse {
  private String brand;
  private String categoryCode;
  private String productCode;
  private String productName;
  private String productSku;
  private Date productCreationDate;
  private String curationStatus;
  private String productLink;
  private boolean halalProduct;
}
