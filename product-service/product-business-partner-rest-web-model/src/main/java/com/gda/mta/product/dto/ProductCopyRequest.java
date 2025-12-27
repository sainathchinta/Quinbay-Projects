package com.gda.mta.product.dto;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Map;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCopyRequest extends BaseDTORequest {

  private static final long serialVersionUID = -7236352811854366398L;

  /* item skus for each product sku */
  private Map<String, List<String>> gdnItemSkus;

  private String pickupPointCode;

  /* target business partner code */
  private String businessPartnerCode;

  /* source business partner code */
  private String sourceBusinessPartnerCode;

  @Override
  public String toString() {
    return new ToStringBuilder(this)
      .append("gdnItemSkus", gdnItemSkus)
      .append("businessPartnerCode", businessPartnerCode)
      .append("sourceBusinessPartnerCode", sourceBusinessPartnerCode)
      .append("pickupPointCode", pickupPointCode)
      .toString();
  }

}
