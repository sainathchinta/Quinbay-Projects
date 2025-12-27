package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSkuDetailResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -7819164651493432668L;
  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String imageUrl;
  private String brandName;
  private String brandCode;
  private boolean suspended;
  private boolean markForDelete;
  private boolean rejected;
  private boolean prelive;
}
