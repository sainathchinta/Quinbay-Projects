package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBasicResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 6325119141330739323L;

  private String productSku;
  private String productCode;
  private String productName;
  private String merchantCode;
  private boolean forceReview;
  private boolean archived;
  private boolean takenDown;
  private boolean markForDelete;
  private boolean bundleProduct;
  private String categoryCode;
  private ProductType productType;
  private boolean b2cActivated;
  private boolean off2OnChannelActive;
  private boolean sharedProduct;
  private boolean syncProduct;
}
