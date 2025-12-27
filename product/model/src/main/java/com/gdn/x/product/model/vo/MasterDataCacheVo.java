package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataCacheVo implements Serializable {

  private static final long serialVersionUID = -3695312305176979937L;

  private ProductDetailResponse productResponse;
  private String brandLogoUrl;
}
