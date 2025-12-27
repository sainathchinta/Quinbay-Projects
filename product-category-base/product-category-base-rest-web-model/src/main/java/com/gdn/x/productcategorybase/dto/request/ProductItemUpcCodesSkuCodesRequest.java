package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemUpcCodesSkuCodesRequest implements Serializable {

  private static final long serialVersionUID = -593831264333380871L;

  private List<String> upcCodes = new ArrayList<>();
  private List<String> skuCodes = new ArrayList<>();
}
