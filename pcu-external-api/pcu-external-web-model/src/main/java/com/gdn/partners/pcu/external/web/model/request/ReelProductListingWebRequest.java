package com.gdn.partners.pcu.external.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReelProductListingWebRequest implements Serializable {
  private static final long serialVersionUID = -2594616863638052554L;
  private String merchantCode;
  private String keyword;
  private List<String> categoryCodes = new ArrayList<>();
  private Boolean inStock;
  private Boolean tradingProduct;
  private List<String> productSkuList = new ArrayList<>();
}
