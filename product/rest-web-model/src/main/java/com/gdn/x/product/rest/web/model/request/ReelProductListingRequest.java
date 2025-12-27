package com.gdn.x.product.rest.web.model.request;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Data;

@Data
public class ReelProductListingRequest implements Serializable {
  @Serial
  private static final long serialVersionUID = 105110669606323576L;
  private String merchantCode;
  private String keyword;
  private List<String> categoryCodes = new ArrayList<>();
  private Boolean inStock;
  private Boolean tradingProduct;
  private List<String> productSkuList = new ArrayList<>();
}
