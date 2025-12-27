package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Data
public class SkuCodesRequest implements Serializable {

  private static final long serialVersionUID = 5478237836286871380L;
  private List<String> skuCodes = new ArrayList<String>();
  private boolean fetchArchived;
  private Boolean fetchImageResponse;

  public SkuCodesRequest() {}

  public SkuCodesRequest(List<String> skuCodes) {
    super();
    this.skuCodes = skuCodes;
  }
}
