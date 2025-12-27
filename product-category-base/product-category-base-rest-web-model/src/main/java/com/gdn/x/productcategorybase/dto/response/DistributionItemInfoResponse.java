package com.gdn.x.productcategorybase.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class DistributionItemInfoResponse implements Serializable {

  private String origin;
  private String omniChannelSku;
  private boolean expiry;
}
