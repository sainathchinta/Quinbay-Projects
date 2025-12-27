package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ItemLevel4WebRequest {
  private static final long serialVersionUID = -1177214241336502181L;
  private Set<String> productSkus;

  public ItemLevel4WebRequest(Set<String> productSkus) {
    this.productSkus = productSkus;
  }

  private boolean needRevision = false;

}